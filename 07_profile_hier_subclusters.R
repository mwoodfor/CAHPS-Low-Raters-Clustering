## ============================================================
## CAHPS Low-Rater Segmentation: Cluster 3 Sub-Cluster Profiling
## DIAGNOSTIC BRANCH -- profiles the hierarchical sub-clusters from
## 06_hierarchical_clustering.R the same way 03/04a profile the
## main k-prototypes clusters, EXCEPT the baseline for comparison is
## the Cluster 3 average (not the full low-rater population), since
## the question here is "does this sub-cluster differ from the rest
## of Cluster 3," not "does it differ from everyone."
## Produces the same interactive-heatmap deliverable format as
## 04c_interactive_heatmap.html, one level down.
##
## NOTE: drafted without an R interpreter available to test-run
## against real data (see note at the top of 05). Review before
## trusting end to end.
## ============================================================

library(dplyr)
library(tidyr)

## ------------------------------------------------------------
## 0. CONFIG
## ------------------------------------------------------------
top_n_per_subcluster <- 5
max_total_rows        <- 16

subcluster_names <- NULL   # e.g. c("1" = "3a: ...", "2" = "3b: ...") -- set after
                            # reviewing the distinguishing-feature table below;
                            # falls back to "Sub-cluster N" if left NULL

## ------------------------------------------------------------
## 1. LOAD & ASSEMBLE
## ------------------------------------------------------------
hier_assignments  <- readRDS("cluster3_hier_assignments.rds")           # MEM_NUM, hier_subcluster
original_units_26 <- readRDS("cluster3_hier_original_units_26.rds")     # MEM_NUM, cluster, PRV_GROUP, 26 features (orig units)
interaction_orig  <- read.csv("cluster3_hier_interaction_features_original_units.csv",
                               stringsAsFactors = FALSE, colClasses = c(MEM_NUM = "character"))

profile_data <- hier_assignments %>%
  left_join(original_units_26, by = "MEM_NUM") %>%
  left_join(interaction_orig, by = "MEM_NUM")

n_unmatched <- sum(is.na(profile_data$cluster))
if (n_unmatched > 0) {
  stop(n_unmatched, " sub-cluster member(s) failed to match the original-units ",
       "feature tables -- investigate before proceeding.")
}
stopifnot(nrow(profile_data) == nrow(hier_assignments))

cat("Profiling data:", nrow(profile_data), "Cluster 3 members across",
    length(unique(profile_data$hier_subcluster)), "sub-clusters\n")
cat("\nSub-cluster sizes:\n")
print(table(profile_data$hier_subcluster))

## ------------------------------------------------------------
## 2. VARIABLE BLOCKS (26 original + 14 curated interaction, original units)
## ------------------------------------------------------------
skewed_continuous_26 <- c(
  "total_pharm_oop", "total_pharm_allow", "total_supply_days",
  "abandoned_scripts", "total_pharm_denied",
  "total_med_allow", "total_med_oop", "total_med_denied",
  "total_grievances", "total_appeals"
)
symmetric_continuous_26 <- c(
  "MEM_AGE", "tenure_years", "ses_index", "charlson_index",
  "ct_condit", "DXCG_RRS_EXP_CON", "family_members", "myblue_visits"
)
binary_26 <- c("lis_ind", "dis_ind", "email_optin", "mail_order_flag", "any_auth")
categorical_26 <- c("MEM_GENDER", "member_plan", "MA_REGION")

interaction_continuous <- c(
  "n_interactions_total", "n_distinct_reasons", "n_distinct_types",
  "days_since_last_interaction", "max_interactions_in_30d", "mean_duration_min"
)
interaction_binary <- c(
  "ever_type_chat", "ever_reason_medical_part_c_appeals_grievances",
  "repeat_same_reason_30d", "channel_switch", "billing_contact_ever",
  "any_long_interaction", "ever_benefit_discussed", "any_interaction"
)

all_continuous_vars <- c(skewed_continuous_26, symmetric_continuous_26, interaction_continuous)
all_binary_vars      <- c(binary_26, interaction_binary)
all_categorical_vars <- c(categorical_26, "dominant_reason", "PRV_GROUP")

## ------------------------------------------------------------
## 3. EFFECT SIZE VS. CLUSTER 3 OVERALL (not vs. the full low-rater
##    population -- this is the key difference from 03/04a)
## ------------------------------------------------------------
cluster3_overall_stats <- profile_data %>%
  summarise(across(all_of(c(all_continuous_vars, all_binary_vars)),
                    list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything(), names_to = "key", values_to = "value") %>%
  mutate(stat = sub(".*_(mean|sd)$", "\\1", key),
         variable = sub("_(mean|sd)$", "", key)) %>%
  select(variable, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)

subcluster_means <- profile_data %>%
  group_by(hier_subcluster) %>%
  summarise(across(all_of(c(all_continuous_vars, all_binary_vars)), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-hier_subcluster, names_to = "variable", values_to = "subcluster_mean")

effect_sizes <- subcluster_means %>%
  left_join(cluster3_overall_stats, by = "variable") %>%
  mutate(effect_size = (subcluster_mean - mean) / sd)

cat("\nEffect sizes (vs. Cluster 3 overall) computed for",
    length(unique(effect_sizes$variable)), "variables across",
    length(unique(effect_sizes$hier_subcluster)), "sub-clusters\n")

## ------------------------------------------------------------
## 4. CATEGORICAL DISTRIBUTIONS (incl. dominant_reason, PRV_GROUP)
## ------------------------------------------------------------
categorical_summary <- list()
for (v in all_categorical_vars) {
  tab <- profile_data %>%
    group_by(hier_subcluster, .data[[v]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(hier_subcluster) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    ungroup() %>%
    rename(level = !!v)
  categorical_summary[[v]] <- tab
  write.csv(tab, paste0("cluster3_hier_profile_categorical_", v, ".csv"), row.names = FALSE)
}
cat("Saved categorical distribution tables for:", paste(all_categorical_vars, collapse = ", "), "\n")

## ------------------------------------------------------------
## 5. CURATE HEATMAP ROW SET (mirrors 04a's top-N-per-group,
##    deduplicated, capped logic)
## ------------------------------------------------------------
top_per_subcluster <- effect_sizes %>%
  group_by(hier_subcluster) %>%
  arrange(desc(abs(effect_size)), .by_group = TRUE) %>%
  slice_head(n = top_n_per_subcluster) %>%
  ungroup()

variable_priority <- top_per_subcluster %>%
  group_by(variable) %>%
  summarise(max_abs_effect = max(abs(effect_size))) %>%
  arrange(desc(max_abs_effect))

curated_vars <- variable_priority %>% slice_head(n = max_total_rows) %>% pull(variable)

cat("\nCurated to", length(curated_vars), "rows for the sub-cluster heatmap:\n")
print(curated_vars)

cat("\n=== Largest single effect size found (vs. Cluster 3 overall) ===\n")
best <- effect_sizes %>% arrange(desc(abs(effect_size))) %>% slice(1)
print(best)
cat("For reference: the interaction-feature SMD screen against the FULL\n")
cat("population topped out at |SMD| = 0.104. If the value above is still\n")
cat("under ~0.2, the sub-clusters found here are no more differentiated\n")
cat("than what the flat comparisons already showed -- hierarchical\n")
cat("clustering did not surface meaningfully distinct groups within\n")
cat("Cluster 3, even though it partitions the members into non-trivially-\n")
cat("sized subsets.\n")

## ------------------------------------------------------------
## 6. BUILD & SAVE HEATMAP DATA + STANDALONE INTERACTIVE HTML
## ------------------------------------------------------------
heatmap_data <- effect_sizes %>%
  filter(variable %in% curated_vars) %>%
  mutate(variable = factor(variable, levels = curated_vars)) %>%
  select(cluster = hier_subcluster, variable,
         cluster_mean = subcluster_mean, overall_mean = mean, effect_size) %>%
  arrange(variable, cluster)

saveRDS(heatmap_data, "cluster3_hier_heatmap_data.rds")
write.csv(heatmap_data, "cluster3_hier_heatmap_data.csv", row.names = FALSE)

subcluster_ids <- sort(unique(heatmap_data$cluster))
if (is.null(subcluster_names)) {
  subcluster_names <- setNames(paste0("Sub-cluster ", subcluster_ids), subcluster_ids)
}
missing_names <- setdiff(as.character(subcluster_ids), names(subcluster_names))
if (length(missing_names) > 0) {
  stop("subcluster_names config is missing names for sub-cluster(s): ",
       paste(missing_names, collapse = ", "))
}

variable_labels_default <- c(
  total_grievances = "Grievances filed", ct_condit = "Chronic conditions (count)",
  charlson_index = "Comorbidity burden (Charlson)", total_med_oop = "Medical out-of-pocket cost",
  total_med_allow = "Medical allowed cost", any_auth = "Any prior authorization",
  total_pharm_oop = "Pharmacy out-of-pocket cost", total_pharm_allow = "Pharmacy allowed cost",
  total_supply_days = "Pharmacy supply days", abandoned_scripts = "Abandoned prescriptions",
  total_pharm_denied = "Pharmacy claims denied", total_med_denied = "Medical claims denied",
  total_appeals = "Appeals filed", DXCG_RRS_EXP_CON = "Predicted risk score",
  MEM_AGE = "Member age", tenure_years = "Tenure (years)", ses_index = "Socioeconomic index",
  family_members = "Household size", myblue_visits = "Member portal visits",
  lis_ind = "Low-income subsidy", dis_ind = "Disability status", email_optin = "Email opt-in",
  mail_order_flag = "Mail-order pharmacy user",
  n_interactions_total = "Total live interactions", n_distinct_reasons = "# distinct contact reasons",
  n_distinct_types = "# distinct channels used",
  days_since_last_interaction = "Days since last contact",
  max_interactions_in_30d = "Peak contacts in any 30-day period",
  mean_duration_min = "Avg interaction length (min)",
  ever_type_chat = "Ever used Chat",
  ever_reason_medical_part_c_appeals_grievances = "Ever contacted about: Appeals/Grievances",
  repeat_same_reason_30d = "Repeated same reason within 30 days",
  channel_switch = "Ever switched channel within 7 days",
  billing_contact_ever = "Ever contacted about billing/premium",
  any_long_interaction = "Ever had a long interaction (>= p90)",
  ever_benefit_discussed = "Ever discussed a specific benefit",
  any_interaction = "Any live contact in window"
)
get_label <- function(v) if (v %in% names(variable_labels_default)) variable_labels_default[[v]] else v

json_rows <- heatmap_data %>%
  mutate(label = sapply(as.character(variable), get_label)) %>%
  rowwise() %>%
  mutate(json_row = sprintf(
    '{"cluster":%d,"variable":"%s","label":"%s","cluster_mean":%s,"overall_mean":%s,"effect_size":%s}',
    cluster, variable, label, round(cluster_mean, 3), round(overall_mean, 3), round(effect_size, 3)
  )) %>%
  ungroup() %>%
  pull(json_row)

data_js <- paste0("[\n", paste(json_rows, collapse = ",\n"), "\n]")
cluster_names_js <- paste0("{", paste(sprintf('%d:"%s"', subcluster_ids,
                                               subcluster_names[as.character(subcluster_ids)]),
                                       collapse = ", "), "}")
subcluster_sizes <- profile_data %>% count(hier_subcluster) %>% arrange(hier_subcluster)
cluster_sizes_js <- paste0("{", paste(sprintf('%d:%d', subcluster_sizes$hier_subcluster,
                                               subcluster_sizes$n), collapse = ", "), "}")

html_template <- '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>Cluster 3 Sub-Cluster Signature Heatmap</title>
<style>
  body { font-family: -apple-system, "Segoe UI", Arial, sans-serif; background: #fafafa; padding: 2rem; color: #1a1a1a; }
  .container { max-width: 900px; margin: 0 auto; background: white; border-radius: 12px; padding: 1.5rem 2rem 2rem; box-shadow: 0 1px 4px rgba(0,0,0,0.08); }
  h1 { font-size: 18px; font-weight: 600; margin: 0 0 4px; }
  p.subtitle { font-size: 13px; color: #666; margin: 0 0 1.25rem; }
  #tooltip { position: absolute; display: none; background: white; border: 1px solid #ddd; border-radius: 8px; padding: 8px 12px; font-size: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.12); pointer-events: none; z-index: 10; max-width: 220px; }
</style>
</head>
<body>
<div class="container" style="position: relative;">
  <h1>Cluster 3 sub-clusters (hierarchical): how each differs from the Cluster 3 average</h1>
  <p class="subtitle">Values are standard deviations from the Cluster 3 overall mean (NOT the full low-rater population).</p>
  <div style="overflow-x: auto;">
    <div id="heatmap-grid" style="display: grid; gap: 4px; min-width: 600px;"></div>
  </div>
  <div style="display: flex; align-items: center; gap: 12px; margin-top: 1.25rem; font-size: 12px; color: #666;">
    <span>Lower than Cluster 3 average</span>
    <div style="flex: 1; height: 10px; border-radius: 5px; background: linear-gradient(to right, #185FA5, #ffffff, #993C1D);"></div>
    <span>Higher than Cluster 3 average</span>
  </div>
  <div id="tooltip"></div>
</div>
<script>
const data = __DATA__;
const clusterNames = __CLUSTER_NAMES__;
const clusterSizes = __CLUSTER_SIZES__;
const clusters = Object.keys(clusterNames).map(Number);
const variables = [...new Set(data.map(d => d.variable))];
const labels = {};
data.forEach(d => labels[d.variable] = d.label);
const maxAbs = Math.max(...data.map(d => Math.abs(d.effect_size)));

function colorFor(v) {
  const t = v / maxAbs;
  if (t >= 0) {
    const r = Math.round(255 - t * (255-153));
    const g = Math.round(255 - t * (255-60));
    const b = Math.round(255 - t * (255-28));
    return `rgb(${r},${g},${b})`;
  } else {
    const at = -t;
    const r = Math.round(255 - at * (255-24));
    const g = Math.round(255 - at * (255-95));
    const b = Math.round(255 - at * (255-165));
    return `rgb(${r},${g},${b})`;
  }
}

function renderGrid() {
  const grid = document.getElementById("heatmap-grid");
  grid.innerHTML = "";
  grid.style.gridTemplateColumns = `220px repeat(${clusters.length}, 1fr)`;
  grid.appendChild(document.createElement("div"));
  clusters.forEach(c => {
    const head = document.createElement("div");
    head.style.textAlign = "center";
    head.style.padding = "8px 4px";
    const nameEl = document.createElement("div");
    nameEl.textContent = clusterNames[c];
    nameEl.style.fontWeight = "600";
    nameEl.style.fontSize = "13px";
    const sizeEl = document.createElement("div");
    sizeEl.textContent = "n = " + clusterSizes[c].toLocaleString();
    sizeEl.style.fontSize = "11px";
    sizeEl.style.color = "#888";
    sizeEl.style.marginTop = "2px";
    head.appendChild(nameEl);
    head.appendChild(sizeEl);
    grid.appendChild(head);
  });
  variables.forEach(v => {
    const rowLabel = document.createElement("div");
    rowLabel.textContent = labels[v];
    rowLabel.style.fontSize = "13px";
    rowLabel.style.display = "flex";
    rowLabel.style.alignItems = "center";
    rowLabel.style.color = "#555";
    grid.appendChild(rowLabel);
    clusters.forEach(c => {
      const cell = data.find(d => d.variable === v && d.cluster === c);
      const tile = document.createElement("div");
      tile.style.background = colorFor(cell.effect_size);
      tile.style.borderRadius = "6px";
      tile.style.display = "flex";
      tile.style.alignItems = "center";
      tile.style.justifyContent = "center";
      tile.style.minHeight = "52px";
      tile.style.fontWeight = "600";
      tile.style.fontSize = "14px";
      tile.style.color = Math.abs(cell.effect_size) > maxAbs * 0.55 ? "#fff" : "#1a1a1a";
      tile.textContent = (cell.effect_size >= 0 ? "+" : "") + cell.effect_size.toFixed(1);
      tile.addEventListener("mouseenter", () => {
        const tooltip = document.getElementById("tooltip");
        tooltip.innerHTML = `<div style="font-weight:600; margin-bottom:4px;">${clusterNames[c]}</div>` +
          `<div style="color:#666;">${labels[v]}</div>` +
          `<div style="margin-top:4px;">Sub-cluster average: <b>${cell.cluster_mean.toLocaleString(undefined, {maximumFractionDigits: 2})}</b></div>` +
          `<div>Cluster 3 average: <b>${cell.overall_mean.toLocaleString(undefined, {maximumFractionDigits: 2})}</b></div>` +
          `<div style="margin-top:4px;">${cell.effect_size >= 0 ? "+" : ""}${cell.effect_size.toFixed(2)} SD from Cluster 3 mean</div>`;
        tooltip.style.display = "block";
        const rect = tile.getBoundingClientRect();
        const containerRect = grid.getBoundingClientRect();
        tooltip.style.left = (rect.left - containerRect.left + rect.width/2 - 90) + "px";
        tooltip.style.top = (rect.top - containerRect.top - 105) + "px";
      });
      tile.addEventListener("mouseleave", () => { document.getElementById("tooltip").style.display = "none"; });
      grid.appendChild(tile);
    });
  });
}
renderGrid();
</script>
</body>
</html>'

html_output <- gsub("__DATA__", data_js, html_template, fixed = TRUE)
html_output <- gsub("__CLUSTER_NAMES__", cluster_names_js, html_output, fixed = TRUE)
html_output <- gsub("__CLUSTER_SIZES__", cluster_sizes_js, html_output, fixed = TRUE)

writeLines(html_output, "07c_cluster3_subcluster_heatmap.html")
cat("\nGenerated 07c_cluster3_subcluster_heatmap.html\n")
cat("Cluster 3 sub-cluster profiling complete.\n")
