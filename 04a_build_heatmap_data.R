## ============================================================
## CAHPS Low-Rater Segmentation: Cluster Signature Heatmap — Data Prep
## Builds a curated effect-size matrix (clusters x top distinguishing
## variables) from the outputs of 03_profile_clusters.R, for use in
## both a static ggplot2 heatmap (slide-ready) and an interactive
## HTML heatmap (exploration).
## ============================================================

library(dplyr)
library(tidyr)

## ------------------------------------------------------------
## 0. CONFIG
## ------------------------------------------------------------
top_n_per_cluster <- 4   # how many top distinguishing variables to pull
                          # FROM EACH cluster before deduplicating into
                          # one shared row set across all clusters
max_total_rows    <- 14  # hard cap on heatmap rows after dedup, so the
                          # chart stays scannable even if clusters share
                          # few top variables (worst case: k x top_n_per_cluster
                          # unique rows)

## EDIT CLUSTER NAMES HERE. Keys must match the cluster numbers produced by
## 02_run_clustering.R / 03_profile_clusters.R (usually 1..k). Names are
## baked into the generated HTML at the next run of this script -- there
## is no live editing in the HTML file itself, so re-run this script after
## changing names below.
cluster_names <- c(
  "1" = "Cluster 1",
  "2" = "Cluster 2",
  "3" = "Cluster 3",
  "4" = "Cluster 4"
)

## ------------------------------------------------------------
## 1. LOAD PROFILING OUTPUTS (from 03_profile_clusters.R)
## ------------------------------------------------------------
cluster_assignments <- readRDS("cluster_assignments.rds")
original_units      <- readRDS("cluster_features_original_units.rds")

profile_data <- cbind(cluster = cluster_assignments$cluster, original_units)

skewed_continuous_vars <- c(
  "total_pharm_oop", "total_pharm_allow", "total_supply_days",
  "abandoned_scripts", "total_pharm_denied",
  "total_med_allow", "total_med_oop", "total_med_denied",
  "total_grievances", "total_appeals"
)
symmetric_continuous_vars <- c(
  "MEM_AGE", "tenure_years", "ses_index", "charlson_index",
  "ct_condit", "DXCG_RRS_EXP_CON", "family_members", "myblue_visits"
)
all_continuous_vars <- c(skewed_continuous_vars, symmetric_continuous_vars)
binary_vars <- c("lis_ind", "dis_ind", "email_optin", "mail_order_flag", "any_auth")

## ------------------------------------------------------------
## 2. EFFECT SIZE FOR EVERY VARIABLE (continuous AND binary, same scale)
## ------------------------------------------------------------
## Binary variables are treated as 0/1 numeric here so they sit on the
## same standard-deviation scale as continuous variables -- this is what
## makes it valid to put them in the same heatmap.
all_vars_for_heatmap <- c(all_continuous_vars, binary_vars)

overall_stats <- profile_data %>%
  summarise(across(all_of(all_vars_for_heatmap),
                    list(mean = ~mean(.x, na.rm = TRUE),
                         sd   = ~sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything(), names_to = "key", values_to = "value") %>%
  mutate(
    stat = sub(".*_(mean|sd)$", "\\1", key),
    variable = sub("_(mean|sd)$", "", key)
  ) %>%
  select(variable, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)

cluster_means <- profile_data %>%
  group_by(cluster) %>%
  summarise(across(all_of(all_vars_for_heatmap), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "cluster_mean")

effect_sizes <- cluster_means %>%
  left_join(overall_stats, by = "variable") %>%
  mutate(effect_size = (cluster_mean - mean) / sd)

cat("Effect sizes computed for", length(unique(effect_sizes$variable)),
    "variables across", length(unique(effect_sizes$cluster)), "clusters\n")

## ------------------------------------------------------------
## 3. CURATE ROW SET: top N distinguishing variables per cluster,
##    deduplicated into one shared row list, capped at max_total_rows
## ------------------------------------------------------------
top_per_cluster <- effect_sizes %>%
  group_by(cluster) %>%
  arrange(desc(abs(effect_size)), .by_group = TRUE) %>%
  slice_head(n = top_n_per_cluster) %>%
  ungroup()

## Rank the deduplicated variable set by the MAX absolute effect size any
## cluster has on that variable, so the most cluster-defining variables
## (even if only relevant to one cluster) appear first
variable_priority <- top_per_cluster %>%
  group_by(variable) %>%
  summarise(max_abs_effect = max(abs(effect_size))) %>%
  arrange(desc(max_abs_effect))

curated_vars <- variable_priority %>%
  slice_head(n = max_total_rows) %>%
  pull(variable)

cat("\nCurated to", length(curated_vars), "rows (variables) for the heatmap:\n")
print(curated_vars)

## ------------------------------------------------------------
## 4. BUILD FINAL HEATMAP DATA (long format: cluster, variable, effect_size)
## ------------------------------------------------------------
heatmap_data <- effect_sizes %>%
  filter(variable %in% curated_vars) %>%
  mutate(variable = factor(variable, levels = curated_vars)) %>%
  select(cluster, variable, cluster_mean, overall_mean = mean, effect_size) %>%
  arrange(variable, cluster)

cat("\n=== Final heatmap data ===\n")
print(heatmap_data, n = 50)

## ------------------------------------------------------------
## 5. SAVE — for both the static ggplot2 script and the interactive HTML
## ------------------------------------------------------------
saveRDS(heatmap_data, "heatmap_data.rds")
write.csv(heatmap_data, "heatmap_data.csv", row.names = FALSE)

cat("\nSaved heatmap_data.rds / heatmap_data.csv\n")
cat("Ready for: 04b_static_heatmap.R (slide-ready PNG)\n")
cat("       and: 04c_interactive_heatmap.html (exploration)\n")

## ------------------------------------------------------------
## 6. GENERATE STANDALONE INTERACTIVE HTML (auto-embeds the data above,
##    so this file stays in sync with the curated row set every run --
##    no manual copy-paste of numbers required)
## ------------------------------------------------------------
js_label_lookup <- variable_labels_default <- c(
  total_grievances    = "Grievances filed",
  ct_condit            = "Chronic conditions (count)",
  charlson_index        = "Comorbidity burden (Charlson)",
  total_med_oop        = "Medical out-of-pocket cost",
  total_med_allow      = "Medical allowed cost",
  any_auth              = "Any prior authorization",
  total_pharm_oop      = "Pharmacy out-of-pocket cost",
  total_pharm_allow    = "Pharmacy allowed cost",
  total_supply_days    = "Pharmacy supply days",
  abandoned_scripts     = "Abandoned prescriptions",
  total_pharm_denied   = "Pharmacy claims denied",
  total_med_denied      = "Medical claims denied",
  total_appeals         = "Appeals filed",
  DXCG_RRS_EXP_CON     = "Predicted risk score",
  MEM_AGE                = "Member age",
  tenure_years           = "Tenure (years)",
  ses_index              = "Socioeconomic index",
  family_members         = "Household size",
  myblue_visits          = "Member portal visits",
  lis_ind                = "Low-income subsidy",
  dis_ind                = "Disability status",
  email_optin            = "Email opt-in",
  mail_order_flag        = "Mail-order pharmacy user"
)

## fall back to the raw variable name if no friendly label is defined above
get_label <- function(v) if (v %in% names(js_label_lookup)) js_label_lookup[[v]] else v

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

cluster_ids <- sort(unique(heatmap_data$cluster))

## Validate that the cluster_names config covers every cluster actually
## produced by the clustering step -- fail loudly rather than silently
## falling back to a default label if the user added/removed clusters
## without updating the config above.
missing_names <- setdiff(as.character(cluster_ids), names(cluster_names))
if (length(missing_names) > 0) {
  stop("cluster_names config is missing names for cluster(s): ",
       paste(missing_names, collapse = ", "),
       ". Update the cluster_names config at the top of this script.")
}

cluster_names_js <- paste0(
  "{", paste(sprintf('%d:"%s"', cluster_ids, cluster_names[as.character(cluster_ids)]),
             collapse = ", "), "}"
)

## Sample size per cluster, for display under each cluster header
cluster_sizes <- cluster_assignments %>%
  count(cluster) %>%
  arrange(cluster)

cluster_sizes_js <- paste0(
  "{", paste(sprintf('%d:%d', cluster_sizes$cluster, cluster_sizes$n), collapse = ", "), "}"
)

html_template <- '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>Cluster Signature Heatmap</title>
<style>
  body { font-family: -apple-system, "Segoe UI", Arial, sans-serif; background: #fafafa; padding: 2rem; color: #1a1a1a; }
  .container { max-width: 800px; margin: 0 auto; background: white; border-radius: 12px; padding: 1.5rem 2rem 2rem; box-shadow: 0 1px 4px rgba(0,0,0,0.08); }
  h1 { font-size: 18px; font-weight: 600; margin: 0 0 4px; }
  p.subtitle { font-size: 13px; color: #666; margin: 0 0 1.25rem; }
  #tooltip { position: absolute; display: none; background: white; border: 1px solid #ddd; border-radius: 8px; padding: 8px 12px; font-size: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.12); pointer-events: none; z-index: 10; max-width: 220px; }
</style>
</head>
<body>
<div class="container" style="position: relative;">
  <h1>Cluster signature: how each segment differs from the overall sample</h1>
  <p class="subtitle">Values are standard deviations from the overall low-rater sample mean.</p>
  <div style="overflow-x: auto;">
    <div id="heatmap-grid" style="display: grid; gap: 4px; min-width: 600px;"></div>
  </div>
  <div style="display: flex; align-items: center; gap: 12px; margin-top: 1.25rem; font-size: 12px; color: #666;">
    <span>Lower than average</span>
    <div style="flex: 1; height: 10px; border-radius: 5px; background: linear-gradient(to right, #185FA5, #ffffff, #993C1D);"></div>
    <span>Higher than average</span>
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
  grid.style.gridTemplateColumns = `200px repeat(${clusters.length}, 1fr)`;
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
          `<div style="margin-top:4px;">Cluster average: <b>${cell.cluster_mean.toLocaleString(undefined, {maximumFractionDigits: 2})}</b></div>` +
          `<div>Overall average: <b>${cell.overall_mean.toLocaleString(undefined, {maximumFractionDigits: 2})}</b></div>` +
          `<div style="margin-top:4px;">${cell.effect_size >= 0 ? "+" : ""}${cell.effect_size.toFixed(2)} SD from overall mean</div>`;
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

writeLines(html_output, "04c_interactive_heatmap.html")
cat("Generated 04c_interactive_heatmap.html (data embedded automatically)\n")
