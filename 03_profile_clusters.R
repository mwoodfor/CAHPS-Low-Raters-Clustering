## ============================================================
## CAHPS Low-Rater Segmentation: Cluster Profiling
## Step 3 of analytic plan — profiles the clusters produced in
## 02_run_clustering.R, in original (non-z-scored) units, with
## summary tables, plots, and a suggested business-friendly name
## per cluster for the user to review and edit.
## ============================================================

library(dplyr)
library(tidyr)
library(RColorBrewer)

## ------------------------------------------------------------
## 0. CONFIG
## ------------------------------------------------------------
plot_dir   <- "cluster_profile_plots"   # subfolder for saved plots
top_n_features <- 5                      # how many distinguishing features
                                          # to surface per cluster for naming

dir.create(plot_dir, showWarnings = FALSE)

## ------------------------------------------------------------
## 1. LOAD DATA
## ------------------------------------------------------------
cluster_assignments <- readRDS("cluster_assignments.rds")          # MEM_NUM, cluster, PRV_GROUP
original_units      <- readRDS("cluster_features_original_units.rds")  # 26 features, original units
member_ids          <- readRDS("member_ids.rds")                   # MEM_NUM, same row order as original_units

stopifnot(nrow(cluster_assignments) == nrow(original_units))
stopifnot(all(member_ids$MEM_NUM == cluster_assignments$MEM_NUM))

profile_data <- cbind(
  cluster = cluster_assignments$cluster,
  original_units,
  PRV_GROUP = cluster_assignments$PRV_GROUP
)

cat("Profiling data:", nrow(profile_data), "members across",
    length(unique(profile_data$cluster)), "clusters\n")
cat("\nCluster sizes:\n")
print(table(profile_data$cluster))

## ------------------------------------------------------------
## 2. VARIABLE BLOCKS (mirrors 01_build_feature_set.R structure)
## ------------------------------------------------------------
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

categorical_vars <- c("MEM_GENDER", "member_plan", "MA_REGION")
profiling_only_categorical <- c("PRV_GROUP")  # descriptive only, not a clustering input

## ------------------------------------------------------------
## 3. CONTINUOUS VARIABLE SUMMARY TABLE (mean + median by cluster)
## ------------------------------------------------------------
continuous_summary <- profile_data %>%
  group_by(cluster) %>%
  summarise(across(all_of(all_continuous_vars),
                    list(mean = ~round(mean(.x, na.rm = TRUE), 2),
                         median = ~round(median(.x, na.rm = TRUE), 2)))) %>%
  ungroup()

## Reshape to a more readable long format: one row per variable per cluster
continuous_summary_long <- continuous_summary %>%
  pivot_longer(-cluster, names_to = "variable_stat", values_to = "value") %>%
  mutate(
    stat = sub(".*_(mean|median)$", "\\1", variable_stat),
    variable = sub("_(mean|median)$", "", variable_stat)
  ) %>%
  select(cluster, variable, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(variable, cluster)

cat("\n=== Continuous variable summary (mean & median by cluster) ===\n")
print(continuous_summary_long, n = 50)

## Overall population benchmark (for "how does this cluster differ from
## the overall low-rater sample" comparisons used in naming, section 6)
continuous_overall <- profile_data %>%
  summarise(across(all_of(all_continuous_vars),
                    list(mean = ~round(mean(.x, na.rm = TRUE), 2)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "overall_mean") %>%
  mutate(variable = sub("_mean$", "", variable))

## ------------------------------------------------------------
## 4. BINARY VARIABLE SUMMARY TABLE (prevalence % by cluster)
## ------------------------------------------------------------
binary_summary <- profile_data %>%
  group_by(cluster) %>%
  summarise(across(all_of(binary_vars), ~round(100 * mean(.x, na.rm = TRUE), 1))) %>%
  ungroup()

binary_summary_long <- binary_summary %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "pct_yes") %>%
  arrange(variable, cluster)

cat("\n=== Binary variable prevalence (% = 1, by cluster) ===\n")
print(binary_summary_long, n = 50)

binary_overall <- profile_data %>%
  summarise(across(all_of(binary_vars), ~round(100 * mean(.x, na.rm = TRUE), 1))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "overall_pct_yes")

## ------------------------------------------------------------
## 5. CATEGORICAL VARIABLE SUMMARY TABLES (distribution % by cluster)
## ------------------------------------------------------------
categorical_summary <- list()
for (v in c(categorical_vars, profiling_only_categorical)) {
  tab <- profile_data %>%
    group_by(cluster, .data[[v]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(cluster) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    ungroup() %>%
    rename(level = !!v)
  categorical_summary[[v]] <- tab
  cat("\n=== ", v, " distribution (%) by cluster ===\n")
  print(tab %>% select(cluster, level, pct) %>% arrange(cluster, desc(pct)), n = 30)
}

## ------------------------------------------------------------
## 6. DISTINGUISHING FEATURES PER CLUSTER (for naming)
## ------------------------------------------------------------
## For each cluster, rank continuous variables by how far the cluster mean
## is from the overall sample mean, in standard-deviation units (a simple,
## transparent effect-size measure -- not a substitute for the SHAP-based
## driver analysis planned for later once those values are available, but
## a reasonable first pass for suggesting cluster names now).
overall_sd <- profile_data %>%
  summarise(across(all_of(all_continuous_vars), ~sd(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "overall_sd")

cluster_means <- profile_data %>%
  group_by(cluster) %>%
  summarise(across(all_of(all_continuous_vars), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "cluster_mean")

distinguishing_features <- cluster_means %>%
  left_join(continuous_overall, by = "variable") %>%
  left_join(overall_sd, by = "variable") %>%
  mutate(
    effect_size = (cluster_mean - overall_mean) / overall_sd,
    direction = if_else(effect_size > 0, "higher", "lower")
  ) %>%
  group_by(cluster) %>%
  arrange(desc(abs(effect_size)), .by_group = TRUE) %>%
  slice_head(n = top_n_features) %>%
  ungroup()

cat("\n=== Top", top_n_features, "distinguishing continuous features per cluster ===\n")
cat("(effect_size = (cluster mean - overall mean) / overall sd; sign shows direction)\n")
print(distinguishing_features %>%
        select(cluster, variable, cluster_mean, overall_mean, effect_size, direction),
      n = 50)

## Also flag binary variables with notably different prevalence by cluster
binary_distinguishing <- binary_summary_long %>%
  left_join(binary_overall, by = "variable") %>%
  mutate(pct_diff = pct_yes - overall_pct_yes) %>%
  group_by(cluster) %>%
  arrange(desc(abs(pct_diff)), .by_group = TRUE) %>%
  slice_head(n = 3) %>%
  ungroup()

cat("\n=== Top distinguishing binary features per cluster (vs. overall prevalence) ===\n")
print(binary_distinguishing %>% select(cluster, variable, pct_yes, overall_pct_yes, pct_diff), n = 30)

## ------------------------------------------------------------
## 7. SUGGESTED CLUSTER NAMES (draft only -- user reviews/edits)
## ------------------------------------------------------------
## This is a simple rule-based first pass using the top distinguishing
## feature per cluster, meant as a starting point for discussion, not a
## final label. Edit suggested_names below after reviewing the tables and
## plots, since business context the script doesn't have (e.g. what
## marketing actually calls these member types) should drive the final name.

cat("\n============================================================\n")
cat("SUGGESTED CLUSTER NAMES (DRAFT -- REVIEW AND EDIT)\n")
cat("============================================================\n")

suggested_names <- list()
for (cl in sort(unique(profile_data$cluster))) {
  top_feat <- distinguishing_features %>% filter(cluster == cl) %>% slice(1)
  label <- paste0("Cluster ", cl, ": ", top_feat$direction, " ", top_feat$variable,
                   " (", round(top_feat$effect_size, 2), " SD from overall)")
  suggested_names[[as.character(cl)]] <- label
  cat(label, "\n")
}

cat("\nThese are mechanical first-pass labels based on the single largest\n")
cat("effect size per cluster. Review the full distinguishing-feature table\n")
cat("and plots before finalizing business-friendly names (e.g. translating\n")
cat("'higher total_grievances' into something like 'high-friction members').\n")

## ------------------------------------------------------------
## 8. PLOTS — boxplots (continuous) and bar charts (binary/categorical)
## ------------------------------------------------------------
k <- length(unique(profile_data$cluster))
cluster_colors <- if (k > 2) brewer.pal(max(k, 3), "Set3")[1:k] else c("lightblue", "orange")[1:k]

## 8a. Continuous variables -> boxplots, original units, one file per variable
for (v in all_continuous_vars) {
  png(file.path(plot_dir, paste0("boxplot_", v, ".png")), width = 700, height = 500)
  boxplot(profile_data[[v]] ~ profile_data$cluster,
          col = cluster_colors, main = v, xlab = "Cluster", ylab = v)
  dev.off()
}
cat("\nSaved", length(all_continuous_vars), "boxplots to", plot_dir, "/\n")

## 8b. Binary variables -> bar charts of prevalence %, one file per variable
for (v in binary_vars) {
  tab <- profile_data %>%
    group_by(cluster) %>%
    summarise(pct = 100 * mean(.data[[v]], na.rm = TRUE)) %>%
    arrange(cluster)
  png(file.path(plot_dir, paste0("barplot_", v, ".png")), width = 700, height = 500)
  barplot(tab$pct, names.arg = tab$cluster, col = cluster_colors,
          main = paste0(v, " (% = 1)"), xlab = "Cluster", ylab = "Percent",
          ylim = c(0, max(tab$pct) * 1.2))
  dev.off()
}
cat("Saved", length(binary_vars), "bar plots to", plot_dir, "/\n")

## 8c. Categorical variables (incl. PRV_GROUP) -> stacked/grouped bar charts
for (v in c(categorical_vars, profiling_only_categorical)) {
  tab <- table(profile_data[[v]], profile_data$cluster)
  tab_pct <- apply(tab, 2, function(col) 100 * col / sum(col))
  png(file.path(plot_dir, paste0("barplot_", v, ".png")), width = 900, height = 550)
  barplot(tab_pct, beside = TRUE, col = brewer.pal(max(nrow(tab_pct), 3), "Paired")[1:nrow(tab_pct)],
          main = paste0(v, " distribution by cluster (%)"),
          xlab = "Cluster", ylab = "Percent", legend.text = rownames(tab_pct),
          args.legend = list(x = "topright", cex = 0.7, bty = "n"))
  dev.off()
}
cat("Saved", length(c(categorical_vars, profiling_only_categorical)), "categorical plots to", plot_dir, "/\n")

## ------------------------------------------------------------
## 9. SAVE TABLES
## ------------------------------------------------------------
write.csv(continuous_summary_long, "profile_continuous_summary.csv", row.names = FALSE)
write.csv(binary_summary_long, "profile_binary_summary.csv", row.names = FALSE)
for (v in names(categorical_summary)) {
  write.csv(categorical_summary[[v]],
            paste0("profile_categorical_", v, ".csv"), row.names = FALSE)
}
write.csv(distinguishing_features, "profile_distinguishing_features.csv", row.names = FALSE)

cat("\nSaved profile_*.csv summary tables and", plot_dir, "/*.png plots.\n")
cat("Cluster profiling complete.\n")
