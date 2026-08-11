## ============================================================
## CAHPS Low-Rater Segmentation: Cluster 3 Provider Group & Region
## Descriptives
## Step 8 of analytic plan -- Cluster 3 ("The Common Low Rater") is
## the largest segment (~60% of the low-rater sample) and the one
## the original 26-feature k-prototypes run could not meaningfully
## separate from the rest of the population (see 03/04a). The
## hierarchical sub-clustering attempt on interaction/contact data
## (05-07) also failed to find internal substructure. This script
## looks at two variables that were deliberately EXCLUDED from (or
## not distinguishing in) the clustering itself -- provider group
## and geographic region -- to see whether Cluster 3 concentrates in
## particular provider groups or regions even though it doesn't
## separate on the clustering features.
##
## Benchmarks Cluster 3 against the OTHER THREE CLUSTERS COMBINED
## (Clusters 1, 2, 4) -- not the full low-rater sample. Cluster 3 is
## ~60% of the sample, so a "vs. everyone" comparison is mostly
## Cluster 3 vs. itself and mutes any real gap. Comparing against the
## other clusters combined gives a cleaner read on whether Cluster 3
## actually looks different on these two variables.
## ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

## ------------------------------------------------------------
## 0. CONFIG
## ------------------------------------------------------------
target_cluster <- 3
plot_dir <- "cluster3_descriptive_plots"
dir.create(plot_dir, showWarnings = FALSE)

## ------------------------------------------------------------
## 1. LOAD & ASSEMBLE (same pattern as 03_profile_clusters.R)
## ------------------------------------------------------------
cluster_assignments <- readRDS("cluster_assignments.rds")             # MEM_NUM, cluster, PRV_GROUP
original_units      <- readRDS("cluster_features_original_units.rds") # 26 features, original units (incl. MA_REGION)
member_ids          <- readRDS("member_ids.rds")                      # MEM_NUM, same row order as original_units

stopifnot(nrow(cluster_assignments) == nrow(original_units))
stopifnot(all(member_ids$MEM_NUM == cluster_assignments$MEM_NUM))

profile_data <- cbind(
  cluster = cluster_assignments$cluster,
  original_units,
  PRV_GROUP = cluster_assignments$PRV_GROUP
)

cat("Full low-rater population:", nrow(profile_data), "members\n")
if (!(target_cluster %in% unique(profile_data$cluster))) {
  stop("target_cluster = ", target_cluster, " not found in cluster_assignments.rds. ",
       "Available clusters: ", paste(sort(unique(profile_data$cluster)), collapse = ", "))
}

cluster3_n <- sum(profile_data$cluster == target_cluster)
overall_n  <- nrow(profile_data)
other_clusters_n <- overall_n - cluster3_n
other_cluster_ids <- sort(setdiff(unique(profile_data$cluster), target_cluster))

cat("Cluster", target_cluster, "members:", cluster3_n,
    sprintf("(%.1f%% of the low-rater sample)\n", 100 * cluster3_n / overall_n))
cat("Other clusters combined (", paste(other_cluster_ids, collapse = ", "), "): ",
    other_clusters_n, " members\n", sep = "")

## ------------------------------------------------------------
## 2. HELPER -- distribution table for a categorical variable,
##    Cluster 3 vs. the OTHER CLUSTERS COMBINED, ranked by gap
## ------------------------------------------------------------
build_distribution_table <- function(data, var, cluster_col, target) {
  ## Benchmark computed on every cluster EXCEPT the target -- this is the
  ## key difference from a "vs. overall sample" comparison (see header note).
  other_tab <- data %>%
    filter(.data[[cluster_col]] != target) %>%
    count(level = .data[[var]]) %>%
    mutate(pct_other_clusters = round(100 * n / sum(n), 1)) %>%
    select(level, n_other_clusters = n, pct_other_clusters)

  cluster_tab <- data %>%
    filter(.data[[cluster_col]] == target) %>%
    count(level = .data[[var]]) %>%
    mutate(pct_cluster3 = round(100 * n / sum(n), 1)) %>%
    select(level, n_cluster3 = n, pct_cluster3)

  other_tab %>%
    left_join(cluster_tab, by = "level") %>%
    mutate(
      n_cluster3   = coalesce(n_cluster3, 0L),
      pct_cluster3 = coalesce(pct_cluster3, 0),
      pct_point_diff = round(pct_cluster3 - pct_other_clusters, 1)
    ) %>%
    arrange(desc(abs(pct_point_diff)))
}

## ------------------------------------------------------------
## 3. PROVIDER GROUP DISTRIBUTION
## ------------------------------------------------------------
prv_group_table <- build_distribution_table(profile_data, "PRV_GROUP", "cluster", target_cluster)
cat("\n=== Provider group: Cluster", target_cluster, "vs. other clusters combined ===\n")
print(prv_group_table, n = 30)
write.csv(prv_group_table, "cluster3_provider_group_distribution.csv", row.names = FALSE)

## ------------------------------------------------------------
## 4. GEOGRAPHIC REGION DISTRIBUTION
## ------------------------------------------------------------
region_table <- build_distribution_table(profile_data, "MA_REGION", "cluster", target_cluster)
cat("\n=== Geographic region (MA_REGION): Cluster", target_cluster,
    "vs. other clusters combined ===\n")
print(region_table, n = 30)
write.csv(region_table, "cluster3_region_distribution.csv", row.names = FALSE)

## ------------------------------------------------------------
## 5. PLOTS -- grouped bar charts, Cluster 3 vs. other clusters combined
## ------------------------------------------------------------
make_comparison_plot <- function(tab, title, x_label) {
  plot_data <- tab %>%
    select(level, `Cluster 3` = pct_cluster3,
           `Other clusters (1, 2, 4)` = pct_other_clusters) %>%
    pivot_longer(-level, names_to = "group", values_to = "pct") %>%
    mutate(level = factor(level, levels = tab$level))  # keep gap-ranked order

  ggplot(plot_data, aes(x = level, y = pct, fill = group)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    scale_fill_manual(values = c("Cluster 3" = "#993C1D",
                                  "Other clusters (1, 2, 4)" = "#B0B0B0")) +
    labs(title = title, x = x_label, y = "Percent of members", fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 40, hjust = 1),
          legend.position = "bottom", panel.grid.minor = element_blank())
}

prv_plot <- make_comparison_plot(
  prv_group_table,
  title = paste0("Provider group distribution: Cluster 3 (n=", cluster3_n,
                  ") vs. other clusters combined (n=", other_clusters_n, ")"),
  x_label = "Provider group"
)
ggsave(file.path(plot_dir, "cluster3_provider_group_barplot.png"),
       prv_plot, width = 10, height = 6, dpi = 200)

region_plot <- make_comparison_plot(
  region_table,
  title = paste0("Geographic region distribution: Cluster 3 (n=", cluster3_n,
                  ") vs. other clusters combined (n=", other_clusters_n, ")"),
  x_label = "MA region"
)
ggsave(file.path(plot_dir, "cluster3_region_barplot.png"),
       region_plot, width = 9, height = 6, dpi = 200)

cat("\nSaved plots to", plot_dir, "/:\n")
cat("  cluster3_provider_group_barplot.png\n")
cat("  cluster3_region_barplot.png\n")

## ------------------------------------------------------------
## 6. DRAFT NARRATIVE STATEMENTS (review before using)
## ------------------------------------------------------------
cat("\n============================================================\n")
cat("DRAFT NARRATIVE STATEMENTS (REVIEW BEFORE USING)\n")
cat("============================================================\n")

top_prv <- prv_group_table %>% slice_head(n = 3)
top_region <- region_table %>% slice_head(n = 3)

narrative_lines <- c()
for (i in seq_len(nrow(top_prv))) {
  r <- top_prv[i, ]
  dir_word <- if (r$pct_point_diff > 0) "over-represented" else "under-represented"
  line <- sprintf(
    "Provider group '%s' is %s in Cluster 3 (%.1f%% vs. %.1f%% in the other clusters, %+.1f pts).",
    r$level, dir_word, r$pct_cluster3, r$pct_other_clusters, r$pct_point_diff
  )
  narrative_lines <- c(narrative_lines, line)
  cat(line, "\n")
}
for (i in seq_len(nrow(top_region))) {
  r <- top_region[i, ]
  dir_word <- if (r$pct_point_diff > 0) "over-represented" else "under-represented"
  line <- sprintf(
    "Region '%s' is %s in Cluster 3 (%.1f%% vs. %.1f%% in the other clusters, %+.1f pts).",
    r$level, dir_word, r$pct_cluster3, r$pct_other_clusters, r$pct_point_diff
  )
  narrative_lines <- c(narrative_lines, line)
  cat(line, "\n")
}

if (max(abs(c(prv_group_table$pct_point_diff, region_table$pct_point_diff))) < 5) {
  cat("\nNote: all gaps are under 5 percentage points -- consistent with the pattern\n")
  cat("already seen in 03/04a (Cluster 3 not well-defined by available variables),\n")
  cat("even against the other clusters combined rather than the full sample.\n")
  cat("Worth stating plainly in the write-up rather than over-reading small gaps.\n")
}

writeLines(narrative_lines, "cluster3_descriptives_narrative_statements.txt")

cat("\nSaved: cluster3_provider_group_distribution.csv,\n")
cat("       cluster3_region_distribution.csv,\n")
cat("       cluster3_descriptives_narrative_statements.txt,\n")
cat("       ", plot_dir, "/cluster3_provider_group_barplot.png,\n", sep = "")
cat("       ", plot_dir, "/cluster3_region_barplot.png\n", sep = "")
cat("Cluster 3 descriptive profiling complete (benchmarked against Clusters 1, 2, 4 combined).\n")
