## ============================================================
## CAHPS Low-Rater Segmentation: Cluster 3 Hierarchical Clustering
## DIAGNOSTIC BRANCH -- tests whether Cluster 3 (undifferentiated
## under k-prototypes and under the interaction-feature SMD screen)
## contains internal substructure a tree-based method can surface.
## Input: cluster3_hier_clustering_matrix.rds from 05.
##
## NOTE: drafted without an R interpreter available to test-run
## against real data (see note at the top of 05). Review before
## trusting end to end -- in particular, watch memory behavior on
## the first daisy() call (Section 2) given this project's prior
## OOM history (see Section 0 config note below).
## ============================================================

library(cluster)   # daisy(), silhouette()
library(dplyr)

## ------------------------------------------------------------
## 0. CONFIG
## ------------------------------------------------------------
seed <- 2026
set.seed(seed)

linkage_methods <- c("ward.D2", "complete", "average")  # stats::hclust() method names;
                                                          # ward.D2 = compact, business-friendly
                                                          # groups (closest analogue to k-prototypes'
                                                          # behavior); complete/average included as
                                                          # points of comparison, since agreement
                                                          # across methods is stronger evidence of
                                                          # real structure than any single method alone.
candidate_k <- 2:6   # tree cuts to evaluate for each linkage

## --- MEMORY SAFETY -----------------------------------------------------
## daisy() with metric="gower" builds a full pairwise dissimilarity object.
## This project already hit an out-of-memory failure in kproto2silhouette()
## (see 02_run_clustering.R) building a full O(n^2) structure above ~3,500
## rows on this 4GB machine -- for a LIGHTER computation than a full Gower
## distance with mixed-type weighting. Cluster 3 has 10,678 members. Treat
## OOM here as a real risk, not a hypothetical one. If daisy() fails or the
## session becomes unresponsive, set subsample_for_daisy <- TRUE below and
## re-run -- do not silently reduce daisy_n_max without noting it in your
## methods write-up, since it changes what population the diagnostic covers.
subsample_for_daisy <- FALSE
daisy_n_max         <- 6000

plot_dir <- "cluster3_hier_plots"
dir.create(plot_dir, showWarnings = FALSE)

## ------------------------------------------------------------
## 1. LOAD CLUSTERING MATRIX
## ------------------------------------------------------------
clustering_matrix <- readRDS("cluster3_hier_clustering_matrix.rds")
n_full_cluster3 <- nrow(clustering_matrix)
cat("Loaded clustering matrix:", n_full_cluster3, "rows x",
    ncol(clustering_matrix), "cols\n")

if (subsample_for_daisy && nrow(clustering_matrix) > daisy_n_max) {
  set.seed(seed)
  keep_idx <- sample(seq_len(nrow(clustering_matrix)), daisy_n_max)
  clustering_matrix <- clustering_matrix[keep_idx, ]
  cat("*** SUBSAMPLED to", nrow(clustering_matrix), "rows for memory safety ***\n")
  cat("*** This diagnostic now covers a", round(100 * nrow(clustering_matrix) / n_full_cluster3, 1),
      "% sample of Cluster 3, not the full group -- note this in any write-up. ***\n")
} else if (!subsample_for_daisy) {
  cat("Attempting full-population Gower distance (", nrow(clustering_matrix),
      " rows). If this fails or the session hangs, set subsample_for_daisy <- TRUE ",
      "in the config and re-run.\n", sep = "")
}

## ------------------------------------------------------------
## 2. GOWER DISTANCE
## ------------------------------------------------------------
## daisy() infers the right sub-distance per column type automatically:
## factor -> nominal (simple matching), logical -> symmetric binary,
## numeric -> interval (range-normalized). Unlike kproto(), daisy() handles
## NA natively via pairwise-available-case weighting -- the ~20% of Cluster
## 3 with no live contact (NA on days_since_last_interaction,
## max_interactions_in_30d, mean_duration_min) do not need to be imputed
## or dropped; those three dimensions are simply excluded from any pairwise
## comparison involving a no-contact member, and their other ~37 features
## still contribute normally.
gower_dist <- daisy(clustering_matrix, metric = "gower")
cat("\nGower distance matrix built. Summary of pairwise dissimilarities:\n")
print(summary(as.vector(gower_dist)))

## ------------------------------------------------------------
## 3. HIERARCHICAL CLUSTERING ACROSS LINKAGE METHODS
## ------------------------------------------------------------
hc_fits <- list()
cophenetic_summary <- data.frame(linkage = character(), cophenetic_cor = numeric())

for (m in linkage_methods) {
  cat("\n--- Fitting hclust, method =", m, "---\n")
  hc <- hclust(gower_dist, method = m)
  hc_fits[[m]] <- hc

  coph_dist <- cophenetic(hc)
  coph_cor <- cor(as.vector(gower_dist), as.vector(coph_dist))
  cophenetic_summary <- rbind(cophenetic_summary,
                               data.frame(linkage = m, cophenetic_cor = coph_cor))
  cat("Cophenetic correlation (tree fidelity to original distances):",
      round(coph_cor, 4), "\n")

  png(file.path(plot_dir, paste0("dendrogram_", m, ".png")), width = 1200, height = 600)
  plot(hc, labels = FALSE, hang = -1,
       main = paste0("Cluster 3 hierarchical clustering (", m, ")"),
       xlab = "", sub = "")
  dev.off()
}

cat("\n=== Cophenetic correlation by linkage method ===\n")
cat("(Higher = tree more faithfully represents the underlying Gower distances.\n")
cat(" Below ~0.6-0.7 is generally considered a poor fit -- treat any resulting\n")
cat(" split from that linkage with real skepticism.)\n")
print(cophenetic_summary)
cat("\nSaved dendrogram_<method>.png to", plot_dir, "/ -- inspect visually for\n")
cat("clean, deep branch splits (real structure) vs. one dominant blob with\n")
cat("items merging in one at a time near the top (chaining -- no structure).\n")

## ------------------------------------------------------------
## 4. CUT TREES AT CANDIDATE k, DIAGNOSTICS PER (linkage, k)
## ------------------------------------------------------------
diagnostics <- data.frame(
  linkage = character(), k = integer(), mean_silhouette = numeric(),
  min_cluster_size = integer(), max_cluster_size = integer(),
  n_clusters_under_100 = integer()
)

cut_labels <- list()   # [[linkage]][[as.character(k)]] -> integer vector of cluster labels

for (m in linkage_methods) {
  cut_labels[[m]] <- list()
  for (k in candidate_k) {
    labels <- cutree(hc_fits[[m]], k = k)
    cut_labels[[m]][[as.character(k)]] <- labels

    sizes <- table(labels)
    sil <- silhouette(labels, gower_dist)
    mean_sil <- mean(sil[, "sil_width"])

    diagnostics <- rbind(diagnostics, data.frame(
      linkage = m, k = k, mean_silhouette = round(mean_sil, 4),
      min_cluster_size = as.integer(min(sizes)), max_cluster_size = as.integer(max(sizes)),
      n_clusters_under_100 = sum(sizes < 100)
    ))
  }
}

cat("\n=== Diagnostics by linkage x k ===\n")
cat("(mean_silhouette: same metric used for the original k-prototypes k-selection\n")
cat(" -- near 0 or negative means no real separation, same interpretation as before.\n")
cat(" min/max_cluster_size and n_clusters_under_100 flag degenerate splits: a few\n")
cat(" hundred members peeled off into singleton-ish clusters is not a usable\n")
cat(" campaign segment, even if the silhouette number looks fine.)\n")
print(diagnostics[order(diagnostics$linkage, diagnostics$k), ], row.names = FALSE)

write.csv(diagnostics, "cluster3_hier_diagnostics.csv", row.names = FALSE)
write.csv(cophenetic_summary, "cluster3_hier_cophenetic_summary.csv", row.names = FALSE)
saveRDS(hc_fits, "cluster3_hier_fits.rds")
saveRDS(cut_labels, "cluster3_hier_cut_labels.rds")
saveRDS(gower_dist, "cluster3_hier_gower_dist.rds")
saveRDS(row.names(clustering_matrix), "cluster3_hier_row_mem_nums.rds")  # MEM_NUM order used for this run
                                                                          # (needed downstream if subsampled)

cat("\nSaved cluster3_hier_diagnostics.csv, cluster3_hier_cophenetic_summary.csv,\n")
cat("      cluster3_hier_fits.rds, cluster3_hier_cut_labels.rds, cluster3_hier_gower_dist.rds,\n")
cat("      cluster3_hier_row_mem_nums.rds\n")

## ------------------------------------------------------------
## 5. REVIEW POINT -- pick final_linkage / final_k before proceeding
## ------------------------------------------------------------
final_linkage <- NULL   # e.g. "ward.D2", set after reviewing dendrograms + diagnostics above
final_k       <- NULL   # e.g. 3

cat("\n============================================================\n")
cat("REVIEW DENDROGRAM PLOTS, COPHENETIC CORRELATIONS, AND THE\n")
cat("DIAGNOSTICS TABLE ABOVE. Set final_linkage and final_k in the\n")
cat("config section, then re-run from Section 6 onward.\n")
cat("============================================================\n")

if (!is.null(final_linkage) && !is.null(final_k)) {

  ## ------------------------------------------------------------
  ## 6. FINALIZE CHOSEN LINKAGE/K AND EXTRACT SUB-CLUSTER ASSIGNMENTS
  ## ------------------------------------------------------------
  final_labels <- cut_labels[[final_linkage]][[as.character(final_k)]]

  cluster3_hier_assignments <- data.frame(
    MEM_NUM = row.names(clustering_matrix),
    hier_subcluster = as.integer(final_labels),
    row.names = NULL
  )

  stopifnot(nrow(cluster3_hier_assignments) == nrow(clustering_matrix))
  stopifnot(sum(is.na(cluster3_hier_assignments$hier_subcluster)) == 0)

  cat("\nFinal sub-cluster sizes (linkage =", final_linkage, ", k =", final_k, "):\n")
  print(table(cluster3_hier_assignments$hier_subcluster))

  saveRDS(cluster3_hier_assignments, "cluster3_hier_assignments.rds")
  write.csv(cluster3_hier_assignments, "cluster3_hier_assignments.csv", row.names = FALSE)
  cat("\nSaved cluster3_hier_assignments.rds/csv. Ready for 07_profile_hier_subclusters.R.\n")
}
