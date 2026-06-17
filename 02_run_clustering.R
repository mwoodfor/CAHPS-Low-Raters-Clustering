## ============================================================
## CAHPS Low-Rater Segmentation: k-Prototypes Clustering
## Step 2 of analytic plan — clusters the 26-feature set built
## in 01_build_feature_set.R, with diagnostics to choose k
## ============================================================

library(clustMixType)
library(dplyr)

## ------------------------------------------------------------
## 0. CONFIG
## ------------------------------------------------------------
k_range          <- 3:8   # candidate cluster counts to evaluate
                           # (target use case calls for 3-5; range
                           # extended to 8 so the elbow/silhouette
                           # trend is visible beyond the target zone)
n_start          <- 10    # random initializations per k
seed             <- 2026  # for reproducibility across runs
stability_k      <- NULL  # set after reviewing diagnostics, e.g. c(4,5)
final_k          <- NULL  # set after reviewing diagnostics, e.g. 4
silhouette_n_max <- 2500  # subsample cap for silhouette computation.
                           # kproto2silhouette() builds a full O(n^2)
                           # distance structure; on a 4GB machine this
                           # was tested to succeed reliably up to ~3000
                           # rows and fail (OOM) above ~3500. Capped at
                           # 2500 for safety margin. Increase only if
                           # running on a machine with more memory.
n_bootstrap      <- 30    # bootstrap resamples for the stability check

set.seed(seed)

## ------------------------------------------------------------
## ARI helper — Adjusted Rand Index between two cluster label vectors.
## Used for the bootstrap stability check below in place of
## clustMixType::stability_kproto(), which has a confirmed bug in this
## package version: the bootstrap-refit cluster vector loses its mapping
## back to original row identity whenever sample(..., replace=TRUE)
## produces duplicate row indices, causing stability_kproto() to return
## NA on every bootstrap iteration. The ARI computation here is the
## standard closed-form (Hubert & Arabie, 1985) and was validated against
## three known properties: identical partitions -> 1, relabeled-but-
## equivalent partitions -> 1, independent random partitions -> ~0.
## ------------------------------------------------------------
adjusted_rand_index <- function(a, b) {
  tab <- table(a, b)
  n <- sum(tab)
  sum_comb_rows <- sum(choose(rowSums(tab), 2))
  sum_comb_cols <- sum(choose(colSums(tab), 2))
  sum_comb_all  <- sum(choose(tab, 2))
  expected <- sum_comb_rows * sum_comb_cols / choose(n, 2)
  max_index <- (sum_comb_rows + sum_comb_cols) / 2
  if (max_index == expected) return(1)
  (sum_comb_all - expected) / (max_index - expected)
}

## ------------------------------------------------------------
## 1. LOAD FEATURE SET (output of 01_build_feature_set.R)
## ------------------------------------------------------------
cluster_features <- readRDS("cluster_features.rds")
member_ids       <- readRDS("member_ids.rds")
profiling_vars   <- readRDS("profiling_vars.rds")  # PRV_GROUP — profiling only

cat("Clustering input:", nrow(cluster_features), "rows x",
    ncol(cluster_features), "cols\n")
stopifnot(ncol(cluster_features) == 26)

## Sanity check: no NAs, since kproto() will error or silently drop rows
stopifnot(sum(is.na(cluster_features)) == 0)

## Confirm variable types match expectation (numeric for z-scored/binary,
## factor for categorical) — kproto() requires this distinction to apply
## the right distance metric per column
var_types <- sapply(cluster_features, class)
cat("\nVariable type counts:\n")
print(table(var_types))

## ------------------------------------------------------------
## 2. LAMBDA CHECK (categorical vs. numeric weighting)
## ------------------------------------------------------------
## With PRV_GROUP excluded, remaining categoricals (MEM_GENDER,
## member_plan, MA_REGION) have low cardinality (2, 3, ~6 levels).
## Using kproto()'s automatic lambda estimation rather than a manual
## override, since no single categorical should dominate at this
## cardinality. lambdaest() is run here only to inspect the value,
## not to override it.
auto_lambda <- lambdaest(cluster_features)
cat("\nAutomatic lambda estimate:", round(auto_lambda, 4), "\n")
cat("(This is computed internally by kproto() too; shown here for visibility.)\n")

## ------------------------------------------------------------
## 3. RUN K-PROTOTYPES ACROSS CANDIDATE K VALUES
## ------------------------------------------------------------
kproto_results <- list()
diagnostics <- data.frame(
  k = integer(), tot_withinss = numeric(), silhouette = numeric()
)

for (k in k_range) {
  cat("\n--- Running kproto for k =", k, "---\n")
  set.seed(seed)
  fit <- kproto(
    x = cluster_features,
    k = k,
    nstart = n_start,
    verbose = FALSE
  )
  kproto_results[[as.character(k)]] <- fit

  ## Total within-cluster dissimilarity (elbow metric)
  tot_diss <- fit$tot.withinss

  ## Silhouette width (cluster separation metric, adapted for mixed types
  ## via kproto2silhouette). This function builds a full O(n^2) pairwise
  ## distance structure internally, which exhausts memory on the full
  ## sample at typical low-rater sample sizes. We compute it on a capped
  ## random subsample instead -- standard practice for silhouette analysis
  ## on larger datasets, since the metric is about relative cluster
  ## separation and a few thousand rows is enough to estimate it reliably.
  if (nrow(cluster_features) > silhouette_n_max) {
    set.seed(seed)
    sil_idx <- sample(1:nrow(cluster_features), silhouette_n_max)
  } else {
    sil_idx <- 1:nrow(cluster_features)
  }
  sil_data <- cluster_features[sil_idx, ]
  set.seed(seed)
  sil_fit <- kproto(sil_data, k = k, nstart = n_start, verbose = FALSE)
  sil <- kproto2silhouette(sil_fit)
  mean_sil <- mean(sil[, "sil_width"])

  diagnostics <- rbind(diagnostics, data.frame(
    k = k, tot_withinss = tot_diss, silhouette = mean_sil
  ))

  cat("k =", k, "| tot.withinss =", round(tot_diss, 1),
      "| mean silhouette =", round(mean_sil, 4), "\n")
}

cat("\n=== Diagnostics summary across k ===\n")
print(diagnostics)

## ------------------------------------------------------------
## 4. ELBOW PLOT (within-cluster dissimilarity vs. k)
## ------------------------------------------------------------
png("elbow_plot.png", width = 800, height = 500)
plot(diagnostics$k, diagnostics$tot_withinss, type = "b", pch = 19,
     xlab = "Number of clusters (k)", ylab = "Total within-cluster dissimilarity",
     main = "Elbow Plot: k-Prototypes")
dev.off()
cat("\nSaved elbow_plot.png\n")

## ------------------------------------------------------------
## 5. SILHOUETTE PLOT (mean silhouette width vs. k)
## ------------------------------------------------------------
png("silhouette_plot.png", width = 800, height = 500)
plot(diagnostics$k, diagnostics$silhouette, type = "b", pch = 19, col = "darkblue",
     xlab = "Number of clusters (k)", ylab = "Mean silhouette width",
     main = "Mean Silhouette Width by k")
abline(h = 0, lty = 2, col = "gray")
dev.off()
cat("Saved silhouette_plot.png\n")

## ------------------------------------------------------------
## 6. STABILITY CHECK (top candidate k values, per user's 3-5 target)
## ------------------------------------------------------------
## For each candidate k, we bootstrap-resample the data, re-fit kproto,
## and compare the resampled assignment to the full-sample assignment
## (on the overlapping rows) via Adjusted Rand Index. High mean ARI across
## resamples means the segmentation isn't an artifact of this particular
## sample draw. (Not using clustMixType::stability_kproto() here -- see
## note above adjusted_rand_index() for why.)
if (is.null(stability_k)) {
  ## Default: check the two candidates inside the target 3-5 range with the
  ## best silhouette, so the stability check focuses on plausible choices
  target_range <- diagnostics %>% filter(k >= 3, k <= 5)
  stability_k <- target_range %>%
    arrange(desc(silhouette)) %>%
    slice_head(n = 2) %>%
    pull(k)
}

cat("\n=== Running stability check for k =", paste(stability_k, collapse = ", "), "===\n")

stability_summary <- data.frame(k = integer(), mean_ari = numeric(), sd_ari = numeric())

for (k in stability_k) {
  cat("\nStability check for k =", k, "(", n_bootstrap, "bootstrap resamples)...\n")
  full_fit <- kproto_results[[as.character(k)]]
  n_obs <- nrow(cluster_features)
  ari_vals <- numeric(n_bootstrap)

  for (b in 1:n_bootstrap) {
    set.seed(seed + b)
    boot_idx <- sample(1:n_obs, n_obs, replace = TRUE)
    boot_data <- cluster_features[boot_idx, ]

    boot_fit <- kproto(boot_data, k = k, nstart = 2, verbose = FALSE)

    ## Compare on the unique rows present in this bootstrap sample:
    ## full_fit$cluster for those original row positions vs. the
    ## bootstrap-refit assignment for the (first occurrence of) the
    ## same row positions within boot_data
    unique_rows <- unique(boot_idx)
    first_occurrence <- match(unique_rows, boot_idx)

    full_labels <- full_fit$cluster[unique_rows]
    boot_labels <- boot_fit$cluster[first_occurrence]

    ari_vals[b] <- adjusted_rand_index(full_labels, boot_labels)
  }

  stability_summary <- rbind(stability_summary, data.frame(
    k = k, mean_ari = mean(ari_vals), sd_ari = sd(ari_vals)
  ))
  cat("k =", k, "| mean ARI across", n_bootstrap, "resamples:",
      round(mean(ari_vals), 4), "| sd:", round(sd(ari_vals), 4), "\n")
}

cat("\n=== Stability summary ===\n")
print(stability_summary)
cat("\nGuideline: mean ARI > 0.7-0.8 suggests a stable, reproducible segmentation.\n")
cat("Lower values suggest the chosen k is sensitive to sample composition.\n")

## ------------------------------------------------------------
## 7. REVIEW POINT — pick final_k before proceeding
## ------------------------------------------------------------
cat("\n============================================================\n")
cat("REVIEW DIAGNOSTICS ABOVE, THEN SET final_k IN THE CONFIG SECTION\n")
cat("(elbow_plot.png, silhouette_plot.png, and stability results above)\n")
cat("Re-run from Section 8 onward once final_k is set.\n")
cat("============================================================\n")

if (!is.null(final_k)) {

  ## ------------------------------------------------------------
  ## 8. FINALIZE CHOSEN K AND EXTRACT CLUSTER ASSIGNMENTS
  ## ------------------------------------------------------------
  final_fit <- kproto_results[[as.character(final_k)]]

  cluster_assignments <- data.frame(
    MEM_NUM = member_ids$MEM_NUM,
    cluster = final_fit$cluster,
    PRV_GROUP = profiling_vars$PRV_GROUP   # carried for post-hoc profiling only
  )

  cat("\nFinal cluster sizes (k =", final_k, "):\n")
  print(table(cluster_assignments$cluster))

  ## ------------------------------------------------------------
  ## 9. SAVE OUTPUTS
  ## ------------------------------------------------------------
  saveRDS(final_fit, "kproto_final_fit.rds")
  saveRDS(cluster_assignments, "cluster_assignments.rds")
  write.csv(cluster_assignments, "cluster_assignments.csv", row.names = FALSE)

  cat("\nSaved: kproto_final_fit.rds, cluster_assignments.rds, cluster_assignments.csv\n")
  cat("Clustering complete. Ready for Step 3 (cluster profiling).\n")
}
