## ============================================================
## CAHPS Low-Rater Segmentation: Cluster 3 Hierarchical-Clustering
## Feature Build -- DIAGNOSTIC BRANCH, not part of the main 01-04
## pipeline. Joins curated interaction/contact-pattern features
## (built by Blueview's 02_build_features_profile.R) onto the
## original 26 k-prototypes features, restricted to Cluster 3
## members only, as input to 06_hierarchical_clustering.R.
##
## Purpose: test whether Cluster 3 (n=10,678, ~60% of the low-rater
## sample) -- which the original k-prototypes run could not separate
## from the rest of the population, and which interaction features
## also fail to differentiate via SMD screen (max |SMD| = 0.104) --
## contains internal substructure that a tree-based method can find
## even though no single feature differentiates it in isolation.
##
## NOTE: drafted without an R interpreter available to test-run end
## to end (no R install in the drafting environment, and no access
## to the real cluster_features.rds / member_interaction_features.csv
## to execute against). Syntax and logic were reviewed carefully
## against the conventions in 01_build_feature_set.R and
## 02_build_features_profile.R, but run this on a small check first
## (e.g. print(dim(...)) / str(...) after each section) before
## trusting it end to end.
## ============================================================

library(dplyr)
library(tidyr)

## ------------------------------------------------------------
## 0. CONFIG -- edit paths for your machine before running
## ------------------------------------------------------------
path_cluster_assignments <- "cluster_assignments.rds"                 # from 02_run_clustering.R (this folder)
path_cluster_features    <- "cluster_features.rds"                    # transformed 26-feature clustering input (this folder)
path_member_ids          <- "member_ids.rds"                          # MEM_NUM, same row order as cluster_features.rds
path_original_units      <- "cluster_features_original_units.rds"     # pre-transform 26 features, for profiling later

path_interaction_features <- "../Blueview/member_interaction_features.csv"  # <- update to your actual path

target_cluster <- 3   # the cluster being sub-clustered

## Curated interaction features -- one representative per concept, no
## per-reason / per-topic dummy explosion (that reduction discipline is
## the same one applied going from 87 -> 26 features originally).
## Names must match columns in member_interaction_features.csv -- see the
## appendix table in lowrater_interaction_profile.html if any of these
## have been renamed since this was drafted.
interaction_vars_skewed <- c(     # right-skewed counts/durations -> log1p + z
  "n_interactions_total", "n_distinct_reasons",
  "days_since_last_interaction", "max_interactions_in_30d",
  "mean_duration_min"
)
interaction_vars_symmetric <- c(  # low-cardinality counts -> z only, no log
  "n_distinct_types"
)
interaction_vars_binary <- c(     # already 0/1, pass through
  "ever_type_chat", "ever_reason_medical_part_c_appeals_grievances",
  "repeat_same_reason_30d", "channel_switch", "billing_contact_ever",
  "any_long_interaction", "ever_benefit_discussed", "any_interaction"
)
interaction_vars_profiling_only <- c("dominant_reason")  # NOT a clustering input -- high
                                                          # cardinality (~15 levels), same
                                                          # treatment as PRV_GROUP in the
                                                          # original 26-feature build; carried
                                                          # through for post-hoc profiling only

all_interaction_vars <- c(interaction_vars_skewed, interaction_vars_symmetric,
                           interaction_vars_binary, interaction_vars_profiling_only)

## ------------------------------------------------------------
## 1. LOAD & ALIGN THE ORIGINAL 26-FEATURE CLUSTERING DATA
## ------------------------------------------------------------
cluster_assignments <- readRDS(path_cluster_assignments)   # MEM_NUM, cluster, PRV_GROUP
cluster_features    <- readRDS(path_cluster_features)      # 26 transformed features, no MEM_NUM
original_units      <- readRDS(path_original_units)        # 26 features, original units, no MEM_NUM
member_ids          <- readRDS(path_member_ids)             # MEM_NUM, same row order as cluster_features

stopifnot(nrow(cluster_assignments) == nrow(cluster_features))
stopifnot(nrow(cluster_assignments) == nrow(original_units))
stopifnot(all(member_ids$MEM_NUM == cluster_assignments$MEM_NUM))

full_data <- cbind(
  MEM_NUM   = cluster_assignments$MEM_NUM,   # carries the raw "000"-suffixed MEM_NUM
  cluster   = cluster_assignments$cluster,
  PRV_GROUP = cluster_assignments$PRV_GROUP,
  cluster_features
)
full_data_original_units <- cbind(
  MEM_NUM   = cluster_assignments$MEM_NUM,
  cluster   = cluster_assignments$cluster,
  PRV_GROUP = cluster_assignments$PRV_GROUP,
  original_units
)

cluster3 <- full_data %>% filter(cluster == target_cluster)
cluster3_original_units <- full_data_original_units %>% filter(cluster == target_cluster)

cat("Cluster", target_cluster, "members (26-feature clustering data):", nrow(cluster3), "\n")
stopifnot(nrow(cluster3) > 0)

## ------------------------------------------------------------
## 2. FIX THE MEM_NUM FORMAT MISMATCH BEFORE JOINING
## ------------------------------------------------------------
## cluster_assignments.rds (this project) carries the raw MEM_NUM with a
## trailing "000" suffix. Blueview's 01_pull_interactions.R strips that
## suffix on read (to match CONTRACT_ID in the interaction view), and the
## stripped version is what flows through to member_interaction_features.csv.
## A join on raw MEM_NUM would silently match nothing (no error, just NAs
## everywhere) -- verify and strip here rather than trust it matches.
bad_suffix <- !grepl("000$", cluster3$MEM_NUM)
if (any(bad_suffix)) {
  stop(sum(bad_suffix), " MEM_NUM value(s) in Cluster ", target_cluster,
       " do not end in '000', e.g. ",
       paste(head(cluster3$MEM_NUM[bad_suffix], 5), collapse = ", "),
       ". Expected every MEM_NUM to carry a '000' suffix, matching the ",
       "convention Blueview's 01_pull_interactions.R assumes -- inspect ",
       "before proceeding.")
}
cluster3$MEM_NUM_JOIN <- substr(cluster3$MEM_NUM, 1, nchar(cluster3$MEM_NUM) - 3)

cat("Stripped trailing '000' suffix from MEM_NUM for the interaction-data join; sample: ",
    paste(head(cluster3$MEM_NUM_JOIN, 3), collapse = ", "), "\n")

## ------------------------------------------------------------
## 3. LOAD INTERACTION FEATURES AND VALIDATE THE JOIN
## ------------------------------------------------------------
interaction_raw <- read.csv(path_interaction_features, stringsAsFactors = FALSE,
                             colClasses = c(MEM_NUM = "character"))

missing_vars <- setdiff(all_interaction_vars, names(interaction_raw))
if (length(missing_vars) > 0) {
  stop("Missing expected interaction variable(s) in ", path_interaction_features,
       ": ", paste(missing_vars, collapse = ", "),
       ". Check column names against the appendix in lowrater_interaction_profile.html.")
}

interaction_curated <- interaction_raw %>%
  select(MEM_NUM, all_of(all_interaction_vars))

joined <- cluster3 %>%
  left_join(interaction_curated, by = c("MEM_NUM_JOIN" = "MEM_NUM"))

## Fail loudly on a broken join: member_interaction_features.csv is built as
## one row per member (including zero-contact members, zero-filled), so
## every Cluster 3 member should match. any_interaction is explicitly
## zero-filled upstream for no-contact members -- if it's NA here, the join
## failed for that row, it is not a legitimate "no data" case.
n_unmatched <- sum(is.na(joined$any_interaction))
if (n_unmatched > 0) {
  stop(n_unmatched, " Cluster ", target_cluster, " member(s) failed to match ",
       "member_interaction_features.csv on stripped MEM_NUM. Sample unmatched: ",
       paste(head(joined$MEM_NUM[is.na(joined$any_interaction)], 5), collapse = ", "),
       ". Investigate before proceeding -- do not silently drop or impute.")
}
stopifnot(nrow(joined) == nrow(cluster3))
cat("Joined interaction features onto all", nrow(joined), "Cluster", target_cluster, "members.\n")

## QA: report missingness on the features that are genuinely NA (by design)
## for members with no live contact in the window, vs. zero-filled features.
cat("\n=== Missingness in curated interaction features (Cluster ", target_cluster, ") ===\n")
na_report <- joined %>%
  summarise(across(all_of(c(interaction_vars_skewed, interaction_vars_symmetric)),
                    ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  arrange(desc(n_missing))
print(na_report)
cat("(Missingness here reflects members with no live contact in the window --",
    "see any_interaction == 0 -- not a join failure. Confirm n_missing for",
    "days_since_last_interaction / max_interactions_in_30d / mean_duration_min",
    "roughly matches the no-live-contact share for this cluster (~19.6% per",
    "the interaction profile report); the other curated features are",
    "zero-filled upstream and should show 0 missing.)\n")

## ------------------------------------------------------------
## 4. TRANSFORM INTERACTION FEATURES (mirrors 01_build_feature_set.R
##    conventions: log1p + z for skewed counts/durations, z-only for
##    low-cardinality symmetric counts, binaries passed through as 0/1)
## ------------------------------------------------------------
for (v in interaction_vars_skewed) {
  log_col <- paste0(v, "_log")
  z_col   <- paste0(v, "_z")
  joined[[log_col]] <- log1p(joined[[v]])
  ## base::scale() computes mean/sd ignoring NA and leaves individual NA
  ## entries as NA in the output -- it does not error or propagate NA to
  ## the whole column, so members with no live contact keep NA here
  ## (handled later by daisy()'s native missing-value support), not an
  ## arbitrary imputed value.
  joined[[z_col]] <- as.numeric(scale(joined[[log_col]], center = TRUE, scale = TRUE))
  if (all(is.na(joined[[z_col]])) && !all(is.na(joined[[v]]))) {
    stop("Z-scoring produced all-NA for ", v, " -- investigate before proceeding.")
  }
}
for (v in interaction_vars_symmetric) {
  z_col <- paste0(v, "_z")
  joined[[z_col]] <- as.numeric(scale(joined[[v]], center = TRUE, scale = TRUE))
}
for (v in interaction_vars_binary) {
  joined[[v]] <- as.numeric(joined[[v]])
  stopifnot(all(joined[[v]] %in% c(0, 1)))
}

interaction_z_cols <- c(paste0(interaction_vars_skewed, "_z"), paste0(interaction_vars_symmetric, "_z"))

## ------------------------------------------------------------
## 5. ASSEMBLE THE CLUSTERING MATRIX (26 original features + 14
##    curated interaction features), with types set so cluster::daisy()
##    infers the right Gower sub-distance for each column automatically:
##    factor -> nominal, logical -> symmetric binary, numeric -> interval.
## ------------------------------------------------------------
binary_vars_26 <- c("lis_ind", "dis_ind", "email_optin", "mail_order_flag", "any_auth")

clustering_matrix <- joined %>%
  select(all_of(c(
    names(cluster_features),              # 26 original clustering features (already log+z / 0-1 / factor)
    interaction_z_cols, interaction_vars_binary   # 14 curated interaction features
  ))) %>%
  mutate(across(all_of(c(binary_vars_26, interaction_vars_binary)), as.logical))

row.names(clustering_matrix) <- joined$MEM_NUM

cat("\nClustering matrix for hierarchical run:", nrow(clustering_matrix), "rows x",
    ncol(clustering_matrix), "cols\n")
cat("(26 original features + ", length(interaction_z_cols) + length(interaction_vars_binary),
    " curated interaction features)\n", sep = "")
var_types <- sapply(clustering_matrix, class)
cat("\nColumn type counts (daisy() infers Gower sub-distance from these):\n")
print(table(var_types))
cat("\nTotal NA cells in clustering matrix:", sum(is.na(clustering_matrix)),
    "-- expected from the ~19.6% no-live-contact members on the 3 temporal/",
    "duration columns (see Section 3 QA above); daisy() handles these natively.\n")

## ------------------------------------------------------------
## 6. SAVE OUTPUTS
## ------------------------------------------------------------
saveRDS(clustering_matrix, "cluster3_hier_clustering_matrix.rds")        # -> input to 06_hierarchical_clustering.R
saveRDS(joined, "cluster3_hier_features_full.rds")                       # all columns, joined + transformed, for reference
saveRDS(cluster3_original_units, "cluster3_hier_original_units_26.rds")  # 26 features, original units, for 07 profiling
write.csv(
  joined %>% select(MEM_NUM, dominant_reason,
                     all_of(c(interaction_vars_skewed, interaction_vars_symmetric,
                              interaction_vars_binary))),
  "cluster3_hier_interaction_features_original_units.csv", row.names = FALSE
)

cat("\nSaved: cluster3_hier_clustering_matrix.rds (daisy() input),\n")
cat("       cluster3_hier_features_full.rds (all columns, joined + transformed),\n")
cat("       cluster3_hier_original_units_26.rds (26 features, original units, for profiling),\n")
cat("       cluster3_hier_interaction_features_original_units.csv (interaction features, original units, for profiling)\n")
cat("Ready for 06_hierarchical_clustering.R.\n")
