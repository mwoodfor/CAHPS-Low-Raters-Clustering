## ============================================================
## CAHPS Low-Rater Segmentation: Feature Set Construction
## Step 1 of analytic plan — builds the clustering-ready dataset
## Output: analysis-ready data frame with 27 clustering features
##         + MEM_NUM retained separately for joins
## ============================================================

library(dplyr)
library(forcats)

## ------------------------------------------------------------
## 0. CONFIG — edit these paths/thresholds as needed
## ------------------------------------------------------------
input_path        <- "low_rater_sample.csv"   # <- update to your actual source file
prv_group_min_pct <- 0.01                       # collapse PRV_GROUP levels below 1% of sample

## ------------------------------------------------------------
## 1. LOAD RAW DATA
## ------------------------------------------------------------
raw <- read.csv(input_path, stringsAsFactors = FALSE)

cat("Raw data dimensions:", nrow(raw), "rows x", ncol(raw), "cols\n")

## Variables this script expects to find in raw data.
## If any are missing, the script will stop with a clear message
## rather than silently proceeding with a partial feature set.
required_vars <- c(
  "MEM_NUM",
  # Demographics & SES
  "MEM_GENDER", "MEM_AGE", "ses_index", "tenure_years",
  # Plan / subsidy / disability status
  "member_plan", "lis_ind", "dis_ind",
  # Household & engagement
  "family_members", "myblue_visits", "email_optin",
  # Geography
  "MA_REGION",
  # Pharmacy utilization
  "total_pharm_oop", "total_pharm_allow", "total_supply_days",
  "abandoned_scripts", "total_pharm_denied", "mail_order_flag",
  # Medical utilization
  "total_med_allow", "total_med_oop", "total_med_denied",
  # Clinical burden & risk
  "ct_condit", "charlson_index", "DXCG_RRS_EXP_CON",
  # Grievances / appeals / auth
  "total_grievances", "total_appeals", "any_auth"
)

# PRV_GROUP is NOT a clustering feature (dropped: ~20 provider-group levels in
# Massachusetts with no natural collapsing hierarchy; risked dominating the
# categorical distance component and producing clusters that just reflect
# provider group rather than member behavior/clinical profile). It is carried
# through separately for POST-HOC descriptive profiling of final clusters only.
profiling_only_vars <- c("PRV_GROUP")

missing_vars <- setdiff(c(required_vars, profiling_only_vars), names(raw))
if (length(missing_vars) > 0) {
  stop("Missing expected variable(s) in source data: ",
       paste(missing_vars, collapse = ", "))
}

## ------------------------------------------------------------
## 2. SELECT FEATURE SET (drop everything not in the final spec)
## ------------------------------------------------------------
df <- raw %>% select(all_of(c(required_vars, profiling_only_vars)))

## ------------------------------------------------------------
## 3. COLLAPSE RARE PRV_GROUP LEVELS INTO "Other" (PROFILING ONLY —
##    this variable does not enter the clustering feature set; the
##    collapse just keeps the post-hoc cross-tab readable)
## ------------------------------------------------------------
prv_freq <- df %>%
  count(PRV_GROUP) %>%
  mutate(pct = n / sum(n))

rare_groups <- prv_freq %>% filter(pct < prv_group_min_pct) %>% pull(PRV_GROUP)

cat("\nPRV_GROUP levels collapsed to 'Other' for profiling (<", prv_group_min_pct * 100,
    "% of sample):", length(rare_groups), "of", nrow(prv_freq), "levels\n")

df <- df %>%
  mutate(PRV_GROUP = if_else(PRV_GROUP %in% rare_groups, "Other", PRV_GROUP))

## ------------------------------------------------------------
## 4. TYPE ASSIGNMENT
## ------------------------------------------------------------

# Categorical (3) — PRV_GROUP excluded; see note in section 2
categorical_vars <- c("MEM_GENDER", "member_plan", "MA_REGION")

# Binary (5) — passed through untouched, 0/1
binary_vars <- c("lis_ind", "dis_ind", "email_optin", "mail_order_flag", "any_auth")

# Continuous, log1p + z-score (10) — right-skewed cost/count/volume vars
skewed_continuous_vars <- c(
  "total_pharm_oop", "total_pharm_allow", "total_supply_days",
  "abandoned_scripts", "total_pharm_denied",
  "total_med_allow", "total_med_oop", "total_med_denied",
  "total_grievances", "total_appeals"
)

# Continuous, z-score only (8) — ages, tenure, index/score variables
symmetric_continuous_vars <- c(
  "MEM_AGE", "tenure_years", "ses_index", "charlson_index",
  "ct_condit", "DXCG_RRS_EXP_CON", "family_members", "myblue_visits"
)

stopifnot(
  length(categorical_vars) + length(binary_vars) +
    length(skewed_continuous_vars) + length(symmetric_continuous_vars) == 26
)

## ------------------------------------------------------------
## 5. MISSING VALUE CHECK (flag before transforming — do not silently impute)
## ------------------------------------------------------------
na_summary <- df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  filter(n_missing > 0) %>%
  arrange(desc(n_missing))

if (nrow(na_summary) > 0) {
  cat("\nVariables with missing values (review before clustering):\n")
  print(na_summary)
} else {
  cat("\nNo missing values detected in selected feature set.\n")
}

## ------------------------------------------------------------
## 6. TRANSFORMATIONS
## ------------------------------------------------------------

## 6a. Skewed continuous -> log1p, then z-score
df_transformed <- df %>%
  mutate(across(all_of(skewed_continuous_vars), ~ log1p(.x), .names = "{.col}_log")) 

# z-score the log-transformed versions
for (v in skewed_continuous_vars) {
  log_col <- paste0(v, "_log")
  z_col   <- paste0(v, "_z")
  df_transformed[[z_col]] <- scale(df_transformed[[log_col]])[, 1]
}

## 6b. Symmetric continuous -> z-score only (no log)
for (v in symmetric_continuous_vars) {
  z_col <- paste0(v, "_z")
  df_transformed[[z_col]] <- scale(df_transformed[[v]])[, 1]
}

## 6c. Binary vars -> ensure numeric 0/1, pass through untouched
for (v in binary_vars) {
  df_transformed[[v]] <- as.numeric(df_transformed[[v]])
}

## 6d. Categorical vars (clustering) -> factors
for (v in categorical_vars) {
  df_transformed[[v]] <- as.factor(df_transformed[[v]])
}

## 6e. PRV_GROUP (profiling only) -> factor, kept out of clustering frame
df_transformed[["PRV_GROUP"]] <- as.factor(df_transformed[["PRV_GROUP"]])

## ------------------------------------------------------------
## 7. ASSEMBLE FINAL CLUSTERING-READY DATASET
## ------------------------------------------------------------

# Final feature columns: z-scored continuous (both types) + binary (raw) + categorical (factor)
z_cols <- c(paste0(skewed_continuous_vars, "_z"), paste0(symmetric_continuous_vars, "_z"))

cluster_features <- df_transformed %>%
  select(all_of(c(z_cols, binary_vars, categorical_vars)))

# Keep ID separately for joining cluster assignments back later
member_ids <- df_transformed %>% select(MEM_NUM)

# Keep PRV_GROUP separately — for POST-HOC profiling of final clusters only,
# never as a clustering input (see note in section 2)
profiling_vars <- df_transformed %>% select(PRV_GROUP)

cat("\nFinal clustering feature set dimensions:", nrow(cluster_features),
    "rows x", ncol(cluster_features), "cols\n")
cat("Expected: 10 skewed-continuous (z) + 8 symmetric-continuous (z) + 5 binary + 3 categorical = 26\n")
stopifnot(ncol(cluster_features) == 26)

## ------------------------------------------------------------
## 8. SAVE OUTPUTS
## ------------------------------------------------------------
saveRDS(cluster_features, "cluster_features.rds")
saveRDS(member_ids, "member_ids.rds")
saveRDS(profiling_vars, "profiling_vars.rds")
write.csv(cbind(MEM_NUM = member_ids$MEM_NUM, cluster_features, profiling_vars),
          "cluster_features.csv", row.names = FALSE)

cat("\nSaved: cluster_features.rds (26-feature clustering input),\n")
cat("       member_ids.rds (join key),\n")
cat("       profiling_vars.rds (PRV_GROUP, for post-hoc profiling only),\n")
cat("       cluster_features.csv (combined, for inspection)\n")
cat("Feature set construction complete. Ready for Step 2 (k-prototypes clustering).\n")
