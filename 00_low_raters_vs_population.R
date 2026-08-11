## ============================================================
## CAHPS Low-Rater Segmentation: Low Raters vs. Remaining Population
## Step 0 of analytic plan -- run BEFORE the segmentation pipeline
## (01-04). Answers a different question than 01-07: not "how do
## low raters differ from each other" but "how do predicted low
## raters differ from everyone else." Sets up the first part of the
## Medicare business-unit storyline (whole population -> low raters
## -> segments within low raters -> Cluster 3 deep dive).
##
## NOTE: drafted without an R interpreter available to test-run end
## to end (same limitation noted in 05/06/07 -- no R install in the
## drafting environment, and no access to the real source file).
## Structure and logic mirror 01_build_feature_set.R and
## 03_profile_clusters.R conventions closely. Run section-by-section
## with print(dim(...)) / str(...) checks the first time through.
## ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)

## ------------------------------------------------------------
## 0. CONFIG -- edit before running
## ------------------------------------------------------------

## This should be the UNFILTERED scored population -- i.e. the same
## source that low_rater_sample.csv was filtered from in
## 01_build_feature_set.R, before the >=70% predicted-probability
## cut. If low_rater_sample.csv is all you have, this script cannot
## build the "remaining members" comparison group -- you need the
## full scored population (or a representative sample of it) here.
input_path <- "C:/Users/mmarti06/OneDrive - BCBSMA/Evaluation (Debbie)/Project Work/Medicare Stars/CAHPS/Low Raters Clustering/raw/scored_population.sas7bdat"   

## How to identify the predicted-low-rater group within input_path.
## Set group_flag_source to "probability" if you have a raw predicted
## probability column and want to apply the same >=70% cut used to
## build low_rater_sample.csv (see methods_writeup.md), or to "flag"
## if the source file already carries a precomputed 0/1 indicator.
group_flag_source   <- "probability"   # "probability" or "flag"
low_rater_prob_col  <- "p_lt_3p5"          # used if group_flag_source == "probability"
low_rater_threshold <- 0.70                       # matches methods_writeup.md definition
#low_rater_flag_col  <- "predicted_low_rater_flag" # used if group_flag_source == "flag"

prv_group_min_pct <- 0.01   # collapse PRV_GROUP levels below 1% of the FULL population
# (mirrors 01_build_feature_set.R; recomputed here on the full
# population rather than reusing the low-rater-only collapse,
# since rare-level thresholds should reflect the population being
# summarized)

top_n_tornado <- 15   # how many variables to show on the tornado chart

plot_dir <- "C:/Users/mmarti06/OneDrive - BCBSMA/Evaluation (Debbie)/Project Work/Medicare Stars/CAHPS/Low Raters Clustering/output/"
dir.create(plot_dir, showWarnings = FALSE)


## ------------------------------------------------------------
## 1. LOAD RAW DATA
## ------------------------------------------------------------
raw <- read_sas(input_path)
cat("Raw population dimensions:", nrow(raw), "rows x", ncol(raw), "cols\n")

## Same 24 substantive variables as 01_build_feature_set.R (MEM_NUM +
## demographics/utilization/clinical/grievance vars), plus PRV_GROUP for
## profiling, plus whichever prediction column identifies the low-rater group.
required_vars <- c(
  "MEM_NUM",
  "MEM_GENDER", "MEM_AGE", "ses_index", "tenure_years",
  "member_plan", "lis_ind", "dis_ind",
  "family_members", "myblue_visits", "email_optin",
  "MA_REGION",
  "total_pharm_oop", "total_pharm_allow", "total_supply_days",
  "abandoned_scripts", "total_pharm_denied", "mail_order_flag",
  "total_med_allow", "total_med_oop", "total_med_denied",
  "ct_condit", "charlson_index", "DXCG_RRS_EXP_CON",
  "total_grievances", "total_appeals", "any_auth"
)
profiling_only_vars <- c("PRV_GROUP")
prediction_col <- if (group_flag_source == "probability") low_rater_prob_col else low_rater_flag_col

missing_vars <- setdiff(c(required_vars, profiling_only_vars, prediction_col), names(raw))
if (length(missing_vars) > 0) {
  stop("Missing expected variable(s) in source data: ",
       paste(missing_vars, collapse = ", "))
}

## ------------------------------------------------------------
## 2. BUILD THE GROUP INDICATOR
## ------------------------------------------------------------
df <- raw %>% select(all_of(c(required_vars, profiling_only_vars, prediction_col)))

if (group_flag_source == "probability") {
  df <- df %>%
    mutate(low_rater_group = if_else(.data[[low_rater_prob_col]] >= low_rater_threshold,
                                      "Predicted low rater", "Remaining members"))
} else {
  stopifnot(all(df[[low_rater_flag_col]] %in% c(0, 1)))
  df <- df %>%
    mutate(low_rater_group = if_else(.data[[low_rater_flag_col]] == 1,
                                      "Predicted low rater", "Remaining members"))
}
df$low_rater_group <- factor(df$low_rater_group,
                              levels = c("Predicted low rater", "Remaining members"))

group_counts <- table(df$low_rater_group)
cat("\nGroup sizes:\n")
print(group_counts)
if (any(group_counts == 0)) {
  stop("One of the two groups has zero members -- check ", prediction_col,
       " and the threshold/flag config above before proceeding.")
}

## ------------------------------------------------------------
## 3. COLLAPSE RARE PRV_GROUP LEVELS (profiling only, computed on the
##    full population so thresholds reflect the whole member base)
## ------------------------------------------------------------
prv_freq <- df %>% count(PRV_GROUP) %>% mutate(pct = n / sum(n))
rare_groups <- prv_freq %>% filter(pct < prv_group_min_pct) %>% pull(PRV_GROUP)
df <- df %>% mutate(PRV_GROUP = if_else(PRV_GROUP %in% rare_groups, "Other", PRV_GROUP))
cat("\nPRV_GROUP levels collapsed to 'Other':", length(rare_groups), "of", nrow(prv_freq), "levels\n")

## ------------------------------------------------------------
## 4. MISSING VALUE CHECK
## ------------------------------------------------------------
na_summary <- df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  filter(n_missing > 0) %>%
  arrange(desc(n_missing))
if (nrow(na_summary) > 0) {
  cat("\nVariables with missing values (review before comparing groups):\n")
  print(na_summary)
} else {
  cat("\nNo missing values detected in selected variable set.\n")
}

## ------------------------------------------------------------
## 5. VARIABLE BLOCKS (mirrors 01_build_feature_set.R / 03_profile_clusters.R)
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
profiling_only_categorical <- c("PRV_GROUP")

## ------------------------------------------------------------
## 6. EFFECT SIZES: predicted low raters vs. remaining members
## ------------------------------------------------------------
## Deliberately Cohen's d between the two groups directly -- (mean_low_rater
## - mean_remaining) / pooled_sd -- rather than "each group vs. the combined
## overall mean" (the convention used in 03/04a for comparing a cluster to
## the population it's drawn from). Here the two groups are mutually
## exclusive and exhaustive, so a direct group-vs-group standardized
## difference is the more natural and more correct "how do low raters
## differ from the rest" statement. Skewed continuous vars are log1p'd
## first, same as 01_build_feature_set.R, so the effect size reflects the
## same transformed scale used elsewhere in this project. Binary vars are
## included on the same scale (0/1 numeric) so everything can sit on one
## tornado chart, consistent with 04a_build_heatmap_data.R's approach.

df_t <- df %>%
  mutate(across(all_of(skewed_continuous_vars), ~ log1p(.x), .names = "{.col}_log"))

effect_size_vars <- c(paste0(skewed_continuous_vars, "_log"), symmetric_continuous_vars, binary_vars)
## friendly variable key for output (strip the _log suffix so it matches
## the underlying variable name used elsewhere in the project)
effect_size_var_key <- c(paste0(skewed_continuous_vars), symmetric_continuous_vars, binary_vars)

cohens_d <- function(x, group) {
  g1 <- x[group == "Predicted low rater"]
  g2 <- x[group == "Remaining members"]
  n1 <- sum(!is.na(g1)); n2 <- sum(!is.na(g2))
  m1 <- mean(g1, na.rm = TRUE); m2 <- mean(g2, na.rm = TRUE)
  s1 <- sd(g1, na.rm = TRUE);   s2 <- sd(g2, na.rm = TRUE)
  pooled_sd <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  list(mean_low_rater = m1, mean_remaining = m2, pooled_sd = pooled_sd,
       d = (m1 - m2) / pooled_sd)
}

effect_size_rows <- lapply(seq_along(effect_size_vars), function(i) {
  v <- effect_size_vars[i]
  key <- effect_size_var_key[i]
  res <- cohens_d(df_t[[v]], df_t$low_rater_group)
  ## Welch's t-test shown for completeness -- with population-scale N this
  ## will very likely be significant for most variables, so lead with the
  ## effect size (d) for the narrative, not the p-value. See guideline
  ## below section 8.
  tt <- t.test(df_t[[v]] ~ df_t$low_rater_group)
  data.frame(
    variable = key,
    mean_low_rater = res$mean_low_rater,
    mean_remaining = res$mean_remaining,
    cohens_d = res$d,
    p_value = tt$p.value
  )
})
effect_sizes <- bind_rows(effect_size_rows) %>%
  arrange(desc(abs(cohens_d)))

cat("\n=== Effect sizes (Cohen's d), predicted low raters vs. remaining members ===\n")
cat("(d computed on log1p-transformed values for right-skewed cost/count variables,\n")
cat(" matching the transformation used elsewhere in this project; means shown are on\n")
cat(" that transformed scale for those variables -- see profile_*_original_units.csv\n")
cat(" style outputs elsewhere in the pipeline for real-units means.)\n")
print(effect_sizes, n = 50)

## Guideline for interpreting |d| (Cohen, 1988): ~0.2 small, ~0.5 medium, ~0.8 large.
cat("\nGuideline: |d| ~0.2 small, ~0.5 medium, ~0.8 large. With a population-scale\n")
cat("sample, p-values will nearly always be significant -- treat |d| as the signal.\n")

## Also compute plain real-units means (no log transform) for the write-up,
## so effect sizes and reported means aren't on mismatched scales for readers
means_original_units <- df %>%
  group_by(low_rater_group) %>%
  summarise(across(all_of(c(all_continuous_vars, binary_vars)), ~round(mean(.x, na.rm = TRUE), 2))) %>%
  pivot_longer(-low_rater_group, names_to = "variable", values_to = "mean_value") %>%
  pivot_wider(names_from = low_rater_group, values_from = mean_value)

cat("\n=== Group means, original units (for narrative/table use) ===\n")
print(means_original_units, n = 50)

## ------------------------------------------------------------
## 7. CATEGORICAL VARIABLE COMPARISON (MEM_GENDER, member_plan, MA_REGION,
##    PRV_GROUP) -- percentage-point differences by level, since these
##    don't collapse onto a single effect-size scale
## ------------------------------------------------------------
categorical_comparison <- list()
for (v in c(categorical_vars, profiling_only_categorical)) {
  tab <- df %>%
    group_by(low_rater_group, .data[[v]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(low_rater_group) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    ungroup() %>%
    rename(level = !!v) %>%
    select(low_rater_group, level, pct) %>%
    pivot_wider(names_from = low_rater_group, values_from = pct, values_fill = 0) %>%
    mutate(pct_point_diff = `Predicted low rater` - `Remaining members`) %>%
    arrange(desc(abs(pct_point_diff)))
  categorical_comparison[[v]] <- tab
  cat("\n=== ", v, ": % by group, ranked by gap ===\n")
  print(tab, n = 30)
}

## ------------------------------------------------------------
## 8. TORNADO CHART -- top distinguishing variables
## ------------------------------------------------------------

## Human-readable labels for the clustering variables -- same mapping used
## in 04a_build_heatmap_data.R (js_label_lookup) and
## 07_profile_hier_subclusters.R (variable_labels_default), kept in sync
## here deliberately so a variable reads the same way across every
## deliverable in this project. If a variable used in this script isn't in
## this lookup, it falls back to the raw name -- extend the list below
## rather than let that happen silently.
variable_labels <- c(
  total_grievances    = "Grievances filed",
  ct_condit           = "Chronic conditions (count)",
  charlson_index      = "Comorbidity burden (Charlson)",
  total_med_oop       = "Medical out-of-pocket cost",
  total_med_allow     = "Medical allowed cost",
  any_auth            = "Any prior authorization",
  total_pharm_oop     = "Pharmacy out-of-pocket cost",
  total_pharm_allow   = "Pharmacy allowed cost",
  total_supply_days   = "Pharmacy supply days",
  abandoned_scripts   = "Abandoned prescriptions",
  total_pharm_denied  = "Pharmacy claims denied",
  total_med_denied    = "Medical claims denied",
  total_appeals       = "Appeals filed",
  DXCG_RRS_EXP_CON    = "DxCG",
  MEM_AGE             = "Member age",
  tenure_years        = "Tenure (years)",
  ses_index           = "Socioeconomic index",
  family_members      = "Household size",
  myblue_visits       = "Member portal visits",
  lis_ind             = "Low-income subsidy",
  dis_ind             = "Disability status",
  email_optin         = "Email opt-in",
  mail_order_flag     = "Mail-order pharmacy user"
)
get_label <- function(v) if (v %in% names(variable_labels)) variable_labels[[v]] else v

## Any variable in the tornado chart missing a label above -- surfaced
## loudly rather than silently falling back to the raw column name.
unlabeled <- setdiff(effect_sizes$variable[seq_len(min(top_n_tornado, nrow(effect_sizes)))],
                      names(variable_labels))
if (length(unlabeled) > 0) {
  cat("\nNOTE: no human-readable label defined for:", paste(unlabeled, collapse = ", "),
      "-- add to variable_labels above; raw variable name used as a fallback for now.\n")
}

tornado_data <- effect_sizes %>%
  slice_head(n = top_n_tornado) %>%
  mutate(
    direction = if_else(cohens_d > 0, "Higher among low raters", "Lower among low raters"),
    ## Explicit level order (not alphabetical) so the bottom legend reads
    ## left-to-right as "Lower" then "Higher" -- matching the direction
    ## each color's bars actually point on the plot (blue bars extend left
    ## of 0, red bars extend right of 0).
    direction = factor(direction, levels = c("Lower among low raters", "Higher among low raters")),
    variable_label = sapply(as.character(variable), get_label),
    variable_label = factor(variable_label, levels = rev(variable_label))
  )

## Wrap the subtitle manually -- ggplot2 does not auto-wrap plot titles/
## subtitles, so a long one-line string (with large group sizes) runs off
## the right edge of the saved image rather than wrapping to a second line.
subtitle_text <- paste0(
  "Standardized difference (Cohen's d), predicted low raters (n=",
  format(group_counts["Predicted low rater"], big.mark = ","),
  ") vs. remaining members (n=",
  format(group_counts["Remaining members"], big.mark = ","), ")"
)
subtitle_wrapped <- paste(strwrap(subtitle_text, width = 65), collapse = "\n")

## Plain-language footnote explaining Cohen's d and how to read the chart,
## for readers who aren't going to look up the statistic elsewhere. Wrapped
## the same way as the subtitle, for the same reason.
caption_text <- paste(
  "Cohen's d measures how far apart the two groups' averages are, in",
  "standard-deviation units -- it accounts for how spread out each group's",
  "values are, not just the raw difference. Rough guide: ~0.2 is a small",
  "difference, ~0.5 is moderate, ~0.8 or higher is large. Bars pointing",
  "right (red) mean predicted low raters average higher on that measure",
  "than the rest of the membership; bars pointing left (blue) mean lower.",
  "Longer bars = bigger gap between the two groups."
)
caption_wrapped <- paste(strwrap(caption_text, width = 95), collapse = "\n")

tornado_plot <- ggplot(tornado_data, aes(x = variable_label, y = cohens_d, fill = direction)) +
  geom_col(width = 0.65) +
  coord_flip() +
  scale_fill_manual(values = c("Higher among low raters" = "#993C1D",
                                "Lower among low raters" = "#185FA5")) +
  labs(
    title = "How predicted low raters differ from the rest of the membership",
    subtitle = subtitle_wrapped,
    x = NULL, y = "Cohen's d",
    fill = NULL,
    caption = caption_wrapped
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 10, color = "grey30"),
    plot.caption = element_text(size = 8, color = "grey40", hjust = 0, lineheight = 1.2),
    plot.caption.position = "plot"
  )

ggsave(file.path(plot_dir, "low_raters_vs_population_tornado.png"),
       tornado_plot, width = 9, height = 7.8, dpi = 200)
cat("\nSaved tornado chart to", file.path(plot_dir, "low_raters_vs_population_tornado.png"), "\n")

## ------------------------------------------------------------
## 9. AUTO-GENERATED PLAIN-LANGUAGE STATEMENTS (draft only -- review
##    before using; mirrors the "suggested cluster names" draft-only
##    pattern in 03_profile_clusters.R)
## ------------------------------------------------------------
cat("\n============================================================\n")
cat("DRAFT NARRATIVE STATEMENTS (REVIEW BEFORE USING)\n")
cat("============================================================\n")

narrative_lines <- c()
for (i in seq_len(min(top_n_tornado, nrow(effect_sizes)))) {
  row <- effect_sizes[i, ]
  orig_row <- means_original_units %>% filter(variable == row$variable)
  if (nrow(orig_row) == 1) {
    line <- sprintf(
      "Predicted low raters average %s on %s, vs. %s among remaining members (d = %.2f).",
      format(round(orig_row$`Predicted low rater`, 2), big.mark = ","),
      row$variable,
      format(round(orig_row$`Remaining members`, 2), big.mark = ","),
      row$cohens_d
    )
    narrative_lines <- c(narrative_lines, line)
    cat(line, "\n")
  }
}
writeLines(narrative_lines, "population_comparison_narrative_statements.txt")

## ------------------------------------------------------------
## 10. SAVE TABLES
## ------------------------------------------------------------
write.csv(effect_sizes, "population_comparison_effect_sizes.csv", row.names = FALSE)
write.csv(means_original_units, "population_comparison_means_original_units.csv", row.names = FALSE)
for (v in names(categorical_comparison)) {
  write.csv(categorical_comparison[[v]],
            paste0("population_comparison_categorical_", v, ".csv"), row.names = FALSE)
}

cat("\nSaved: population_comparison_effect_sizes.csv,\n")
cat("       population_comparison_means_original_units.csv,\n")
cat("       population_comparison_categorical_*.csv,\n")
cat("       population_comparison_narrative_statements.txt,\n")
cat("       ", plot_dir, "/low_raters_vs_population_tornado.png\n", sep = "")
cat("Population comparison complete. Feeds the opening section of the storyline\n")
cat("(whole population -> low raters -> segments -> Cluster 3 deep dive).\n")
