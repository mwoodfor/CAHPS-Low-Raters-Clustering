# Methods: Segmentation of Predicted Low CAHPS Raters

## Overview

We began with a sample of Medicare Advantage members predicted to be low CAHPS raters (≥70% probability of rating below 3.5 stars), drawing on a feature set of 87 member-level variables spanning demographics, socioeconomic status, geography, plan and subsidy status, pharmacy and medical utilization, clinical comorbidity burden, risk scores, and grievance/appeal history.

## Feature Reduction

Many of the original variables were redundant with one another or measured overlapping concepts at different levels of granularity. We reduced the feature set from 87 to 26 variables through a structured review:

- Collapsing redundant geography (state, zip code, and county) down to a single regional indicator.
- Removing pharmacy variables that duplicated overall utilization at a vendor-specific level (e.g., CVS-specific cost and fill counts that mirrored total pharmacy costs).
- Replacing ten individual chronic-condition flags with two summary measures (a condition count and a Charlson comorbidity index) that capture the same clinical information more efficiently.
- Dropping variables that were exact sums or near-duplicates of other retained variables (for example, total out-of-pocket cost, which was the sum of its medical and pharmacy components already in the model).
- Excluding provider group from the clustering inputs specifically because its roughly 20 distinct categories, with no natural way to collapse them into clinically or operationally meaningful groups, risked dominating the segmentation and producing clusters defined by administrative grouping rather than member behavior. Provider group was retained separately for descriptive profiling of the final segments.

The resulting 26 variables were each transformed appropriately for clustering: right-skewed cost and utilization variables were log-transformed and standardized, more symmetric variables (age, tenure, risk scores) were standardized without transformation, and binary indicators were left in their native 0/1 form.

## Clustering

Because the feature set combines continuous, binary, and categorical variables, we used k-prototypes clustering (an extension of k-means designed for mixed-type data), which applies an appropriate distance measure to each variable type rather than forcing everything onto a single numeric scale. We evaluated candidate solutions ranging from 3 to 8 segments, selecting among them using two complementary criteria: a within-cluster dissimilarity (elbow) measure to identify diminishing returns from adding further segments, and silhouette width to assess how distinct and well-separated the segments were from one another. A four-segment solution was selected as the best balance of statistical separation and practical interpretability for outreach and clinical use.

## Summarizing Findings

For each segment, we compared its average value on every clustering variable to the average across the full predicted-low-rater sample, expressing the difference in standard deviation units. This effect-size approach puts variables measured in very different units (dollars, counts, percentages) onto a common, comparable scale, making it possible to identify which variables most distinguish each segment from the overall population and from each other. These distinguishing variables were assembled into a single summary visualization (a heatmap of standardized differences by segment), which we used as the basis for naming each segment and identifying segment-specific implications for outreach and clinical intervention.
