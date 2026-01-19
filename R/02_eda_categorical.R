# =============================================================================
# 02_eda_categorical.R - Exploratory Data Analysis: Categorical Predictors
# =============================================================================
# Author: Shuaibo
# Task: Analyze categorical predictors and their relationship with y
# =============================================================================

# Load data (this sources 00_setup.R internally)
source("R/01_data_load.R")

message("\n", strrep("=", 60))
message("SECTION 2C: EDA - CATEGORICAL PREDICTORS")
message(strrep("=", 60))

# Define categorical predictors
categorical_vars <- c("sexe", "school_type", "parent_educ", "sleep_qual",
                      "agecat", "attend_pct_cat", "web_access", "trav_time",
                      "extra_act", "study_method")

# =============================================================================
# 1. Frequency Tables
# =============================================================================

message("\n--- Frequency Tables for Categorical Variables ---")

# TODO: Create frequency tables for each categorical variable using tabyl()
#   - Include percentages using adorn_pct_formatting()
#   - Add totals using adorn_totals()

# TODO: Create formatted summary table using gtsummary::tbl_summary()
#   - Add descriptive labels for each variable

# =============================================================================
# 2. Boxplots: y by Each Categorical Predictor
# =============================================================================

message("\n--- Creating Boxplots: y by Categorical Predictors ---")

# TODO: Create boxplot function:
#   - Order categories by median using fct_reorder()
#   - Use geom_boxplot() with fill = "lightblue"
#   - Add geom_jitter() for data points
#   - Add stat_summary() for mean (red diamond)
#   - Use coord_flip()

# TODO: Create boxplots for all 10 categorical variables

# TODO: Combine into 3 panels and save:
#   - output/fig_05_categorical_boxplots_1.png (sexe, school_type, parent_educ, sleep_qual)
#   - output/fig_06_categorical_boxplots_2.png (agecat, attend_pct_cat, web_access, trav_time)
#   - output/fig_07_categorical_boxplots_3.png (extra_act, study_method)

# =============================================================================
# 3. Ordinal Variables - Monotone Trend Analysis
# =============================================================================

message("\n--- Ordinal Variable Trend Analysis ---")

# TODO: For ordinal variables (parent_educ, attend_pct_cat, sleep_qual, agecat):
#   - Calculate mean y and SE for each level
#   - Create line plot connecting group means
#   - Add error bars (mean ± 1.96*SE)
#   - Point size proportional to n

# TODO: Combine into panel and save to output/fig_08_ordinal_trends.png

# =============================================================================
# 4. Statistical Tests by Group (ANOVA)
# =============================================================================

message("\n--- ANOVA F-tests: y by Categorical Predictors ---")

# TODO: For each categorical variable:
#   - Fit ANOVA: aov(y ~ variable, data = train_data)
#   - Extract F-statistic, df, p-value
#   - Calculate eta-squared (effect size): SS_between / SS_total

# TODO: Create results table with:
#   - Variable, F_statistic, df1, df2, p_value, eta_squared
#   - Add significance stars (*, **, ***)
#   - Add effect size interpretation (Small/Medium/Large)

# TODO: Identify significant predictors (p < 0.05)

# =============================================================================
# 5. Summary Statistics by Category
# =============================================================================

message("\n--- Mean Exam Score by Each Category ---")

# TODO: For each categorical variable:
#   - Calculate n, mean_y, sd_y by group
#   - Sort by mean_y descending

# =============================================================================
# 6. Key Findings Summary
# =============================================================================

message("\n", strrep("-", 60))
message("KEY FINDINGS: Categorical Predictors")
message(strrep("-", 60))

# TODO: Summarize:
#   1. Strongest categorical predictors (by eta-squared)
#   2. Ordinal variable trends (monotone increasing/decreasing?)
#   3. Notable patterns in boxplots
#   4. Modeling recommendations

message("\n", strrep("=", 60))
message("EDA - CATEGORICAL PREDICTORS COMPLETE")
message(strrep("=", 60), "\n")
