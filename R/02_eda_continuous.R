# =============================================================================
# 02_eda_continuous.R - Exploratory Data Analysis: Continuous Predictors
# =============================================================================
# Author: YY
# Task: Analyze continuous predictors (age, study_hrs, sleep_hrs, attend_pct)
# =============================================================================

# Load data (this sources 00_setup.R internally)
source("R/01_data_load.R")

message("\n", strrep("=", 60))
message("SECTION 2B: EDA - CONTINUOUS PREDICTORS")
message(strrep("=", 60))

# Define continuous predictors
continuous_vars <- c("age", "study_hrs", "sleep_hrs", "attend_pct")

# =============================================================================
# 1. Descriptive Statistics
# =============================================================================

message("\n--- Descriptive Statistics for Continuous Predictors ---")

# TODO: Calculate summary statistics for each continuous variable:
#   - n, mean, sd, median, IQR, min, max

# TODO: Create formatted table using gtsummary::tbl_summary()
#   - Use type = everything() ~ "continuous2"
#   - Include mean, SD, median, IQR, range
#   - Add descriptive labels

# =============================================================================
# 2. Distribution Plots
# =============================================================================

message("\n--- Creating Distribution Plots ---")

# TODO: Create histogram with density for each variable
#   - Use geom_histogram(aes(y = after_stat(density)))
#   - Add geom_density() overlay
#   - Add mean line

# TODO: Create 4 histograms:
#   - p_age_hist, p_study_hist, p_sleep_hist, p_attend_hist

# TODO: Combine using patchwork: (p1 | p2) / (p3 | p4)

# TODO: Save to output/fig_02_continuous_distributions.png

# =============================================================================
# 3. Scatterplots: y vs Each Continuous Predictor
# =============================================================================

message("\n--- Creating Scatterplots: y vs Continuous Predictors ---")

# TODO: Create scatterplot function with LOESS and linear fit:
#   - geom_point() with fill = "dodgerblue"
#   - geom_smooth(method = "loess") in red
#   - geom_smooth(method = "lm") in green dashed

# TODO: Create 4 scatterplots:
#   - p_y_age, p_y_study, p_y_sleep, p_y_attend

# TODO: Combine and save to output/fig_03_continuous_scatterplots.png

# =============================================================================
# 4. Correlation with Response
# =============================================================================

message("\n--- Correlations with Response Variable (y) ---")

# TODO: Calculate correlation matrix for y and continuous predictors

# TODO: Create correlation table showing:
#   - Variable, Correlation, Absolute Correlation, Strength
#   - Strength categories: Strong (>=0.7), Moderate (>=0.4), Weak (>=0.2)

# =============================================================================
# 5. Linearity Assessment
# =============================================================================

message("\n--- Linearity Assessment ---")

# TODO: For each continuous predictor:
#   - Fit linear model: lm(y ~ x)
#   - Fit quadratic model: lm(y ~ x + I(x^2))
#   - Compare using anova()
#   - Report if quadratic term is significant

# =============================================================================
# 6. Heteroskedasticity Check (Visual)
# =============================================================================

message("\n--- Visual Heteroskedasticity Check ---")

# TODO: For each predictor, create residual plot:
#   - Fit simple lm(y ~ x)
#   - Plot residuals vs predictor
#   - Add horizontal line at 0
#   - Add LOESS smooth

# TODO: Combine and save to output/fig_04_continuous_residuals.png

# =============================================================================
# 7. Transformation Assessment
# =============================================================================

message("\n--- Transformation Assessment ---")

# TODO: Check skewness of each predictor using moments::skewness()

# TODO: Make recommendations:
#   - |skew| > 1: Consider transformation
#   - |skew| > 0.5: Minor skew, optional
#   - |skew| < 0.5: No transformation needed

# =============================================================================
# 8. Summary: Key Findings
# =============================================================================

message("\n", strrep("-", 60))
message("KEY FINDINGS: Continuous Predictors")
message(strrep("-", 60))

# TODO: Summarize:
#   1. Strongest predictors by correlation with y
#   2. Variable distributions (means, SDs)
#   3. Linearity assessment results
#   4. Recommendations for modeling

message("\n", strrep("=", 60))
message("EDA - CONTINUOUS PREDICTORS COMPLETE")
message(strrep("=", 60), "\n")
