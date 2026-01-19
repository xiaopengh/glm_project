# =============================================================================
# 04_diagnostics.R - Model Diagnostics
# =============================================================================
# Author: Xiaopeng
# Task: Comprehensive diagnostic checking for the final model
# =============================================================================

# Load modeling results (this sources previous scripts)
source("R/03_modeling.R")

message("\n", strrep("=", 60))
message("SECTION 4: MODEL DIAGNOSTICS")
message(strrep("=", 60))

# TODO: Add diagnostic values to train_data:
#   - fitted = fitted(final_model)
#   - residuals = residuals(final_model)
#   - std_residuals = rstandard(final_model)
#   - student_residuals = rstudent(final_model)
#   - cooks_d = cooks.distance(final_model)
#   - leverage = hatvalues(final_model)

# =============================================================================
# 1. Residuals vs Fitted Values
# =============================================================================

message("\n--- Residuals vs Fitted Values ---")

# TODO: Create scatterplot of residuals vs fitted values
#   - Add horizontal line at y = 0
#   - Add LOESS smooth
#   - Check for non-linearity and heteroskedasticity

# TODO: Perform Breusch-Pagan test using lmtest::bptest()
#   - Report BP statistic and p-value
#   - Interpret: p < 0.05 suggests heteroskedasticity

# =============================================================================
# 2. Residuals vs Each Predictor
# =============================================================================

message("\n--- Residuals vs Predictors ---")

# TODO: For each continuous predictor in the model:
#   - Create residual plot
#   - Add horizontal line at 0
#   - Add LOESS smooth
#   - Look for patterns

# TODO: Combine plots and save to output/fig_13_residuals_vs_predictors.png

# =============================================================================
# 3. Residuals vs Omitted Variables
# =============================================================================

message("\n--- Residuals vs Omitted Variables ---")

# TODO: For variables NOT in the final model:
#   - If numeric: test correlation with residuals
#   - If categorical: test ANOVA of residuals by category

# TODO: Report any significant patterns (suggest adding variable)

# =============================================================================
# 4. Q-Q Plot for Normality
# =============================================================================

message("\n--- Q-Q Plot: Normality of Residuals ---")

# TODO: Create Q-Q plot using qqplotr package:
#   - stat_qq_band() for confidence bands
#   - stat_qq_line() for reference line
#   - stat_qq_point() for data points

# TODO: Perform Shapiro-Wilk test on residuals
#   - Report W statistic and p-value
#   - Interpret: p < 0.05 suggests non-normality

# =============================================================================
# 5. Influence & Leverage Diagnostics
# =============================================================================

message("\n--- Influence & Leverage Diagnostics ---")

# TODO: Calculate thresholds:
#   - Cook's distance: 4/n
#   - Leverage: 2*p/n (where p = number of parameters)

# TODO: Identify influential observations exceeding thresholds

# TODO: Create Cook's distance plot:
#   - Points with Cook's D
#   - Horizontal threshold line
#   - Label high-influence points

# TODO: Create leverage vs residuals plot:
#   - X = leverage, Y = standardized residuals
#   - Point size = Cook's distance
#   - Add threshold lines

# TODO: Combine and save to output/fig_14_influence_diagnostics.png

# TODO: Create table of top 5-10 influential points:
#   - id, y, fitted, residual, Cook's D, leverage

# =============================================================================
# 6. Robustness Check
# =============================================================================

message("\n--- Robustness Check: Excluding Influential Points ---")

# TODO: If high-influence points exist:
#   - Refit model excluding those observations
#   - Compare coefficients (original vs robust)
#   - Calculate % change for each coefficient
#   - Flag if any change > 20%

# =============================================================================
# 7. Diagnostic Summary Panel
# =============================================================================

message("\n--- Creating Diagnostic Summary Panel ---")

# TODO: Use performance::check_model(final_model) for comprehensive panel

# TODO: Save to output/fig_15_diagnostic_panel.png

# =============================================================================
# 8. Summary Statement
# =============================================================================

message("\n", strrep("-", 60))
message("DIAGNOSTIC SUMMARY")
message(strrep("-", 60))

# TODO: Create assumptions check table:
#   - Assumption, Test, Result, Note
#   - Linearity, Homoskedasticity, Normality, Independence, Multicollinearity

# TODO: Overall assessment:
#   - Count passed checks
#   - List any concerns
#   - Recommendations (e.g., robust standard errors if heteroskedasticity)

message("\n", strrep("=", 60))
message("MODEL DIAGNOSTICS COMPLETE")
message(strrep("=", 60), "\n")
