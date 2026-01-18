# =============================================================================
# 05_interpretation.R - Model Interpretation & Inference
# =============================================================================
# Author: ZZ
# Task: Interpret model coefficients and make scenario-based predictions
# =============================================================================

# Load modeling results (this sources previous scripts)
source("R/03_modeling.R")

message("\n", strrep("=", 60))
message("SECTION 5: MODEL INTERPRETATION & INFERENCE")
message(strrep("=", 60))

# =============================================================================
# 1. Model Equation
# =============================================================================

message("\n--- Model Equation ---")

# TODO: Extract coefficients using coef(final_model)

# TODO: Write out the model equation:
#   E[y] = β0 + β1*x1 + β2*x2 + ...

# TODO: Create LaTeX format for report:
#   \hat{y} = ...

# TODO: Document reference categories for factors

# =============================================================================
# 2. Coefficient Table
# =============================================================================

message("\n--- Coefficient Table ---")

# TODO: Create coefficient table using broom::tidy(final_model, conf.int = TRUE)

# TODO: Add significance stars:
#   - *** p < 0.001
#   - ** p < 0.01
#   - * p < 0.05
#   - . p < 0.1

# TODO: Format columns:
#   - Term, Estimate, Std. Error, 95% CI, t value, p value, Sig

# TODO: Create formatted version for report using kableExtra

# =============================================================================
# 3. Interpretation of Key Effects
# =============================================================================

message("\n--- Interpretation of Key Effects ---")

# TODO: For each significant coefficient, write interpretation:
#
#   For continuous predictors:
#   "For each 1-unit increase in [X], the exam score
#    increases/decreases by [β] points on average,
#    holding all other variables constant.
#    95% CI: [lower, upper]"
#
#   For categorical predictors:
#   "Compared to [reference category], students in [level]
#    score [β] points higher/lower on average,
#    holding all other variables constant.
#    95% CI: [lower, upper]"

# TODO: Include practical interpretations:
#   - study_hrs: "5 more hours/week → +[5*β] points"
#   - attend_pct: "10% increase → +[10*β] points"

# =============================================================================
# 4. Scenario-Based Predictions
# =============================================================================

message("\n--- Scenario-Based Predictions ---")

# TODO: Create profile data based on model variables
#   Profile A (High-achieving):
#     - study_hrs = 12, attend_pct = 90, sleep_hrs = 8
#     - sleep_qual = "Good", school_type = "Private"
#     - parent_educ = "PhD", extra_act = "Yes"
#
#   Profile B (At-risk):
#     - study_hrs = 5, attend_pct = 60, sleep_hrs = 5
#     - sleep_qual = "Poor", school_type = "Public"
#     - parent_educ = "High School", extra_act = "No"

# TODO: Make predictions with confidence intervals:
#   predict(final_model, newdata = profiles, interval = "confidence")

# TODO: Make prediction intervals (for individual students):
#   predict(final_model, newdata = profiles, interval = "prediction")

# TODO: Calculate and interpret difference between profiles

# =============================================================================
# 5. Practical vs Statistical Significance
# =============================================================================

message("\n--- Practical vs Statistical Significance ---")

# TODO: Calculate effect sizes relative to SD of y:
#   Effect_Pct_SD = |estimate| / sd(train_data$y) * 100

# TODO: Classify effect sizes:
#   - > 50% of SD: Large
#   - > 20% of SD: Medium
#   - > 10% of SD: Small
#   - < 10% of SD: Negligible

# TODO: Compare statistical significance (p < 0.05) with practical significance

# TODO: Identify effects that are:
#   - Statistically significant but practically small
#   - Large effects that matter practically

# =============================================================================
# 6. Overall Model Fit Summary
# =============================================================================

message("\n--- Overall Model Fit Summary ---")

# TODO: Extract model fit using broom::glance(final_model)

# TODO: Report and interpret:
#   - R² (% variance explained)
#   - Adjusted R²
#   - Residual Standard Error (in score units)
#   - F-statistic and p-value

# TODO: Practical interpretation:
#   - "The model explains ~X% of variation in exam scores"
#   - "Typical prediction error is ±Y points"

# =============================================================================
# 7. Key Findings Summary
# =============================================================================

message("\n", strrep("-", 60))
message("KEY FINDINGS: Model Interpretation")
message(strrep("-", 60))

# TODO: Summarize:
#   1. Strongest effects (top 3 by magnitude)
#   2. Practical significance assessment
#   3. Scenario predictions (High vs At-risk difference)
#   4. Model fit summary
#   5. Recommendations for interventions

message("\n", strrep("=", 60))
message("MODEL INTERPRETATION COMPLETE")
message(strrep("=", 60), "\n")
