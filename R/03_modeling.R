# =============================================================================
# 03_modeling.R - Model Building & Selection
# =============================================================================
# Authors: Yugoo + Yicheng (Collaborative)
# Task: Build, compare, and select the best multiple regression model
# =============================================================================

# Load data (this sources 00_setup.R internally)
source("R/01_data_load.R")

message("\n", strrep("=", 60))
message("SECTION 3: MODEL BUILDING & SELECTION")
message(strrep("=", 60))

# =============================================================================
# 1. Simple Linear Regressions (Baseline Understanding)
# =============================================================================

message("\n--- Simple Linear Regressions ---")

# TODO: Fit simple linear regressions for key predictors:
#   - lm(y ~ study_hrs)
#   - lm(y ~ attend_pct)
#   - lm(y ~ sleep_hrs)
#   - lm(y ~ age)

# TODO: Extract for each model:
#   - Coefficient estimate, std error, t-value, p-value
#   - R-squared, adjusted R-squared

# TODO: Create summary table of simple regression results

# =============================================================================
# 2. Build Multiple Regression Models
# =============================================================================

message("\n--- Building Multiple Regression Models ---")

# TODO: Model 1 (Null): Intercept only
#   model_null <- lm(y ~ 1, data = train_data)

# TODO: Model 2 (Parsimonious): Top 3-4 predictors from EDA
#   model_parsimonious <- lm(y ~ study_hrs + attend_pct + sleep_qual,
#                            data = train_data)

# TODO: Model 3 (Main Effects): Key predictors without interactions
#   model_main <- lm(y ~ study_hrs + attend_pct + sleep_hrs + sleep_qual +
#                      school_type + parent_educ + extra_act, data = train_data)

# TODO: Model 4 (Full): All predictors
#   model_full <- lm(y ~ study_hrs + attend_pct + sleep_hrs + age +
#                      sleep_qual + school_type + parent_educ +
#                      web_access + trav_time + extra_act, data = train_data)

# TODO: Model 5 (Interaction): Based on Yicheng's hypotheses
#   model_interaction <- lm(y ~ ... + study_hrs:sleep_qual +
#                             attend_pct:school_type, data = train_data)

# TODO: Store all models in a list

# =============================================================================
# 3. Model Comparison Table
# =============================================================================

message("\n--- Model Comparison ---")

# TODO: Extract metrics for each model:
#   - Number of predictors
#   - R-squared, Adjusted R-squared
#   - RSE (residual standard error)
#   - AIC, BIC
#   - F-statistic and p-value

# TODO: Create comparison table

# TODO: Identify best model by each criterion:
#   - Highest Adj R²
#   - Lowest AIC
#   - Lowest BIC

# =============================================================================
# 4. Formal Nested Model Tests
# =============================================================================

message("\n--- Nested Model F-Tests ---")

# TODO: Compare nested models using anova():
#   - Parsimonious vs Main Effects
#   - Main Effects vs Full
#   - Main Effects vs Interaction

# TODO: Report F-statistics and p-values
# TODO: Interpret which additional terms are significant

# =============================================================================
# 5. Multicollinearity Check (VIF)
# =============================================================================

message("\n--- Multicollinearity Check (VIF) ---")

# TODO: Calculate VIF for main effects model using car::vif()

# TODO: Calculate VIF for full model

# TODO: Flag any VIF > 5 (potential multicollinearity)

# TODO: Report recommendation

# =============================================================================
# 6. Model Selection
# =============================================================================

message("\n--- Final Model Selection ---")

# TODO: Create selection summary table:
#   - Criterion, Best Model
#   - Adj R², AIC, BIC, Parsimony, VIF Check

# TODO: Check if interaction terms are significant

# TODO: Make decision based on multiple criteria

# TODO: Assign final_model

# =============================================================================
# 7. Final Model Summary
# =============================================================================

message("\n--- Final Model Summary ---")

# TODO: Print summary(final_model)

# TODO: Create tidy coefficient table using broom::tidy()
#   - Include confidence intervals
#   - Add significance stars

# TODO: Report model fit statistics:
#   - R², Adjusted R²
#   - RSE
#   - F-statistic and p-value
#   - AIC, BIC

# =============================================================================
# 8. Alternative Model Exploration
# =============================================================================

message("\n--- Alternative Model: Stepwise Selection (for comparison) ---")

# TODO: Run stepwise selection using step()
#   - Start from null model
#   - Scope to full model
#   - Direction = "both"

# TODO: Compare stepwise result with manual selection

# =============================================================================
# 9. Save Model Objects
# =============================================================================

message("\n--- Model Objects Saved ---")
message("Available for subsequent scripts:")
message("  - final_model: Selected final model")
message("  - models: List of all candidate models")
message("  - comparison_table: Model comparison metrics")

# TODO: Print final VIF check

message("\n", strrep("=", 60))
message("MODEL BUILDING & SELECTION COMPLETE")
message(strrep("=", 60), "\n")
