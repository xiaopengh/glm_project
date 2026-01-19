# =============================================================================
# 02_eda_associations.R - Exploratory Data Analysis: Predictor Associations
# =============================================================================
# Author: Yicheng
# Task: Analyze associations between predictors and check for collinearity
# =============================================================================

# Load data (this sources 00_setup.R internally)
source("R/01_data_load.R")

message("\n", strrep("=", 60))
message("SECTION 2D: EDA - PREDICTOR ASSOCIATIONS & COLLINEARITY")
message(strrep("=", 60))

# Define variable groups
continuous_vars <- c("age", "study_hrs", "sleep_hrs", "attend_pct")
categorical_vars <- c("sexe", "school_type", "parent_educ", "sleep_qual",
                      "agecat", "attend_pct_cat", "web_access", "trav_time",
                      "extra_act", "study_method")

# =============================================================================
# 1. Numeric-Numeric Associations
# =============================================================================

message("\n--- Correlation Matrix: Continuous Variables ---")

# TODO: Calculate correlation matrix for y and continuous predictors
#   - Use cor() with use = "pairwise.complete.obs"

# TODO: Identify high correlations (|r| > 0.5, excluding diagonal)
#   - Report variable pairs with high correlation

# TODO: Create correlation heatmap using GGally::ggcorr()
#   - Include labels with 2 decimal places

# TODO: Create pairs plot using GGally::ggpairs()
#   - Include y, study_hrs, attend_pct, sleep_hrs

# TODO: Save to output/fig_09_correlation_heatmap.png
# TODO: Save to output/fig_10_pairs_plot.png

# =============================================================================
# 2. Categorical-Categorical Associations
# =============================================================================

message("\n--- Cross-Tabulations: Key Categorical Pairs ---")

# TODO: Create function for chi-square test with summary:
#   - Create contingency table
#   - Run chisq.test() with simulate.p.value = TRUE
#   - Report chi-square statistic and p-value

# TODO: Test key pairs:
#   - school_type × parent_educ
#   - web_access × school_type
#   - extra_act × study_method
#   - sleep_qual × extra_act

# =============================================================================
# 3. Mixed Associations (Categorical-Numeric)
# =============================================================================

message("\n--- Mixed Associations: Continuous by Categorical ---")

# TODO: Calculate group means for key combinations:
#   - Study hours by school type
#   - Study hours by parent education
#   - Sleep hours by sleep quality
#   - Attendance % by school type
#   - Study hours by extra activities

# TODO: Create boxplots for these combinations

# TODO: Combine and save to output/fig_11_mixed_associations.png

# =============================================================================
# 4. Potential Confounding Patterns
# =============================================================================

message("\n--- Potential Confounding Analysis ---")

# TODO: Check if school type is confounded with other predictors:
#   - Use t.test() for continuous predictors by school_type
#   - Report significant differences

# TODO: Check parent education correlation with continuous predictors:
#   - Convert parent_educ to numeric
#   - Calculate correlations
#   - Report significant associations

# =============================================================================
# 5. Interaction Hypotheses
# =============================================================================

message("\n--- Proposed Interaction Hypotheses ---")

# TODO: Based on EDA findings, propose interactions to test:
#   1. study_hrs × sleep_qual (study effectiveness depends on sleep?)
#   2. attend_pct × school_type (attendance effect differs by school?)
#   3. study_hrs × study_method (method moderates study hours effect?)
#   4. parent_educ × web_access (resources compound?)

# TODO: Create visual check for key interactions:
#   - Scatterplot of y vs continuous, colored by categorical
#   - Add separate regression lines

# TODO: Save to output/fig_12_interaction_exploration.png

# =============================================================================
# 6. VIF Pre-check (Simple Model)
# =============================================================================

message("\n--- VIF Pre-check with Simple Model ---")

# TODO: Fit simple model with continuous predictors:
#   lm(y ~ study_hrs + sleep_hrs + attend_pct + age)

# TODO: Calculate VIF using car::vif()

# TODO: Interpret VIF values:
#   - VIF < 2: No concern
#   - VIF 2-5: Moderate correlation
#   - VIF > 5: Potential multicollinearity issue

# =============================================================================
# 7. Key Findings Summary
# =============================================================================

message("\n", strrep("-", 60))
message("KEY FINDINGS: Predictor Associations & Collinearity")
message(strrep("-", 60))

# TODO: Summarize:
#   1. Numeric-numeric correlations (strongest, any collinearity concerns)
#   2. Categorical-categorical associations (significant pairs)
#   3. Confounding patterns identified
#   4. Recommended interactions to test
#   5. Multicollinearity check result

message("\n", strrep("=", 60))
message("EDA - PREDICTOR ASSOCIATIONS COMPLETE")
message(strrep("=", 60), "\n")
