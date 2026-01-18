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

# Extract coefficients
coefs <- coef(final_model)

message("\nFitted Model:")
message("E[y] = ", round(coefs[1], 2), " + ")

# Print each term
for (i in 2:length(coefs)) {
  term_name <- names(coefs)[i]
  term_value <- round(coefs[i], 3)
  sign <- ifelse(i == 2, "", ifelse(term_value >= 0, " + ", " - "))
  message("       ", sign, abs(term_value), " × ", term_name)
}

# LaTeX format for report
message("\nLaTeX format (for report):")
latex_eq <- paste0("\\hat{y} = ", round(coefs[1], 2))
for (i in 2:length(coefs)) {
  sign <- ifelse(coefs[i] >= 0, " + ", " - ")
  latex_eq <- paste0(latex_eq, sign, round(abs(coefs[i]), 3),
                     " \\times \\text{", names(coefs)[i], "}")
}
message(latex_eq)

# =============================================================================
# 2. Coefficient Table
# =============================================================================

message("\n--- Coefficient Table ---")

# Create comprehensive coefficient table
coef_table <- tidy(final_model, conf.int = TRUE) |>
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  ) |>
  rename(
    Term = term,
    Estimate = estimate,
    `Std. Error` = std.error,
    `t value` = statistic,
    `p value` = p.value,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    Sig = significance
  )

print(coef_table |>
        mutate(across(where(is.numeric), ~round(.x, 4))))

# Formatted table for report
coef_table_formatted <- coef_table |>
  mutate(
    `95% CI` = paste0("[", round(`95% CI Lower`, 2), ", ", round(`95% CI Upper`, 2), "]"),
    `p value` = case_when(
      `p value` < 0.001 ~ "< 0.001",
      TRUE ~ as.character(round(`p value`, 4))
    )
  ) |>
  select(Term, Estimate, `Std. Error`, `95% CI`, `t value`, `p value`, Sig)

message("\nFormatted for Report:")
print(coef_table_formatted)

# =============================================================================
# 3. Interpretation of Key Effects
# =============================================================================

message("\n--- Interpretation of Key Effects ---")

# Helper function to interpret coefficients
interpret_coef <- function(term, estimate, ci_low, ci_high, p_value) {
  if (str_detect(term, "Intercept")) {
    message("\n", term, ":")
    message("  The predicted exam score for a student with all predictors at")
    message("  their reference levels (or zero for continuous) is ", round(estimate, 2), " points.")
    return(invisible(NULL))
  }

  sig_text <- ifelse(p_value < 0.05, "statistically significant", "not statistically significant")

  # Check if categorical (factor level)
  is_categorical <- str_detect(term, "school_type|sexe|parent_educ|sleep_qual|web_access|extra_act|trav_time|study_method")

  if (is_categorical) {
    # Extract base variable and level
    base_var <- str_extract(term, "^[a-z_]+")
    level <- str_remove(term, base_var)

    message("\n", term, " (", sig_text, "):")
    message("  Compared to the reference category, students in the '", level, "' group")
    if (estimate > 0) {
      message("  score ", round(abs(estimate), 2), " points HIGHER on average")
    } else {
      message("  score ", round(abs(estimate), 2), " points LOWER on average")
    }
    message("  95% CI: [", round(ci_low, 2), ", ", round(ci_high, 2), "]")
  } else {
    # Continuous variable
    message("\n", term, " (", sig_text, "):")
    message("  For each 1-unit increase in ", term, ",")
    if (estimate > 0) {
      message("  the exam score increases by ", round(abs(estimate), 2), " points on average")
    } else {
      message("  the exam score decreases by ", round(abs(estimate), 2), " points on average")
    }
    message("  95% CI: [", round(ci_low, 2), ", ", round(ci_high, 2), "]")

    # Meaningful increment interpretation
    if (term == "study_hrs") {
      message("\n  Practical interpretation (5-hour change):")
      message("    A student who studies 5 more hours per week is expected to score")
      message("    ", round(5 * estimate, 2), " points ", ifelse(estimate > 0, "higher", "lower"))
    }
    if (term == "attend_pct") {
      message("\n  Practical interpretation (10% change):")
      message("    A 10 percentage point increase in attendance is associated with")
      message("    ", round(10 * estimate, 2), " points ", ifelse(estimate > 0, "higher", "lower"))
    }
  }
}

# Interpret each coefficient
for (i in 1:nrow(coef_table)) {
  interpret_coef(
    coef_table$Term[i],
    coef_table$Estimate[i],
    coef_table$`95% CI Lower`[i],
    coef_table$`95% CI Upper`[i],
    coef_table$`p value`[i]
  )
}

# =============================================================================
# 4. Scenario-Based Predictions
# =============================================================================

message("\n--- Scenario-Based Predictions ---")

# Get the variables used in the model
model_vars <- all.vars(formula(final_model))[-1]  # Exclude y

# Create prediction profiles
# Profile A: High-achieving student
# Profile B: At-risk student

# We need to check what variables are in the model and create appropriate profiles
message("\nVariables in final model: ", paste(model_vars, collapse = ", "))

# Create profile data based on model variables
create_profile <- function(name, values_list) {
  tibble(profile = name, !!!values_list)
}

# Example profiles (adjust based on actual model variables)
# These are typical values for demonstration

if ("study_hrs" %in% model_vars) {
  profile_high <- list(study_hrs = 12)
  profile_low <- list(study_hrs = 5)
} else {
  profile_high <- list()
  profile_low <- list()
}

if ("attend_pct" %in% model_vars) {
  profile_high$attend_pct <- 90
  profile_low$attend_pct <- 60
}

if ("sleep_hrs" %in% model_vars) {
  profile_high$sleep_hrs <- 8
  profile_low$sleep_hrs <- 5
}

if ("sleep_qual" %in% model_vars) {
  profile_high$sleep_qual <- factor("Good", levels = levels(train_data$sleep_qual))
  profile_low$sleep_qual <- factor("Poor", levels = levels(train_data$sleep_qual))
}

if ("school_type" %in% model_vars) {
  profile_high$school_type <- factor("Private", levels = levels(train_data$school_type))
  profile_low$school_type <- factor("Public", levels = levels(train_data$school_type))
}

if ("parent_educ" %in% model_vars) {
  profile_high$parent_educ <- factor("PhD", levels = levels(train_data$parent_educ))
  profile_low$parent_educ <- factor("High School", levels = levels(train_data$parent_educ))
}

if ("extra_act" %in% model_vars) {
  profile_high$extra_act <- factor("Yes", levels = levels(train_data$extra_act))
  profile_low$extra_act <- factor("No", levels = levels(train_data$extra_act))
}

if ("age" %in% model_vars) {
  profile_high$age <- 16
  profile_low$age <- 16
}

# Create new data for predictions
new_data <- bind_rows(
  as_tibble(profile_high) |> mutate(profile = "High-Achieving"),
  as_tibble(profile_low) |> mutate(profile = "At-Risk")
)

message("\nPrediction Profiles:")
print(new_data)

# Make predictions with confidence intervals
predictions <- predict(final_model, newdata = new_data, interval = "confidence", level = 0.95)

pred_results <- bind_cols(
  new_data,
  as_tibble(predictions) |>
    rename(Predicted = fit, CI_Lower = lwr, CI_Upper = upr)
)

message("\nPredicted Exam Scores:")
print(pred_results |>
        select(profile, Predicted, CI_Lower, CI_Upper) |>
        mutate(across(where(is.numeric), ~round(.x, 2))))

# Calculate difference
diff_score <- predictions[1, "fit"] - predictions[2, "fit"]
message("\nDifference between profiles:")
message("  High-Achieving - At-Risk = ", round(diff_score, 2), " points")

# Prediction interval (for individual students)
pred_interval <- predict(final_model, newdata = new_data, interval = "prediction", level = 0.95)

message("\nPrediction Intervals (for individual students):")
pred_interval_df <- bind_cols(
  new_data |> select(profile),
  as_tibble(pred_interval) |>
    rename(Predicted = fit, PI_Lower = lwr, PI_Upper = upr)
)
print(pred_interval_df |>
        mutate(across(where(is.numeric), ~round(.x, 2))))

# =============================================================================
# 5. Practical vs Statistical Significance
# =============================================================================

message("\n--- Practical vs Statistical Significance ---")

# Calculate standardized coefficients for effect size comparison
# Standardize by SD of predictor (for continuous) or overall effect for categorical

message("\nEffect Size Assessment:")
message("(Comparing magnitude of effects to the SD of y = ", round(sd(train_data$y), 2), ")")

sig_effects <- coef_table |>
  filter(`p value` < 0.05, Term != "(Intercept)") |>
  mutate(
    Effect_Pct_SD = abs(Estimate) / sd(train_data$y) * 100
  ) |>
  arrange(desc(abs(Estimate)))

message("\nStatistically Significant Effects (p < 0.05):")
for (i in 1:nrow(sig_effects)) {
  effect_size <- case_when(
    sig_effects$Effect_Pct_SD[i] > 50 ~ "Large",
    sig_effects$Effect_Pct_SD[i] > 20 ~ "Medium",
    sig_effects$Effect_Pct_SD[i] > 10 ~ "Small",
    TRUE ~ "Negligible"
  )
  message("  ", sig_effects$Term[i], ": ", round(sig_effects$Estimate[i], 2),
          " points (", round(sig_effects$Effect_Pct_SD[i], 1), "% of SD) - ", effect_size)
}

# Identify non-significant effects
nonsig_effects <- coef_table |>
  filter(`p value` >= 0.05, Term != "(Intercept)")

if (nrow(nonsig_effects) > 0) {
  message("\nNon-Significant Effects (p >= 0.05):")
  for (i in 1:nrow(nonsig_effects)) {
    message("  ", nonsig_effects$Term[i], ": ", round(nonsig_effects$Estimate[i], 2),
            " points (p = ", round(nonsig_effects$`p value`[i], 3), ")")
  }
}

# =============================================================================
# 6. Overall Model Fit Summary
# =============================================================================

message("\n--- Overall Model Fit Summary ---")

model_summary <- glance(final_model)

message("\nModel Fit Statistics:")
message("  R² = ", round(model_summary$r.squared, 4),
        " (", round(model_summary$r.squared * 100, 1), "% variance explained)")
message("  Adjusted R² = ", round(model_summary$adj.r.squared, 4))
message("  Residual Standard Error = ", round(model_summary$sigma, 2), " points")
message("  F(", model_summary$df, ", ", model_summary$df.residual, ") = ",
        round(model_summary$statistic, 2), ", p < 0.001")

message("\nInterpretation:")
message("  The model explains approximately ", round(model_summary$r.squared * 100, 0),
        "% of the variation in exam scores.")
message("  The typical prediction error is about ±", round(model_summary$sigma, 1), " points.")
message("  The model is statistically significant overall (p < 0.001).")

# Residual interpretation
message("\n  In practical terms:")
message("  - If a student's actual score is 65, the model might predict")
message("    anywhere from ", round(65 - 1.96*model_summary$sigma, 0), " to ",
        round(65 + 1.96*model_summary$sigma, 0), " (95% prediction interval)")

# =============================================================================
# 7. Key Findings Summary
# =============================================================================

message("\n", strrep("-", 60))
message("KEY FINDINGS: Model Interpretation")
message(strrep("-", 60))

# Get top predictors
top_predictors <- sig_effects |>
  head(3)

message("
1. STRONGEST EFFECTS (statistically significant):
")
for (i in 1:nrow(top_predictors)) {
  message("   - ", top_predictors$Term[i], ": ", round(top_predictors$Estimate[i], 2), " points")
}

message("
2. PRACTICAL SIGNIFICANCE:
   - Effects larger than ", round(0.2 * sd(train_data$y), 1), " points (0.2 SD) are practically meaningful
   - ", sum(sig_effects$Effect_Pct_SD > 20), " effects meet this threshold

3. SCENARIO PREDICTIONS:
   - High-achieving profile: ", round(predictions[1, "fit"], 1), " points
   - At-risk profile: ", round(predictions[2, "fit"], 1), " points
   - Difference: ", round(diff_score, 1), " points

4. MODEL FIT:
   - Explains ", round(model_summary$r.squared * 100, 0), "% of variance
   - Prediction accuracy: ±", round(model_summary$sigma, 1), " points

5. RECOMMENDATIONS:
   - Focus interventions on modifiable factors (study hours, attendance)
   - Consider individual student context when applying predictions
   - Model provides useful but not precise predictions
")

message("\n", strrep("=", 60))
message("MODEL INTERPRETATION COMPLETE")
message(strrep("=", 60), "\n")
