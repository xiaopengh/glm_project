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

simple_predictors <- c("study_hrs", "attend_pct", "sleep_hrs", "age")

simple_models <- list()
simple_results <- tibble(
  Predictor = character(),
  Estimate = numeric(),
  Std_Error = numeric(),
  t_value = numeric(),
  p_value = numeric(),
  R_squared = numeric(),
  Adj_R_squared = numeric()
)

for (pred in simple_predictors) {
  formula <- as.formula(paste("y ~", pred))
  model <- lm(formula, data = train_data)
  simple_models[[pred]] <- model
  
  model_summary <- summary(model)
  coef_info <- coef(model_summary)[2, ]
  
  simple_results <- bind_rows(simple_results, tibble(
    Predictor = pred,
    Estimate = coef_info[1],
    Std_Error = coef_info[2],
    t_value = coef_info[3],
    p_value = coef_info[4],
    R_squared = model_summary$r.squared,
    Adj_R_squared = model_summary$adj.r.squared
  ))
}

#Summary Table

message("\nSimple Regression Results:")
print(simple_results |>
        mutate(across(where(is.numeric), ~round(.x, 4))) |>
        arrange(desc(R_squared)))


# =============================================================================
# 2. Build Multiple Regression Models
# =============================================================================

message("\n--- Building Multiple Regression Models ---")

# Model 1 (Null)

model_null <- lm(y ~ 1, data = train_data)

# Model 2 (Parsimonious): Top 3-4 predictors from EDA
model_parsimonious <- lm(y ~ study_hrs + attend_pct + sleep_qual,
                            data = train_data)

# Model 3 (Main Effects): Key predictors without interactions
model_main <- lm(y ~ study_hrs + attend_pct + sleep_hrs + sleep_qual +
                      school_type + parent_educ + extra_act, data = train_data)

# Model 4 (Full)
model_full <- lm(y ~ study_hrs + attend_pct + sleep_hrs + age +
                      sleep_qual + school_type + parent_educ +
                      web_access + trav_time + extra_act, data = train_data)

# Model 5 (Interaction): Based on Yicheng's hypotheses
model_interaction <- lm(y ~ study_hrs + attend_pct + sleep_hrs +
                          sleep_qual + school_type + parent_educ + extra_act +
                          study_hrs:sleep_qual + attend_pct:school_type,
                        data = train_data)

# Store all models in a list
models <- list(
  "Null" = model_null,
  "Parsimonious" = model_parsimonious,
  "Main Effects" = model_main,
  "Full" = model_full,
  "Interaction" = model_interaction
)

# =============================================================================
# 3. Model Comparison Table
# =============================================================================

message("\n--- Model Comparison ---")

#Extract metrics for each model

get_model_metrics <- function(model, name) {
  s <- summary(model)
  tibble(
    Model = name,
    n_predictors = length(coef(model)) - 1,
    R_squared = s$r.squared,
    Adj_R_squared = s$adj.r.squared,
    RSE = s$sigma,
    AIC = AIC(model),
    BIC = BIC(model),
    F_stat = ifelse(!is.null(s$fstatistic), s$fstatistic[1], NA),
    F_pvalue = ifelse(!is.null(s$fstatistic),
                      pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3],
                         lower.tail = FALSE), NA)
  )
}

# Comparison table

comparison_table <- map2_dfr(models, names(models), get_model_metrics)

message("\nModel Comparison Table:")
print(comparison_table |>
        mutate(across(where(is.numeric), ~round(.x, 4))))


# Identify best model by each criterion:

message("\nBest Model by Criterion:")
message("  - Highest Adj R²: ", comparison_table$Model[which.max(comparison_table$Adj_R_squared)])
message("  - Lowest AIC: ", comparison_table$Model[which.min(comparison_table$AIC)])
message("  - Lowest BIC: ", comparison_table$Model[which.min(comparison_table$BIC)])


# =============================================================================
# 4. Formal Nested Model Tests
# =============================================================================

message("\n--- Nested Model F-Tests ---")

# TODO: Compare nested models using anova():

# Parsimonious vs Main Effects
test_pars_main <- anova(model_parsimonious, model_main)
message("\nParsimonious vs Main Effects:")
print(test_pars_main)

# Main Effects vs Full
test_main_full <- anova(model_main, model_full)
message("\nMain Effects vs Full:")
print(test_main_full)

# Main Effects vs Interaction
test_main_int <- anova(model_main, model_interaction)
message("\nMain Effects vs Interaction:")
print(test_main_int)


# =============================================================================
# 5. Multicollinearity Check (VIF)
# =============================================================================

message("\n--- Multicollinearity Check (VIF) ---")

# Check VIF for main effects model
message("\nVIF for Main Effects Model:")
vif_main <- car::vif(model_main)
print(vif_main)

# Check VIF for full model
message("\nVIF for Full Model:")
vif_full <- car::vif(model_full)
print(vif_full)

# Flag high VIF values
if (any(vif_main > 5)) {
  message("\nWARNING: VIF > 5 in Main Effects model - consider removing variables")
  message("  High VIF: ", paste(names(vif_main)[vif_main > 5], collapse = ", "))
}

if (any(vif_full > 5)) {
  message("\nWARNING: VIF > 5 in Full model - consider removing variables")
  message("  High VIF: ", paste(names(vif_full)[vif_full > 5], collapse = ", "))
}

# =============================================================================
# 6. Model Selection
# =============================================================================

message("\n--- Final Model Selection ---")

# Criteria-based selection
selection_summary <- tibble(
  Criterion = c("Adj R²", "AIC", "BIC", "Parsimony", "VIF Check"),
  Best_Model = c(
    comparison_table$Model[which.max(comparison_table$Adj_R_squared)],
    comparison_table$Model[which.min(comparison_table$AIC)],
    comparison_table$Model[which.min(comparison_table$BIC)],
    "Parsimonious or Main Effects",
    ifelse(all(vif_main < 5), "Main Effects passes", "Check required")
  )
)

print(selection_summary)

# Decision logic
message("\n--- Model Selection Decision ---")

# Check if interaction terms are significant
int_coefs <- tidy(model_interaction) |>
  filter(str_detect(term, ":")) |>
  filter(p.value < 0.05)

if (nrow(int_coefs) > 0) {
  message("\nSignificant interactions detected:")
  print(int_coefs)
  recommended_model <- "Interaction"
} else {
  message("\nNo significant interactions detected at α = 0.05")
  
  # Compare Main Effects vs Full
  full_test_p <- test_main_full$`Pr(>F)`[2]
  if (full_test_p < 0.05) {
    message("Full model significantly better than Main Effects (p = ",
            round(full_test_p, 4), ")")
    recommended_model <- "Full"
  } else {
    message("Full model NOT significantly better (p = ", round(full_test_p, 4), ")")
    message("Prefer more parsimonious Main Effects model")
    recommended_model <- "Main Effects"
  }
}

message("\n", strrep("-", 40))
message("RECOMMENDED MODEL: ", recommended_model)
message(strrep("-", 40))

# Select final model
final_model <- models[[recommended_model]]

# =============================================================================
# 7. Final Model Summary
# =============================================================================

message("\n--- Final Model Summary ---")

print(summary(final_model))

# Tidy coefficient table
final_coefs <- tidy(final_model, conf.int = TRUE) |>
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  )

message("\nCoefficient Table (Final Model):")
print(final_coefs |>
        mutate(across(where(is.numeric), ~round(.x, 4))))

# Model fit statistics
final_glance <- glance(final_model)
message("\nModel Fit Statistics:")
message("  R² = ", round(final_glance$r.squared, 4))
message("  Adjusted R² = ", round(final_glance$adj.r.squared, 4))
message("  RSE = ", round(final_glance$sigma, 4))
message("  F(", final_glance$df, ", ", final_glance$df.residual, ") = ",
        round(final_glance$statistic, 2), ", p < 0.001")
message("  AIC = ", round(final_glance$AIC, 2))
message("  BIC = ", round(final_glance$BIC, 2))


# =============================================================================
# 8. Save Model Objects
# =============================================================================

message("\n--- Model Objects Saved ---")
message("Available for subsequent scripts:")
message("  - final_model: Selected final model")
message("  - models: List of all candidate models")
message("  - comparison_table: Model comparison metrics")

# Final VIF check
message("\nFinal Model VIF:")
if (length(coef(final_model)) > 2) {
  print(car::vif(final_model))
}

message("\n", strrep("=", 60))
message("MODEL BUILDING & SELECTION COMPLETE")
message(strrep("=", 60), "\n")
