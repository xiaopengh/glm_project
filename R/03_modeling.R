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

# Model 2 (minimal): Top 4 predictors from EDA
model_minimal <- lm(y ~ study_hrs + attend_pct + sleep_qual + parent_educ,
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


# Let's pay special attention to the last model. Let us do Type I and II analysis:
type_I <- anova(model_interaction)
type_II <- Anova(model_interaction)
message("\nType I and II analysis:")
print(type_I)
print(type_II)

#In both tests, the p value for attend_pct:school_type are pretty high. 
#Therefore, we decided to remove it from the model.

model_new <- lm(y ~ study_hrs + attend_pct + sleep_hrs +
                  sleep_qual + school_type + parent_educ + extra_act +
                  study_hrs:sleep_qual,
                data = train_data)

test1 <- anova(model_new)
test2 <- Anova(model_new)

print(test1)
print(test2)

#we are satisfied with the new model.

# Store all models in a list
models <- list(
  "Null" = model_null,
  "minimal" = model_minimal,
  "Main Effects" = model_main,
  "Full" = model_full,
  "Interaction" = model_interaction,
  "New" = model_new
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
# 4. Multicollinearity Check (VIF)
# =============================================================================

message("\n--- Multicollinearity Check (VIF) ---")

#check VIF for the new model
message("\nVIF for New Model:")
if (length(coef(model_new)) > 2) {
  print(car::vif(model_new))
}
message("No variables need to be removed, since each VIF value is lower than 5.
        And we can see the VIF values of some variables are around 3 but still
        acceptable. ")


# =============================================================================
# 5. Final Model Summary
# =============================================================================

message("\n--- Final Model Summary ---")

final_model <- model_new

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
# 6. Save Model Objects
# =============================================================================

message("\n--- Model Objects Saved ---")
message("  - final_model: New model")

message("\n", strrep("=", 60))
message("MODEL BUILDING & SELECTION COMPLETE")
message(strrep("=", 60), "\n")

