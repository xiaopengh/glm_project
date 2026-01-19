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

# Get residuals and fitted values
train_data_diag <- train_data |>
  mutate(
    fitted = fitted(final_model),
    residuals = residuals(final_model),
    std_residuals = rstandard(final_model),
    student_residuals = rstudent(final_model),
    cooks_d = cooks.distance(final_model),
    leverage = hatvalues(final_model)
  )

# =============================================================================
# 1. Residuals vs Fitted Values
# =============================================================================

message("\n--- Residuals vs Fitted Values ---")

p_resid_fitted <- ggplot(train_data_diag, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "orange", linewidth = 1) +
  labs(
    title = "Residuals vs Fitted Values",
    subtitle = "Check for non-linearity and heteroskedasticity",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_glm()

print(p_resid_fitted)

# Formal test for heteroskedasticity (Breusch-Pagan)
bp_test <- lmtest::bptest(final_model)
message("\nBreusch-Pagan Test for Heteroskedasticity:")
message("  BP = ", round(bp_test$statistic, 4))
message("  p-value = ", format.pval(bp_test$p.value, digits = 4))
if (bp_test$p.value < 0.05) {
  message("  → Evidence of heteroskedasticity (p < 0.05)")
} else {
  message("  → No evidence of heteroskedasticity (p >= 0.05)")
}

# =============================================================================
# 2. Residuals vs Each Predictor
# =============================================================================

message("\n--- Residuals vs Predictors ---")

# Get predictor names from model
predictor_names <- names(coef(final_model))[-1]  # Exclude intercept

# Continuous predictors in model
cont_in_model <- intersect(c("study_hrs", "attend_pct", "sleep_hrs", "age"),
                            all.vars(formula(final_model)))

# Create residual plots for continuous predictors
if (length(cont_in_model) >= 1) {
  resid_plots <- list()

  for (pred in cont_in_model) {
    resid_plots[[pred]] <- ggplot(train_data_diag, aes(x = .data[[pred]], y = residuals)) +
      geom_point(alpha = 0.5, color = "gray40") +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      geom_smooth(method = "loess", se = FALSE, color = "blue") +
      labs(title = paste("Residuals vs", pred), x = pred, y = "Residuals") +
      theme_glm(base_size = 11)
  }

  # Combine plots
  if (length(resid_plots) >= 4) {
    p_resid_pred <- (resid_plots[[1]] | resid_plots[[2]]) /
                    (resid_plots[[3]] | resid_plots[[4]])
  } else if (length(resid_plots) >= 2) {
    p_resid_pred <- resid_plots[[1]] | resid_plots[[2]]
  } else {
    p_resid_pred <- resid_plots[[1]]
  }

  p_resid_pred <- p_resid_pred +
    plot_annotation(
      title = "Residuals vs Continuous Predictors",
      theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    )

  print(p_resid_pred)

  ggsave("output/fig_13_residuals_vs_predictors.png", p_resid_pred,
         width = 12, height = 10, dpi = 300)
}

# =============================================================================
# 3. Residuals vs Omitted Variables
# =============================================================================

message("\n--- Residuals vs Omitted Variables ---")

# Check if any important predictors were omitted
omitted_vars <- c("age", "sexe", "web_access", "trav_time", "study_method")
omitted_in_data <- intersect(omitted_vars, names(train_data))

# Test for pattern in residuals vs omitted variables
omitted_tests <- tibble(
  Variable = character(),
  Test_Type = character(),
  Statistic = numeric(),
  p_value = numeric()
)

for (var in omitted_in_data) {
  if (!var %in% all.vars(formula(final_model))) {
    if (is.numeric(train_data_diag[[var]])) {
      # Correlation test for numeric
      test <- cor.test(train_data_diag$residuals, train_data_diag[[var]])
      omitted_tests <- bind_rows(omitted_tests, tibble(
        Variable = var,
        Test_Type = "Correlation",
        Statistic = test$estimate,
        p_value = test$p.value
      ))
    } else {
      # ANOVA for categorical
      aov_test <- summary(aov(residuals ~ .data[[var]], data = train_data_diag))[[1]]
      omitted_tests <- bind_rows(omitted_tests, tibble(
        Variable = var,
        Test_Type = "ANOVA F-test",
        Statistic = aov_test$`F value`[1],
        p_value = aov_test$`Pr(>F)`[1]
      ))
    }
  }
}

if (nrow(omitted_tests) > 0) {
  message("\nTests for omitted variable patterns:")
  print(omitted_tests)

  sig_omitted <- omitted_tests |> filter(p_value < 0.05)
  if (nrow(sig_omitted) > 0) {
    message("\nWARNING: Significant patterns detected in residuals for: ",
            paste(sig_omitted$Variable, collapse = ", "))
    message("Consider adding these variables to the model")
  } else {
    message("\nNo significant patterns detected - omitted variables appear appropriate")
  }
}

# =============================================================================
# 4. Q-Q Plot for Normality
# =============================================================================

message("\n--- Q-Q Plot: Normality of Residuals ---")

p_qq <- ggplot(train_data_diag, aes(sample = std_residuals)) +
  stat_qq_band(fill = "lightblue", alpha = 0.5, conf = 0.95) +
  stat_qq_line(color = "red", linewidth = 1) +
  stat_qq_point(size = 2, alpha = 0.6, color = "steelblue") +
  labs(
    title = "Normal Q-Q Plot of Standardized Residuals",
    subtitle = "Points should follow the red line; shaded area = 95% CI",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_glm()

print(p_qq)

# Shapiro-Wilk test on residuals
shapiro_resid <- shapiro.test(train_data_diag$residuals)
message("\nShapiro-Wilk Test for Normality of Residuals:")
message("  W = ", round(shapiro_resid$statistic, 4))
message("  p-value = ", format.pval(shapiro_resid$p.value, digits = 4))
if (shapiro_resid$p.value < 0.05) {
  message("  → Evidence against normality (p < 0.05)")
  message("  → Consider: robust standard errors or transformation")
} else {
  message("  → Normality assumption appears satisfied")
}

# =============================================================================
# 5. Influence & Leverage Diagnostics
# =============================================================================

message("\n--- Influence & Leverage Diagnostics ---")

n <- nrow(train_data_diag)
p <- length(coef(final_model))

# Thresholds
cooks_threshold <- 4 / n
leverage_threshold <- 2 * p / n

# Identify influential observations
influential_obs <- train_data_diag |>
  filter(cooks_d > cooks_threshold | leverage > leverage_threshold) |>
  select(id, y, fitted, residuals, cooks_d, leverage) |>
  arrange(desc(cooks_d))

message("\nCook's Distance threshold (4/n): ", round(cooks_threshold, 4))
message("Leverage threshold (2p/n): ", round(leverage_threshold, 4))
message("\nNumber of potentially influential observations: ", nrow(influential_obs))

if (nrow(influential_obs) > 0) {
  message("\nTop 10 influential observations:")
  print(influential_obs |> head(10) |>
          mutate(across(where(is.numeric), ~round(.x, 4))))
}

# Cook's distance plot
p_cooks <- ggplot(train_data_diag, aes(x = seq_len(n), y = cooks_d)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = cooks_threshold, color = "red",
             linetype = "dashed", linewidth = 1) +
  geom_text(data = train_data_diag |> filter(cooks_d > cooks_threshold),
            aes(label = id), hjust = -0.3, size = 3) +
  labs(
    title = "Cook's Distance",
    subtitle = paste("Threshold = 4/n =", round(cooks_threshold, 4)),
    x = "Observation Index",
    y = "Cook's Distance"
  ) +
  theme_glm()

# Leverage vs Residuals
p_leverage <- ggplot(train_data_diag, aes(x = leverage, y = std_residuals)) +
  geom_point(aes(size = cooks_d), alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = c(-2, 0, 2), color = c("orange", "black", "orange"),
             linetype = c("dashed", "solid", "dashed")) +
  geom_vline(xintercept = leverage_threshold, color = "red", linetype = "dashed") +
  labs(
    title = "Leverage vs Standardized Residuals",
    subtitle = "Size proportional to Cook's distance",
    x = "Leverage (Hat Values)",
    y = "Standardized Residuals",
    size = "Cook's D"
  ) +
  theme_glm()

p_influence <- p_cooks | p_leverage

print(p_influence)

ggsave("output/fig_14_influence_diagnostics.png", p_influence,
       width = 14, height = 6, dpi = 300)

# =============================================================================
# 6. Robustness Check
# =============================================================================

message("\n--- Robustness Check: Excluding Influential Points ---")

# Identify high influence points (Cook's D > threshold)
high_influence_ids <- train_data_diag |>
  filter(cooks_d > cooks_threshold) |>
  pull(id)

if (length(high_influence_ids) > 0) {
  # Refit model without influential points
  train_data_robust <- train_data |>
    filter(!id %in% high_influence_ids)

  model_robust <- update(final_model, data = train_data_robust)

  # Compare coefficients
  coef_comparison <- tibble(
    Term = names(coef(final_model)),
    Original = coef(final_model),
    Robust = coef(model_robust),
    Difference = coef(model_robust) - coef(final_model),
    Pct_Change = 100 * (coef(model_robust) - coef(final_model)) / coef(final_model)
  )

  message("\nCoefficient Comparison (Original vs Robust):")
  print(coef_comparison |>
          mutate(across(where(is.numeric), ~round(.x, 4))))

  # Check for substantial changes
  large_changes <- coef_comparison |>
    filter(abs(Pct_Change) > 20)

  if (nrow(large_changes) > 0) {
    message("\nWARNING: Large coefficient changes (>20%) for: ",
            paste(large_changes$Term, collapse = ", "))
    message("Influential points may be affecting estimates")
  } else {
    message("\nCoefficients are stable (all changes < 20%)")
    message("Model is robust to influential observations")
  }
} else {
  message("\nNo observations exceed Cook's distance threshold")
  message("Robustness check not required")
}

# =============================================================================
# 7. Diagnostic Summary Panel
# =============================================================================

message("\n--- Creating Diagnostic Summary Panel ---")

# Use performance package for comprehensive diagnostics
p_check_model <- performance::check_model(final_model)
print(p_check_model)

# Save the panel
ggsave("output/fig_15_diagnostic_panel.png", p_check_model,
       width = 14, height = 12, dpi = 300)

# =============================================================================
# 8. Summary Statement
# =============================================================================

message("\n", strrep("-", 60))
message("DIAGNOSTIC SUMMARY")
message(strrep("-", 60))

# Compile assessment
assumptions_check <- tibble(
  Assumption = c("Linearity", "Homoskedasticity", "Normality", "Independence", "No Multicollinearity"),
  Test = c("Residual patterns", "Breusch-Pagan", "Shapiro-Wilk", "Visual inspection", "VIF"),
  Result = c(
    "Check residual plots",
    ifelse(bp_test$p.value > 0.05, "PASS", "CONCERN"),
    ifelse(shapiro_resid$p.value > 0.05, "PASS", "CONCERN"),
    "Assumed OK (no time series)",
    ifelse(all(car::vif(final_model) < 5), "PASS", "CONCERN")
  ),
  Note = c(
    "Look for patterns in residual vs fitted plot",
    paste("p =", round(bp_test$p.value, 4)),
    paste("p =", round(shapiro_resid$p.value, 4)),
    "Independence assumed for cross-sectional data",
    paste("Max VIF =", round(max(car::vif(final_model)), 2))
  )
)

print(assumptions_check)

message("
OVERALL ASSESSMENT:
")

pass_count <- sum(assumptions_check$Result == "PASS")
total_checks <- nrow(assumptions_check)

if (pass_count == total_checks) {
  message("✓ All diagnostic checks PASSED")
  message("  Model assumptions appear to be reasonably satisfied")
  message("  Proceed with interpretation and inference")
} else {
  message("! ", pass_count, "/", total_checks, " diagnostic checks passed")
  concerns <- assumptions_check |> filter(Result == "CONCERN")
  message("  Concerns identified:")
  for (i in 1:nrow(concerns)) {
    message("    - ", concerns$Assumption[i], ": ", concerns$Note[i])
  }
  message("\n  RECOMMENDATIONS:")
  if (bp_test$p.value < 0.05) {
    message("    - Consider robust standard errors (HC3)")
  }
  if (shapiro_resid$p.value < 0.05) {
    message("    - Check for outliers or consider transformation")
    message("    - With large n, mild violations are often acceptable")
  }
}

message("\n", strrep("=", 60))
message("MODEL DIAGNOSTICS COMPLETE")
message(strrep("=", 60), "\n")
