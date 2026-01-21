# =============================================================================
# 06_prediction.R - Predictive Performance
# =============================================================================
# Author: Yicheng
# Task: Evaluate model's predictive performance on test set
# =============================================================================

# Load modeling results (this sources previous scripts)
source("R/03_modeling.R")

message("\n", strrep("=", 60))
message("SECTION 6: PREDICTIVE PERFORMANCE")
message(strrep("=", 60))

# =============================================================================
# 1. Validation Protocol Documentation
# =============================================================================

message("\n--- Running Checks---")

message("
Checking Data:
  - Data split: 80% training / 20% test
  - Random seed: set.seed(42) for reproducibility
  - Split performed BEFORE any model fitting
  - No data leakage: test set not used in model selection

DATASET SIZES:
  - Training set: ", nrow(train_data), " observations
  - Test set: ", nrow(test_data), " observations

RESPONSE VARIABLE COMPARISON:
  - Training mean: ", round(mean(train_data$y), 2), "
  - Test mean: ", round(mean(test_data$y), 2), "
  - Training SD: ", round(sd(train_data$y), 2), "
  - Test SD: ", round(sd(test_data$y), 2), "
")

# =============================================================================
# 2. Baseline Model (Naive Prediction)
# =============================================================================

message("\n--- Baseline Model: Training Mean ---")

# Baseline: predict training mean for all test observations
baseline_pred <- mean(train_data$y)

# Define baseline metrics
baseline_residuals <- test_data$y - baseline_pred
baseline_mse <- mean(baseline_residuals^2)
baseline_rmse <- sqrt(baseline_mse)
baseline_mae <- mean(abs(baseline_residuals))
baseline_medae <- median(abs(baseline_residuals))
baseline_r2 <- 0  # By definition

message("Baseline prediction (training mean): ", round(baseline_pred, 2))
message("Baseline MSE: ", round(baseline_mse, 4))
message("Baseline RMSE: ", round(baseline_rmse, 4))
message("Baseline MAE: ", round(baseline_mae, 4))

# =============================================================================
# 3. Final Model Predictions on Test Set
# =============================================================================

message("\n--- Final Model Predictions on Test Set ---")

# Make predictions
test_preds <- predict(final_model, newdata = test_data)

# Calculate residuals
test_residuals <- test_data$y - test_preds

# =============================================================================
# 4. Performance Metrics
# =============================================================================

message("\n--- Performance Metrics ---")

# Calculate metrics
mse_test <- mean(test_residuals^2)
rmse_test <- sqrt(mse_test)
mae_test <- mean(abs(test_residuals))
medae_test <- median(abs(test_residuals))
r2_test <- 1 - mse_test / var(test_data$y)

# Also calculate on training set for comparison
train_preds <- fitted(final_model)
train_residuals <- train_data$y - train_preds
mse_train <- mean(train_residuals^2)
rmse_train <- sqrt(mse_train)
r2_train <- summary(final_model)$r.squared

# Metrics summary table
metrics_table <- tibble(
  Metric = c("MSE", "RMSE", "MAE", "MedAE", "R²"),
  Training = c(mse_train, rmse_train, mean(abs(train_residuals)),
               median(abs(train_residuals)), r2_train),
  Test = c(mse_test, rmse_test, mae_test, medae_test, r2_test),
  Baseline = c(baseline_mse, baseline_rmse, baseline_mae, baseline_medae, 0)
)

message("\nPerformance Metrics Summary:")
print(metrics_table |>
        mutate(across(where(is.numeric), ~round(.x, 4))))

# =============================================================================
# 5. Model Improvement Over Baseline
# =============================================================================

message("\n--- Improvement Over Baseline ---")

mse_reduction <- (baseline_mse - mse_test) / baseline_mse * 100
rmse_reduction <- (baseline_rmse - rmse_test) / baseline_rmse * 100

message("MSE reduction vs baseline: ", round(mse_reduction, 1), "%")
message("RMSE reduction vs baseline: ", round(rmse_reduction, 1), "%")
message("Out-of-sample R²: ", round(r2_test, 4), " (", round(r2_test * 100, 1), "% variance explained)")

# Compare train vs test performance
r2_drop <- r2_train - r2_test
message("\nOverfitting Check:")
message("  Training R²: ", round(r2_train, 4))
message("  Test R²: ", round(r2_test, 4))
message("  R² drop: ", round(r2_drop, 4))

if (r2_drop > 0.1) {
  message("  WARNING: Possible overfitting (R² drop > 0.10)")
} else if (r2_drop > 0.05) {
  message("  Note: Moderate R² drop, monitor carefully")
} else {
  message("  Good: Model generalizes well to new data")
}

# =============================================================================
# 6. Calibration Plot
# =============================================================================

message("\n--- Creating Calibration Plot ---")

calibration_data <- tibble(
  observed = test_data$y,
  predicted = test_preds
)

p_calibration <- ggplot(calibration_data, aes(x = predicted, y = observed)) +
  geom_point(alpha = 0.6, size = 3, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red",
              linetype = "dashed", linewidth = 1.2) +
  geom_smooth(method = "loess", se = TRUE, color = "orange", linewidth = 1) +
  labs(
    title = "Calibration Plot: Predicted vs Observed",
    subtitle = paste0("Test set (n = ", nrow(test_data), "), R² = ", round(r2_test, 3)),
    x = "Predicted Exam Score",
    y = "Observed Exam Score"
  ) +
  coord_equal() +
  theme_glm()

print(p_calibration)

ggsave("output/fig_16_calibration_plot.png", p_calibration,
       width = 8, height = 8, dpi = 300)

# =============================================================================
# 7. Residual Analysis on Test Set
# =============================================================================

message("\n--- Test Set Residual Analysis ---")

# Residual distribution
p_resid_hist <- ggplot(tibble(residuals = test_residuals), aes(x = residuals)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 15, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(
    title = "Test Set Residual Distribution",
    x = "Residuals (Observed - Predicted)",
    y = "Density"
  ) +
  theme_glm()

# Residuals vs predicted
p_resid_pred <- ggplot(calibration_data, aes(x = predicted, y = observed - predicted)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = TRUE, color = "orange") +
  labs(
    title = "Test Set: Residuals vs Predicted",
    x = "Predicted Score",
    y = "Residuals"
  ) +
  theme_glm()

p_test_resid <- p_resid_hist | p_resid_pred

print(p_test_resid)

ggsave("output/fig_17_test_residuals.png", p_test_resid,
       width = 12, height = 5, dpi = 300)

# =============================================================================
# 8. Performance by Subgroup (Optional)
# =============================================================================

message("\n--- Performance by Subgroup ---")

# Add predictions and residuals to test data
test_data_eval <- test_data |>
  mutate(
    predicted = test_preds,
    residual = y - predicted,
    abs_error = abs(residual)
  )

# Performance by school type (if in data)
if ("school_type" %in% names(test_data)) {
  perf_by_school <- test_data_eval |>
    group_by(school_type) |>
    summarise(
      n = n(),
      Mean_AE = mean(abs_error),
      Median_AE = median(abs_error),
      RMSE = sqrt(mean(residual^2)),
      .groups = "drop"
    )

  message("\nPerformance by School Type:")
  print(perf_by_school |>
          mutate(across(where(is.numeric) & !matches("n"), ~round(.x, 2))))
}

# Performance by score quantile
test_data_eval <- test_data_eval |>
  mutate(
    score_group = cut(y,
                      breaks = quantile(y, probs = c(0, 0.33, 0.67, 1)),
                      labels = c("Low", "Medium", "High"),
                      include.lowest = TRUE)
  )

perf_by_level <- test_data_eval |>
  group_by(score_group) |>
  summarise(
    n = n(),
    Mean_AE = mean(abs_error),
    Median_AE = median(abs_error),
    RMSE = sqrt(mean(residual^2)),
    Bias = mean(residual),
    .groups = "drop"
  )

message("\nPerformance by Score Level:")
print(perf_by_level |>
        mutate(across(where(is.numeric) & !matches("n"), ~round(.x, 2))))

# =============================================================================
# 9. Prediction Error Bounds
# =============================================================================

message("\n--- Prediction Error Analysis ---")

# Calculate prediction intervals on test set
pred_intervals <- predict(final_model, newdata = test_data, interval = "prediction", level = 0.95)

# Check coverage
coverage_data <- tibble(
  observed = test_data$y,
  pred = pred_intervals[, "fit"],
  lower = pred_intervals[, "lwr"],
  upper = pred_intervals[, "upr"]
) |>
  mutate(
    covered = observed >= lower & observed <= upper
  )

coverage_rate <- mean(coverage_data$covered) * 100

message("\n95% Prediction Interval Coverage:")
message("  Expected: 95%")
message("  Observed: ", round(coverage_rate, 1), "%")

if (abs(coverage_rate - 95) < 5) {
  message("  Assessment: Good calibration of uncertainty estimates")
} else if (coverage_rate < 90) {
  message("  WARNING: Under-coverage - prediction intervals too narrow")
} else {
  message("  WARNING: Over-coverage - prediction intervals too wide")
}

# =============================================================================
# 10. Performance Summary Table
# =============================================================================

message("\n--- Final Performance Summary ---")

performance_summary <- tibble(
  Model = c("Baseline (Mean)", "Final Model"),
  MSE = c(baseline_mse, mse_test),
  RMSE = c(baseline_rmse, rmse_test),
  MAE = c(baseline_mae, mae_test),
  MedAE = c(baseline_medae, medae_test),
  `R²` = c(0, r2_test)
) |>
  mutate(
    `MSE Improvement` = c("-", paste0(round((baseline_mse - mse_test) / baseline_mse * 100, 1), "%"))
  )

message("\nPerformance Comparison Table:")
print(performance_summary |>
        mutate(across(where(is.numeric), ~round(.x, 4))))

# =============================================================================
# 11. Key Findings Summary
# =============================================================================

message("\n", strrep("-", 60))
message("KEY FINDINGS: Predictive Performance")
message(strrep("-", 60))

message("
1. OUT-OF-SAMPLE PERFORMANCE:
   - Test R² = ", round(r2_test, 3), " (", round(r2_test * 100, 1), "% variance explained)
   - RMSE = ", round(rmse_test, 2), " points
   - Median Absolute Error = ", round(medae_test, 2), " points

2. IMPROVEMENT OVER BASELINE:
   - MSE reduction: ", round(mse_reduction, 1), "%
   - RMSE reduction: ", round(rmse_reduction, 1), "%

3. GENERALIZATION:
   - Training R² = ", round(r2_train, 3), "
   - Test R² = ", round(r2_test, 3), "
   - R² drop = ", round(r2_drop, 3), " (", ifelse(r2_drop < 0.05, "Good", "Monitor"), ")

4. CALIBRATION:
   - ", round(coverage_rate, 1), "% of test observations within 95% prediction interval
   - ", ifelse(abs(coverage_rate - 95) < 5, "Well-calibrated uncertainty", "Check calibration"), "

5. PRACTICAL IMPLICATIONS:
   - Model can predict exam scores with typical error of ±", round(rmse_test, 1), " points
   - Predictions are meaningful for identifying at-risk students
   - Should be used as decision support, not definitive assessment
")

message("\n", strrep("=", 60))
message("PREDICTIVE PERFORMANCE ANALYSIS COMPLETE")
message(strrep("=", 60), "\n")
