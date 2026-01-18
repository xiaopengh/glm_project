# =============================================================================
# 06_prediction.R - Predictive Performance
# =============================================================================
# Author: WW
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

message("\n--- Validation Protocol ---")

# TODO: Document validation protocol:
#   - Data split: 80% training / 20% test
#   - Random seed: set.seed(42)
#   - Split performed BEFORE model fitting
#   - Confirm no data leakage

# TODO: Report dataset sizes:
#   - Training set: n observations
#   - Test set: n observations

# TODO: Compare response distribution in train vs test:
#   - Mean, SD of y in each set

# =============================================================================
# 2. Baseline Model (Naive Prediction)
# =============================================================================

message("\n--- Baseline Model: Training Mean ---")

# TODO: Calculate baseline prediction = mean(train_data$y)

# TODO: Calculate baseline metrics on test set:
#   - baseline_residuals = test_data$y - baseline_pred
#   - baseline_mse = mean(baseline_residuals^2)
#   - baseline_rmse = sqrt(baseline_mse)
#   - baseline_mae = mean(abs(baseline_residuals))
#   - baseline_r2 = 0 (by definition)

# =============================================================================
# 3. Final Model Predictions on Test Set
# =============================================================================

message("\n--- Final Model Predictions on Test Set ---")

# TODO: Make predictions: test_preds <- predict(final_model, newdata = test_data)

# TODO: Calculate residuals: test_residuals <- test_data$y - test_preds

# =============================================================================
# 4. Performance Metrics
# =============================================================================

message("\n--- Performance Metrics ---")

# TODO: Calculate test set metrics:
#   - mse_test = mean(test_residuals^2)
#   - rmse_test = sqrt(mse_test)
#   - mae_test = mean(abs(test_residuals))
#   - medae_test = median(abs(test_residuals))
#   - r2_test = 1 - mse_test / var(test_data$y)

# TODO: Calculate training set metrics for comparison

# TODO: Create metrics summary table:
#   - Metric, Training, Test, Baseline

# =============================================================================
# 5. Model Improvement Over Baseline
# =============================================================================

message("\n--- Improvement Over Baseline ---")

# TODO: Calculate improvements:
#   - mse_reduction = (baseline_mse - mse_test) / baseline_mse * 100
#   - rmse_reduction = (baseline_rmse - rmse_test) / baseline_rmse * 100

# TODO: Check for overfitting:
#   - r2_drop = r2_train - r2_test
#   - If drop > 0.10: possible overfitting
#   - If drop > 0.05: moderate, monitor
#   - If drop < 0.05: good generalization

# =============================================================================
# 6. Calibration Plot
# =============================================================================

message("\n--- Creating Calibration Plot ---")

# TODO: Create calibration plot:
#   - X = predicted, Y = observed
#   - geom_point() for data
#   - geom_abline(slope = 1, intercept = 0) for perfect calibration
#   - geom_smooth(method = "loess") for actual calibration
#   - Title with R² value

# TODO: Save to output/fig_16_calibration_plot.png

# =============================================================================
# 7. Residual Analysis on Test Set
# =============================================================================

message("\n--- Test Set Residual Analysis ---")

# TODO: Create residual histogram:
#   - Check for normality
#   - Add vertical line at 0

# TODO: Create residuals vs predicted plot:
#   - Check for patterns
#   - Add horizontal line at 0
#   - Add LOESS smooth

# TODO: Combine and save to output/fig_17_test_residuals.png

# =============================================================================
# 8. Performance by Subgroup (Optional)
# =============================================================================

message("\n--- Performance by Subgroup ---")

# TODO: Calculate performance by school type (if in data):
#   - n, Mean_AE, Median_AE, RMSE for each group

# TODO: Calculate performance by score level (Low/Medium/High):
#   - Divide y into tertiles
#   - Calculate metrics for each group
#   - Check for bias (over/under prediction at extremes)

# =============================================================================
# 9. Prediction Error Bounds
# =============================================================================

message("\n--- Prediction Error Analysis ---")

# TODO: Calculate 95% prediction intervals on test set:
#   predict(final_model, newdata = test_data, interval = "prediction")

# TODO: Check coverage rate:
#   - % of test observations within prediction interval
#   - Expected: ~95%
#   - If < 90%: intervals too narrow (under-coverage)
#   - If > 98%: intervals too wide (over-coverage)

# =============================================================================
# 10. Performance Summary Table
# =============================================================================

message("\n--- Final Performance Summary ---")

# TODO: Create performance comparison table:
#   - Model, MSE, RMSE, MAE, MedAE, R², MSE Improvement

# =============================================================================
# 11. Key Findings Summary
# =============================================================================

message("\n", strrep("-", 60))
message("KEY FINDINGS: Predictive Performance")
message(strrep("-", 60))

# TODO: Summarize:
#   1. Out-of-sample performance (R², RMSE, MedAE)
#   2. Improvement over baseline (% MSE reduction)
#   3. Generalization (train vs test R² drop)
#   4. Calibration quality
#   5. Practical implications

message("\n", strrep("=", 60))
message("PREDICTIVE PERFORMANCE ANALYSIS COMPLETE")
message(strrep("=", 60), "\n")
