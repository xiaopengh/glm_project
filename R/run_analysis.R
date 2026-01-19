# =============================================================================
# run_analysis.R - Master Script for GLM Project
# =============================================================================
# This script runs the complete analysis pipeline
# Authors: Yugoo, Xiaopeng, Shuaibo, Yicheng
#
# Usage: source("R/run_analysis.R")
# Or from command line: Rscript R/run_analysis.R
# =============================================================================

# Record start time
start_time <- Sys.time()

cat("\n")
cat(strrep("=", 70), "\n")
cat("  GLM PROJECT: PREDICTING STUDENT EXAM SCORES\n")
cat("  Complete Analysis Pipeline\n")
cat(strrep("=", 70), "\n")
cat("\n")

# Set working directory to project root if needed
if (!file.exists("data/project.csv")) {
  stop("Error: Cannot find data/project.csv. Please run from project root.")
}

# =============================================================================
# Phase 0: Setup
# =============================================================================

cat("\n[PHASE 0] Loading setup and packages...\n")
cat(strrep("-", 50), "\n")

source("R/00_setup.R")

cat("Setup complete.\n")

# =============================================================================
# Phase 1: Data Loading
# =============================================================================

cat("\n[PHASE 1] Loading and preparing data...\n")
cat(strrep("-", 50), "\n")

source("R/01_data_load.R")

# TODO: Print summary of train/test split

# =============================================================================
# Phase 2: Exploratory Data Analysis
# =============================================================================

cat("\n[PHASE 2] Exploratory Data Analysis\n")
cat(strrep("-", 50), "\n")

cat("\n  [2A] Response variable EDA (Yugoo)...\n")
source("R/02_eda_response.R")

cat("\n  [2B] Continuous predictors EDA (Xiaopeng)...\n")
source("R/02_eda_continuous.R")

cat("\n  [2C] Categorical predictors EDA (Shuaibo)...\n")
source("R/02_eda_categorical.R")

cat("\n  [2D] Predictor associations EDA (Yicheng)...\n")
source("R/02_eda_associations.R")

cat("\nEDA complete.\n")

# =============================================================================
# Phase 3: Model Building
# =============================================================================

cat("\n[PHASE 3] Model Building & Selection (Yugoo + Yicheng)...\n")
cat(strrep("-", 50), "\n")

source("R/03_modeling.R")

# TODO: Print final model formula and Adj R²

# =============================================================================
# Phase 4: Model Diagnostics
# =============================================================================

cat("\n[PHASE 4] Model Diagnostics (Xiaopeng)...\n")
cat(strrep("-", 50), "\n")

source("R/04_diagnostics.R")

cat("\nDiagnostics complete.\n")

# =============================================================================
# Phase 5: Interpretation
# =============================================================================

cat("\n[PHASE 5] Model Interpretation (Shuaibo)...\n")
cat(strrep("-", 50), "\n")

source("R/05_interpretation.R")

cat("\nInterpretation complete.\n")

# =============================================================================
# Phase 6: Predictive Performance
# =============================================================================

cat("\n[PHASE 6] Predictive Performance (Yicheng)...\n")
cat(strrep("-", 50), "\n")

source("R/06_prediction.R")

cat("\nPredictive performance analysis complete.\n")

# =============================================================================
# Summary
# =============================================================================

end_time <- Sys.time()
elapsed_time <- difftime(end_time, start_time, units = "secs")

cat("\n")
cat(strrep("=", 70), "\n")
cat("  ANALYSIS COMPLETE\n")
cat(strrep("=", 70), "\n")
cat("\n")

# TODO: Print summary statistics:
#   - Total execution time
#   - Training/test observations
#   - Final model predictors
#   - Training R², Test R²

cat("\n")
cat("OUTPUT FILES:\n")
cat("  - Figures saved to: output/\n")

# TODO: List generated figures

cat("\n")
cat("KEY OBJECTS AVAILABLE:\n")
cat("  - data_clean: Full cleaned dataset\n")
cat("  - train_data: Training set (80%)\n")
cat("  - test_data: Test set (20%)\n")
cat("  - final_model: Selected regression model\n")
cat("  - models: List of all candidate models\n")
cat("  - comparison_table: Model comparison metrics\n")

cat("\n")
cat("Next steps:\n")
cat("  1. Review output figures in output/ directory\n")
cat("  2. Render the Quarto report: quarto render report.qmd\n")
cat("  3. Review the final report.html\n")

cat("\n")
cat(strrep("=", 70), "\n")
cat("  GLM PROJECT - ANALYSIS PIPELINE FINISHED\n")
cat(strrep("=", 70), "\n")
cat("\n")
