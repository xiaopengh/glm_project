# =============================================================================
# 00_setup.R - Shared Setup for GLM Project
# =============================================================================
# This script contains common settings, packages, and helper functions
# used across all analysis scripts.
#
# Authors: XX, YY, ZZ, WW
# =============================================================================

# Reproducibility
set.seed(42)

# Global options
options(
  pillar.width = 130,
  width = 200,
  scipen = 999,
  digits = 5
)

# -----------------------------------------------------------------------------
# Load Required Packages
# -----------------------------------------------------------------------------

# Core modeling packages
library(broom)        # tidy model outputs
library(performance)  # model diagnostics
library(parameters)   # model parameters
library(car)          # hypothesis tests (linearHypothesis, vif)
library(lmtest)       # coeftest, waldtest

# Visualization packages
library(ggplot2)      # visualization
library(ggpubr)       # labs_pubr()
library(patchwork)    # combine plots
library(qqplotr)      # Q-Q plots
library(GGally)       # correlation plots, ggpairs

# Table formatting packages
library(gtsummary)    # formatted tables
library(kableExtra)   # table formatting
library(janitor)      # tabyl, clean_names

# Data manipulation (load last to avoid conflicts)
library(tidyverse)

# -----------------------------------------------------------------------------
# Custom Theme for Plots
# -----------------------------------------------------------------------------

theme_glm <- function(base_size = 14) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

# Set as default theme
theme_set(theme_glm())

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Calculate common summary statistics
#' @param x Numeric vector
#' @return Named vector of summary statistics
summary_stats <- function(x) {
  c(
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    IQR = IQR(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}

#' Identify outliers using IQR rule
#' @param x Numeric vector
#' @param k Multiplier for IQR (default 1.5)
#' @return Logical vector indicating outliers
identify_outliers <- function(x, k = 1.5) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - k * iqr
  upper <- q3 + k * iqr
  x < lower | x > upper
}

#' Calculate model performance metrics
#' @param observed Observed values
#' @param predicted Predicted values
#' @return Named list of metrics
calc_metrics <- function(observed, predicted) {
  residuals <- observed - predicted
  mse <- mean(residuals^2)
  list(
    MSE = mse,
    RMSE = sqrt(mse),
    MAE = mean(abs(residuals)),
    MedAE = median(abs(residuals)),
    R2 = 1 - mse / var(observed)
  )
}

#' Create a formatted coefficient table
#' @param model A fitted lm object
#' @return A formatted tibble
coef_table <- function(model) {
  broom::tidy(model, conf.int = TRUE) |>
    mutate(
      significance = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ ".",
        TRUE            ~ ""
      )
    ) |>
    select(term, estimate, std.error, statistic, p.value,
           conf.low, conf.high, significance)
}

# -----------------------------------------------------------------------------
# Project Paths
# -----------------------------------------------------------------------------

# Define relative paths (from project root)
DATA_PATH <- "data/project.csv"
OUTPUT_PATH <- "output/"

# Create output directory if it doesn't exist
if (!dir.exists(OUTPUT_PATH)) {
  dir.create(OUTPUT_PATH, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# Print Setup Confirmation
# -----------------------------------------------------------------------------

message("GLM Project setup loaded successfully!")
message("Seed set to: 42")
message("Data path: ", DATA_PATH)
