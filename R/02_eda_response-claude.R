# =============================================================================
# 02_eda_response.R - Exploratory Data Analysis: Response Variable
# =============================================================================
# Author: Yugoo
# Task: Analyze the distribution of the response variable y (exam score)
# =============================================================================

# Load data (this sources 00_setup.R internally)
source("R/01_data_load.R")

message("\n", strrep("=", 60))
message("SECTION 2A: EDA - RESPONSE VARIABLE (y)")
message(strrep("=", 60))

# =============================================================================
# 1. Summary Statistics for y
# =============================================================================

message("\n--- Summary Statistics for y (Exam Score) ---")

y_stats <- train_data |>
  summarise(
    n = n(),
    mean = mean(y),
    sd = sd(y),
    median = median(y),
    IQR = IQR(y),
    min = min(y),
    max = max(y),
    Q1 = quantile(y, 0.25),
    Q3 = quantile(y, 0.75),
    skewness = moments::skewness(y),
    kurtosis = moments::kurtosis(y)
  )

print(y_stats)

# Formatted summary table
tbl_response <- train_data |>
  select(y) |>
  tbl_summary(
    type = y ~ "continuous2",
    statistic = y ~ c(
      "{mean} ({sd})",
      "{median} [{p25}, {p75}]",
      "{min} - {max}"
    ),
    label = y ~ "Exam Score (y)"
  ) |>
  bold_labels()

print(tbl_response)

# =============================================================================
# 2. Distribution Plots
# =============================================================================

message("\n--- Creating Distribution Plots ---")

# 2.1 Histogram with density overlay
p_hist <- ggplot(train_data, aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 25,
                 fill = "steelblue",
                 color = "white",
                 alpha = 0.7) +
  geom_density(color = "darkred", linewidth = 1.2) +
  geom_vline(aes(xintercept = mean(y)),
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(y)),
             color = "blue", linetype = "dotted", linewidth = 1) +
  labs(
    title = "Distribution of Exam Scores",
    subtitle = "Red dashed = Mean, Blue dotted = Median",
    x = "Exam Score (y)",
    y = "Density"
  ) +
  theme_glm()

# 2.2 Boxplot with outlier identification
p_box <- ggplot(train_data, aes(x = "", y = y)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, width = 0.5,
               outlier.shape = 21, outlier.fill = "red",
               outlier.size = 3) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 2) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "red") +
  labs(
    title = "Boxplot of Exam Scores",
    subtitle = "Diamond = Mean, Red points = Outliers",
    x = "",
    y = "Exam Score (y)"
  ) +
  coord_flip() +
  theme_glm()

# 2.3 Q-Q Plot for normality
p_qq <- ggplot(train_data, aes(sample = y)) +
  stat_qq_band(fill = "lightblue", alpha = 0.5) +
  stat_qq_line(color = "red", linewidth = 1) +
  stat_qq_point(size = 2, alpha = 0.6) +
  labs(
    title = "Q-Q Plot for Normality Assessment",
    subtitle = "Points should follow the red line if normal",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_glm()

# Combine plots
p_response_combined <- (p_hist | p_box) / p_qq +
  plot_annotation(
    title = "Response Variable Analysis: Exam Score (y)",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

print(p_response_combined)

# Save plot
ggsave("output/fig_01_response_distribution.png", p_response_combined,
       width = 12, height = 10, dpi = 300)

# =============================================================================
# 3. Normality Assessment
# =============================================================================

message("\n--- Normality Assessment ---")

# Shapiro-Wilk test
shapiro_result <- shapiro.test(train_data$y)
message("Shapiro-Wilk test:")
message("  W = ", round(shapiro_result$statistic, 4))
message("  p-value = ", format.pval(shapiro_result$p.value, digits = 4))

if (shapiro_result$p.value > 0.05) {
  message("  Conclusion: Cannot reject normality (p > 0.05)")
} else {
  message("  Conclusion: Evidence against normality (p < 0.05)")
}

# Skewness and Kurtosis interpretation
skew <- moments::skewness(train_data$y)
kurt <- moments::kurtosis(train_data$y)

message("\nSkewness: ", round(skew, 3))
if (abs(skew) < 0.5) {
  message("  Interpretation: Approximately symmetric")
} else if (skew > 0) {
  message("  Interpretation: Right-skewed (positive skew)")
} else {
  message("  Interpretation: Left-skewed (negative skew)")
}

message("\nKurtosis: ", round(kurt, 3))
if (abs(kurt - 3) < 0.5) {
  message("  Interpretation: Approximately mesokurtic (normal-like)")
} else if (kurt > 3) {
  message("  Interpretation: Leptokurtic (heavy tails)")
} else {
  message("  Interpretation: Platykurtic (light tails)")
}

# =============================================================================
# 4. Outlier Identification
# =============================================================================

message("\n--- Outlier Identification (IQR Rule) ---")

Q1 <- quantile(train_data$y, 0.25)
Q3 <- quantile(train_data$y, 0.75)
IQR_y <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR_y
upper_fence <- Q3 + 1.5 * IQR_y

outliers <- train_data |>
  filter(y < lower_fence | y > upper_fence) |>
  arrange(y)

message("Lower fence: ", round(lower_fence, 2))
message("Upper fence: ", round(upper_fence, 2))
message("Number of outliers: ", nrow(outliers))

if (nrow(outliers) > 0) {
  message("\nOutlier observations:")
  print(outliers |> select(id, y, study_hrs, attend_pct, sleep_qual))
}

# =============================================================================
# 5. Transformation Assessment
# =============================================================================

message("\n--- Transformation Assessment ---")

# Check if transformation might help
message("
Based on the analysis:
")

if (abs(skew) > 1) {
  message("- Skewness (", round(skew, 2), ") suggests transformation may be beneficial")

  # Compare original vs log/sqrt transformations
  message("\nComparing transformations:")
  message("  Original skewness: ", round(skew, 3))

  # Log transformation (only if all values > 0)
  if (all(train_data$y > 0)) {
    log_skew <- moments::skewness(log(train_data$y))
    message("  Log(y) skewness: ", round(log_skew, 3))
  }

  # Square root transformation (only if all values >= 0)
  if (all(train_data$y >= 0)) {
    sqrt_skew <- moments::skewness(sqrt(train_data$y))
    message("  Sqrt(y) skewness: ", round(sqrt_skew, 3))
  }
} else {
  message("- Skewness (", round(skew, 2), ") is acceptable; no transformation needed")
}

if (shapiro_result$p.value > 0.01) {
  message("- Distribution is reasonably normal for linear regression")
} else {
  message("- Some deviation from normality, but may not be problematic")
  message("  (Central Limit Theorem applies with sufficient sample size)")
}

# =============================================================================
# 6. Summary: Key Findings
# =============================================================================

message("\n", strrep("-", 60))
message("KEY FINDINGS: Response Variable y (Exam Score)")
message(strrep("-", 60))

message("
1. CENTRAL TENDENCY:
   - Mean exam score: ", round(mean(train_data$y), 1), "
   - Median exam score: ", round(median(train_data$y), 1), "
   - Range: ", round(min(train_data$y), 1), " to ", round(max(train_data$y), 1), "

2. VARIABILITY:
   - Standard deviation: ", round(sd(train_data$y), 1), "
   - IQR: ", round(IQR(train_data$y), 1), "

3. DISTRIBUTION SHAPE:
   - Skewness: ", round(skew, 3), " (",
   ifelse(abs(skew) < 0.5, "symmetric", ifelse(skew > 0, "right-skewed", "left-skewed")), ")
   - Kurtosis: ", round(kurt, 3), "
   - Normality test p-value: ", format.pval(shapiro_result$p.value, digits = 3), "

4. OUTLIERS:
   - Number of outliers (IQR rule): ", nrow(outliers), "
   - ", ifelse(nrow(outliers) > 0, "Should investigate these observations", "No extreme outliers detected"), "

5. RECOMMENDATION:
   - ", ifelse(abs(skew) < 1, "No transformation recommended", "Consider log/sqrt transformation"), "
   - Proceed with linear regression on untransformed y
")

message("\n", strrep("=", 60))
message("EDA - RESPONSE VARIABLE COMPLETE")
message(strrep("=", 60), "\n")
