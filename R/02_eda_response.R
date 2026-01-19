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

# Calculate summary statistics for y in train_data:
#   - n, mean, sd, median, IQR, min, max, Q1, Q3
#   - skewness and kurtosis (use moments package)

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

#Summary table

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

# Histogram with density overlay

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


# Boxplot with outlier identification 

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

# Q-Q plot for normality

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

# Combine plots using patchwork

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

# TODO: Perform Shapiro-Wilk test: shapiro.test(train_data$y)
#   - Report W statistic and p-value
#   - Interpret result (p > 0.05 suggests normality)

# TODO: Interpret skewness:
#   - |skew| < 0.5: approximately symmetric
#   - skew > 0: right-skewed
#   - skew < 0: left-skewed

# TODO: Interpret kurtosis:
#   - kurt ≈ 3: mesokurtic (normal-like)
#   - kurt > 3: leptokurtic (heavy tails)
#   - kurt < 3: platykurtic (light tails)

# =============================================================================
# 4. Outlier Identification
# =============================================================================

message("\n--- Outlier Identification (IQR Rule) ---")

# TODO: Calculate Q1, Q3, IQR for y

# TODO: Calculate lower fence = Q1 - 1.5*IQR

# TODO: Calculate upper fence = Q3 + 1.5*IQR

# TODO: Identify outliers (observations outside fences)

# TODO: Report number of outliers and their values

# =============================================================================
# 5. Transformation Assessment
# =============================================================================

message("\n--- Transformation Assessment ---")

# TODO: If skewness > 1, compare transformations:
#   - Original skewness
#   - Log(y) skewness (if all y > 0)
#   - Sqrt(y) skewness (if all y >= 0)

# TODO: Make recommendation about transformation need

# =============================================================================
# 6. Summary: Key Findings
# =============================================================================

message("\n", strrep("-", 60))
message("KEY FINDINGS: Response Variable y (Exam Score)")
message(strrep("-", 60))

# TODO: Summarize key findings:
#   1. Central tendency (mean, median, range)
#   2. Variability (SD, IQR)
#   3. Distribution shape (skewness, kurtosis, normality test)
#   4. Outliers (number and recommendation)
#   5. Transformation recommendation

message("\n", strrep("=", 60))
message("EDA - RESPONSE VARIABLE COMPLETE")
message(strrep("=", 60), "\n")
