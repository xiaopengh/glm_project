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

# TODO: Calculate summary statistics for y in train_data:
#   - n, mean, sd, median, IQR, min, max, Q1, Q3
#   - skewness and kurtosis (use moments package)

# TODO: Create formatted summary table using tbl_summary()

# =============================================================================
# 2. Distribution Plots
# =============================================================================

message("\n--- Creating Distribution Plots ---")

# TODO: Create histogram with density overlay (p_hist)
#   - Use geom_histogram(aes(y = after_stat(density)))
#   - Add geom_density() overlay in red
#   - Add vertical lines for mean (dashed) and median (dotted)
#   - Title: "Distribution of Exam Scores"

# TODO: Create boxplot with outlier identification (p_box)
#   - Use geom_boxplot() with jittered points
#   - Add stat_summary() for mean point (diamond shape)
#   - Use coord_flip()

# TODO: Create Q-Q plot for normality (p_qq)
#   - Use qqplotr package: stat_qq_band(), stat_qq_line(), stat_qq_point()
#   - Add confidence bands

# TODO: Combine plots using patchwork: (p_hist | p_box) / p_qq

# TODO: Save combined plot to output/fig_01_response_distribution.png

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
