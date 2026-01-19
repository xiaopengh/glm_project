# =============================================================================
# 02_eda_continuous.R - Exploratory Data Analysis: Continuous Predictors
# =============================================================================
# Author: Xiaopeng
# Task: Analyze continuous predictors (age, study_hrs, sleep_hrs, attend_pct)
# =============================================================================

# Load data (this sources 00_setup.R internally)
source("R/01_data_load.R")

message("\n", strrep("=", 60))
message("SECTION 2B: EDA - CONTINUOUS PREDICTORS")
message(strrep("=", 60))

# Define continuous predictors
continuous_vars <- c("age", "study_hrs", "sleep_hrs", "attend_pct")

# =============================================================================
# 1. Descriptive Statistics
# =============================================================================

message("\n--- Descriptive Statistics for Continuous Predictors ---")

# Summary statistics table
cont_summary <- train_data |>
  select(all_of(continuous_vars)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  group_by(variable) |>
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    IQR = IQR(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .groups = "drop"
  )

print(cont_summary)

# Formatted table using gtsummary
tbl_continuous <- train_data |>
  select(all_of(continuous_vars)) |>
  tbl_summary(
    type = everything() ~ "continuous2",
    statistic = everything() ~ c(
      "{mean} ({sd})",
      "{median} [{p25}, {p75}]",
      "{min} - {max}"
    ),
    label = list(
      age ~ "Age (years)",
      study_hrs ~ "Study Hours (per week)",
      sleep_hrs ~ "Sleep Hours (per day)",
      attend_pct ~ "Attendance (%)"
    )
  ) |>
  bold_labels()

print(tbl_continuous)

# =============================================================================
# 2. Distribution Plots
# =============================================================================

message("\n--- Creating Distribution Plots ---")

# Function to create histogram with density
create_hist <- function(data, var, title) {
  ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 20, fill = "steelblue",
                   color = "white", alpha = 0.7) +
    geom_density(color = "darkred", linewidth = 1) +
    geom_vline(aes(xintercept = mean(.data[[var]])),
               color = "red", linetype = "dashed") +
    labs(title = title, x = var, y = "Density") +
    theme_glm(base_size = 12)
}

# Create histograms for each variable
p_age_hist <- create_hist(train_data, "age", "Distribution of Age")
p_study_hist <- create_hist(train_data, "study_hrs", "Distribution of Study Hours")
p_sleep_hist <- create_hist(train_data, "sleep_hrs", "Distribution of Sleep Hours")
p_attend_hist <- create_hist(train_data, "attend_pct", "Distribution of Attendance %")

# Combine distribution plots
p_distributions <- (p_age_hist | p_study_hist) / (p_sleep_hist | p_attend_hist) +
  plot_annotation(
    title = "Distributions of Continuous Predictors",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

print(p_distributions)

# Save
ggsave("output/fig_02_continuous_distributions.png", p_distributions,
       width = 12, height = 10, dpi = 300)

# =============================================================================
# 3. Scatterplots: y vs Each Continuous Predictor
# =============================================================================

message("\n--- Creating Scatterplots: y vs Continuous Predictors ---")

# Function to create scatterplot with LOESS
create_scatter <- function(data, xvar, yvar = "y") {
  ggplot(data, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(size = 2, shape = 21, fill = "dodgerblue",
               color = "black", alpha = 0.6) +
    geom_smooth(method = "loess", se = TRUE, color = "red",
                linewidth = 1.2, fill = "pink", alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "darkgreen",
                linetype = "dashed", linewidth = 1) +
    labs(
      title = paste("Exam Score vs", xvar),
      subtitle = "Red: LOESS smooth | Green dashed: Linear fit",
      x = xvar,
      y = "Exam Score (y)"
    ) +
    theme_glm(base_size = 12)
}

# Create scatterplots
p_y_age <- create_scatter(train_data, "age")
p_y_study <- create_scatter(train_data, "study_hrs")
p_y_sleep <- create_scatter(train_data, "sleep_hrs")
p_y_attend <- create_scatter(train_data, "attend_pct")

# Combine scatterplots
p_scatters <- (p_y_age | p_y_study) / (p_y_sleep | p_y_attend) +
  plot_annotation(
    title = "Relationship between Exam Score and Continuous Predictors",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

print(p_scatters)

# Save
ggsave("output/fig_03_continuous_scatterplots.png", p_scatters,
       width = 14, height = 12, dpi = 300)

# =============================================================================
# 4. Correlation with Response
# =============================================================================

message("\n--- Correlations with Response Variable (y) ---")

correlations <- train_data |>
  select(y, all_of(continuous_vars)) |>
  cor(use = "pairwise.complete.obs")

cor_with_y <- correlations["y", continuous_vars]

cor_table <- tibble(
  Variable = continuous_vars,
  Correlation = cor_with_y,
  `Abs Correlation` = abs(cor_with_y),
  Strength = case_when(
    abs(cor_with_y) >= 0.7 ~ "Strong",
    abs(cor_with_y) >= 0.4 ~ "Moderate",
    abs(cor_with_y) >= 0.2 ~ "Weak",
    TRUE ~ "Very Weak"
  )
) |>
  arrange(desc(`Abs Correlation`))

print(cor_table)

# =============================================================================
# 5. Linearity Assessment
# =============================================================================

message("\n--- Linearity Assessment ---")

# Fit simple linear models and check for non-linearity
assess_linearity <- function(data, xvar, yvar = "y") {
  # Fit linear and quadratic models
  lm_linear <- lm(as.formula(paste(yvar, "~", xvar)), data = data)
  lm_quad <- lm(as.formula(paste(yvar, "~", xvar, "+ I(", xvar, "^2)")), data = data)

  # Compare using F-test
  anova_result <- anova(lm_linear, lm_quad)
  p_value <- anova_result$`Pr(>F)`[2]

  # R-squared comparison
  r2_linear <- summary(lm_linear)$r.squared
  r2_quad <- summary(lm_quad)$r.squared

  list(
    variable = xvar,
    r2_linear = r2_linear,
    r2_quadratic = r2_quad,
    r2_improvement = r2_quad - r2_linear,
    quad_p_value = p_value,
    nonlinear = p_value < 0.05
  )
}

linearity_results <- map_dfr(continuous_vars, ~assess_linearity(train_data, .x))

message("\nLinearity Assessment Results:")
print(linearity_results)

message("\nInterpretation:")
for (i in 1:nrow(linearity_results)) {
  row <- linearity_results[i, ]
  if (row$nonlinear) {
    message("  ", row$variable, ": Significant non-linear component detected (p = ",
            format.pval(row$quad_p_value, digits = 3), ")")
  } else {
    message("  ", row$variable, ": Linear relationship appears adequate (p = ",
            format.pval(row$quad_p_value, digits = 3), ")")
  }
}

# =============================================================================
# 6. Heteroskedasticity Check (Visual)
# =============================================================================

message("\n--- Visual Heteroskedasticity Check ---")

# Residual plots for each predictor
create_resid_plot <- function(data, xvar) {
  lm_fit <- lm(as.formula(paste("y ~", xvar)), data = data)
  data_resid <- data |>
    mutate(
      fitted = fitted(lm_fit),
      residuals = residuals(lm_fit)
    )

  ggplot(data_resid, aes(x = .data[[xvar]], y = residuals)) +
    geom_point(alpha = 0.5, color = "steelblue") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(method = "loess", se = FALSE, color = "orange") +
    labs(
      title = paste("Residuals vs", xvar),
      x = xvar,
      y = "Residuals"
    ) +
    theme_glm(base_size = 11)
}

p_resid_age <- create_resid_plot(train_data, "age")
p_resid_study <- create_resid_plot(train_data, "study_hrs")
p_resid_sleep <- create_resid_plot(train_data, "sleep_hrs")
p_resid_attend <- create_resid_plot(train_data, "attend_pct")

p_residuals <- (p_resid_age | p_resid_study) / (p_resid_sleep | p_resid_attend) +
  plot_annotation(
    title = "Residual Plots for Simple Linear Regressions",
    subtitle = "Check for patterns indicating non-linearity or heteroskedasticity",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

print(p_residuals)

ggsave("output/fig_04_continuous_residuals.png", p_residuals,
       width = 12, height = 10, dpi = 300)

# =============================================================================
# 7. Transformation Assessment
# =============================================================================

message("\n--- Transformation Assessment ---")

# Check skewness of each predictor
skewness_check <- train_data |>
  select(all_of(continuous_vars)) |>
  summarise(across(everything(), ~moments::skewness(.x))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "skewness") |>
  mutate(
    recommendation = case_when(
      abs(skewness) > 1 ~ "Consider log/sqrt transformation",
      abs(skewness) > 0.5 ~ "Minor skew, transformation optional",
      TRUE ~ "No transformation needed"
    )
  )

print(skewness_check)

# =============================================================================
# 8. Summary: Key Findings
# =============================================================================

message("\n", strrep("-", 60))
message("KEY FINDINGS: Continuous Predictors")
message(strrep("-", 60))

# Sort by correlation strength
sorted_cors <- cor_table |> arrange(desc(`Abs Correlation`))

message("
1. STRONGEST PREDICTORS (by correlation with y):
   - ", sorted_cors$Variable[1], " (r = ", round(sorted_cors$Correlation[1], 3), "): ",
        sorted_cors$Strength[1], " correlation
   - ", sorted_cors$Variable[2], " (r = ", round(sorted_cors$Correlation[2], 3), "): ",
        sorted_cors$Strength[2], " correlation

2. VARIABLE DISTRIBUTIONS:
   - age: ", round(mean(train_data$age), 1), " years (SD = ",
        round(sd(train_data$age), 1), ")
   - study_hrs: ", round(mean(train_data$study_hrs), 1), " hours/week (SD = ",
        round(sd(train_data$study_hrs), 1), ")
   - sleep_hrs: ", round(mean(train_data$sleep_hrs), 1), " hours/day (SD = ",
        round(sd(train_data$sleep_hrs), 1), ")
   - attend_pct: ", round(mean(train_data$attend_pct), 1), "% (SD = ",
        round(sd(train_data$attend_pct), 1), ")

3. LINEARITY:
   - Linear relationships appear ", ifelse(any(linearity_results$nonlinear),
                                            "adequate for most variables",
                                            "adequate for all variables"), "
   - Consider polynomial terms for: ",
        paste(linearity_results$variable[linearity_results$nonlinear], collapse = ", "), "

4. RECOMMENDATIONS:
   - Primary predictors for modeling: study_hrs, attend_pct
   - Check for interactions in model building phase
   - Monitor heteroskedasticity in final model diagnostics
")

message("\n", strrep("=", 60))
message("EDA - CONTINUOUS PREDICTORS COMPLETE")
message(strrep("=", 60), "\n")
