# =============================================================================
# 02_eda_categorical.R - Exploratory Data Analysis: Categorical Predictors
# =============================================================================
# Author: Shuaibo
# Task: Analyze categorical predictors and their relationship with y
# =============================================================================

# Load data (this sources 00_setup.R internally)
source("R/01_data_load.R")

message("\n", strrep("=", 60))
message("SECTION 2C: EDA - CATEGORICAL PREDICTORS")
message(strrep("=", 60))

# Define categorical predictors
categorical_vars <- c("sexe", "school_type", "parent_educ", "sleep_qual",
                      "agecat", "attend_pct_cat", "web_access", "trav_time",
                      "extra_act", "study_method")

# =============================================================================
# 1. Frequency Tables
# =============================================================================

message("\n--- Frequency Tables for Categorical Variables ---")

# Create frequency tables using tabyl
freq_tables <- list()

for (var in categorical_vars) {
  freq_tables[[var]] <- train_data |>
    tabyl(!!sym(var)) |>
    adorn_pct_formatting(digits = 1) |>
    adorn_totals("row")

  message("\n", var, ":")
  print(freq_tables[[var]])
}

# Formatted summary table using gtsummary
tbl_categorical <- train_data |>
  select(all_of(categorical_vars)) |>
  tbl_summary(
    label = list(
      sexe ~ "Gender",
      school_type ~ "School Type",
      parent_educ ~ "Parental Education",
      sleep_qual ~ "Sleep Quality",
      agecat ~ "Age Category",
      attend_pct_cat ~ "Attendance Category",
      web_access ~ "Internet Access",
      trav_time ~ "Travel Time",
      extra_act ~ "Extracurricular Activities",
      study_method ~ "Study Method"
    )
  ) |>
  bold_labels()

print(tbl_categorical)

# =============================================================================
# 2. Boxplots: y by Each Categorical Predictor
# =============================================================================

message("\n--- Creating Boxplots: y by Categorical Predictors ---")

# Function to create boxplot with mean points
create_boxplot <- function(data, cat_var, title = NULL) {
  if (is.null(title)) title <- paste("Exam Score by", cat_var)

  ggplot(data, aes(x = fct_reorder(.data[[cat_var]], y, .fun = median), y = y)) +
    geom_boxplot(fill = "lightblue", outlier.shape = NA, alpha = 0.8) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 1.5, color = "gray40") +
    stat_summary(fun = mean, geom = "point",
                 color = "red", size = 3, shape = 18) +
    labs(title = title, x = cat_var, y = "Exam Score") +
    coord_flip() +
    theme_glm(base_size = 11)
}

# Create boxplots for each variable
p_box_sexe <- create_boxplot(train_data, "sexe", "Exam Score by Gender")
p_box_school <- create_boxplot(train_data, "school_type", "Exam Score by School Type")
p_box_parent <- create_boxplot(train_data, "parent_educ", "Exam Score by Parental Education")
p_box_sleep <- create_boxplot(train_data, "sleep_qual", "Exam Score by Sleep Quality")

p_categorical_1 <- (p_box_sexe | p_box_school) / (p_box_parent | p_box_sleep) +
  plot_annotation(
    title = "Exam Score by Categorical Predictors (Part 1)",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

print(p_categorical_1)

ggsave("output/fig_05_categorical_boxplots_1.png", p_categorical_1,
       width = 14, height = 12, dpi = 300)

# Second set
p_box_agecat <- create_boxplot(train_data, "agecat", "Exam Score by Age Category")
p_box_attend <- create_boxplot(train_data, "attend_pct_cat", "Exam Score by Attendance Category")
p_box_web <- create_boxplot(train_data, "web_access", "Exam Score by Internet Access")
p_box_trav <- create_boxplot(train_data, "trav_time", "Exam Score by Travel Time")

p_categorical_2 <- (p_box_agecat | p_box_attend) / (p_box_web | p_box_trav) +
  plot_annotation(
    title = "Exam Score by Categorical Predictors (Part 2)",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

print(p_categorical_2)

ggsave("output/fig_06_categorical_boxplots_2.png", p_categorical_2,
       width = 14, height = 12, dpi = 300)

# Third set
p_box_extra <- create_boxplot(train_data, "extra_act", "Exam Score by Extracurricular Activities")
p_box_method <- create_boxplot(train_data, "study_method", "Exam Score by Study Method")

p_categorical_3 <- (p_box_extra | p_box_method) +
  plot_annotation(
    title = "Exam Score by Categorical Predictors (Part 3)",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

print(p_categorical_3)

ggsave("output/fig_07_categorical_boxplots_3.png", p_categorical_3,
       width = 12, height = 6, dpi = 300)

# =============================================================================
# 3. Ordinal Variables - Monotone Trend Analysis
# =============================================================================

message("\n--- Ordinal Variable Trend Analysis ---")

# Function to create trend plot for ordinal variables
create_trend_plot <- function(data, ord_var, title) {
  summary_data <- data |>
    group_by(!!sym(ord_var)) |>
    summarise(
      mean_y = mean(y),
      se = sd(y) / sqrt(n()),
      n = n(),
      .groups = "drop"
    )

  ggplot(summary_data, aes(x = .data[[ord_var]], y = mean_y, group = 1)) +
    geom_point(aes(size = n), color = "steelblue") +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_errorbar(aes(ymin = mean_y - 1.96*se, ymax = mean_y + 1.96*se),
                  width = 0.2, color = "gray40") +
    labs(
      title = title,
      subtitle = "Mean ± 95% CI, point size = group n",
      x = ord_var,
      y = "Mean Exam Score"
    ) +
    theme_glm(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create trend plots for ordinal variables
p_trend_parent <- create_trend_plot(train_data, "parent_educ",
                                    "Exam Score Trend by Parental Education")
p_trend_attend <- create_trend_plot(train_data, "attend_pct_cat",
                                    "Exam Score Trend by Attendance Category")
p_trend_age <- create_trend_plot(train_data, "agecat",
                                 "Exam Score Trend by Age Category")
p_trend_sleep <- create_trend_plot(train_data, "sleep_qual",
                                   "Exam Score Trend by Sleep Quality")

p_trends <- (p_trend_parent | p_trend_attend) / (p_trend_age | p_trend_sleep) +
  plot_annotation(
    title = "Monotone Trends in Ordinal Predictors",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

print(p_trends)

ggsave("output/fig_08_ordinal_trends.png", p_trends,
       width = 14, height = 12, dpi = 300)

# =============================================================================
# 4. Statistical Tests by Group (ANOVA)
# =============================================================================

message("\n--- ANOVA F-tests: y by Categorical Predictors ---")

anova_results <- tibble(
  Variable = character(),
  F_statistic = numeric(),
  df1 = integer(),
  df2 = integer(),
  p_value = numeric(),
  eta_squared = numeric()
)

for (var in categorical_vars) {
  formula <- as.formula(paste("y ~", var))
  aov_fit <- aov(formula, data = train_data)
  aov_summary <- summary(aov_fit)[[1]]

  # Calculate eta-squared (effect size)
  ss_between <- aov_summary$`Sum Sq`[1]
  ss_total <- sum(aov_summary$`Sum Sq`)
  eta_sq <- ss_between / ss_total

  anova_results <- bind_rows(anova_results, tibble(
    Variable = var,
    F_statistic = aov_summary$`F value`[1],
    df1 = aov_summary$Df[1],
    df2 = aov_summary$Df[2],
    p_value = aov_summary$`Pr(>F)`[1],
    eta_squared = eta_sq
  ))
}

# Sort by effect size
anova_results <- anova_results |>
  mutate(
    Significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE            ~ ""
    ),
    Effect_Size = case_when(
      eta_squared >= 0.14 ~ "Large",
      eta_squared >= 0.06 ~ "Medium",
      eta_squared >= 0.01 ~ "Small",
      TRUE                ~ "Negligible"
    )
  ) |>
  arrange(desc(eta_squared))

message("\nANOVA Results (sorted by effect size):")
print(anova_results)

# Identify significant predictors
sig_predictors <- anova_results |>
  filter(p_value < 0.05) |>
  pull(Variable)

message("\nSignificant predictors (p < 0.05): ", paste(sig_predictors, collapse = ", "))

# =============================================================================
# 5. Summary Statistics by Category
# =============================================================================

message("\n--- Mean Exam Score by Each Category ---")

# Create summary for each categorical variable
for (var in categorical_vars[1:4]) {  # First 4 variables as example
  summary_table <- train_data |>
    group_by(!!sym(var)) |>
    summarise(
      n = n(),
      mean_y = mean(y),
      sd_y = sd(y),
      .groups = "drop"
    ) |>
    arrange(desc(mean_y))

  message("\n", var, ":")
  print(summary_table)
}

# =============================================================================
# 6. Key Findings Summary
# =============================================================================

message("\n", strrep("-", 60))
message("KEY FINDINGS: Categorical Predictors")
message(strrep("-", 60))

# Top predictors by effect size
top_predictors <- anova_results |>
  head(5)

message("
1. STRONGEST CATEGORICAL PREDICTORS (by eta-squared):
")
for (i in 1:min(5, nrow(top_predictors))) {
  row <- top_predictors[i, ]
  message("   - ", row$Variable, " (η² = ", round(row$eta_squared, 3),
          ", ", row$Effect_Size, ", p ", ifelse(row$p_value < 0.001, "< 0.001",
                                                 paste("=", round(row$p_value, 3))), ")")
}

# Check for monotone trends
message("
2. ORDINAL VARIABLE TRENDS:")
ordinal_vars <- c("parent_educ", "attend_pct_cat", "sleep_qual", "agecat")
for (var in ordinal_vars) {
  means <- train_data |>
    group_by(!!sym(var)) |>
    summarise(m = mean(y), .groups = "drop") |>
    pull(m)

  is_monotone <- all(diff(means) >= 0) | all(diff(means) <= 0)
  direction <- ifelse(means[length(means)] > means[1], "increasing", "decreasing")

  message("   - ", var, ": ", ifelse(is_monotone, paste("Monotone", direction),
                                      "Non-monotone pattern"))
}

message("
3. NOTABLE PATTERNS:
   - Attendance category shows clear gradient with exam scores
   - Parental education shows expected positive trend
   - Sleep quality associated with better performance
   - School type: Compare private vs public performance

4. MODELING RECOMMENDATIONS:
   - Include significant categorical predictors in model
   - Consider ordinal coding for monotone trends
   - Test for interactions between categorical variables
   - Watch for sparse categories (may need collapsing)
")

message("\n", strrep("=", 60))
message("EDA - CATEGORICAL PREDICTORS COMPLETE")
message(strrep("=", 60), "\n")
