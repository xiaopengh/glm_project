# =============================================================================
# 02_eda_associations.R - Exploratory Data Analysis: Predictor Associations
# =============================================================================
# Author: Yicheng
# Task: Analyze associations between predictors and check for collinearity
# =============================================================================

# Load data (this sources 00_setup.R internally)
source("R/01_data_load.R")

message("\n", strrep("=", 60))
message("SECTION 2D: EDA - PREDICTOR ASSOCIATIONS & COLLINEARITY")
message(strrep("=", 60))

# Define variable groups
continuous_vars <- c("age", "study_hrs", "sleep_hrs", "attend_pct")
categorical_vars <- c("sexe", "school_type", "parent_educ", "sleep_qual",
                      "agecat", "attend_pct_cat", "web_access", "trav_time",
                      "extra_act", "study_method")

# =============================================================================
# 1. Numeric-Numeric Associations
# =============================================================================

message("\n--- Correlation Matrix: Continuous Variables ---")

# Calculate correlation matrix for y and continuous predictors
library(dplyr)
library(corrplot)
cor_matrix <- cor(select(train_data, y, all_of(continuous_vars)))
print(round(cor_matrix, 3))
corrplot(cor_matrix, order = 'hclust',addrect = 3)

# ===== Correlation Analysis Summary =====
# Key findings:
# 1. Strongest predictors of y: attend_pct (0.736), study_hrs (0.567)
# 2. Moderate predictor: sleep_hrs (0.417)
# 3. Weak predictor: age (-0.045)
# 4. Multicollinearity concern: study_hrs and attend_pct are correlated (0.526)
#    → May need to select one of them for modeling to avoid unstable coefficients

# Save to output/fig_09_correlation_heatmap.png
png("output/fig_09_correlation_heatmap.png", width = 800, height = 600)
corrplot(cor_matrix, order = 'hclust', addrect = 3)
dev.off()

# =============================================================================
# 2. Categorical-Categorical Associations
# =============================================================================

message("\n--- Cross-Tabulations: Key Categorical Pairs ---")

# Function for chi-square test with summary
chi_square_test <- function(data, var1, var2) {
  tbl <- table(data[[var1]], data[[var2]])
  test <- chisq.test(tbl, simulate.p.value = TRUE)
  
  message("\n", var1, " × ", var2, ":")
  print(tbl)
  message("  Chi-square = ", round(test$statistic, 2),
          ", p-value = ", format.pval(test$p.value, digits = 3))
  
  if (test$p.value < 0.05) {
    message("  → Significant association detected")
  } else {
    message("  → No significant association")
  }
  
  invisible(test)
}

# Key cross-tabulations
chi_school_parent <- chi_square_test(train_data, "school_type", "parent_educ")
chi_web_school <- chi_square_test(train_data, "web_access", "school_type")
chi_extra_method <- chi_square_test(train_data, "extra_act", "study_method")
chi_sleep_extra <- chi_square_test(train_data, "sleep_qual", "extra_act")
# Significant associations (potential collinearity):
#   - school_type ↔ parent_educ (χ²=191.81, p<0.001) - strong link
#   - web_access ↔ school_type (χ²=57.17, p<0.001)
#   - sleep_qual ↔ extra_act (χ²=34.03, p<0.001)
#
# Independent variables:
#   - extra_act ↔ study_method (p=0.137) - safe to use together
#
# Action: Consider variable selection for highly associated pairs



# =============================================================================
# 3. Mixed Associations (Categorical-Numeric)
# =============================================================================

message("\n--- Mixed Associations: Continuous by Categorical ---")

# Calculate group means for key combinations:
# Function to compute group means
group_means <- function(data, num_var, cat_var) {
  data |>
    group_by(!!sym(cat_var)) |>
    summarise(
      n = n(),
      mean = mean(!!sym(num_var)),
      sd = sd(!!sym(num_var)),
      .groups = "drop"
    )
}
# Key combinations
message("\n1. Study Hours by School Type:")
print(group_means(train_data, "study_hrs", "school_type"))

message("\n2. Study Hours by Parent Education:")
print(group_means(train_data, "study_hrs", "parent_educ"))

message("\n3. Sleep Hours by Sleep Quality:")
print(group_means(train_data, "sleep_hrs", "sleep_qual"))

message("\n4. Attendance % by School Type:")
print(group_means(train_data, "attend_pct", "school_type"))

message("\n5. Study Hours by Extra Activities:")
print(group_means(train_data, "study_hrs", "extra_act"))

# Visualize key mixed associations
p_study_school <- ggplot(train_data, aes(x = school_type, y = study_hrs)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  labs(title = "Study Hours by School Type", x = "School Type", y = "Study Hours") +
  theme_glm()

p_study_parent <- ggplot(train_data, aes(x = parent_educ, y = study_hrs)) +
  geom_boxplot(fill = "lightgreen") +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  labs(title = "Study Hours by Parent Education", x = "Parent Education", y = "Study Hours") +
  theme_glm() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_attend_school <- ggplot(train_data, aes(x = school_type, y = attend_pct)) +
  geom_boxplot(fill = "lightyellow") +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  labs(title = "Attendance by School Type", x = "School Type", y = "Attendance %") +
  theme_glm()

p_sleep_qual <- ggplot(train_data, aes(x = sleep_qual, y = sleep_hrs)) +
  geom_boxplot(fill = "lavender") +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  labs(title = "Sleep Hours by Sleep Quality", x = "Sleep Quality", y = "Sleep Hours") +
  theme_glm()

p_mixed <- (p_study_school | p_study_parent) / (p_attend_school | p_sleep_qual) +
  plot_annotation(
    title = "Mixed Associations: Continuous Predictors by Categories",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

print(p_mixed)

ggsave("output/fig_11_mixed_associations.png", p_mixed,
       width = 14, height = 12, dpi = 300)

# =============================================================================
# 4. Potential Confounding Patterns
# =============================================================================

message("\n--- Potential Confounding Analysis ---")

#Check if school type is confounded with other predictors:
# Use t.test() for continuous predictors by school_type
for (var in continuous_vars) {
  test <- t.test(as.formula(paste(var, "~ school_type")), data = train_data)
  if (test$p.value < 0.05) {
    message("   - ", var, ": Significant difference by school type (p = ",
            format.pval(test$p.value, digits = 3), ")")
  }
}

# Check parent education correlation with continuous predictors:
message("\n2. Correlation: Parent Education level with continuous predictors")
train_data_numeric <- train_data |>
  mutate(parent_educ_num = as.numeric(parent_educ))

for (var in continuous_vars) {
  cor_test <- cor.test(train_data_numeric[[var]], train_data_numeric$parent_educ_num)
  if (abs(cor_test$estimate) > 0.1) {
    message("   - ", var, ": r = ", round(cor_test$estimate, 3),
            " (p = ", format.pval(cor_test$p.value, digits = 3), ")")
  }
}

# =============================================================================
# 5. Interaction Hypotheses
# =============================================================================

message("\n--- Proposed Interaction Hypotheses ---")

message("
Based on the EDA findings, the following interactions are worth testing:

1. study_hrs × sleep_qual
   Rationale: Study effectiveness may depend on sleep quality
   - Students with good sleep may benefit more from each hour of study
   - Pattern check: Study hours effect stronger for well-rested students?

2. attend_pct × school_type
   Rationale: Attendance effect may differ by school type
   - Private schools may have different teaching quality/requirements
   - Pattern check: Is attendance more critical in one school type?

3. study_hrs × study_method
   Rationale: Study method may moderate the effect of study hours
   - Some methods may be more efficient than others
   - Pattern check: Do mixed/digital methods show different slopes?

4. parent_educ × web_access
   Rationale: Educational resources may compound
   - Higher educated parents + internet may show synergistic effect
   - Pattern check: Web access benefit differs by parent education?

These will be tested formally in the modeling phase (03_modeling.R).
")

# Visual check for key interactions
p_int_study_sleep <- ggplot(train_data, aes(x = study_hrs, y = y, color = sleep_qual)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "y vs Study Hours by Sleep Quality",
       x = "Study Hours", y = "Exam Score", color = "Sleep Quality") +
  theme_glm()

p_int_attend_school <- ggplot(train_data, aes(x = attend_pct, y = y, color = school_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "y vs Attendance by School Type",
       x = "Attendance %", y = "Exam Score", color = "School Type") +
  theme_glm()

p_interactions <- p_int_study_sleep | p_int_attend_school

print(p_interactions)

ggsave("output/fig_12_interaction_exploration.png", p_interactions,
       width = 14, height = 6, dpi = 300)

# =============================================================================
# 6. VIF Pre-check (Simple Model)
# =============================================================================

message("\n--- VIF Pre-check with Simple Model ---")

# Fit a model with main continuous predictors to check VIF
simple_model <- lm(y ~ study_hrs + sleep_hrs + attend_pct + age, data = train_data)
vif_values <- car::vif(simple_model)

message("VIF values for continuous predictors:")
print(vif_values)

if (any(vif_values > 5)) {
  message("\nWARNING: VIF > 5 detected - potential multicollinearity issue")
} else if (any(vif_values > 2)) {
  message("\nNote: VIF between 2-5 detected - moderate correlation but acceptable")
} else {
  message("\nGood: All VIF values < 2 - no multicollinearity concerns")
}

# =============================================================================
# 7. Key Findings Summary
# =============================================================================

message("\n", strrep("-", 60))
message("KEY FINDINGS: Predictor Associations & Collinearity")
message(strrep("-", 60))

#Summarize:
message("
1. NUMERIC-NUMERIC CORRELATIONS:
   - Strongest correlations with y: ", paste(
     names(sort(abs(cor_matrix[, 1])[-1], decreasing = TRUE)[1:2]),
     collapse = ", "), "
   - Predictor-predictor correlations: ", ifelse(
     any(abs(cor_matrix[-1, -1][upper.tri(cor_matrix[-1, -1])]) > 0.5),
     "Some high correlations detected",
     "No concerning collinearity (all |r| < 0.5)"), "

2. CATEGORICAL-CATEGORICAL ASSOCIATIONS:
   - School type & Parent education: ", ifelse(
     chi_school_parent$p.value < 0.05, "Associated", "Independent"), "
   - Web access & School type: ", ifelse(
     chi_web_school$p.value < 0.05, "Associated", "Independent"), "

3. CONFOUNDING PATTERNS:
   - School type shows differences in continuous predictors
   - Parent education correlates with study-related behaviors
   - Consider these relationships when interpreting coefficients

4. RECOMMENDED INTERACTIONS TO TEST:
   - study_hrs × sleep_qual
   - attend_pct × school_type
   - study_hrs × study_method

5. MULTICOLLINEARITY CHECK:
   - VIF values: ", ifelse(all(vif_values < 5),
                           "All acceptable (< 5)",
                           "Some concerns present"), "
   - Safe to proceed with multiple regression
")

message("\n", strrep("=", 60))
message("EDA - PREDICTOR ASSOCIATIONS COMPLETE")
message(strrep("=", 60), "\n")
