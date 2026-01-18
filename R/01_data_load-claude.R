# =============================================================================
# 01_data_load.R - Data Loading & Factor Conversion
# =============================================================================
# Author: XX
# Task: Import dataset, convert categorical variables, create train/test split
# =============================================================================

# Load shared setup
source("R/00_setup.R")

# =============================================================================
# Task A: Data Loading
# =============================================================================

message("\n", strrep("=", 60))
message("SECTION 1: DATA LOADING & PREPARATION")
message(strrep("=", 60))

# -----------------------------------------------------------------------------
# 1.1 Import Dataset
# -----------------------------------------------------------------------------

data_raw <- read_csv(DATA_PATH, show_col_types = FALSE)

# Report structure
message("\n--- Dataset Structure ---")
message("Number of observations: ", nrow(data_raw))
message("Number of variables: ", ncol(data_raw))

# Display variable types
message("\nVariable types:")
print(glimpse(data_raw))

# -----------------------------------------------------------------------------
# 1.2 Convert Coded Categorical Variables to Labeled Factors
# -----------------------------------------------------------------------------

data_clean <- data_raw |>
  mutate(
    # agecat: 5 levels (age ranges)
    # Based on typical age distribution in student data
    agecat = factor(
      agecat,
      levels = 1:5,
      labels = c("14-15", "15-16", "16-17", "17-18", "18+"),
      ordered = TRUE
    ),

    # sexe: Female/Male/Other
    sexe = factor(
      sexe,
      levels = 1:3,
      labels = c("Female", "Male", "Other")
    ),

    # school_type: Public/Private
    school_type = factor(
      school_type,
      levels = 1:2,
      labels = c("Public", "Private")
    ),

    # parent_educ: 6 levels (No Formal -> PHD)
    parent_educ = factor(
      parent_educ,
      levels = 1:6,
      labels = c("No Formal", "High School", "Some College",
                 "Bachelor", "Master", "PhD"),
      ordered = TRUE
    ),

    # sleep_qual: Poor/Average/Good
    sleep_qual = factor(
      sleep_qual,
      levels = 1:3,
      labels = c("Poor", "Average", "Good"),
      ordered = TRUE
    ),

    # attend_pct_cat: 4 levels
    attend_pct_cat = factor(
      attend_pct_cat,
      levels = 1:4,
      labels = c("<60%", "60-75%", "75-85%", ">85%"),
      ordered = TRUE
    ),

    # web_access: No/Yes
    web_access = factor(
      web_access,
      levels = 1:2,
      labels = c("No", "Yes")
    ),

    # trav_time: 4 levels (travel time to school)
    trav_time = factor(
      trav_time,
      levels = 1:4,
      labels = c("<15 min", "15-30 min", "30-60 min", ">60 min"),
      ordered = TRUE
    ),

    # extra_act: No/Yes (extracurricular activities)
    extra_act = factor(
      extra_act,
      levels = 1:2,
      labels = c("No", "Yes")
    ),

    # study_method: 6 levels
    study_method = factor(
      study_method,
      levels = 1:6,
      labels = c("Traditional", "Digital", "Group Study",
                 "Tutoring", "Self-study", "Mixed")
    )
  )

# -----------------------------------------------------------------------------
# 1.3 Set Reference Categories
# -----------------------------------------------------------------------------

# Set sensible reference categories for modeling
# (most common or baseline category)

data_clean <- data_clean |>
  mutate(
    # Public school as reference (typically larger group)
    school_type = relevel(school_type, ref = "Public"),

    # Female as reference (alphabetically first)
    sexe = relevel(sexe, ref = "Female"),

    # No internet access as baseline
    web_access = relevel(web_access, ref = "No"),

    # No extra activities as baseline
    extra_act = relevel(extra_act, ref = "No"),

    # Traditional study method as baseline
    study_method = relevel(study_method, ref = "Traditional")
  )

message("\n--- Factor Levels (with reference categories) ---")
data_clean |>
  select(where(is.factor)) |>
  map(levels) |>
  print()

# -----------------------------------------------------------------------------
# 1.4 Document Decision: Continuous vs Categorical
# -----------------------------------------------------------------------------

message("\n--- Decision: Continuous vs Categorical Variables ---")
message("
The dataset contains paired continuous/categorical versions of some variables:
  - age (continuous) vs agecat (categorical: 5 age groups)
  - attend_pct (continuous) vs attend_pct_cat (categorical: 4 attendance groups)

DECISION:
  - For EDA: Analyze BOTH versions to understand patterns
  - For Modeling: Start with CONTINUOUS versions (age, attend_pct)
    as they retain more information
  - Consider categorical if non-linear patterns are detected

RATIONALE:
  - Continuous variables allow detection of non-linear relationships
  - Continuous variables have more statistical power
  - Categorical versions can be used for stratified analyses
")

# -----------------------------------------------------------------------------
# 1.5 Create Train/Test Split (80/20)
# -----------------------------------------------------------------------------

set.seed(42)  # Ensure reproducibility

n <- nrow(data_clean)
train_idx <- sample(1:n, size = floor(0.8 * n), replace = FALSE)

train_data <- data_clean[train_idx, ]
test_data  <- data_clean[-train_idx, ]

message("\n--- Train/Test Split ---")
message("Training set: ", nrow(train_data), " observations (",
        round(100 * nrow(train_data) / n, 1), "%)")
message("Test set: ", nrow(test_data), " observations (",
        round(100 * nrow(test_data) / n, 1), "%)")

# Verify response distribution is similar
message("\nResponse (y) distribution check:")
message("  Train mean: ", round(mean(train_data$y), 2),
        " | Test mean: ", round(mean(test_data$y), 2))
message("  Train SD: ", round(sd(train_data$y), 2),
        " | Test SD: ", round(sd(test_data$y), 2))

# =============================================================================
# Summary of Variables
# =============================================================================

message("\n--- Variable Summary ---")
message("\nResponse Variable:")
message("  y: Exam score (continuous)")

message("\nContinuous Predictors (4):")
message("  age: Student age in years")
message("  study_hrs: Weekly study hours")
message("  sleep_hrs: Daily sleep hours")
message("  attend_pct: Attendance percentage")

message("\nCategorical Predictors (10):")
message("  sexe: Gender (Female/Male/Other)")
message("  school_type: School type (Public/Private)")
message("  parent_educ: Parental education (6 levels, ordered)")
message("  sleep_qual: Sleep quality (Poor/Average/Good, ordered)")
message("  agecat: Age category (5 levels, ordered)")
message("  attend_pct_cat: Attendance category (4 levels, ordered)")
message("  web_access: Internet access (No/Yes)")
message("  trav_time: Travel time (4 levels, ordered)")
message("  extra_act: Extracurricular activities (No/Yes)")
message("  study_method: Study method (6 levels)")

# =============================================================================
# Create Summary Table
# =============================================================================

tbl_data_summary <- data_clean |>
  select(-id) |>
  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c(
      "{mean} ({sd})",
      "{median} [{p25}, {p75}]",
      "{min} - {max}"
    ),
    label = list(
      y ~ "Exam Score",
      age ~ "Age (years)",
      study_hrs ~ "Study Hours (per week)",
      sleep_hrs ~ "Sleep Hours (per day)",
      attend_pct ~ "Attendance (%)"
    )
  ) |>
  bold_labels()

print(tbl_data_summary)

# =============================================================================
# Save Outputs
# =============================================================================

# Save cleaned data objects for use by other scripts
# (These will be available when this script is sourced)

message("\n--- Data Objects Available ---")
message("  data_raw: Original raw data")
message("  data_clean: Cleaned data with factors")
message("  train_data: Training set (80%)")
message("  test_data: Test set (20%)")

message("\n", strrep("=", 60))
message("DATA LOADING COMPLETE")
message(strrep("=", 60), "\n")
