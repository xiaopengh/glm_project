# =============================================================================
# 01_data_load.R - Data Loading & Factor Conversion
# =============================================================================
# Author: Yugoo
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

# TODO: Import the dataset from DATA_PATH using read_csv()

data_raw <- read_csv(DATA_PATH, show_col_types = FALSE)

# TODO: Report structure - number of observations and variables

message("\n--- Dataset Structure ---")
message("Number of observations: ", nrow(data_raw))
message("Number of variables: ", ncol(data_raw))

# TODO: Display variable types using glimpse()

print(glimpse(data_raw))

# -----------------------------------------------------------------------------
# 1.2 Convert Coded Categorical Variables to Labeled Factors
# -----------------------------------------------------------------------------

# TODO: Create data_clean by converting coded variables to factors:
#   - agecat: 5 levels ("14-15", "15-16", "16-17", "17-18", "18+"), ordered
#   - sexe: 3 levels ("Female", "Male", "Other")
#   - school_type: 2 levels ("Public", "Private")
#   - parent_educ: 6 levels ("No Formal" to "PhD"), ordered
#   - sleep_qual: 3 levels ("Poor", "Average", "Good"), ordered
#   - attend_pct_cat: 4 levels ("<60%", "60-75%", "75-85%", ">85%"), ordered
#   - web_access: 2 levels ("No", "Yes")
#   - trav_time: 4 levels ("<15 min", "15-30 min", "30-60 min", ">60 min"), ordered
#   - extra_act: 2 levels ("No", "Yes")
#   - study_method: 6 levels ("Traditional", "Digital", "Group Study",
#                             "Tutoring", "Self-study", "Mixed")

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

# TODO: Set sensible reference categories using relevel():
#   - school_type: "Public" as reference
#   - sexe: "Female" as reference
#   - web_access: "No" as reference
#   - extra_act: "No" as reference
#   - study_method: "Traditional" as reference

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

# TODO: Print factor levels to verify

message("\n--- Factor Levels (with reference categories) ---")
data_clean |>
  select(where(is.factor)) |>
  map(levels) |>
  print()

# -----------------------------------------------------------------------------
# 1.4 Document Decision: Continuous vs Categorical
# -----------------------------------------------------------------------------

# TODO: Document the decision about using continuous vs categorical versions:
#   - age vs agecat
#   - attend_pct vs attend_pct_cat
# Recommendation: Use continuous versions for modeling (more information)

# -----------------------------------------------------------------------------
# 1.5 Create Train/Test Split (80/20)
# -----------------------------------------------------------------------------

# TODO: Set seed for reproducibility: set.seed(42)

set.seed(42)

# TODO: Create train_idx using sample() for 80% of data

n <- nrow(data_clean)
train_idx <- sample(1:n, size = floor(0.8 * n), replace = FALSE)

# TODO: Create train_data and test_data

train_data <- data_clean[train_idx, ]
test_data  <- data_clean[-train_idx, ]

# TODO: Report split sizes and verify response distribution is similar

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

# TODO: Print summary of variable categories

# =============================================================================
# Create Summary Table
# =============================================================================

# TODO: Create formatted summary table using tbl_summary()

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

message("\n--- Data Objects Available ---")
message("  data_raw: Original raw data")
message("  data_clean: Cleaned data with factors")
message("  train_data: Training set (80%)")
message("  test_data: Test set (20%)")

message("\n", strrep("=", 60))
message("DATA LOADING COMPLETE")
message(strrep("=", 60), "\n")
