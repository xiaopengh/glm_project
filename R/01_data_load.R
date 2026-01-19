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

# TODO: Report structure - number of observations and variables

# TODO: Display variable types using glimpse()

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

# -----------------------------------------------------------------------------
# 1.3 Set Reference Categories
# -----------------------------------------------------------------------------

# TODO: Set sensible reference categories using relevel():
#   - school_type: "Public" as reference
#   - sexe: "Female" as reference
#   - web_access: "No" as reference
#   - extra_act: "No" as reference
#   - study_method: "Traditional" as reference

# TODO: Print factor levels to verify

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

# TODO: Create train_idx using sample() for 80% of data

# TODO: Create train_data and test_data

# TODO: Report split sizes and verify response distribution is similar

# =============================================================================
# Summary of Variables
# =============================================================================

# TODO: Print summary of variable categories

# =============================================================================
# Create Summary Table
# =============================================================================

# TODO: Create formatted summary table using tbl_summary()

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
