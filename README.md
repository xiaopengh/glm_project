# GLM Project: Student Exam Score Prediction

Multiple linear regression analysis predicting student exam scores from demographic, behavioral, and environmental factors.

## Team Members
- XX, YY, ZZ, WW

---

## Quick Start

### 1. Clone the Repository

```bash
git clone <repository-url>
cd glm_project
```

### 2. Setup R Environment with renv

Open the project in RStudio (double-click `glm_project.Rproj`), then run:

```r
# Install renv if you don't have it
install.packages("renv")

# Restore the project library (installs all required packages)
renv::restore()
```

This installs the exact package versions everyone else is using. If `renv::restore()` asks for confirmation, type `y`.

**Troubleshooting renv:**
```r
# If restore fails, try:
renv::repair()
renv::restore()

# If a specific package fails, install manually then snapshot:
install.packages("package_name")
renv::snapshot()
```

### 3. Verify Setup

```r
source("R/00_setup.R")  # Should load without errors
```

---

## Git Workflow Rules

### Before You Start Working

```bash
git pull
```

### File Ownership (Do Not Edit Others' Files)

| Member | Owns These Files |
|--------|------------------|
| **XX** | `01_data_load.R`, `02_eda_response.R` |
| **YY** | `02_eda_continuous.R`, `04_diagnostics.R` |
| **ZZ** | `02_eda_categorical.R`, `05_interpretation.R` |
| **WW** | `02_eda_associations.R`, `06_prediction.R` |
| **XX+WW** | `03_modeling.R` (coordinate before editing) |

**If you need to edit someone else's file:** message them first and wait for confirmation.

### Commit and Push Workflow

After making changes, use this exact sequence:

```bash
git add -A
git commit -m "Short descriptive message"
git pull --rebase
git push
```

**Example commit messages:**
- `Complete histogram for response variable`
- `Add correlation matrix to associations`
- `Fix missing factor conversion`

### If You Get Conflicts During Rebase

```bash
# 1. Open the conflicted file(s) and resolve conflicts manually
# 2. Then run:
git add -A
git rebase --continue
git push
```

### Golden Rules

1. **Pull before you start, pull before you push**
2. **Commit small, commit often** (every logical step)
3. **Push frequently** (don't sit on local commits)
4. **Never use `git push -f`** on the shared branch
5. **Test your code before pushing** (`source("R/your_file.R")`)

---

## Running the Analysis

### Run Individual Scripts

```r
source("R/01_data_load.R")
source("R/02_eda_response.R")
# ... etc.
```

### Run Complete Pipeline

```r
source("R/run_analysis.R")
```

### Render the Report

```bash
quarto render report.qmd
```

---

## Project Structure

```
glm_project/
├── data/
│   └── project.csv          # Dataset (DO NOT MODIFY)
├── R/
│   ├── 00_setup.R           # Shared setup (COMPLETE)
│   ├── 01_data_load.R       # XX: Data loading
│   ├── 02_eda_*.R           # EDA scripts (one per member)
│   ├── 03_modeling.R        # XX+WW: Model building
│   ├── 04_diagnostics.R     # YY: Diagnostics
│   ├── 05_interpretation.R  # ZZ: Interpretation
│   ├── 06_prediction.R      # WW: Prediction
│   ├── run_analysis.R       # Master script
│   └── *-claude.R           # Reference implementations
├── output/                  # Generated figures
├── report.qmd               # Quarto report
├── renv.lock                # Package versions (DO NOT EDIT MANUALLY)
└── glm_project.Rproj
```

---

## Coordination Checklist

Use your group chat to announce:
- "Starting work on `file_x.R`"
- "Done with `file_x.R`, pushed"

---

## Adding New Packages

If you need a new package:

```r
install.packages("new_package")
renv::snapshot()
```

Then commit the updated `renv.lock`:

```bash
git add renv.lock
git commit -m "Add new_package to renv"
git pull --rebase
git push
```

**Tell your teammates to run `renv::restore()` after pulling.**
