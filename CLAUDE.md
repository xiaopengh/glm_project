---
editor_options: 
  markdown: 
    wrap: 72
---

# GLM Project

This repository is a course project for GLM (Generalized Linear Models)
in R programming language. We are 4 group members working on this
project together: XX, YY, ZZ, and WW.

## Project Overview

**Objective:** Multiple linear regression analysis predicting student
exam scores (`y`) from various predictors (demographic, behavioral, and
environmental factors).

**Dataset:** `data/project.csv` with 16 variables (1 response + 15
predictors)

**Key Requirements:** - Reproducibility: `set.seed(42)` - Relative paths
only - No data leakage (train/test split before modeling)

------------------------------------------------------------------------

## Project Structure

```         
glm_project/
├── data/
│   └── project.csv
├── R/
│   ├── 00_setup.R              # Shared setup (libraries, helpers) - COMPLETE
│   ├── 01_data_load.R          # XX: Data loading (template with #TODOs)
│   ├── 02_eda_response.R       # XX: Response EDA (template)
│   ├── 02_eda_continuous.R     # YY: Continuous predictors EDA (template)
│   ├── 02_eda_categorical.R    # ZZ: Categorical predictors EDA (template)
│   ├── 02_eda_associations.R   # WW: Associations & collinearity (template)
│   ├── 03_modeling.R           # XX+WW: Model building (template)
│   ├── 04_diagnostics.R        # YY: Model diagnostics (template)
│   ├── 05_interpretation.R     # ZZ: Interpretation (template)
│   ├── 06_prediction.R         # WW: Predictive performance (template)
│   ├── run_analysis.R          # Master script (template)
│   └── *-claude.R              # Reference implementations (with actual code)
├── output/                     # Generated figures
├── report.qmd                  # Quarto report
├── references.bib              # Bibliography
└── glm_project.Rproj
```

------------------------------------------------------------------------

## Task Assignments

| Phase | XX | YY | ZZ | WW |
|------------------|--------------|--------------|--------------|--------------|
| **1: EDA** | Data load + Response y | Continuous predictors | Categorical predictors | Associations |
| **2: Modeling** | Model building | \- | \- | Model building |
| **3: Analysis** | \- | Diagnostics | Interpretation | Prediction |
| **4: Report** | Intro + Data | EDA + Diagnostics | Model + Interp | Prediction + Conclusion |

### Detailed Assignments

**XX (Data & Response):** - `01_data_load.R` - Import data, convert
factors, train/test split - `02_eda_response.R` - Distribution of y,
normality, outliers - `03_modeling.R` - Collaborate with WW on model
building

**YY (Continuous & Diagnostics):** - `02_eda_continuous.R` - Analyze
age, study_hrs, sleep_hrs, attend_pct - `04_diagnostics.R` - Residual
plots, normality, influence diagnostics

**ZZ (Categorical & Interpretation):** - `02_eda_categorical.R` -
Analyze all categorical variables - `05_interpretation.R` - Coefficient
interpretation, scenario predictions

**WW (Associations & Prediction):** - `02_eda_associations.R` -
Correlations, collinearity, interaction hypotheses - `03_modeling.R` -
Collaborate with XX on model building - `06_prediction.R` - Test set
performance, calibration

------------------------------------------------------------------------

## How to Work

### For Each Team Member:

1.  **Open your assigned `.R` file** (the template with `#TODO`
    comments)
2.  **Replace each `#TODO` with actual R code**
3.  **Reference the `-claude.R` file** if you need guidance
4.  **Test your script** by running it: `source("R/your_script.R")`

### File Types:

-   `*.R` - **Template files** with `#TODO` placeholders (work on these)
-   `*-claude.R` - **Reference files** with complete code (use as guide)

### Running the Analysis:

``` r
# Run complete pipeline
source("R/run_analysis.R")

# Or run individual scripts
source("R/01_data_load.R")
source("R/02_eda_response.R")
# etc.
```

### Rendering the Report:

``` bash
quarto render report.qmd
```

------------------------------------------------------------------------

## Coding Standards

``` r
# Use native pipe
data |> mutate(...) |> select(...)

# Visualization style
ggplot(aes(x = var1, y = var2)) +
  geom_point(size = 2, shape = 21, fill = "dodgerblue") +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  theme_bw(base_size = 14) +
  labs_pubr()

# Tables
tbl_summary(type = all_continuous() ~ "continuous2") |>
  bold_labels()

# Model output
broom::tidy(model, conf.int = TRUE)
```

------------------------------------------------------------------------

## Deliverables Checklist

-   [ ] All `.R` scripts completed (TODOs replaced with code)
-   [ ] `run_analysis.R` executes without errors
-   [ ] Figures saved to `output/` directory
-   [ ] `report.qmd` renders to `report.html`
-   [ ] AI use statement included in report

------------------------------------------------------------------------

## Timeline Workflow

```         
Phase 1: EDA (All work in parallel)
    XX: 01_data_load.R + 02_eda_response.R
    YY: 02_eda_continuous.R
    ZZ: 02_eda_categorical.R
    WW: 02_eda_associations.R
         ↓
Phase 2: Modeling (XX + WW collaborate)
    03_modeling.R
         ↓
Phase 3: Post-Modeling (All work in parallel)
    YY: 04_diagnostics.R
    ZZ: 05_interpretation.R
    WW: 06_prediction.R
         ↓
Phase 4: Report Assembly (All collaborate)
    report.qmd
```
