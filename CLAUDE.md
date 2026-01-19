---
editor_options: 
  markdown: 
    wrap: 72
---

# GLM Project

This repository is a course project for GLM (Generalized Linear Models)
in R programming language. We are 4 group members working on this
project together: Yugoo, Xiaopeng, Shuaibo, and Yicheng.

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
│   ├── 01_data_load.R          # Yugoo: Data loading (template with #TODOs)
│   ├── 02_eda_response.R       # Yugoo: Response EDA (template)
│   ├── 02_eda_continuous.R     # Xiaopeng: Continuous predictors EDA (template)
│   ├── 02_eda_categorical.R    # Shuaibo: Categorical predictors EDA (template)
│   ├── 02_eda_associations.R   # Yicheng: Associations & collinearity (template)
│   ├── 03_modeling.R           # Yugoo+Yicheng: Model building (template)
│   ├── 04_diagnostics.R        # Xiaopeng: Model diagnostics (template)
│   ├── 05_interpretation.R     # Shuaibo: Interpretation (template)
│   ├── 06_prediction.R         # Yicheng: Predictive performance (template)
│   ├── run_analysis.R          # Master script (template)
│   └── *-claude.R              # Reference implementations (with actual code)
├── output/                     # Generated figures
├── report.qmd                  # Quarto report
├── references.bib              # Bibliography
└── glm_project.Rproj
```

------------------------------------------------------------------------

## Task Assignments

| Phase | Yugoo | Xiaopeng | Shuaibo | Yicheng |
|---------------|---------------|---------------|---------------|---------------|
| **1: EDA** | Data load + Response y | Continuous predictors | Categorical predictors | Associations |
| **2: Modeling** | Model building | \- | \- | Model building |
| **3: Analysis** | \- | Diagnostics | Interpretation | Prediction |
| **4: Report** | Intro + Data | EDA + Diagnostics | Model + Interp | Prediction + Conclusion |

### Detailed Assignments

**Yugoo (Data & Response):** - `01_data_load.R` - Import data, convert
factors, train/test split - `02_eda_response.R` - Distribution of y,
normality, outliers - `03_modeling.R` - Collaborate with Yicheng on model
building

**Xiaopeng (Continuous & Diagnostics):** - `02_eda_continuous.R` - Analyze
age, study_hrs, sleep_hrs, attend_pct - `04_diagnostics.R` - Residual
plots, normality, influence diagnostics

**Shuaibo (Categorical & Interpretation):** - `02_eda_categorical.R` -
Analyze all categorical variables - `05_interpretation.R` - Coefficient
interpretation, scenario predictions

**Yicheng (Associations & Prediction):** - `02_eda_associations.R` -
Correlations, collinearity, interaction hypotheses - `03_modeling.R` -
Collaborate with Yugoo on model building - `06_prediction.R` - Test set
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
    Yugoo: 01_data_load.R + 02_eda_response.R
    Xiaopeng: 02_eda_continuous.R
    Shuaibo: 02_eda_categorical.R
    Yicheng: 02_eda_associations.R
         ↓
Phase 2: Modeling (Yugoo + Yicheng collaborate)
    03_modeling.R
         ↓
Phase 3: Post-Modeling (All work in parallel)
    Xiaopeng: 04_diagnostics.R
    Shuaibo: 05_interpretation.R
    Yicheng: 06_prediction.R
         ↓
Phase 4: Report Assembly (All collaborate)
    report.qmd
```
