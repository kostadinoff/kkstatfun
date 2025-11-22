# kkstatfun

**R Statistical Analysis Toolkit for Medical and Epidemiology**

A comprehensive R package designed for medical statistics, epidemiology, and general data analysis. It provides a suite of tools for:

- **Epidemiology**: Odds Ratios, Relative Risks, 2x2 table analysis (`epi_stats`).
- **Medical Statistics**: Sensitivity, Specificity, PPV, NPV, Likelihood Ratios (`confusion_metrics_ci`).
- **Survival Analysis**: Kaplan-Meier plots with risk tables (`survival_plot`).
- **Regression**: Unified wrapper for linear, logistic, and ordinal regression with auto-diagnostics (`regression_analysis`).
- **Summary Statistics**: Publication-ready Table 1 (`table1_summary`) and comprehensive descriptive stats (`comprehensive_summary`).
- **Visualization**: Publication-quality plots with automatic font handling (`kkplot`, `univariate_plot`).
- **Bulgarian EGN**: Parsing and validation of Bulgarian personal ID numbers (`extract_egn_info`).

## Installation

You can install the package directly from GitHub:

```r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install kkstatfun from GitHub
devtools::install_github("kostadinoff/kkstatfun")
```

## Usage Examples

### Epidemiology Stats (OR/RR)

```r
library(kkstatfun)

# Calculate OR and RR from vectors
exposure <- c(1, 1, 0, 0, 1, 0, 1, 1)
outcome <- c(1, 0, 0, 1, 1, 0, 1, 0)

epi_stats(exposure = exposure, outcome = outcome)
```

### Survival Analysis

```r
library(survival)
library(kkstatfun)

# Using the lung dataset
survival_plot(lung, time_col = "time", status_col = "status", group_col = "sex")
```

### Diagnostic Test Metrics

```r
# TP=100, FP=20, FN=10, TN=870
confusion_metrics_ci(c(tp=100, fp=20, fn=10, tn=870))
```

### Regression Analysis

```r
# Auto-detects logistic regression for binary outcome
regression_analysis(mtcars, outcome = "am", predictors = c("mpg", "wt"))
```

## License

MIT
