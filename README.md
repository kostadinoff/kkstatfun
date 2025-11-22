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

# Legacy function
epi_stats(exposure = exposure, outcome = outcome)

# New Tidyverse-friendly function
# Returns OR, RR, RD, Attributable Fraction (AF), PAF, and Preventable Fraction (PF)
df <- data.frame(exposure, outcome)
kk_epi_2x2(df, exposure, outcome)
```

### Survival Analysis

```r
library(survival)
library(kkstatfun)

# Using the lung dataset
# Legacy
survival_plot(lung, time_col = "time", status_col = "status", group_col = "sex")

# New Tidyverse-friendly
kk_survival_plot(lung, time, status, sex)
```

### Table 1 Summary

```r
# Create a publication-ready Table 1
kk_table1(mtcars, by = "am", variables = c("mpg", "hp", "wt"))
```

### Diagnostic Test Metrics

```r
# TP=100, FP=20, FN=10, TN=870
confusion_metrics_ci(c(tp=100, fp=20, fn=10, tn=870))

# From data frame with truth and prediction columns
kk_diagnostic(data, truth_col, pred_col)
```

### Regression Analysis

```r
# Auto-detects logistic regression for binary outcome
regression_analysis(mtcars, outcome = "am", predictors = c("mpg", "wt"))
```

## License

MIT

## Citation

If you use this package in your research, please cite it as follows:

```bibtex
@Manual{kkstatfun,
  title = {kkstatfun: R Statistical Analysis Toolkit for Medical statistics and Epidemiology},
  author = {Kostadinov, Kostadin},
  year = {2025},
  note = {R package version 0.1.0},
  url = {https://github.com/kostadinoff/kkstatfun},
}
```

## Credits

This package was architected, refactored, and enhanced by **Antigravity**, an advanced AI coding assistant developed by **Google DeepMind**. 

It serves as a showcase of high-level AI capability in:
- **R Package Development**: Structuring complex scripts into modular, installable packages.
- **Statistical Programming**: Implementing robust medical statistics and epidemiological methods.
- **Code Refactoring**: Modernizing legacy code with best practices and error handling.

