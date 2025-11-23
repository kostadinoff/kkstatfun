# kkstatfun Package Manual

## Overview
**kkstatfun** is a statistical analysis toolkit for medical and epidemiology research in R. It provides a suite of functions for proportion analysis, regression modeling, time series analysis, and more.

## Proportion Analysis (`R/proportions.R`)

This module provides functions for calculating confidence intervals for proportions and comparing proportions between groups.

### `pcit(data, conf.level = 0.95)`

Calculates binomial confidence intervals for proportions based on the first two numeric columns (successes, trials).

**Arguments:**
- `data`: A data frame containing at least two numeric columns. The first numeric column is treated as "successes" (x) and the second as "trials" (n).
- `conf.level`: Confidence level for the intervals (default: 0.95).

**Returns:**
A tibble containing the original data plus:
- `successes`: Count of successes
- `trials`: Count of trials
- `proportion`: Calculated proportion (x/n)
- `lower`: Lower bound of the confidence interval
- `upper`: Upper bound of the confidence interval
- `conf.level`: The confidence level used

**Example:**
```r
df <- data.frame(
  group = c("A", "B"),
  events = c(10, 20),
  total = c(100, 100)
)
pcit(df)
```

---

### `compare_proportions_kk_glm(data, group, x, n, ...)`

Compares proportions between groups using logistic regression with robust standard errors. This is robust to heteroscedasticity and clustering if specified.

**Arguments:**
- `data`: Data frame.
- `group`: The grouping variable (e.g., treatment arm).
- `x`: Variable name for success counts.
- `n`: Variable name for trial counts.
- `by`: (Optional) Stratification variable.
- `covariates`: (Optional) Character vector of covariate names to adjust for.
- `adjust`: Method for p-value adjustment (default: "holm").
- `conf.level`: Confidence level (default: 0.95).
- `vcov_type`: Robust variance type (default: "HC3").
- `drop_empty`: Whether to drop empty groups (default: TRUE).

**Returns:**
A tibble with pairwise comparisons:
- `group1`, `group2`: Groups being compared
- `estimate`: Difference in proportions (or log-odds depending on scale, usually response scale here)
- `conf_low`, `conf_high`: Confidence intervals
- `p_value`: P-value
- `adjust`: Adjustment method used

**Example:**
```r
compare_proportions_kk_glm(
  data = my_data,
  group = treatment,
  x = events,
  n = total,
  adjust = "bonferroni"
)
```

---

### `compare_proportions(data, conf.level = 0.95, method = "holm")`

Performs simple pairwise comparisons of proportions using the normal approximation (z-test).

**Arguments:**
- `data`: Data frame containing `proportion` and `trials` columns.
- `conf.level`: Confidence level (default: 0.95).
- `method`: P-value adjustment method (default: "holm").

**Returns:**
A tibble with pairwise comparisons, including Z-scores and adjusted p-values.

---

### `compare_proportions_by(data, conf.level = 0.95, method = "holm")`

Compares proportions *within* groups (stratified analysis). Useful when you want to compare subgroups within a larger category.

**Arguments:**
- `data`: Data frame with `proportion` and `trials`.
- `conf.level`: Confidence level (default: 0.95).
- `method`: Adjustment method.

**Returns:**
A tibble with comparisons performed within each level of the grouping variable.

---

## Power Analysis (`R/power_analysis.R`)

### `power_proportions(n = NULL, p1 = NULL, p2 = NULL, power = NULL, sig.level = 0.05, alternative = "two.sided")`

Calculates power or sample size for two-sample proportion tests. Exactly one of `n`, `p1`, `p2`, `power`, or `sig.level` must be NULL.

**Arguments:**
- `n`: Number of observations (per group).
- `p1`: Probability in group 1.
- `p2`: Probability in group 2.
- `power`: Power of test (1 minus Type II error probability).
- `sig.level`: Significance level (Type I error probability).
- `alternative`: One of "two.sided" (default) or "one.sided".

**Returns:**
Object of class "power.htest".

**Example:**
```r
# Calculate sample size needed for 80% power
power_proportions(p1 = 0.5, p2 = 0.7, power = 0.8)
```

---

## Plotting (`R/plotting.R`)

### `plot_proportion_comparisons(results, title = "Proportion Comparisons", subtitle = NULL, xlab = "Difference in Proportions")`

Visualizes the output of `compare_proportions` or `compare_proportions_kk_glm` using a forest plot.

**Arguments:**
- `results`: Output tibble from comparison functions.
- `title`: Plot title.
- `subtitle`: Plot subtitle.
- `xlab`: Label for x-axis.

**Returns:**
A ggplot object.

**Example:**
```r
res <- compare_proportions(my_data)
plot_proportion_comparisons(res)
```

---

## Epidemiology Statistics (`R/epi_stats.R`)

### `risk_ratio(data, exposure = NULL, outcome = NULL, conf.level = 0.95)`

Calculates Relative Risk (RR) with confidence intervals.

**Arguments:**
- `data`: Data frame or 2x2 table.
- `exposure`: Exposure variable (column name).
- `outcome`: Outcome variable (column name).
- `conf.level`: Confidence level (default: 0.95).

**Returns:**
Tibble with RR estimate and CI.

### `odds_ratio(data, exposure = NULL, outcome = NULL, conf.level = 0.95)`

Calculates Odds Ratio (OR) with confidence intervals.

**Arguments:**
- `data`: Data frame or 2x2 table.
- `exposure`: Exposure variable (column name).
- `outcome`: Outcome variable (column name).
- `conf.level`: Confidence level (default: 0.95).

**Returns:**
Tibble with OR estimate and CI.

---

## Trend Tests (`R/proportions.R`)

### `prop_trend_test(data, x, n = NULL, group = NULL)`

Performs the Cochran-Armitage test for trend in proportions.

**Arguments:**
- `data`: Data frame.
- `x`: Variable for number of successes (or binary outcome if `n` is NULL).
- `n`: Variable for number of trials (optional if `x` is binary outcome).
- `group`: Grouping variable (ordered).

**Returns:**
Result of `stats::prop.trend.test`.

**Example:**
```r
# Summarized data
prop_trend_test(df, x = events, n = total, group = dose)

# Raw data
prop_trend_test(raw_df, x = outcome, group = dose)
```

---

## Diagnostic Statistics (`R/confusion_matrix.R`)

### `diagnostic_summary(data, truth, test, cutoff = 0.5, positive = NULL, ...)`

A wrapper around `confusion_metrics_ci` to produce a summary table of diagnostic metrics (Sensitivity, Specificity, PPV, NPV, etc.) from raw data.

**Arguments:**
- `data`: Data frame.
- `truth`: Column with true status (binary).
- `test`: Column with test result (binary or numeric).
- `cutoff`: Cutoff for numeric test results (default: 0.5).
- `positive`: Value indicating positive case (optional, auto-detected).
- `...`: Additional arguments passed to `confusion_metrics_ci`.

**Returns:**
Tibble with diagnostic metrics.

**Example:**
```r
diagnostic_summary(data, truth = disease_status, test = test_result)
```

---

## Descriptive Statistics (`R/summary.R`, `R/table1.R`)

### `kk_summary(data, col, var_name = NULL, ...)`

Calculates detailed summary statistics for a numeric variable, including robust measures (Huber M-estimator) and normality tests.

**Arguments:**
- `data`: Data frame.
- `col`: Numeric column.
- `var_name`: Optional label.

**Returns:**
Tibble with summary stats.

### `kk_table1(data, by = NULL, variables = NULL, label_list = NULL, ...)`

Creates a "Table 1" description of baseline characteristics, stratified by a group if needed.

**Arguments:**
- `data`: Data frame.
- `by`: Stratification variable.
- `variables`: List of variables to include.
- `label_list`: Named list of labels.

**Returns:**
Formatted tibble.

---

## Regression (`R/regression.R`)

### `kk_reg(data, outcome, predictors, log_outcome = FALSE, ...)`

Performs univariate and multivariate regression (linear or logistic based on outcome type).

**Arguments:**
- `data`: Data frame.
- `outcome`: Outcome variable.
- `predictors`: List of predictor variables.
- `log_outcome`: Whether to log-transform the outcome (for linear reg).

**Returns:**
List containing model objects and result tables.

---

## Time Series (`R/time_series.R`)

### `kk_time_series(data, value_col = NULL, date_col = "date", group_cols = NULL, ...)`

Analyzes time series data, including decomposition, stationarity tests, and ARIMA modeling.

**Arguments:**
- `data`: Data frame.
- `value_col`: Numeric value column.
- `date_col`: Date column.
- `group_cols`: Grouping columns.

**Returns:**
Tibble with time series analysis results.

### `kk_time_metrics(data, value_col = NULL, date_col = "date", group_cols = NULL)`

Calculates time series metrics like entropy, stability, and linearity.

**Returns:**
Tibble with metrics.

---

## Survival Analysis (`R/survival.R`)

### `kk_survival_plot(data, time, status, group = NULL, ...)`

Creates a Kaplan-Meier survival plot with risk table.

**Arguments:**
- `data`: Data frame.
- `time`: Time variable.
- `status`: Status variable (0/1).
- `group`: Grouping variable.

**Returns:**
ggplot object.

---

## Utilities (`R/utils.R`, `R/one_hot.R`, `R/egn.R`)

### `kkonehot(data, column)`
One-hot encodes a categorical column.

### `extract_age_from_egn(egn, admission_date = Sys.Date())`
Extracts age from EGN (Bulgarian personal ID).

### `set_plot_font(font = "Roboto Condensed", ...)`
Sets the default font for ggplot2.

### `kkplot(...)`
Wrapper around `ggplot()` that adds `guide_axis(cap = "both")` to axes.

### `univariate_plot(data, variable)`
Automatically creates a bar chart (categorical) or density plot (numeric).
