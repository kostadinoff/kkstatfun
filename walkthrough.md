# Walkthrough - New Functions Implementation

I have implemented the following new functions as requested:

## 1. Power Analysis for Proportions
- **Function:** `power_proportions()`
- **Location:** `R/power_analysis.R`
- **Description:** A wrapper around `stats::power.prop.test` to calculate sample size or power for proportion differences.

## 2. Forest Plot for Proportion Comparisons
- **Function:** `plot_proportion_comparisons()`
- **Location:** `R/plotting.R`
- **Description:** Visualizes the output of `compare_proportions` or `compare_proportions_kk_glm` using a forest plot.

## 3. Risk Ratio and Odds Ratio
- **Functions:** `risk_ratio()`, `odds_ratio()`
- **Location:** `R/epi_stats.R`
- **Description:** Dedicated functions to calculate Relative Risk (RR) and Odds Ratio (OR) with confidence intervals, returning a tidy tibble.

## 4. Trend Test for Proportions
- **Function:** `prop_trend_test()`
- **Location:** `R/proportions.R`
- **Description:** Performs the Cochran-Armitage test for trend in proportions. Handles both summarized data (x, n) and raw data (binary outcome).

## 5. Diagnostic Test Accuracy Summary
- **Function:** `diagnostic_summary()`
- **Location:** `R/confusion_matrix.R`
- **Description:** A user-friendly wrapper around `confusion_metrics_ci` that takes raw data columns (Truth, Test) and produces a comprehensive summary table.

## Verification
All functions have been verified with a test script.
- `power_proportions` correctly calculates power/sample size.
- `risk_ratio` and `odds_ratio` return correct metrics.
- `prop_trend_test` handles NSE correctly and runs the trend test.
- `diagnostic_summary` correctly computes metrics from raw data.
- `plot_proportion_comparisons` generates a ggplot object.

## 6. Plotting Consistency
- Enforced usage of `kkplot()` wrapper in `plot_proportion_comparisons()` (`R/plotting.R`) and `kk_risk_plot()` (`R/epi_stats.R`) to ensure consistent axis capping.
- Cleaned up duplicated functions in `R/epi_stats.R`.
