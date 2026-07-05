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

---

## Health Economics & HTA (`R/health_economics.R`)

Tools for the economic evaluation side of health technology assessment.

### `kk_icer(data, cost, effect, strategy = NULL)`
Incremental cost-effectiveness analysis over mutually exclusive strategies. Ranks by effectiveness, flags strongly **dominated** and extendedly (**ext.dominated**) options, and reports the ICER along the efficiency **frontier**.

**Arguments:**
- `data`: Data frame (one row per strategy) or a numeric vector of costs.
- `cost`, `effect`: Columns of total cost and total effect (e.g. QALYs). When `data` is numeric, `cost` is the effects vector.
- `strategy`: Optional column of strategy labels.

**Returns:** Tibble ordered by cost with `inc_cost`, `inc_effect`, `icer`, and a `status` column (`frontier` / `dominated` / `ext.dominated`).

### `kk_nmb(data, cost, effect, wtp = 50000, strategy = NULL)`
Net monetary benefit (`effect * wtp - cost`) and net health benefit (`effect - cost / wtp`) at one or more willingness-to-pay thresholds, flagging the optimal strategy per threshold. Returns a strategies × thresholds tibble with `nmb`, `nhb`, `optimal`.

### `kk_ceac(data, sim, strategy, cost, effect, wtp)`
Cost-effectiveness acceptability curve from probabilistic sensitivity analysis draws (long format). For each threshold, returns the probability each strategy has the highest NMB (`prob_ce`) and the acceptability-frontier flag (`on_frontier`).

### `kk_markov(transition, costs, utilities, cycles, init = NULL, disc_cost = 0.03, disc_effect = 0.03, cycle_length = 1, half_cycle = TRUE)`
Markov cohort model for cost-utility analysis. A closed cohort moves through health states under a transition matrix (or a `state × state × cycle` array for time-varying transitions); per-cycle costs and utilities are accumulated, discounted, and half-cycle corrected. Returns a list with `trace` (cohort distribution by cycle) and `summary` (total discounted/undiscounted cost and QALYs).

### `kk_discount(x, rate = 0.03, times = NULL)`
Discounts a stream of costs or effects to present value (`PV = sum(x_t / (1 + rate)^t)`). Returns a one-row tibble with `undiscounted`, `present_value`, `rate`.

---

## Infectious Disease Transmission Modeling (`R/infectious_models.R`)

Deterministic compartmental epidemic models with a dependency-free Runge-Kutta solver, plus reproduction-number and final-size tools.

### `kk_seir(beta, gamma, sigma = NULL, S0, I0, R0_init = 0, E0 = 0, times = 0:180, mu = 0, nu = 0)`
Simulates an **SIR** model, or an **SEIR** model when a latent rate `sigma` is supplied. Optional vital dynamics (`mu`) and vaccination (`nu`). Returns a time-series tibble of compartments with `incidence` and cumulative incidence `C`; the implied `R0` is attached as an attribute (`attr(out, "R0")`).

### `kk_r0(method = c("params", "growth", "final_size"), beta, gamma, r, sigma, attack_rate)`
Estimates the basic reproduction number from transmission parameters (`beta / gamma`), from the early exponential growth rate (`1 + r / gamma`, extended to SEIR with `sigma`), or from the final attack rate. Returns a one-row tibble with `R0` and `method`.

### `kk_final_size(R0)`
Solves `z = 1 - exp(-R0 * z)` for the final proportion infected in a closed, fully susceptible population, and reports the herd-immunity threshold `1 - 1/R0`. Returns a one-row tibble with `R0`, `attack_rate`, `herd_immunity`.

---

## Advanced Methodological Suite (`R/butler_ks.R`, `R/tmle.R`, `R/simex.R`, `R/gsf.R`, `R/nb_scan.R`)

Computationally intensive methods beyond standard parametric modelling. All are permutation- or simulation-based and take a `seed` argument for reproducibility.

### `kk_butler_ks(x, outcome = NULL, treatment = NULL, strata = NULL, n_sim = 9999, seed = NULL)`
Butler's symmetry-based Kolmogorov-Smirnov test — a Fisherian randomization test of the sharp null of no treatment effect in matched-pair or stratified randomized experiments. Pass `x` as a numeric vector of within-pair differences (sign-flip null) or as a data frame with `outcome`/`treatment`/`strata` (within-stratum permutation null). Robust to asymmetric and heavy-tailed effect distributions. Returns a one-row tibble with statistic `D`, Monte-Carlo `p.value`, `method`, `n`, `n_sim`; the null distribution is attached as attribute `null_distribution`.

### `kk_tmle(data, outcome, treatment, covariates, g_bounds = c(0.025, 0.975), conf.level = 0.95)`
Targeted maximum likelihood estimation of the average treatment effect (risk difference for a binary outcome, mean difference for continuous) for a single-time-point binary exposure. Double-robust: consistent if either the outcome (`Q`) model or the propensity (`g`) model is correct, with efficient-influence-curve inference. Returns a one-row tibble with `ate`, `std.error`, CI, `p.value`, and counterfactual means `ey1`, `ey0`; the naive G-computation estimate is attached as attribute `gcomp`. For Super-Learner nuisance models or longitudinal/survival data use the `tmle` / `ltmle` packages.

### `kk_simex(model, variable, error_sd, lambda = c(0.5, 1, 1.5, 2), B = 100, degree = 2, seed = NULL)`
Simulation extrapolation (SIMEX) correction for classical additive measurement error in a continuous covariate of a fitted `lm`/`glm`. Simulates increasing error, refits, and extrapolates the coefficient trend back to zero error. Returns a tibble with the `naive` and bias-corrected `simex` estimate per coefficient, its SIMEX standard error, and a 95% CI.

### `kk_gsf(model_fn, theta, times, sigma = 1, delta = 1e-4)`
Generalized sensitivity functions (Thomaseth-Cobelli) for a mechanistic model fit by least squares (e.g. an SEIR model fit to surveillance data). Quantifies how much each estimated parameter depends on the data at each time point, identifying the most informative surveillance windows. `model_fn(theta)` returns the model output at `times`. Returns a tibble with a `time` column and one cumulative-GSF column per parameter (each rising 0→1); the Fisher information matrix is attached as attribute `fisher_information`.

### `kk_nb_scan(data, region, time, count, expected, coords = c("x","y"), type = c("elevated","trend"), max_radius = Inf, max_temporal = Inf, size = NULL, n_sim = 999, seed = NULL)`
Negative-binomial space-time scan statistic (Tango-Takahashi style) for detecting localized disease clusters under overdispersion. Searches candidate space-time cylinders for a constant excess (`type = "elevated"`) or a gradually rising trend (`type = "trend"`), standardizing by the negative-binomial variance (dispersion `size` estimated via `MASS::glm.nb` if not supplied). Significance is a Monte-Carlo p-value correcting for the multiplicity of overlapping windows. Returns a one-row tibble describing the most likely cluster (score, `p.value`, centre, member regions, temporal window, observed/expected counts); the null maxima are attached as attribute `null_distribution`.
