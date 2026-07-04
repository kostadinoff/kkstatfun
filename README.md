# kkstatfun

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18936020.svg)](https://doi.org/10.5281/zenodo.18936020)

**R Statistical Analysis Toolkit for Medical and Epidemiology**

A comprehensive R package designed for medical statistics, epidemiology, and general clinical data analysis. It provides a suite of tidyverse-friendly tools with full support for piping (`%>%` / `|>`) and grouping (`group_by()`).

---

## Installation

You can install the package directly from GitHub:

```r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install kkstatfun from GitHub
devtools::install_github("kostadinoff/kkstatfun")
```

---

## Comprehensive Function Reference (with Epi & Clinical Examples)

### 1. Epidemiology & Measures of Association

#### `kk_twobytwo(data, exposure, outcome)`

Computes classic measures of association from $2 \times 2$ contingency tables: Odds Ratio (OR), Relative Risk (RR), Risk Difference (RD), Attributable Fraction (AF), Population Attributable Fraction (PAF), and Preventable Fraction (PF) with 95% confidence intervals.

*   **Epidemiological Example**: Assessing the risk of developing lung cancer among smokers vs. non-smokers.

```r
library(kkstatfun)
df <- data.frame(
  smoking = c(rep(1, 80), rep(0, 120)),
  lung_cancer = c(rep(1, 30), rep(0, 50), rep(1, 10), rep(0, 110))
)
df %>% kk_twobytwo(smoking, lung_cancer)
```

#### `kk_stratified_2x2(data, exposure, outcome, stratum)`

Performs stratified analysis on $2 \times 2$ tables, calculating stratum-specific ORs, the Cochran-Mantel-Haenszel (CMH) pooled Odds Ratio, and the Breslow-Day test for homogeneity of odds ratios to detect confounding and effect modification.

*   **Epidemiological Example**: Studying the association between coffee consumption and coronary heart disease, stratified by smoking status to control for confounding.
```r
df_strat <- data.frame(
  coffee = c(rep(1, 50), rep(0, 50), rep(1, 40), rep(0, 60)),
  chd = c(rep(1, 15), rep(0, 35), rep(1, 5), rep(0, 45), rep(1, 10), rep(0, 30), rep(1, 12), rep(0, 48)),
  smoking = c(rep("Smoker", 100), rep("Non-Smoker", 100))
)
df_strat %>% kk_stratified_2x2(coffee, chd, smoking)
```

#### `kk_reri(data, exp1, exp2, outcome)`

Calculates the Relative Excess Risk due to Interaction (RERI), Attributable Proportion due to Interaction (AP), and Synergy Index (S) to evaluate additive interactions.

*   **Epidemiological Example**: Assessing if the combined effect of asbestos exposure and smoking on lung cancer is greater than the sum of their individual effects.
```r
df_int <- data.frame(
  asbestos = rep(c(1, 0), each = 100),
  smoking = rep(c(1, 0, 1, 0), each = 50),
  cancer = rbinom(200, 1, 0.3)
)
df_int %>% kk_reri(asbestos, smoking, cancer)
```

#### `kk_trend_test(data, exposure_level, outcome)` / `prop_trend_test()`
Performs the Cochran-Armitage test for trend in proportions to evaluate linear trends in binomial proportions across ordinal levels.
*   **Epidemiological Example**: Evaluating if the prevalence of cardiovascular disease increases with increasing levels of daily alcohol intake (None, Low, Moderate, High).
```r
df_trend <- data.frame(
  alcohol = factor(rep(c("None", "Low", "Med", "High"), each = 50), levels = c("None", "Low", "Med", "High")),
  cvd = rbinom(200, 1, c(0.1, 0.15, 0.25, 0.40)[rep(1:4, each = 50)])
)
df_trend %>% kk_trend_test(alcohol, cvd)
```

#### `kk_nnt(data, exposure, outcome)`
Calculates the Number Needed to Treat (NNT) or Number Needed to Harm (NNH) alongside the Absolute Risk Reduction (ARR) and Relative Risk Reduction (RRR).
*   **Clinical Example**: Calculating the number of patients required to be treated with a new statin to prevent one primary cardiovascular event.
```r
df_treatment <- data.frame(
  statin = c(rep(1, 500), rep(0, 500)),
  event = c(rep(1, 15), rep(0, 485), rep(1, 35), rep(0, 465))
)
df_treatment %>% kk_nnt(statin, event)
```

#### `kk_sensitivity_analysis(or_observed, p_bias)`
Computes the sensitivity of an observed Odds Ratio or Relative Risk against unmeasured confounding, assessing the strength of a potential confounder required to explain away the finding.
```r
# Observed OR of 2.5 with 20% estimated confounding bias prevalence
kk_sensitivity_analysis(or_observed = 2.5, p_bias = 0.20)
```

---

### 2. Diagnostic Tests, Agreement & Rater Studies

#### `kk_diagnostic(data, truth, prediction)` / `confusion_metrics_ci()`
Computes key diagnostic test performance parameters: Sensitivity, Specificity, Positive/Negative Predictive Value (PPV/NPV), and Likelihood Ratios (LR+/LR-) with Wilson score confidence intervals.
*   **Clinical Example**: Evaluating the diagnostic accuracy of a rapid PCR test against the gold standard viral culture.
```r
df_test <- data.frame(
  gold_standard = c(rep(1, 100), rep(0, 900)),
  pcr_test = c(rep(1, 95), rep(0, 5), rep(1, 20), rep(0, 880))
)
df_test %>% kk_diagnostic(gold_standard, pcr_test)
```

#### `kk_kappa(data, rater1, rater2)`
Computes Cohen's Kappa coefficient ($\kappa$), standard error, Z-statistic, and confidence intervals to evaluate inter-rater agreement for categorical classifications.
*   **Clinical Example**: Assessing diagnostic agreement between two independent neurologists classifying MRI scans as "Normal", "Inconclusive", or "Pathological".
```r
df_agree <- data.frame(
  neuro1 = c(rep("Normal", 50), rep("Abnormal", 50)),
  neuro2 = c(rep("Normal", 45), rep("Abnormal", 5), rep("Normal", 8), rep("Abnormal", 42))
)
df_agree %>% kk_kappa(neuro1, neuro2)
```

#### `kk_bland_altman(data, method1, method2)`
Computes limits of agreement and constructs Bland-Altman plots to evaluate the clinical agreement between two continuous measurement methods.
*   **Clinical Example**: Assessing agreement between systolic blood pressure measurements from an arterial line vs. an oscillometric arm cuff.
```r
df_bp <- data.frame(
  arterial = rnorm(50, 120, 15),
  cuff = rnorm(50, 118, 14)
)
df_bp %>% kk_bland_altman(arterial, cuff)
```

#### `kk_mcnemar(data, test1, test2)`
Performs McNemar's test for paired categorical data (e.g. matched case-control designs or two diagnostic tests on the same subjects).
*   **Epidemiological Example**: Comparing the screening outcomes of Mammography and Ultrasound performed on the same cohort of patients.
```r
df_paired <- data.frame(
  mammography = c(rep(1, 80), rep(0, 120)),
  ultrasound = c(rep(1, 60), rep(0, 20), rep(1, 15), rep(0, 105))
)
df_paired %>% kk_mcnemar(mammography, ultrasound)
```

---

### 3. Non-Parametric Tests & Baseline Group Comparisons

#### `kk_table1(data, by, variables)` / `kk_compare_groups_table()`
Builds a standard, publication-ready "Table 1" summarizing baseline demographics and clinical variables across groups, automatically performing parametric or non-parametric tests depending on variable characteristics.
*   **Clinical Example**: Summarizing age, gender, blood pressure, and smoking status at baseline stratified by placebo vs. active treatment arm.
```r
kk_table1(mtcars, by = "am", variables = c("mpg", "hp", "wt"))
```

#### `kk_median_test(data, x, group)`
Performs the Median Test for $k$ Independent Samples, comparing proportions above vs. below/equal to the composite median.
*   **Clinical Example**: Comparing the median hospital stay duration (continuous days) across three different surgical wards when stay durations are highly non-normally distributed.
```r
df_stay <- data.frame(
  days = c(3, 4, 2, 10, 15, 8, 4, 3, 1, 9, 12, 14, 5, 2, 7),
  ward = rep(c("Ward A", "Ward B", "Ward C"), each = 5)
)
df_stay %>% kk_median_test(days, ward)
```

#### `kk_vdw_test(data, x, group)`
Performs the van der Waerden Normal-Scores Test as a highly powerful non-parametric alternative to ANOVA, converting ranks to normal distribution quantiles.
*   **Clinical Example**: Evaluating clinical cognitive scores under three independent noise environments when data shape assumptions are violated.
```r
df_noise <- data.frame(
  score = c(8, 10, 9, 10, 9, 7, 8, 5, 8, 5, 4, 8, 7, 5, 7),
  noise = rep(c("Quiet", "Classical", "Rock"), each = 5)
)
df_noise %>% kk_vdw_test(score, noise)
```

---

### 4. Sequence Randomness & Serial Independence

#### `kk_runs_test(data, x, method)` / `kk_random_seq()`
Wald-Wolfowitz runs test for categorical variables, or Wallis-Moore up-down runs test for quantitative serial randomness.
*   **Epidemiological Example**: Evaluating if successive healthcare-associated infection outbreaks in an ICU follow a random temporal sequence.
```r
infection_seq <- c("N", "N", "Y", "N", "Y", "Y", "N", "N", "Y", "N")
kk_runs_test(infection_seq)
```

#### `kk_frequency_test(data, x)`
Chi-Square Goodness-of-Fit or Binomial test checking if categories occur with equal probability.
*   **Epidemiological Example**: Verifying if congenital abnormality admissions occur uniformly across days of the week.
```r
admission_days <- c(1, 3, 2, 7, 5, 2, 4, 3, 1, 6, 7, 2, 5, 3)
kk_frequency_test(admission_days)
```

#### `kk_mssd_test(data, x)`
Von Neumann Mean Square Successive Difference (MSSD) test for serial correlation on continuous quantitative data.
*   **Clinical Example**: Checking if continuous blood pressure readings over time represent independent random fluctuations or exhibit serial correlation.
```r
bp_ticks <- c(120, 122, 121, 125, 124, 122, 120, 118, 119, 122)
kk_mssd_test(bp_ticks)
```

---

### 5. Regression Modeling & Proportions Comparisons

#### `kk_reg(data, outcome, predictors)` / `regression_analysis()`
A unified modeling wrapper that automatically detects binomial outcomes (triggering Logistic Regression with odds ratios and ROC curve diagnostics) or continuous outcomes (triggering Linear Regression with check plots).
*   **Epidemiological Example**: Modeling the risk of hypertension based on age, BMI, and family history.
```r
regression_analysis(mtcars, outcome = "am", predictors = c("mpg", "wt"))
```

#### `compare_proportions(data)` / `compare_proportions_by()`
Pairwise comparison of proportions utilizing normal approximations with multiple comparison adjustments (e.g. Holm/Bonferroni).
```r
df_prop <- data.frame(
  proportion = c(0.3, 0.5, 0.25),
  trials = c(100, 120, 90),
  clinic = c("A", "B", "C")
)
compare_proportions(df_prop)
```

#### `kk_compare_independent_correlations(r, n)`
Fisher Z-transformation tests evaluating differences between correlation coefficients obtained from independent samples.
*   **Epidemiological Example**: Comparing the correlation of daily dietary sodium intake and systolic blood pressure in males vs. females.
```r
# Male: r = 0.65 (n=50), Female: r = 0.40 (n=60)
kk_compare_independent_correlations(c(0.65, 0.40), c(50, 60))
```

#### `kk_compare_dependent_correlations(rxz, ryz, rxy, n)`
Steiger's t-test comparing two dependent correlations sharing a common criterion variable within the same sample.
*   **Epidemiological Example**: Evaluating if daily sugar intake ($X$) is a significantly stronger predictor of dental cavities ($Z$) than daily sodium intake ($Y$) in the same cohort.
```r
# BP vs Sugar (rxz) = 0.72, BP vs Salt (ryz) = 0.35, Sugar vs Salt (rxy) = 0.28, n = 50
kk_compare_dependent_correlations(0.72, 0.35, 0.28, 50)
```

---

### 6. Survival Analysis & Time-to-Event

#### `kk_survival_plot(data, time, status, group)` / `survival_plot()`
Kaplan-Meier survival curves with publication-quality formatting, confidence bands, and risk tables.
*   **Clinical Example**: Plotting time-to-death of advanced-stage lung cancer patients stratified by chemotherapy regimen.
```r
library(survival)
kk_survival_plot(lung, time, status, sex)
```

---

### 7. Demographics & EGN Utilities (Bulgarian Registry)

#### `extract_egn_info(egn_vector)` / `extract_age_from_egn()`
Parses, validates, and extracts demographic profiles (Date of Birth, Gender, Age, and Birth Region) from Bulgarian Personal Identification Numbers (EGN).
*   **Epidemiological Example**: Cleaning and automatically extracting birth dates, gender, and geographic cohorts from Bulgarian electronic medical records.
```r
egn_sample <- c("9201014321", "8812128765")
extract_egn_info(egn_sample)
```

---

## License

MIT

## Citation

If you use this package in your research, please cite it as follows:

```bibtex
@Software{kkstatfun,
  title = {kkstatfun: R Statistical Analysis Toolkit for Medical statistics and Epidemiology},
  author = {Kostadinov, Kostadin},
  year = {2026},
  version = {0.1.14},
  url = {https://github.com/kostadinoff/kkstatfun},
  doi = {10.5281/zenodo.18936020},
}
```

## Credits

This package was architected, refactored, and enhanced by **Antigravity**, an advanced AI coding assistant developed by **Google DeepMind**. 
