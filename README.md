<!-- README.md is generated from README.Rmd. Please edit that file -->



# kkstatfun

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18936020.svg)](https://doi.org/10.5281/zenodo.18936020)

**R Statistical Analysis Toolkit for Medical and Epidemiology**

A comprehensive R package designed for medical statistics, epidemiology, and general clinical data analysis. It provides a suite of tidyverse-friendly tools with full support for piping (`%>%` / `|>`) and grouping (`group_by()`).

---

## Installation

You can install the package directly from GitHub:


``` r
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


``` r
library(kkstatfun)
df <- data.frame(
  smoking = c(rep(1, 80), rep(0, 120)),
  lung_cancer = c(rep(1, 30), rep(0, 50), rep(1, 10), rep(0, 110))
)
df %>% kk_twobytwo(smoking, lung_cancer)
#> # A tibble: 19 × 7
#>    Metric                           Estimate   Lower   Upper      P_Value Test  Conf_Level
#>    <chr>                               <dbl>   <dbl>   <dbl>        <dbl> <chr>      <dbl>
#>  1 Odds Ratio                         6.6      3.00   14.5    0.00000112  Fish…       0.95
#>  2 Relative Risk                      4.5      2.33    8.68   0.000000438 Chi-…       0.95
#>  3 Risk Difference                    0.292    0.175   0.409  0.000000438 Chi-…       0.95
#>  4 Risk in Exposed                    0.375   NA      NA     NA           <NA>        0.95
#>  5 Risk in Unexposed                  0.0833  NA      NA     NA           <NA>        0.95
#>  6 Population Risk                    0.2     NA      NA     NA           <NA>        0.95
#>  7 Exposure Prevalence                0.4     NA      NA     NA           <NA>        0.95
#>  8 Outcome Prevalence                 0.2     NA      NA     NA           <NA>        0.95
#>  9 Phi Coefficient                    0.357   NA      NA     NA           <NA>        0.95
#> 10 Yule's Q                           0.737    0.499   0.871  0.00000112  Fish…       0.95
#> 11 Yule's Y                           0.440    0.268   0.584  0.00000112  Fish…       0.95
#> 12 Cohen's h                          0.732   NA      NA     NA           <NA>        0.95
#> 13 Attributable Fraction (Exposed)    0.778    0.571   0.885  0.000000438 Chi-…       0.95
#> 14 Population Attributable Fraction   0.583    0.460   0.707  0.000000438 Chi-…       0.95
#> 15 Population Attributable Risk       0.117   NA      NA      0.000000438 Chi-…       0.95
#> 16 Preventable Fraction (Exposed)    NA       NA      NA     NA           <NA>        0.95
#> 17 Prevented Fraction in Population  NA       NA      NA     NA           <NA>        0.95
#> 18 Excess cases per 1000 exposed    292.     175.    409.     0.000000438 Chi-…       0.95
#> 19 NNH                                3.43     2.45    5.73   0.000000438 Chi-…       0.95
```

> **Interpretation.** Smokers had 4.5× the risk of lung cancer (RR 4.5, 95% CI 2.3–8.7) and 6.6× the odds (OR 6.6). Because the outcome is common (20% overall), the OR overstates the RR — report the RR here. The risk difference of 0.29 means ~29 extra cases per 100 smokers, i.e. one extra case for every ~3.4 smokers (NNH 3.4). 78% of cancers *among smokers* are attributable to smoking (AF), and 58% of *all* cancers in the population would be prevented if smoking were removed (PAF). Every CI excludes the null, so the association is significant.

#### `kk_stratified_2x2(data, exposure, outcome, stratum)`

Performs stratified analysis on $2 \times 2$ tables, calculating stratum-specific ORs, the Cochran-Mantel-Haenszel (CMH) pooled Odds Ratio, and the Breslow-Day test for homogeneity of odds ratios to detect confounding and effect modification.

*   **Epidemiological Example**: Studying the association between coffee consumption and coronary heart disease, stratified by smoking status to control for confounding.

``` r
df_strat <- data.frame(
  coffee = c(rep(1, 50), rep(0, 50), rep(1, 40), rep(0, 60)),
  chd = c(rep(1, 15), rep(0, 35), rep(1, 5), rep(0, 45), rep(1, 10), rep(0, 30), rep(1, 12), rep(0, 48)),
  smoking = c(rep("Smoker", 100), rep("Non-Smoker", 100))
)
df_strat %>% kk_stratified_2x2(coffee, chd, smoking)
#> $pooled_or
#> # A tibble: 1 × 4
#>   Estimate Lower Upper Method         
#>      <dbl> <dbl> <dbl> <chr>          
#> 1     2.16  1.07  4.35 Mantel-Haenszel
#> 
#> $pooled_rr
#> # A tibble: 1 × 4
#>   Estimate Lower Upper Method         
#>      <dbl> <dbl> <dbl> <chr>          
#> 1     1.85  1.05  3.25 Mantel-Haenszel
#> 
#> $homogeneity_test
#> # A tibble: 1 × 4
#>   Test                       Statistic    DF P_Value
#>   <chr>                          <dbl> <dbl>   <dbl>
#> 1 Mantel-Haenszel Chi-square      3.96     1  0.0465
#> 
#> $breslow_day
#> # A tibble: 1 × 5
#>   Test        Statistic    DF P_Value Interpretation        
#>   <chr>           <dbl> <dbl>   <dbl> <chr>                 
#> 1 Breslow-Day      2.97     1  0.0850 ORs appear homogeneous
#> 
#> $stratum_specific
#> # A tibble: 2 × 7
#>   Stratum       OR    RR Exposed_Cases Exposed_Total Unexposed_Cases Unexposed_Total
#>   <chr>      <dbl> <dbl>         <dbl>         <dbl>           <dbl>           <dbl>
#> 1 Smoker      3.86  3               15            50               5              50
#> 2 Non-Smoker  1.33  1.25            10            40              12              60
```

> **Interpretation.** Pooling across smoking strata, coffee is associated with 2.16× the odds of CHD (Mantel-Haenszel OR 2.16, 95% CI 1.07–4.35). The Breslow-Day test is non-significant (p = 0.085), so the odds ratios are reasonably *homogeneous* across strata — no strong evidence of effect modification, and the single pooled estimate is a fair summary. The stratum-specific ORs (3.86 in smokers vs 1.33 in non-smokers) hint at a stronger effect among smokers, but the sample is too small to confirm interaction.

#### `kk_reri(data, exp1, exp2, outcome)`

Calculates the Relative Excess Risk due to Interaction (RERI), Attributable Proportion due to Interaction (AP), and Synergy Index (S) to evaluate additive interactions.

*   **Epidemiological Example**: Assessing if the combined effect of asbestos exposure and smoking on lung cancer is greater than the sum of their individual effects.

``` r
df_int <- data.frame(
  asbestos = rep(c(1, 0), each = 100),
  smoking = rep(c(1, 0, 1, 0), each = 50),
  cancer = rbinom(200, 1, 0.3)
)
df_int %>% kk_reri(asbestos, smoking, cancer)
#> # A tibble: 3 × 6
#>   Metric Estimate    Lower Upper    SE Conf_Level
#>   <chr>     <dbl>    <dbl> <dbl> <dbl>      <dbl>
#> 1 RERI      0.439 -0.417    1.29 0.437       0.95
#> 2 AP        0.524 -0.524    1.57 0.535       0.95
#> 3 S         0.271  0.00746  9.87 0.498       0.95
```

> **Interpretation.** RERI is the excess risk on the *additive* scale beyond the sum of the two separate exposure effects. Here RERI = 0.44 but its 95% CI (−0.42 to 1.29) includes 0, so there is no evidence of additive interaction — the joint effect is consistent with independent effects. (Simulated data, so the point estimate will differ from a real study.)

#### `kk_trend_test(data, exposure_level, outcome)` / `prop_trend_test()`

Performs the Cochran-Armitage test for trend in proportions to evaluate linear trends in binomial proportions across ordinal levels.

*   **Epidemiological Example**: Evaluating if the prevalence of cardiovascular disease increases with increasing levels of daily alcohol intake (None, Low, Moderate, High).

``` r
df_trend <- data.frame(
  alcohol = factor(rep(c("None", "Low", "Med", "High"), each = 50), levels = c("None", "Low", "Med", "High")),
  cvd = rbinom(200, 1, c(0.1, 0.15, 0.25, 0.40)[rep(1:4, each = 50)])
)
df_trend %>% kk_trend_test(alcohol, cvd)
#> # A tibble: 2 × 8
#>   Test                   Statistic Lower Upper P_Value Conf_Level N_Groups Total_N
#>   <chr>                      <dbl> <dbl> <dbl>   <dbl>      <dbl>    <int>   <int>
#> 1 Cochran-Armitage Trend      3.00 NA    NA    0.00268       0.95        4     200
#> 2 Trend OR (per unit)         1.62  1.17  2.24 0.00337       0.95        4     200
```

> **Interpretation.** Both rows agree that risk rises across the ordered alcohol scale (None → Low → Med → High). The *Cochran-Armitage* row is the formal linear-trend test on the proportions, and the *Trend OR (per unit)* gives the effect size: the odds of CVD rise ~1.6× per step up the scale, with the CI excluding 1. A significant trend with a CI excluding 1 is the key evidence of a monotonic dose-response.

#### `kk_nnt(data, exposure, outcome)`

Calculates the Number Needed to Treat (NNT) or Number Needed to Harm (NNH) alongside the Absolute Risk Reduction (ARR) and Relative Risk Reduction (RRR).

*   **Clinical Example**: Calculating the number of patients required to be treated with a new statin to prevent one primary cardiovascular event.

``` r
df_treatment <- data.frame(
  statin = c(rep(1, 500), rep(0, 500)),
  event = c(rep(1, 15), rep(0, 485), rep(1, 35), rep(0, 465))
)
df_treatment %>% kk_nnt(statin, event)
#> # A tibble: 3 × 6
#>   Metric Estimate   Lower   Upper Conf_Level Note         
#>   <chr>     <dbl>   <dbl>   <dbl>      <dbl> <chr>        
#> 1 NNT      25     14.9    76.3          0.95 "Significant"
#> 2 ARR       0.04   0.0131  0.0669       0.95 ""           
#> 3 RRR       0.571  0.225   0.763        0.95 ""
```

> **Interpretation.** The statin cut the event rate from 7% to 3% — an absolute risk reduction of 4% (ARR 0.04) and a 57% relative reduction (RRR). You would need to treat 25 patients (NNT, 95% CI 15–76) to prevent one cardiovascular event; the CI excludes infinity, consistent with a significant benefit.

#### `kk_sensitivity_analysis(or_observed, p_bias)`

Computes the sensitivity of an observed Odds Ratio or Relative Risk against unmeasured confounding, assessing the strength of a potential confounder required to explain away the finding.


``` r
# Observed OR of 2.5 with 20% estimated confounding bias prevalence
kk_sensitivity_analysis(or_observed = 2.5, p_bias = 0.20)
#> # A tibble: 3 × 4
#>   Metric                                    Value Interpretation                     Note 
#>   <chr>                                     <dbl> <chr>                              <chr>
#> 1 Observed Association                        2.5 Harmful association                Base…
#> 2 Confounder Prevalence Difference (p_bias)   0.2 Assumed maximum prevalence differ… Assu…
#> 3 Required Confounder Association (RR_UD)     8.5 Low sensitivity (robust) - a very… Mini…
```

> **Interpretation.** To fully explain away the observed OR of 2.5 through unmeasured confounding, a confounder would need to be associated with the outcome by a risk ratio of at least 8.5 (given the assumed 20% prevalence difference). Such a strong hidden confounder is implausible in most settings, so the finding is relatively robust.

#### `kk_incidence_rate(data, cases, person_time, by = NULL)`

Computes incidence rates per unit of person-time with **exact (Poisson) confidence intervals**, and — when a stratifying variable is supplied — incidence **rate ratios** versus the reference level.

*   **Epidemiological Example**: Comparing the incidence rate of myocardial infarction per 1000 person-years between a high-exposure and a reference cohort.

``` r
df_ir <- data.frame(
  arm = c("exposed", "unexposed"),
  cases = c(40, 15),
  pyears = c(1000, 1200)
)
df_ir %>% kk_incidence_rate(cases, pyears, by = arm)
#> # A tibble: 2 × 13
#>   stratum   cases person_time  rate rate_low rate_high multiplier conf.level rate_ratio
#>   <chr>     <dbl>       <dbl> <dbl>    <dbl>     <dbl>      <dbl>      <dbl>      <dbl>
#> 1 exposed      40        1000  40      28.6       54.5       1000       0.95      1    
#> 2 unexposed    15        1200  12.5     7.00      20.6       1000       0.95      0.312
#> # ℹ 4 more variables: rr_low <dbl>, rr_high <dbl>, rr_p <dbl>, reference <chr>
```

> **Interpretation.** The exposed cohort had 40 events per 1000 person-years (exact 95% CI 28.6–54.5) versus 12.5 (7.0–20.6) in the unexposed. The rate ratio is computed against the first row (exposed = reference), so the unexposed value of 0.31 means the exposed had ~3.2× the event rate. The non-overlapping exact Poisson CIs indicate a real rate difference.

#### `kk_rr_reg(data, outcome, predictors)`

Estimates **adjusted risk ratios** for a common binary outcome using the modified Poisson approach (Zou 2004): a log-link Poisson model with robust sandwich standard errors. This is the preferred alternative to logistic regression when the outcome is frequent, since odds ratios overstate the risk ratio.

*   **Epidemiological Example**: Estimating the adjusted risk ratio of post-operative infection for an exposure, controlling for age, in a cohort where infection is common (>10%).

``` r
df_rr <- data.frame(
  infection = rbinom(400, 1, 0.35),
  exposure = rbinom(400, 1, 0.5),
  age = rnorm(400, 50, 10)
)
kk_rr_reg(df_rr, infection, c("exposure", "age"))
#> # A tibble: 4 × 9
#>   term     model_type risk_ratio conf.low conf.high std.error statistic p.value conf.level
#>   <chr>    <chr>           <dbl>    <dbl>     <dbl>     <dbl>     <dbl>   <dbl>      <dbl>
#> 1 exposure univariate       1.01    0.780      1.30   0.131      0.0603   0.952       0.95
#> 2 age      univariate       1.00    0.992      1.02   0.00596    0.618    0.537       0.95
#> 3 exposure multivari…       1.01    0.779      1.30   0.131      0.0483   0.961       0.95
#> 4 age      multivari…       1.00    0.992      1.02   0.00596    0.616    0.538       0.95
```

> **Interpretation.** Each row is an adjusted risk ratio; read the `multivariable` rows for the mutually-adjusted estimates. Exposure has RR ≈ 1.01 (95% CI 0.78–1.30, p ≈ 0.96) — no association, as expected since the outcome was simulated independently. The advantage over logistic regression: these are risk ratios, directly interpretable even though infection is common.

#### `kk_rate_reg(data, outcome, predictors, person_time)` / `kk_poisson()`

Fits **Poisson rate regression** returning incidence-rate ratios (IRR), with an `offset(log(person_time))` for rate outcomes. Overdispersion is detected automatically and a **negative-binomial** model is substituted when appropriate.

*   **Epidemiological Example**: Modelling the rate of hospital admissions per person-year as a function of age and sex.

``` r
df_rate <- data.frame(
  admissions = rpois(200, 3),
  pyears = runif(200, 50, 150),
  age = rnorm(200, 50, 10),
  sex = rbinom(200, 1, 0.5)
)
kk_rate_reg(df_rate, admissions, c("age", "sex"), person_time = pyears)
#> # A tibble: 4 × 12
#>   term  model_type    IRR conf.low conf.high std.error statistic p.value family dispersion
#>   <chr> <chr>       <dbl>    <dbl>     <dbl>     <dbl>     <dbl>   <dbl> <chr>       <dbl>
#> 1 age   univariate   1.00    0.992      1.01   0.00428     0.200   0.842 poiss…       1.50
#> 2 sex   univariate   1.13    0.958      1.33   0.0845      1.44    0.149 poiss…       1.48
#> 3 age   multivaria…  1.00    0.993      1.01   0.00433     0.344   0.731 poiss…       1.50
#> 4 sex   multivaria…  1.13    0.960      1.34   0.0849      1.47    0.142 poiss…       1.50
#> # ℹ 2 more variables: AIC <dbl>, conf.level <dbl>
```

> **Interpretation.** Coefficients are incidence-rate ratios (per person-year via the offset). The `dispersion` column (~1.5) is checked automatically; here it stayed below the negative-binomial threshold so a Poisson fit was kept (`family = poisson`). Neither age (IRR ≈ 1.00) nor sex (IRR ≈ 1.13, p ≈ 0.14) is significant in this simulated data.

#### `kk_smd(data, treatment, variables)` / `kk_balance_table()`

Computes **standardized mean differences (SMD)** between two groups for each covariate — the standard diagnostic for baseline balance in matched, weighted, or propensity-score cohorts. Absolute SMD > 0.1 flags meaningful imbalance.

*   **Epidemiological Example**: Checking covariate balance between treated and control arms after propensity-score matching.

``` r
df_bal <- data.frame(
  arm = rep(c("treated", "control"), each = 100),
  age = c(rnorm(100, 55, 8), rnorm(100, 50, 8)),
  smoker = rbinom(200, 1, 0.4)
)
df_bal %>% kk_smd(arm, variables = c("age", "smoker"))
#> # A tibble: 2 × 8
#>   variable level type        group1 group2    smd abs_smd imbalanced
#> * <chr>    <chr> <chr>        <dbl>  <dbl>  <dbl>   <dbl> <lgl>     
#> 1 age      <NA>  continuous   49.2   54.8  -0.683   0.683 TRUE      
#> 2 smoker   1     categorical   0.37   0.44 -0.143   0.143 TRUE
```

> **Interpretation.** SMDs measure imbalance independent of sample size. Both covariates exceed the conventional |SMD| > 0.1 threshold (age −0.68, smoker −0.14) and are flagged `imbalanced = TRUE`, so the raw treated and control groups are not comparable — weighting or matching (e.g. `kk_iptw`) is warranted before estimating an effect.

#### `kk_iptw(data, treatment, outcome, covariates)`

Estimates a **propensity score**, forms **inverse-probability-of-treatment weights** (ATE or ATT, optionally stabilized), and returns the weighted **risk difference and risk ratio** with robust CIs. Use `kk_smd` on the weighted sample to confirm the covariates are balanced.

*   **Epidemiological Example**: Estimating the causal effect of a treatment on a binary outcome from observational data, adjusting for confounding by age and sex via IPTW.

``` r
df_ip <- data.frame(
  trt = rbinom(500, 1, 0.5),
  age = rnorm(500, 50, 10),
  sex = rbinom(500, 1, 0.5)
)
df_ip$out <- rbinom(500, 1, plogis(-1 + 0.5 * df_ip$trt + 0.02 * df_ip$age))
kk_iptw(df_ip, trt, out, covariates = c("age", "sex"))
#> # A tibble: 2 × 7
#>   estimand metric          estimate conf.low conf.high p.value conf.level
#>   <chr>    <chr>              <dbl>    <dbl>     <dbl>   <dbl>      <dbl>
#> 1 ATE      Risk difference   0.0967   0.0104     0.183  0.0281       0.95
#> 2 ATE      Risk ratio        1.18     1.02       1.37   0.0289       0.95
```

> **Interpretation.** After inverse-probability weighting to balance age and sex, the *average treatment effect* is a risk difference of +0.097 (95% CI 0.010–0.183) and a risk ratio of 1.18 (1.02–1.37); both exclude the null (p ≈ 0.03), so treatment raises the outcome probability by ~10 percentage points. Confirm the weights achieved balance by re-running `kk_smd` on the weighted sample.

#### `kk_epi_stats(data, exposure, outcome)`

Computes the **Odds Ratio and Relative Risk** (with confidence intervals) from two binary variables — a lightweight alternative to `kk_twobytwo` when only OR and RR are needed.

*   **Epidemiological Example**: Estimating the OR and RR of disease among exposed vs. unexposed subjects.

``` r
df_epi <- data.frame(
  exposure = c(rep(1, 50), rep(0, 50)),
  outcome = c(rep(1, 20), rep(0, 30), rep(1, 10), rep(0, 40))
)
kk_epi_stats(df_epi, exposure, outcome)
#> # A tibble: 2 × 9
#>   Metric       Estimate Lower Upper Conf_Level Exposed_Event Exposed_Total Unexposed_Event
#>   <chr>           <dbl> <dbl> <dbl>      <dbl>         <int>         <int>           <int>
#> 1 Odds Ratio       2.67  1.09  6.52       0.95            20            50              10
#> 2 Relative Ri…     2     1.04  3.83       0.95            20            50              10
#> # ℹ 1 more variable: Unexposed_Total <int>
```

> **Interpretation.** Exposed subjects had twice the risk (RR 2.0, 95% CI 1.04–3.83) and 2.67× the odds (OR 2.67) of the outcome; both CIs exclude 1, so the association is significant. As always the OR sits further from 1 than the RR, so with a 20% exposed-group risk the RR is the more faithful effect measure.

#### `odds_ratio(data, exposure, outcome)` / `risk_ratio(data, exposure, outcome)`

Convenience wrappers that return a single measure of association (the OR or the RR, respectively) with its confidence interval and p-value.


``` r
df_or <- data.frame(exposure = c(1, 1, 0, 0, 1, 0), outcome = c(1, 0, 1, 0, 1, 0))
odds_ratio(df_or, exposure, outcome)
#> # A tibble: 1 × 6
#>   Metric     Estimate Lower Upper P_Value Conf_Level
#>   <chr>         <dbl> <dbl> <dbl>   <dbl>      <dbl>
#> 1 Odds Ratio        4 0.134  119.       1       0.95
risk_ratio(df_or, exposure, outcome)
#> # A tibble: 1 × 6
#>   Metric        Estimate Lower Upper P_Value Conf_Level
#>   <chr>            <dbl> <dbl> <dbl>   <dbl>      <dbl>
#> 1 Relative Risk        2 0.334  12.0   0.414       0.95
```

#### `kk_std_rates(data, count, pop, std_pop)`

Performs **direct age-standardization** of rates against a reference (standard) population, returning crude and standardized rates with confidence intervals.

*   **Epidemiological Example**: Age-standardizing cancer incidence to the WHO world standard population to allow comparison across regions.

``` r
df_std <- data.frame(
  age_group = c("0-19", "20-39", "40-59", "60+"),
  cases = c(10, 25, 50, 100),
  pop = c(5000, 8000, 6000, 4000)
)
std_pop <- c(4000, 7000, 6000, 3000)
kk_std_rates(df_std, cases, pop, std_pop, multiplier = 1000)
#> # A tibble: 2 × 6
#>   Type           Rate Lower Upper Multiplier Conf_Level
#>   <chr>         <dbl> <dbl> <dbl>      <dbl>      <dbl>
#> 1 Crude Rate     8.04 NA    NA          1000       0.95
#> 2 Adjusted Rate  7.74  6.62  8.87       1000       0.95
```

> **Interpretation.** The crude rate (8.04 per 1000) is what you observe; the age-standardized rate (7.74, 95% CI 6.62–8.87) is what the population *would* experience under the reference age structure. Standardizing removes age as a confounder so regions or periods can be compared fairly. The small drop from crude to adjusted shows this population is slightly older than the standard.

#### `kk_smr(data, observed, pop, ref_rate)`

Performs **indirect standardization**, comparing observed events with those expected under a reference population's stratum-specific rates to give the **Standardized Mortality/Morbidity Ratio (SMR)** with an exact Poisson CI. The counterpart to the direct `kk_std_rates` when stratum-specific rates in the study population are unstable.

*   **Epidemiological Example**: Assessing whether mortality in an occupational cohort exceeds that expected from national age-specific death rates.

``` r
df_smr <- data.frame(
  age_group = c("0-39", "40-59", "60+"),
  deaths = c(5, 20, 60),
  pyears = c(20000, 15000, 8000),
  ref_rate = c(0.0002, 0.0015, 0.008)
)
kk_smr(df_smr, deaths, pyears, ref_rate)
#> # A tibble: 1 × 9
#>   observed expected   smr smr_low smr_high std_rate multiplier p.value conf.level
#>      <dbl>    <dbl> <dbl>   <dbl>    <dbl>    <dbl>      <dbl>   <dbl>      <dbl>
#> 1       85     90.5 0.939   0.750     1.16     1.98       1000   0.599       0.95
```

> **Interpretation.** 85 deaths were observed against 90.5 expected from the reference population's age-specific rates, giving an SMR of 0.94 (95% CI 0.75–1.16). Because the CI includes 1 (p = 0.60), the cohort's mortality is not significantly different from expected — no excess risk. An SMR > 1 with a CI excluding 1 would signal excess mortality.

#### `kk_risk_plot(data, title)`

Renders a **forest plot** of risk estimates (OR / RR and CIs) from the output of `kk_twobytwo`, `kk_epi_stats`, or any tibble with `Metric`, `Estimate`, `Lower`, `Upper` columns.


``` r
res <- tibble::tibble(
  Metric = c("OR", "RR"),
  Estimate = c(1.5, 1.2),
  Lower = c(1.1, 1.0),
  Upper = c(2.0, 1.5)
)
kk_risk_plot(res)
```

<div class="figure">
<img src="man/figures/README-ex-17-1.png" alt="plot of chunk ex-17" width="75%" />
<p class="caption">plot of chunk ex-17</p>
</div>

---

### 2. Diagnostic Tests, Agreement & Rater Studies

#### `kk_diagnostic(data, truth, prediction)` / `confusion_metrics_ci()`

Computes key diagnostic test performance parameters: Sensitivity, Specificity, Positive/Negative Predictive Value (PPV/NPV), and Likelihood Ratios (LR+/LR-) with Wilson score confidence intervals.

*   **Clinical Example**: Evaluating the diagnostic accuracy of a rapid PCR test against the gold standard viral culture.


``` r
df_test <- data.frame(
  gold_standard = c(rep(1, 100), rep(0, 900)),
  pcr_test = c(rep(1, 95), rep(0, 5), rep(1, 20), rep(0, 880))
)
df_test %>% kk_diagnostic(gold_standard, pcr_test)
#> # A tibble: 7 × 2
#>   Metric      Value
#>   <chr>       <dbl>
#> 1 Sensitivity 0.95 
#> 2 Specificity 0.978
#> 3 PPV         0.826
#> 4 NPV         0.994
#> 5 Accuracy    0.975
#> 6 F1 Score    0.884
#> 7 AUC         0.964
```

> **Interpretation.** The PCR test is highly accurate: 95% sensitivity and 97.8% specificity, AUC 0.96. Note the gap between PPV (0.83) and NPV (0.99) — driven by the low 10% prevalence, a positive result is only ~83% likely to be a true case, while a negative result all but rules disease out.

#### `kk_roc(data, truth, predictor)`

Builds an ROC curve for a continuous marker, returning the **AUC with a DeLong confidence interval** and the **Youden-optimal cutoff** together with the sensitivity, specificity, PPV, and NPV achieved at that threshold.

*   **Clinical Example**: Determining the optimal biomarker cutoff and discriminative accuracy for identifying sepsis.

``` r
df_marker <- data.frame(disease = rbinom(300, 1, 0.4))
df_marker$biomarker <- df_marker$disease * 0.8 + rnorm(300)
df_marker %>% kk_roc(disease, biomarker)
#> # A tibble: 1 × 11
#>     auc auc_low auc_high youden_j optimal_threshold sensitivity specificity   ppv   npv
#>   <dbl>   <dbl>    <dbl>    <dbl>             <dbl>       <dbl>       <dbl> <dbl> <dbl>
#> 1 0.726   0.669    0.783    0.350             0.519       0.662       0.688 0.619 0.727
#> # ℹ 2 more variables: n <int>, conf.level <dbl>
```

> **Interpretation.** The marker discriminates cases moderately well (AUC 0.73, DeLong 95% CI 0.67–0.78; 0.5 is chance). The Youden-optimal cutoff of 0.52 balances sensitivity (0.66) and specificity (0.69) — use it when you need a single decision threshold rather than the whole curve.

#### `kk_compare_roc(data, truth, predictor1, predictor2)`

Compares the AUCs of two markers measured on the same subjects using **DeLong's test** for paired ROC curves.

*   **Clinical Example**: Testing whether a novel biomarker significantly improves discrimination over an established one for the same patients.

``` r
df_marker$established <- df_marker$disease * 0.3 + rnorm(300)
df_marker %>% kk_compare_roc(disease, biomarker, established)
#> # A tibble: 1 × 9
#>   marker1   marker2      auc1  auc2 auc_difference statistic p_value method     conf.level
#>   <chr>     <chr>       <dbl> <dbl>          <dbl>     <dbl>   <dbl> <chr>           <dbl>
#> 1 biomarker established 0.726 0.582          0.144      3.15 0.00165 DeLong's …       0.95
```

> **Interpretation.** The first biomarker discriminates significantly better than the established one (AUC 0.73 vs 0.58; difference 0.14, DeLong p = 0.0017). Because both markers are measured on the *same* subjects, the paired DeLong test is the correct comparison — a two-sample test would ignore their correlation and overstate uncertainty.

#### `kk_calibration(data, truth, predicted)`

Assesses **calibration** of a risk model — the **Brier score**, the **Hosmer-Lemeshow** goodness-of-fit test, and a decile table of predicted vs. observed risk for a calibration plot. The complement to `kk_roc`: discrimination tells you *if* the model separates cases, calibration tells you whether the predicted probabilities are *accurate*.

*   **Clinical Example**: Checking whether a cardiovascular risk score's predicted 10-year probabilities agree with observed event rates before deploying it.

``` r
df_cal <- data.frame(y = rbinom(500, 1, 0.3))
df_cal$p <- plogis(qlogis(0.3) + 0.8 * df_cal$y + rnorm(500))
kk_calibration(df_cal, y, p)
#> # A tibble: 10 × 5
#>      grp     n observed_events observed_rate predicted_rate
#>    <int> <int>           <dbl>         <dbl>          <dbl>
#>  1     1    50               4          0.08         0.0779
#>  2     2    50               5          0.1          0.152 
#>  3     3    50              10          0.2          0.213 
#>  4     4    50              12          0.24         0.263 
#>  5     5    50              10          0.2          0.324 
#>  6     6    50              14          0.28         0.379 
#>  7     7    50              11          0.22         0.438 
#>  8     8    50              19          0.38         0.507 
#>  9     9    50              25          0.5          0.595 
#> 10    10    50              28          0.56         0.743
```

> **Interpretation.** Each row is a risk decile: compare `observed_rate` with `predicted_rate`. Good calibration means the two track closely. Here the model over-predicts in the higher-risk deciles (decile 7: 22% observed vs 44% predicted; decile 10: 56% vs 74%), so its probabilities are too high for high-risk patients. Retrieve the Brier score and Hosmer-Lemeshow test from `attr(x, "brier")` and `attr(x, "hosmer_lemeshow")`.

#### `kk_kappa(data, rater1, rater2)`

Computes Cohen's Kappa coefficient ($\kappa$), standard error, Z-statistic, and confidence intervals to evaluate inter-rater agreement for categorical classifications.

*   **Clinical Example**: Assessing diagnostic agreement between two independent neurologists classifying MRI scans as "Normal", "Inconclusive", or "Pathological".


``` r
df_agree <- data.frame(
  neuro1 = c(rep("Normal", 50), rep("Abnormal", 50)),
  neuro2 = c(rep("Normal", 45), rep("Abnormal", 5), rep("Normal", 8), rep("Abnormal", 42))
)
df_agree %>% kk_kappa(neuro1, neuro2)
#> # A tibble: 1 × 10
#>   method        kappa std_error z_statistic  p_value conf_lower conf_upper n_obs
#>   <chr>         <dbl>     <dbl>       <dbl>    <dbl>      <dbl>      <dbl> <int>
#> 1 Cohen's Kappa  0.74    0.0673        11.0 3.74e-28      0.608      0.872   100
#> # ℹ 2 more variables: observed_agreement <dbl>, chance_agreement <dbl>
```

> **Interpretation.** Cohen's κ of 0.74 (95% CI 0.61–0.87) indicates *substantial* agreement between the two neurologists beyond chance (Landis-Koch: 0.61–0.80 = substantial). The CI excludes 0 and p is minuscule, so the agreement is well above what chance alone would produce.

#### `kk_bland_altman(data, method1, method2)`

Computes limits of agreement and constructs Bland-Altman plots to evaluate the clinical agreement between two continuous measurement methods.

*   **Clinical Example**: Assessing agreement between systolic blood pressure measurements from an arterial line vs. an oscillometric arm cuff.


``` r
df_bp <- data.frame(
  arterial = rnorm(50, 120, 15),
  cuff = rnorm(50, 118, 14)
)
df_bp %>% kk_bland_altman(arterial, cuff)
```

<div class="figure">
<img src="man/figures/README-ex-23-1.png" alt="plot of chunk ex-23" width="75%" />
<p class="caption">plot of chunk ex-23</p>
</div>

#### `kk_mcnemar(data, test1, test2)`
Performs McNemar's test for paired categorical data (e.g. matched case-control designs or two diagnostic tests on the same subjects).

*   **Epidemiological Example**: Comparing the screening outcomes of Mammography and Ultrasound performed on the same cohort of patients.


``` r
df_paired <- data.frame(
  mammography = c(rep(1, 80), rep(0, 120)),
  ultrasound = c(rep(1, 60), rep(0, 20), rep(1, 15), rep(0, 105))
)
df_paired %>% kk_mcnemar(mammography, ultrasound)
#> # A tibble: 2 × 9
#>   Metric          Estimate  Lower Upper P_Value Test  Discordant_b Discordant_c Conf_Level
#>   <chr>              <dbl>  <dbl> <dbl>   <dbl> <chr>        <int>        <int>      <dbl>
#> 1 Conditional Od…     1.33  0.683  2.60   0.500 <NA>            15           20       0.95
#> 2 McNemar's Test     NA    NA     NA      0.500 Exac…           15           20       0.95
```

> **Interpretation.** McNemar's test uses only the *discordant* pairs (15 vs 20 here). The conditional OR is 1.33 with p = 0.50, so there is no significant difference in the positivity rates of the two paired tests — mammography and ultrasound detect at similar rates in this cohort.

#### `kk_confusion_matrix(x, ...)`

Computes a **full panel of classifier metrics** with confidence intervals (sensitivity, specificity, PPV, NPV, accuracy, F1, MCC, likelihood ratios, and more) directly from TP/FP/FN/TN counts. Optional bootstrap CIs via `boot = TRUE`. Supersedes the deprecated `confusion_metrics_ci()`.

*   **Clinical Example**: Full performance characterization of a screening test with 85 true positives, 10 false positives, 15 false negatives, and 890 true negatives.

``` r
kk_confusion_matrix(c(tp = 85, fp = 10, fn = 15, tn = 890))
#> # A tibble: 21 × 7
#>   metric            estimate   lower  upper ci_level ci_method                    note 
#>   <chr>                <dbl>   <dbl>  <dbl>    <dbl> <chr>                        <chr>
#> 1 prevalence          0.1    0.0821  0.120      0.95 exact binomial               <NA> 
#> 2 accuracy            0.975  0.963   0.984      0.95 exact binomial               <NA> 
#> 3 sensitivity (TPR)   0.85   0.765   0.914      0.95 exact binomial               <NA> 
#> 4 specificity (TNR)   0.989  0.980   0.995      0.95 exact binomial               <NA> 
#> 5 PPV (precision)     0.895  0.815   0.948      0.95 exact binomial               <NA> 
#> 6 NPV                 0.983  0.973   0.991      0.95 exact binomial               <NA> 
#> 7 FPR                 0.0111 0.00534 0.0203     0.95 complement of specificity CI <NA> 
#> 8 FNR                 0.15   0.0865  0.235      0.95 complement of sensitivity CI <NA> 
#> # ℹ 13 more rows
```

> **Interpretation.** From the four cell counts the function derives 21 metrics with exact CIs. Sensitivity is 0.85 and specificity 0.99; despite the high specificity, the 10% prevalence still shapes the predictive values (PPV 0.90, NPV 0.98). Scroll the full tibble for likelihood ratios and MCC, which summarise performance in a single prevalence-independent number.

#### `diagnostic_summary(data, truth, test)`

Produces a tidy summary of diagnostic metrics from **raw patient-level data**, applying a cutoff to numeric test results automatically.


``` r
df_diag <- data.frame(truth = c(1, 0, 1, 0, 1, 0), test = c(1, 0, 0, 1, 1, 0))
diagnostic_summary(df_diag, truth, test)
#> # A tibble: 21 × 7
#>   metric            estimate   lower upper ci_level ci_method                    note 
#>   <chr>                <dbl>   <dbl> <dbl>    <dbl> <chr>                        <chr>
#> 1 prevalence           0.5   0.118   0.882     0.95 exact binomial               <NA> 
#> 2 accuracy             0.667 0.223   0.957     0.95 exact binomial               <NA> 
#> 3 sensitivity (TPR)    0.667 0.0943  0.992     0.95 exact binomial               <NA> 
#> 4 specificity (TNR)    0.667 0.0943  0.992     0.95 exact binomial               <NA> 
#> 5 PPV (precision)      0.667 0.0943  0.992     0.95 exact binomial               <NA> 
#> 6 NPV                  0.667 0.0943  0.992     0.95 exact binomial               <NA> 
#> 7 FPR                  0.333 0.00840 0.906     0.95 complement of specificity CI <NA> 
#> 8 FNR                  0.333 0.00840 0.906     0.95 complement of sensitivity CI <NA> 
#> # ℹ 13 more rows
```

#### `kk_agreement(data, rater1, rater2, weights)`

Computes **weighted Cohen's Kappa** (unweighted, linear, or quadratic weights) for ordinal rater agreement, with standard error and confidence interval.

*   **Clinical Example**: Quantifying agreement between two raters scoring tumour grade on an ordinal scale, giving partial credit for near-misses via quadratic weights.

``` r
df_rate <- data.frame(rater1 = c(1, 0, 1, 0, 1), rater2 = c(1, 1, 0, 0, 1))
kk_agreement(df_rate, rater1, rater2, weights = "quadratic")
#> # A tibble: 4 × 8
#>   Measure           Estimate  Lower Upper     SE Conf_Level     N K_Categories
#>   <chr>                <dbl>  <dbl> <dbl>  <dbl>      <dbl> <int>        <int>
#> 1 Cohen's Kappa        0.167 -0.728  1.06  0.456       0.95     5            2
#> 2 Weighted Kappa      NA     NA     NA    NA           0.95     5            2
#> 3 PABAK                0.2   -0.659  1.06  0.438       0.95     5            2
#> 4 Percent Agreement   60     NA     NA    NA           0.95     5            2
```

> **Interpretation.** With a binary rating the *weighted* κ is undefined (weights need ≥3 ordered categories), so read the unweighted κ (0.17) and PABAK (0.20). Both are low, but the tiny sample (n = 5) makes them unstable (CI −0.73 to 1.06). Quadratic weights pay off only when the scale is genuinely ordinal with 3+ levels, giving partial credit for near-misses.

#### `kk_icc(data, raters)`

Computes the **Intraclass Correlation Coefficient** in all six Shrout-Fleiss forms (single and average rater) with F-tests and CIs, for the agreement of continuous measurements — the continuous-scale counterpart to `kk_kappa`.

*   **Clinical Example**: Quantifying the reliability of a tumour-size measurement made by three radiologists on the same set of scans.

``` r
ratings <- data.frame(
  r1 = c(9, 6, 8, 7, 10, 6, 8, 7, 9, 5),
  r2 = c(8, 6, 7, 8, 9, 5, 8, 6, 9, 6),
  r3 = c(9, 5, 8, 8, 9, 6, 7, 7, 8, 5)
)
kk_icc(ratings)
#> # A tibble: 6 × 9
#>   type    icc conf.low conf.high F_stat   df1   df2     p.value conf.level
#>   <chr> <dbl>    <dbl>     <dbl>  <dbl> <dbl> <dbl>       <dbl>      <dbl>
#> 1 ICC1  0.849    0.639     0.956   17.9     9    20 0.000000103       0.95
#> 2 ICC2  0.849    0.638     0.956   17.7     9    18 0.000000335       0.95
#> 3 ICC3  0.848    0.627     0.956   17.7     9    18 0.000000335       0.95
#> 4 ICC1k 0.944    0.841     0.985   17.9     9    20 0.000000103       0.95
#> 5 ICC2k 0.944    0.841     0.985   17.7     9    18 0.000000335       0.95
#> 6 ICC3k 0.943    0.834     0.985   17.7     9    18 0.000000335       0.95
```

> **Interpretation.** Read `ICC2` for the reliability of a *single* rater (0.85) and `ICC2k` for the *average* of the three raters (0.94) under a two-way random model. Values above 0.75 indicate excellent reliability, so these raters are highly consistent. Pick the row matching your design: ICC1 (one-way), ICC2 (two-way random), ICC3 (two-way fixed).

#### `kk_reliability(data, items)`

Computes **Cronbach's alpha** (raw and standardized) with per-item statistics including alpha-if-item-dropped, for validating multi-item scales and questionnaires.

*   **Clinical Example**: Assessing the internal consistency of a four-item patient-reported quality-of-life scale.

``` r
items <- data.frame(
  q1 = c(4, 5, 3, 4, 5, 2, 4, 5),
  q2 = c(4, 4, 3, 5, 5, 2, 3, 5),
  q3 = c(5, 5, 2, 4, 4, 3, 4, 4),
  q4 = c(4, 5, 3, 4, 5, 2, 4, 5)
)
kk_reliability(items)
#> # A tibble: 4 × 9
#>   item      n raw.r std.r r.cor r.drop  mean    sd alpha_if_dropped
#>   <chr> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>            <dbl>
#> 1 q1        8 0.973 0.972 0.814  0.949  4    1.07             0.858
#> 2 q2        8 0.883 0.875 0.884  0.782  3.88 1.13             0.917
#> 3 q3        8 0.778 0.789 0.716  0.638  3.88 0.991            0.958
#> 4 q4        8 0.973 0.972 0.814  0.949  4    1.07             0.858
```

> **Interpretation.** The per-item table shows each item's correlation with the scale total and the alpha the scale would have if that item were dropped. No `alpha_if_dropped` value exceeds the overall α (retrieve it via `attr(x, "alpha")` — here raw α ≈ 0.92), so every item contributes and none should be removed. α ≥ 0.7 is the usual bar for acceptable internal consistency.

#### `kk_chisq_test(data, r, c)`

Chi-square test of independence for **r × c contingency tables** (data frame or matrix input) with expected counts, residuals, and effect sizes (Cramér's V).

*   **Epidemiological Example**: Testing association between species and biting frequency across multiple categories.

``` r
species <- c(rep("Mice", 60), rep("Gerbils", 50))
biting <- c(rep("Not", 30), rep("Mild", 30), rep("Not", 25), rep("Mild", 25))
df_chi <- data.frame(species = species, biting = biting)
kk_chisq_test(df_chi, species, biting)
#> # A tibble: 1 × 7
#>   method                        statistic    df p_value observed expected pairwise_results
#>   <chr>                             <dbl> <int>   <dbl> <chr>    <chr>    <list>          
#> 1 Chi-Square Test of Independe…         0     1       1 Gerbils… Gerbils… <chr [1]>
```

---

### 3. Non-Parametric Tests & Baseline Group Comparisons

#### `kk_table1(data, by, variables)` / `kk_compare_groups_table()`

Builds a standard, publication-ready "Table 1" summarizing baseline demographics and clinical variables across groups, automatically performing parametric or non-parametric tests depending on variable characteristics.

*   **Clinical Example**: Summarizing age, gender, blood pressure, and smoking status at baseline stratified by placebo vs. active treatment arm.


``` r
kk_table1(mtcars, by = "am", variables = c("mpg", "hp", "wt"))
#> # A tibble: 3 × 5
#>   Characteristic N     `0   N = 19`            `1   N = 13`           `p-value`
#>   <chr>          <chr> <chr>                   <chr>                  <chr>    
#> 1 __mpg__        32    17.30 (14.70, 19.20)    22.80 (21.00, 30.40)   0.001    
#> 2 __hp__         32    175.00 (110.00, 205.00) 109.00 (66.00, 113.00) 0.044    
#> 3 __wt__         32    3.52 (3.44, 3.85)       2.32 (1.94, 2.78)      <0.001
```

> **Interpretation.** Each cell is median (IQR) by group, with an automatically chosen test in the p-value column. Automatic-transmission cars (am = 1) have higher mpg (22.8 vs 17.3, p = 0.001) and lower weight (2.32 vs 3.52, p < 0.001). This is the standard baseline-characteristics "Table 1" for a manuscript.

#### `kk_compare_groups_table(data, group, variables)` / `compare_groups_table()`

Produces a **detailed two-group comparison table** with per-group summaries, mean/proportion differences, confidence intervals, effect sizes, and p-values. Fully tidyselect- and `group_by()`-aware for stratified analyses.

*   **Clinical Example**: Comparing continuous outcomes between two treatment arms, optionally stratified by a third variable.

``` r
# Direct comparison
kk_compare_groups_table(mtcars, am, c(mpg, hp))
#> # A tibble: 2 × 14
#>   Characteristic n_Total Total      n_1   `1`   n_0   `0`   Difference ci_95 p_value Test 
#>   <chr>          <chr>   <chr>      <chr> <chr> <chr> <chr> <chr>      <chr> <chr>   <chr>
#> 1 mpg            32      20.09 (6.… 13    24.3… 19    17.1… 7.24       3.21… 0.001   t-te…
#> 2 hp             32      146.69 (6… 13    126.… 19    160.… -33.42     -88.… 0.221   t-te…
#> # ℹ 3 more variables: Statistic <chr>, df <chr>, effect_size <chr>

# Stratified by a third variable
library(dplyr)
mtcars |>
  group_by(vs) |>
  kk_compare_groups_table(am, c(mpg, hp))
#> # A tibble: 4 × 15
#>      vs Characteristic n_Total Total      n_1   `1`   n_0   `0`   Difference ci_95 p_value
#>   <dbl> <chr>          <chr>   <chr>      <chr> <chr> <chr> <chr> <chr>      <chr> <chr>  
#> 1     0 mpg            18      16.62 (3.… 6     19.7… 12    15.0… 4.70       0.45… 0.034  
#> 2     0 hp             18      189.72 (6… 6     180.… 12    194.… -13.33     -116… 0.760  
#> 3     1 mpg            14      24.56 (5.… 7     28.3… 7     20.7… 7.63       3.05… 0.004  
#> 4     1 hp             14      91.36 (24… 7     80.5… 7     102.… -21.57     -47.… 0.100  
#> # ℹ 4 more variables: Test <chr>, Statistic <chr>, df <chr>, effect_size <chr>
```

> **Interpretation.** Unlike Table 1, this reports the actual between-group *difference* with its CI and effect size. Manual cars average 7.24 mpg higher (p = 0.001), while the horsepower difference (−33) is not significant (p = 0.22). The stratified call repeats the comparison within each `vs` level — useful for checking effect modification.

#### `table1_summary(data, by, variables)`

A `gtsummary`-backed "Table 1" builder returning a publication-styled summary object (an alternative rendering to `kk_table1`).


``` r
table1_summary(mtcars, by = "am", variables = c("mpg", "hp", "wt"))
#> # A tibble: 3 × 5
#>   Characteristic N     `0   N = 19`      `1   N = 13`      `p-value`
#>   <chr>          <chr> <chr>             <chr>             <chr>    
#> 1 __mpg__        32    17.3 (14.7, 19.2) 22.8 (21.0, 30.4) 0.001    
#> 2 __hp__         32    175 (110, 205)    109 (66, 113)     0.044    
#> 3 __wt__         32    3.52 (3.44, 3.85) 2.32 (1.94, 2.78) <0.001
```

#### `kk_median_test(data, x, group)`

Performs the Median Test for $k$ Independent Samples, comparing proportions above vs. below/equal to the composite median.

*   **Clinical Example**: Comparing the median hospital stay duration (continuous days) across three different surgical wards when stay durations are highly non-normally distributed.

``` r
df_stay <- data.frame(
  days = c(3, 4, 2, 10, 15, 8, 4, 3, 1, 9, 12, 14, 5, 2, 7),
  ward = rep(c("Ward A", "Ward B", "Ward C"), each = 5)
)
df_stay %>% kk_median_test(days, ward)
#> # A tibble: 1 × 8
#>   method alternative statistic_name statistic    df p_value composite_median table_summary
#>   <chr>  <chr>       <chr>              <dbl> <int>   <dbl>            <dbl> <chr>        
#> 1 Media… two.sided   Chi-Square         0.536     2   0.765                5 Ward A: Abov…
```

> **Interpretation.** The median test dichotomises each observation at the pooled (composite) median of 5 days and compares the above/below split across wards. χ² = 0.54 on 2 df, p = 0.77 — no evidence that median stay differs by ward. It trades power for robustness, so it is most useful with heavily skewed data.

#### `kk_vdw_test(data, x, group)`

Performs the van der Waerden Normal-Scores Test as a highly powerful non-parametric alternative to ANOVA, converting ranks to normal distribution quantiles.

*   **Clinical Example**: Evaluating clinical cognitive scores under three independent noise environments when data shape assumptions are violated.


``` r
df_noise <- data.frame(
  score = c(8, 10, 9, 10, 9, 7, 8, 5, 8, 5, 4, 8, 7, 5, 7),
  noise = rep(c("Quiet", "Classical", "Rock"), each = 5)
)
df_noise %>% kk_vdw_test(score, noise)
#> # A tibble: 1 × 7
#>   method   statistic    df p_value variance_normal_scores group_summaries pairwise_results
#>   <chr>        <dbl> <int>   <dbl>                  <dbl> <chr>           <list>          
#> 1 van der…      8.51     2  0.0142                  0.709 Classical: n=5… <tibble [3 × 5]>
```

> **Interpretation.** The van der Waerden normal-scores test is a rank-based, more powerful alternative to Kruskal-Wallis/ANOVA. Here the statistic is 8.51 on 2 df, p = 0.014, so cognitive scores differ significantly across the three noise environments; the `pairwise_results` list column localises which environments differ.

---

### 4. Sequence Randomness & Serial Independence

#### `kk_runs_test(data, x, method)` / `kk_random_seq()`
Wald-Wolfowitz runs test for categorical variables, or Wallis-Moore up-down runs test for quantitative serial randomness.

*   **Epidemiological Example**: Evaluating if successive healthcare-associated infection outbreaks in an ICU follow a random temporal sequence.


``` r
infection_seq <- c("N", "N", "Y", "N", "Y", "Y", "N", "N", "Y", "N")
kk_runs_test(infection_seq)
#> # A tibble: 1 × 8
#>   method   alternative n_obs observed_runs expected_runs variance_runs z_statistic p_value
#>   <chr>    <chr>       <dbl>         <int>         <dbl>         <dbl>       <dbl>   <dbl>
#> 1 Single-… two.sided      10             7           5.8          1.92       0.506   0.613
```

> **Interpretation.** The Wald-Wolfowitz runs test checks whether a binary sequence is randomly ordered. Observed runs (7) are close to the number expected under randomness (5.8), giving z = 0.51, p = 0.61 — no evidence of clustering or alternation, so the outbreak sequence is consistent with random timing.

#### `kk_frequency_test(data, x)`

Chi-Square Goodness-of-Fit or Binomial test checking if categories occur with equal probability.

*   **Epidemiological Example**: Verifying if congenital abnormality admissions occur uniformly across days of the week.

``` r
admission_days <- c(1, 3, 2, 7, 5, 2, 4, 3, 1, 6, 7, 2, 5, 3)
kk_frequency_test(admission_days)
#> # A tibble: 1 × 8
#>   method              alternative statistic_name statistic    df p_value observed expected
#>   <chr>               <chr>       <chr>              <dbl> <dbl>   <dbl> <chr>    <chr>   
#> 1 Chi-Square Goodnes… two.sided   Chi-Square             2     6   0.920 1=2, 2=… 1=2, 2=…
```

> **Interpretation.** A goodness-of-fit test for whether categories occur equally often. χ² = 2 on 6 df, p = 0.92 — admissions are spread evenly across the days of the week, with no evidence of a "busy day" pattern.

#### `kk_mssd_test(data, x)`

Von Neumann Mean Square Successive Difference (MSSD) test for serial correlation on continuous quantitative data.

*   **Clinical Example**: Checking if continuous blood pressure readings over time represent independent random fluctuations or exhibit serial correlation.

``` r
bp_ticks <- c(120, 122, 121, 125, 124, 122, 120, 118, 119, 122)
kk_mssd_test(bp_ticks)
#> # A tibble: 1 × 8
#>   method      alternative n_obs statistic_C s_variance s_ms_difference z_statistic p_value
#>   <chr>       <chr>       <int>       <dbl>      <dbl>           <dbl>       <dbl>   <dbl>
#> 1 Mean Squar… two.sided      10       0.477       4.68            2.44        1.68  0.0930
```

> **Interpretation.** The von Neumann MSSD test detects serial correlation in a continuous sequence. z = 1.68, p = 0.093 — borderline but not significant at 5%, so these blood-pressure readings are (just) consistent with independent fluctuations rather than a drifting/autocorrelated trend.

---

### 5. Regression Modeling & Proportions Comparisons

#### `kk_reg(data, outcome, predictors)` / `regression_analysis()` / `krk_reg()`
A unified modeling wrapper that automatically detects binomial outcomes (triggering Logistic Regression with odds ratios and ROC curve diagnostics) or continuous outcomes (triggering Linear Regression with check plots).

*   **Epidemiological Example**: Modeling the risk of hypertension based on age, BMI, and family history.

``` r
regression_analysis(mtcars, outcome = "am", predictors = c("mpg", "wt"))
#> # A tibble: 7 × 21
#>   term     estimate std.error statistic p.value conf.low conf.high model_type outcome_type
#>   <chr>       <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <chr>      <chr>       
#> 1 (Interc… -5.91e-1    0.253    -2.33   2.64e-2  -1.11     -0.0741 univariate continuous  
#> 2 mpg       4.97e-2    0.0121    4.11   2.85e-4   0.0250    0.0744 univariate continuous  
#> 3 (Interc…  1.54e+0    0.226     6.84   1.38e-7   1.08      2.00   univariate continuous  
#> 4 wt       -3.53e-1    0.0672   -5.26   1.13e-5  -0.490    -0.216  univariate continuous  
#> 5 (Interc…  1.56e+0    0.863     1.80   8.19e-2  -0.210     3.32   multivari… continuous  
#> 6 mpg      -3.41e-4    0.0223   -0.0153 9.88e-1  -0.0460    0.0453 multivari… continuous  
#> 7 wt       -3.55e-1    0.137    -2.58   1.51e-2  -0.636    -0.0739 multivari… continuous  
#> # ℹ 12 more variables: AIC <dbl>, BIC <dbl>, estimate_label <chr>, coef.type <chr>,
#> #   percent_change <dbl>, percent_change_low <dbl>, percent_change_high <dbl>,
#> #   r_squared <dbl>, adj_r_squared <dbl>, residual_std_error <dbl>, model_p_value <dbl>,
#> #   predictor <chr>
```

> **Interpretation.** The wrapper returns both univariate and multivariable models in one tidy frame (`model_type` column). Univariately both mpg and wt predict transmission, but in the multivariable model mpg loses significance (p = 0.99) while wt remains (coef −0.36, p = 0.015) — evidence that mpg and weight are collinear and weight carries the signal. For a binary outcome the estimates would be odds ratios with ROC diagnostics attached.

#### `compare_proportions(data)` / `compare_proportions_by()`

Pairwise comparison of proportions utilizing normal approximations with multiple comparison adjustments (e.g. Holm/Bonferroni).


``` r
df_prop <- data.frame(
  proportion = c(0.3, 0.5, 0.25),
  trials = c(100, 120, 90),
  clinic = c("A", "B", "C")
)
compare_proportions(df_prop)
#> # A tibble: 3 × 8
#>   gr1_clinic gr2_clinic prop_diff z_score  p_value ci_lower ci_upper adj_p_value
#>   <chr>      <chr>          <dbl>   <dbl>    <dbl>    <dbl>    <dbl>       <dbl>
#> 1 A          B              -0.2   -3.09  0.00199   -0.327   -0.0732    0.00397 
#> 2 A          C               0.05   0.773 0.439     -0.0768   0.177     0.439   
#> 3 B          C               0.25   3.87  0.000108   0.123    0.377     0.000323
```

> **Interpretation.** All pairwise differences in proportion with unpooled (Wald) z-tests and Holm-adjusted p-values. Clinic B differs significantly from both A and C (adjusted p = 0.004 and 0.0003), while A vs C does not (adjusted p = 0.44). Read `adj_p_value` — not the raw `p_value` — when drawing conclusions across multiple comparisons.

#### `pcit(data, conf.level)`

Computes **binomial confidence intervals for proportions** from a data frame of successes and trials (the first two numeric columns), returning the proportion and its CI per row.


``` r
df_ci <- data.frame(successes = c(10, 20), trials = c(100, 100), group = c("A", "B"))
pcit(df_ci)
#> # A tibble: 2 × 7
#>   group successes trials proportion  lower upper conf.level
#>   <chr>     <dbl>  <dbl>      <dbl>  <dbl> <dbl>      <dbl>
#> 1 A            10    100        0.1 0.0490 0.176       0.95
#> 2 B            20    100        0.2 0.127  0.292       0.95
```

#### `compare_proportions_kk_glm(data, group, x, n)`

Compares proportions between groups using **logistic regression with robust (sandwich) standard errors**, supporting covariate adjustment and stratification — more flexible than the normal-approximation `compare_proportions`.

*   **Epidemiological Example**: Comparing event rates across treatment arms while adjusting for a confounder.

``` r
df_glm <- data.frame(group = c("A", "B"), x = c(15, 25), n = c(50, 50))
compare_proportions_kk_glm(df_glm, group, x, n)
#> # A tibble: 1 × 6
#>   group1 group2 estimate p_value adjust conf_level
#>   <chr>  <chr>     <dbl>   <dbl> <chr>       <dbl>
#> 1 A      B          -0.2  0.0371 holm         0.95
```

> **Interpretation.** The estimate is the difference in proportion (A − B = −0.20) from a logistic model with robust SEs; p = 0.037, so group B's event rate is significantly higher. Unlike `compare_proportions`, this route accepts covariates and strata for adjusted contrasts.

#### `power_proportions(n, p1, p2, power, sig.level)`

Power / sample-size calculation for **two-sample proportion tests**; supply all but one parameter to solve for the missing one.

*   **Clinical Example**: Finding the power to detect a difference between a 50% and 60% response rate with 100 patients per arm.

``` r
power_proportions(n = 100, p1 = 0.5, p2 = 0.6)
#> 
#>      Two-sample comparison of proportions power calculation 
#> 
#>               n = 100
#>              p1 = 0.5
#>              p2 = 0.6
#>       sig.level = 0.05
#>           power = 0.2941273
#>     alternative = two.sided
#> 
#> NOTE: n is number in *each* group
```

> **Interpretation.** With 100 patients per arm, a study comparing a 50% vs 60% response rate has only **29% power** — far below the conventional 80% target. In other words this design would miss a true 10-point difference most of the time; you would need a substantially larger sample. Leave `power` out and supply `n` to solve for power, or leave `n` out to solve for the required sample size.

#### `plot_proportion_comparisons(results)`

Renders a **forest plot** of the pairwise differences produced by `compare_proportions` or `compare_proportions_kk_glm`.


``` r
res <- compare_proportions(df_prop)
plot_proportion_comparisons(res)
```

<div class="figure">
<img src="man/figures/README-ex-44-1.png" alt="plot of chunk ex-44" width="75%" />
<p class="caption">plot of chunk ex-44</p>
</div>

#### `kk_compare_independent_correlations(r, n)`

Fisher Z-transformation tests evaluating differences between correlation coefficients obtained from independent samples.

*   **Epidemiological Example**: Comparing the correlation of daily dietary sodium intake and systolic blood pressure in males vs. females.


``` r
# Male: r = 0.65 (n=50), Female: r = 0.40 (n=60)
kk_compare_independent_correlations(c(0.65, 0.40), c(50, 60))
#> # A tibble: 1 × 8
#>   method       alternative statistic_name statistic    df p_value common_r group_summaries
#>   <chr>        <chr>       <chr>              <dbl> <int>   <dbl>    <dbl> <chr>          
#> 1 Comparison … two.sided   Z                   1.78    NA  0.0743    0.525 1: r=0.650 (n=…
```

> **Interpretation.** A Fisher z-test for whether two correlations from *independent* samples differ. z = 1.78, p = 0.074 — the male (r = 0.65) and female (r = 0.40) correlations are not significantly different at 5%, despite the apparent gap, because the samples are modest. `common_r` (0.53) is the pooled estimate under the null.

#### `kk_compare_dependent_correlations(rxz, ryz, rxy, n)`

Steiger's t-test comparing two dependent correlations sharing a common criterion variable within the same sample.

*   **Epidemiological Example**: Evaluating if daily sugar intake ($X$) is a significantly stronger predictor of dental cavities ($Z$) than daily sodium intake ($Y$) in the same cohort.


``` r
# BP vs Sugar (rxz) = 0.72, BP vs Salt (ryz) = 0.35, Sugar vs Salt (rxy) = 0.28, n = 50
kk_compare_dependent_correlations(0.72, 0.35, 0.28, 50)
#> # A tibble: 1 × 10
#>   method        alternative statistic_name statistic    df p_value   rxz   ryz   rxy     n
#>   <chr>         <chr>       <chr>              <dbl> <int>   <dbl> <dbl> <dbl> <dbl> <int>
#> 1 Comparison o… two.sided   t                  -2.95    47 0.00494  0.72  0.35  0.28    50
```

> **Interpretation.** Steiger's test compares two correlations that share a variable *within the same sample*. Sugar correlates with cavities more strongly (0.72) than salt does (0.35), and the difference is significant (t = −2.95, df = 47, p = 0.005) — sugar is the stronger predictor even after accounting for the sugar–salt correlation (0.28).

#### `kk_firth(data, outcome, predictors)`

Fits **Firth-penalized logistic regression**, which yields finite, bias-reduced odds ratios under complete or quasi-complete **separation** (rare events or a zero cell) where ordinary logistic regression breaks down. Separation is detected and reported.

*   **Epidemiological Example**: Estimating an odds ratio for a rare adverse event where one exposure category has zero events, so standard logistic regression returns an infinite estimate.

``` r
df_sep <- data.frame(
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  x = c(1, 2, 3, 4, 5, 6, 7, 8),
  z = rnorm(8)
)
kk_firth(df_sep, y, c("x", "z"))
#> # A tibble: 2 × 8
#>   term  odds_ratio conf.low conf.high std.error statistic p.value conf.level
#>   <chr>      <dbl>    <dbl>     <dbl>     <dbl>     <dbl>   <dbl>      <dbl>
#> 1 x           2.34   0.779       7.01     0.561     1.51    0.130       0.95
#> 2 z           1.22   0.0585     25.6      1.55      0.130   0.897       0.95
```

> **Interpretation.** Here `x` perfectly separates the outcome, so ordinary logistic regression would return an infinite coefficient. Firth's penalty shrinks it to a finite, usable OR of 2.34 (95% CI 0.78–7.01). The `attr(x, "separation")` flag is `TRUE`, confirming separation was detected — the reason to use this instead of `glm()`.

---

### 6. Survival Analysis & Time-to-Event

#### `kk_survival_plot(data, time, status, group)` / `survival_plot()`
Kaplan-Meier survival curves with publication-quality formatting, confidence bands, and risk tables.
*   **Clinical Example**: Plotting time-to-death of advanced-stage lung cancer patients stratified by chemotherapy regimen.

``` r
library(survival)
kk_survival_plot(lung, time, status, sex)
```

<div class="figure">
<img src="man/figures/README-ex-48-1.png" alt="plot of chunk ex-48" width="75%" />
<p class="caption">plot of chunk ex-48</p>
</div>

#### `kk_coxph(data, time, status, predictors)`

Fits univariate and multivariable **Cox proportional hazards models**, returning a tidy hazard-ratio table alongside the **Schoenfeld residual test** of the proportional-hazards assumption (per-term and global) plus concordance and AIC.

*   **Clinical Example**: Modelling time-to-death by age, sex, and ECOG performance status, while checking whether the proportional-hazards assumption holds.

``` r
library(survival)
kk_coxph(lung, time, status, predictors = c("age", "sex", "ph.ecog"))
#> # A tibble: 6 × 15
#>   term    model_type    hazard_ratio conf.low conf.high std.error statistic p.value   ph_p
#>   <chr>   <chr>                <dbl>    <dbl>     <dbl>     <dbl>     <dbl>   <dbl>  <dbl>
#> 1 age     univariate           1.02     1.00      1.04    0.00920      2.03 4.19e-2 0.556 
#> 2 sex     univariate           0.588    0.424     0.816   0.167       -3.18 1.49e-3 0.0906
#> 3 ph.ecog univariate           1.61     1.29      2.01    0.113        4.20 2.69e-5 0.134 
#> 4 age     multivariable        1.01     0.993     1.03    0.00927      1.19 2.32e-1 0.665 
#> 5 sex     multivariable        0.575    0.414     0.799   0.168       -3.29 9.86e-4 0.129 
#> 6 ph.ecog multivariable        1.59     1.27      1.99    0.114        4.08 4.45e-5 0.152 
#> # ℹ 6 more variables: ph_global_p <dbl>, n <int>, n_events <dbl>, concordance <dbl>,
#> #   AIC <dbl>, conf.level <dbl>
```

> **Interpretation.** Read the `multivariable` rows for mutually-adjusted hazard ratios: female sex (sex = 2) roughly halves the hazard of death (HR 0.58, 95% CI 0.41–0.80) and worse ECOG performance raises it (HR 1.59 per level), while age is not independently significant. Crucially, every `ph_p` (Schoenfeld test) is > 0.05, so the proportional-hazards assumption holds and the HRs are valid; had `ph_p` been small, you would switch to `kk_rmst`.

#### `kk_logrank(data, time, status, group)`

Compares survival across groups with the **log-rank test** (`rho = 0`) or the Peto-Peto / Gehan-Wilcoxon weighting (`rho = 1`), returning per-group observed/expected counts and the overall statistic.

*   **Clinical Example**: Testing whether survival differs between two chemotherapy regimens.

``` r
library(survival)
kk_logrank(lung, time, status, sex)
#> # A tibble: 2 × 9
#>   group     n observed expected oe_ratio chisq    df p_value method  
#>   <chr> <dbl>    <dbl>    <dbl>    <dbl> <dbl> <dbl>   <dbl> <chr>   
#> 1 1       138      112     91.6    1.22   10.3     1 0.00131 Log-rank
#> 2 2        90       53     73.4    0.722  10.3     1 0.00131 Log-rank
```

> **Interpretation.** Group 1 (male) had more deaths than expected (observed 112 vs expected 91.6; O/E 1.22) and group 2 (female) fewer (53 vs 73.4; O/E 0.72). The log-rank test is significant (χ² = 10.3, df = 1, p = 0.0013), so survival differs by sex — consistent with the protective HR seen in `kk_coxph`.

#### `kk_rmst(data, time, status, group, tau)`

Computes the **Restricted Mean Survival Time** (area under the KM curve up to a horizon `tau`) for each group, plus the between-group **difference and ratio** with confidence intervals — an assumption-light effect measure to reach for when `kk_coxph` flags a proportional-hazards violation.

*   **Clinical Example**: Reporting the mean survival time gained (in days, within the first year) for women vs. men with lung cancer, without assuming proportional hazards.

``` r
library(survival)
kk_rmst(lung, time, status, sex, tau = 365)
#> # A tibble: 4 × 8
#>   group                     tau   rmst    se conf.low conf.high   p.value conf.level
#>   <chr>                   <dbl>  <dbl> <dbl>    <dbl>     <dbl>     <dbl>      <dbl>
#> 1 1                         365 241.    10.4   221.      262.   NA              0.95
#> 2 2                         365 297.    10.8   276.      319.   NA              0.95
#> 3 RMST difference (2 - 1)   365  56.0   15.0    26.7      85.3   0.000183       0.95
#> 4 RMST ratio (2 / 1)        365   1.23  NA       1.10      1.38  0.000207       0.95
```

> **Interpretation.** Within the first year, women lived on average 297 days versus 241 for men — a **56-day gain** (95% CI 26.7–85.3, p < 0.001), equivalently an RMST ratio of 1.23. Unlike the hazard ratio, this is a direct, clinically meaningful contrast in mean survival time that needs no proportional-hazards assumption, which is why it is the fallback when `kk_coxph`'s Schoenfeld test fails.

#### `kk_cuminc(data, time, status, group, cause)`

Estimates the **competing-risks cumulative incidence** (Aalen-Johansen) for an event of interest in the presence of competing events, with **Gray's test** across groups (when the `cmprsk` package is installed).

*   **Epidemiological Example**: Estimating the cumulative incidence of relapse when death without relapse is a competing risk, compared across two treatment arms.

``` r
df_cr <- data.frame(
  time = rexp(200, 0.1),
  status = sample(0:2, 200, replace = TRUE), # 0 = censored, 1 = relapse, 2 = death
  arm = rep(c("A", "B"), each = 100)
)
df_cr %>% kk_cuminc(time, status, arm, cause = 1)
#> # A tibble: 2 × 5
#>   group  time   cif     se cause
#>   <chr> <dbl> <dbl>  <dbl> <chr>
#> 1 A      42.6 0.523 0.0612 1    
#> 2 B      61.1 0.435 0.0626 1
```

> **Interpretation.** The cumulative incidence of relapse (cause 1), accounting for death as a competing risk, reaches ~0.52 in arm A and ~0.44 in arm B by the reported times. Using the Aalen-Johansen estimator (not naïve 1 − KM) avoids over-stating incidence when competing events remove people from risk. Gray's test — available via `attr(x, "gray_test")` when `cmprsk` is installed — formally compares the two curves.

---

### 7. Demographics & EGN Utilities (Bulgarian Registry)

#### `extract_egn_info(egn_vector)` / `extract_age_from_egn()`
Parses, validates, and extracts demographic profiles (Date of Birth, Gender, Age, and Birth Region) from Bulgarian Personal Identification Numbers (EGN).
*   **Epidemiological Example**: Cleaning and automatically extracting birth dates, gender, and geographic cohorts from Bulgarian electronic medical records.

``` r
egn_sample <- c("9201014321", "8812128765")
extract_egn_info(egn_sample)
#>   age birth_date is_valid gender region birth_order invalid_egn        invalid_reason
#> 1  34 1992-01-01     TRUE   Male Pleven           2        <NA>                  <NA>
#> 2  37 1988-12-12    FALSE   Male Shumen           4        <NA> Invalid control digit
```

---

### 8. Descriptive Statistics & Summaries

#### `kk_summary(data, col)` / `comprehensive_summary()`

Computes an extensive numeric summary for a variable — central tendency, dispersion, robust estimators (Huber M), skewness/kurtosis, and normality tests — with full `group_by()` support.

*   **Clinical Example**: Describing the distribution of a biomarker overall and by treatment arm before modelling.

``` r
kk_summary(mtcars, mpg)
#> # A tibble: 1 × 33
#>   var_name type  n_total n_miss n_valid miss_pct  mean huber_mean trim_mean geometric_mean
#>   <chr>    <chr>   <int>  <int>   <int>    <dbl> <dbl>      <dbl>     <dbl>          <dbl>
#> 1 mpg      nume…      32      0      32        0  20.1       19.6      19.7           19.3
#> # ℹ 23 more variables: median <dbl>, min <dbl>, max <dbl>, range <dbl>, variance <dbl>,
#> #   sd <dbl>, se <dbl>, cv_pct <dbl>, mad <dbl>, iqr <dbl>, q1 <dbl>, q3 <dbl>,
#> #   skewness <dbl>, kurtosis <dbl>, pct_5_95 <list>, ci_mean_low <dbl>, ci_mean_up <dbl>,
#> #   shapiro_p <dbl>, shapiro_int <chr>, ks_p <lgl>, ks_int <chr>, n_outliers <int>,
#> #   outlier_values <list>

# Grouped and abbreviated
library(dplyr)
mtcars |>
  group_by(am) |>
  kk_summary(mpg, verbose = "basic")
#> # A tibble: 2 × 12
#> # Groups:   am [2]
#>      am var_name type    n_total n_miss n_valid miss_pct  mean median   min   max    sd
#>   <dbl> <chr>    <chr>     <int>  <int>   <int>    <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1     0 mpg      numeric      19      0      19        0  17.1   17.3  10.4  24.4  3.83
#> 2     1 mpg      numeric      13      0      13        0  24.4   22.8  15    33.9  6.17
```

---

### 9. Time-Series Analysis

#### `kk_time_series(data, value_col, date_col)` / `time_series_analysis()`

Analyzes a time series end-to-end: decomposition, stationarity tests (ADF/KPSS), autocorrelation, and automatic ARIMA modelling, with optional grouping.

*   **Epidemiological Example**: Characterising a monthly surveillance count series and fitting a forecasting model.

``` r
date <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)
value <- 100 + cumsum(rnorm(24, 2, 5))
df_ts <- data.frame(date = date, value = value)
kk_time_series(df_ts)
#> # A tibble: 48 × 2
#>   Metric             Value
#>   <chr>              <dbl>
#> 1 Length of Series    24  
#> 2 Mean               129. 
#> 3 Median             132. 
#> 4 Standard Deviation  15.5
#> 5 Variance           240. 
#> 6 Min                 98.8
#> 7 Max                154. 
#> 8 Range               55.6
#> # ℹ 40 more rows
```

#### `kk_time_metrics(data, value_col, date_col)`

Returns compact time-series descriptors — entropy, stability, linearity, and trend strength — useful for screening many series at once.

``` r
kk_time_metrics(df_ts)
#> # A tibble: 1 × 30
#>   group autocorr_r1 durbinwatson_qbp ljungbox_pval spearman_corr spearman_pval
#>   <chr>       <dbl>            <dbl>         <dbl>         <dbl>         <dbl>
#> 1 1           0.961           0.0013             0         0.958             0
#> # ℹ 24 more variables: anderson_stat <dbl>, anderson_pval <dbl>, mean_absinc_chain <dbl>,
#> #   mean_devrate_chain <dbl>, mean_incrate_chain <dbl>, mean_absinc_fixed <dbl>,
#> #   mean_devrate_fixed <dbl>, mean_incrate_fixed <dbl>, sd_absinc_chain <dbl>,
#> #   sd_devrate_chain <dbl>, sd_incrate_chain <dbl>, sd_absinc_fixed <dbl>,
#> #   sd_devrate_fixed <dbl>, sd_incrate_fixed <dbl>, geom_mean_growth <dbl>,
#> #   geom_mean_growth_pct <dbl>, mean_incrate <dbl>, mean_incrate_pct <dbl>,
#> #   per_absinc_chain <list>, per_devrate_chain <list>, per_incrate_chain <list>, …
```

---

### 10. Visualization

#### `kkplot(..., rangeframe, minor_ticks, cap)`

A drop-in `ggplot()` replacement applying **Edward Tufte's data-ink principles** to the axes. **By default it behaves exactly like `ggplot()` with capped axes** (`cap = "both"`), which is safe for any `aes()` mapping and complex/faceted data. The Tufte range frame is strictly **optional**.

The Tufte options are named arguments that come *after* `...`, so always pass them **by name**:


``` r
library(ggplot2)

# Default — capped axes, works with any data / aes() (recommended default)
kkplot(mtcars, aes(x = mpg, y = wt)) + geom_point()
```

<div class="figure">
<img src="man/figures/README-ex-57-1.png" alt="plot of chunk ex-57" width="75%" />
<p class="caption">plot of chunk ex-57</p>
</div>

``` r

# Optional: true Tufte range frame (axis lines span exactly the data range).
# Best for plots with continuous x AND y; pass the option BY NAME.
kkplot(mtcars, aes(x = mpg, y = wt), rangeframe = TRUE) + geom_point()
```

<div class="figure">
<img src="man/figures/README-ex-57-2.png" alt="plot of chunk ex-57" width="75%" />
<p class="caption">plot of chunk ex-57</p>
</div>

``` r

# Optional: subtle minor ticks for precise value reading
kkplot(mtcars, aes(x = mpg, y = wt), minor_ticks = TRUE) + geom_point()
```

<div class="figure">
<img src="man/figures/README-ex-57-3.png" alt="plot of chunk ex-57" width="75%" />
<p class="caption">plot of chunk ex-57</p>
</div>

``` r

# Combine, or change the cap style
kkplot(mtcars, aes(x = mpg, y = wt), rangeframe = TRUE, minor_ticks = TRUE) + geom_point()
```

<div class="figure">
<img src="man/figures/README-ex-57-4.png" alt="plot of chunk ex-57" width="75%" />
<p class="caption">plot of chunk ex-57</p>
</div>

``` r
kkplot(mtcars, aes(x = mpg, y = wt), cap = "none") + geom_point()
```

<div class="figure">
<img src="man/figures/README-ex-57-5.png" alt="plot of chunk ex-57" width="75%" />
<p class="caption">plot of chunk ex-57</p>
</div>

| Argument | Default | Effect |
|---|---|---|
| `cap` | `"both"` | Cap axis lines at the outer ticks (`"both"`, `"lower"`, `"upper"`, `"none"`). |
| `rangeframe` | `FALSE` | Draw a true Tufte range frame spanning the data range (continuous x/y only). |
| `minor_ticks` | `FALSE` | Add subtle minor ticks to both axes. |

Pairs with the Tufte theme applied by `set_plot_font()`.

#### `univariate_plot(data, variable)` / `univariate_cat_plot()` / `univariate_cont_plot()`

Automatically renders the appropriate univariate figure — bar chart for categorical variables, density/histogram for continuous — with optional grouping. The `_cat_` and `_cont_` variants (also aliased as `univariate_categorical_plot()` and `univariate_continuous_plot()`) force a specific type.

``` r
univariate_plot(mtcars, "mpg")          # continuous -> density
```

<div class="figure">
<img src="man/figures/README-ex-58-1.png" alt="plot of chunk ex-58" width="75%" />
<p class="caption">plot of chunk ex-58</p>
</div>

``` r
univariate_plot(mtcars, "am")           # categorical -> bar
```

<div class="figure">
<img src="man/figures/README-ex-58-2.png" alt="plot of chunk ex-58" width="75%" />
<p class="caption">plot of chunk ex-58</p>
</div>

``` r
univariate_cont_plot(mtcars, "mpg", group = "cyl")
```

<div class="figure">
<img src="man/figures/README-ex-58-3.png" alt="plot of chunk ex-58" width="75%" />
<p class="caption">plot of chunk ex-58</p>
</div>

``` r
univariate_cat_plot(mtcars, "am", group = "cyl")
```

<div class="figure">
<img src="man/figures/README-ex-58-4.png" alt="plot of chunk ex-58" width="75%" />
<p class="caption">plot of chunk ex-58</p>
</div>

#### `kk_fullcorplot(data, method)`

Produces a full **correlation matrix plot** with significance annotations (Pearson, Spearman, or Kendall), with optional multiple-comparison adjustment.

``` r
kk_fullcorplot(mtcars, method = "kendall")
```

<div class="figure">
<img src="man/figures/README-ex-59-1.png" alt="plot of chunk ex-59" width="75%" />
<p class="caption">plot of chunk ex-59</p>
</div>

#### `set_plot_font(font, size)`

Sets a global ggplot2 theme font (loaded via `sysfonts`/`showtext`) for consistent typography across all figures.

``` r
set_plot_font("Roboto Condensed", size = 14)
```

---

### 11. Data Utilities & Setup

#### `kkonehot(data, column)` / `one_hot_encode()`

One-hot encodes a categorical column into indicator variables.

``` r
df_oh <- tibble::tibble(id = 1:3, color = c("red", "blue", "red"))
kkonehot(df_oh, "color")
#> # A tibble: 3 × 4
#>      id color color_blue color_red
#>   <int> <chr>      <dbl>     <dbl>
#> 1     1 red            0         1
#> 2     2 blue           1         0
#> 3     3 red            0         1
```

#### `mutate_round(data, digits)`

Rounds all numeric columns in a data frame to a given number of digits (half-up rounding).

``` r
mutate_round(data.frame(a = c(1.234, 5.678), b = c("x", "y")), 1)
#>     a b
#> 1 1.2 x
#> 2 5.7 y
```

#### `format_tibble(data, digits)`

Formats numeric values in a results tibble for display (fixed decimals, thousands separators).

``` r
format_tibble(tibble::tibble(Value = c(10.567, 2.3, NA)))
#> # A tibble: 3 × 2
#>   Value Value_display
#>   <dbl> <chr>        
#> 1  10.6 10.57        
#> 2   2.3 2.30         
#> 3  NA   NA
```

#### `kk_setup(cores, scipen)`

One-call session setup: configures parallel cores, the Bayesian backend, display options, and disables scientific notation.

``` r
kk_setup()
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
  version = {0.1.18},
  url = {https://github.com/kostadinoff/kkstatfun},
  doi = {10.5281/zenodo.18936020},
}
```

## Credits

This package was architected, refactored, and enhanced by **Antigravity**, an advanced AI coding assistant developed by **Google DeepMind**. 
