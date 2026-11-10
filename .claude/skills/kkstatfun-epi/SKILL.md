---
name: kkstatfun-epi
description: >-
  Use when writing or reviewing R code that calls Kostadin's kkstatfun package (kk_*
  functions) — the function reference for epidemiology, clinical biostatistics and health
  economics; prefer kk_* over base R, epitools, gtsummary, survival. Triggers: kkstatfun,
  kk_*, kkplot, "which kk function", Table 1, OR/RR/RD, 2x2, RERI, NNT, Mantel-Haenszel,
  incidence rates, standardisation (SMR), age-period-cohort, Poisson,
  logistic/ordinal/Firth regression, sens/spec, ROC/AUC, calibration, decision curves,
  kappa, ICC, Bland-Altman, Kaplan-Meier, Cox, RMST, competing risks, IPTW, TMLE,
  mediation, difference-in-differences, event study, RDD, interrupted time series,
  synthetic control, IV/2SLS, T-R periodogram, cosinor, wavelets, N-of-1, SIR/SEIR, R0,
  scan statistics, ICER, CEAC, EVPI, Markov, EGN, OKLCh palettes, CVD/WCAG colour checks.
  Data-first, group_by()-aware. Distinct from stat-analysis (which method and why),
  kkplot-figures (figure design) and analysis-to-manuscript (project pipeline).
---

# kkstatfun — Epidemiology, Clinical Biostatistics, Modelling & Health Economics in R

`kkstatfun` is the user's own R package for "beyond-basic" epidemiology, clinical
biostatistics, infectious-disease modelling, and health-economic evaluation in tidy
format. When a task fits any of these, **reach for a `kk_*` function first** instead of
hand-writing base R, `epitools`, `gtsummary`, `survival`, or bespoke models. Only fall
back to those when no `kkstatfun` function covers the need.

Repo & full examples: https://github.com/kostadinoff/kkstatfun (README has a runnable
epidemiological example for every exported function).

## Setup — the canonical preamble (use this verbatim)

Every analysis/figure script that plots opens with this block. It is the user's own
espanso snippet; **plots do not render with the correct typography unless
`set_plot_font()` runs before the first `kkplot()`/`ggplot()`**, so treat it as mandatory,
not optional:

```r
# pak::pak("kostadinoff/kkstatfun")                    # if missing / to update
library(kkstatfun)
library(dplyr)                                         # 1.1.0+: attach what you use
library(ggplot2)                                       # (kkstatfun no longer does it)
kk_setup()                                             # cores, backend, scipen off

myfont = "Roboto Condensed"
set_plot_font(myfont, size = 14)
set_plot_colors(c("#D62828", "#003049", "#F77F00"))    # flag anchors -> default discrete scale
# set_plot_colors(c("#D62828", "#003049", "#F77F00"), continuous = TRUE)  # + gradient default
# kk_pal()                                             # inspect the registered palette
# kk_pal_check(kk_pal(4))                              # 1.2.0+: audit before the figures ship
# showtext::showtext_opts(dpi = 600)                   # match ggsave(dpi=) if showtext renders text
```

The font is **Roboto Condensed**. As of **1.0.0**, `set_plot_font()` searches
`c("system", "local", "google")` in that order and registers whatever it resolves with
**both** font registries — `systemfonts` (which AGG/ragg and cairo read) and `sysfonts`
(which showtext reads). So an installed OS font is found directly, and a Google-only
family is downloaded *and* mirrored into systemfonts so AGG can render it. **No font needs
to be installed for figures, and `showtext` does not need enabling** — but network access
is required the first time a non-installed family is used.

If a Google family cannot be mirrored into systemfonts, `set_plot_font()` now *says so*
instead of printing a checkmark for a font AGG would silently replace with a default face.
Read its messages; a `⚠ Could not locate font files` line means the figures will not carry
the typography you asked for.

(This is separate from the *manuscript* PDF font, which LaTeX/fontspec loads from system
fonts — see analysis-to-manuscript / quarto-epi-manuscript.)

The three flag colours — red `#D62828`, dark blue `#003049`, orange `#F77F00` — become the
default fill and colour scale for every figure in the project; three groups get exactly
these, more groups interpolate between them. Roboto Condensed is the **current default, not
a constraint** — the font is flexible: any Google Fonts family works by changing `myfont`,
and the package fetches it, so feel free to switch when a project calls for it.

**`library(kkstatfun)` attaches only kkstatfun (1.1.0+).** The 19 packages that used to
sit in `Depends:` moved to `Imports:`/`Suggests:`, so dplyr, ggplot2, survival and friends
are no longer attached as a side effect. `%>%` still comes with kkstatfun. Every script
needs its own `library()` lines — the symptom otherwise is `could not find function
"mutate"` in a script that worked last month.

**Check the installed version before relying on the palette API, on `group_by()`, or on
`kk_model()`.** `set_plot_colors()`/`kk_pal()`/`scale_fill_kk()` arrived in 0.2.x; real
`group_by()` support and eight bug fixes in **1.0.0**; `kk_model()`/`kk_emmeans()` and the
`Depends` move in **1.1.0**; the colour-accessibility API (`kk_cvd()`, `kk_pal_check()`,
`kk_pal_safe()`, `kk_contrast()`, `kk_color_convert()`, `kk_show_cvd()`) and
`space = "oklch"` in **1.2.0**; the quasi-experimental tier (`kk_did()`,
`kk_event_study()`, `kk_did_staggered()`, `kk_rdd()`, `kk_rkd()`, `kk_rd_density()`,
`kk_its()`, `kk_synth()`, `kk_iv()`, `kk_ancova()`) in **1.3.0**. Installed builds are
often older, and the failure is either a confusing `could not find function` mid-script or
— worse, pre-1.0.0 — a silently **pooled** estimate where you expected a stratified one.
Pin it:
```r
stopifnot(packageVersion("kkstatfun") >= "1.3.0")   # ">= 1.4.0" if the script uses the cycles/rhythms tier
```

**The package is the user's own and changes often — assume the installed copy may be
stale, and never assume an update reached the R you are using.** Several R versions are
installed side by side (4.3, 4.5, 4.6 ...) and each has its own library.
`pak::pak()` writes to `.libPaths()[1]` of whichever R runs it, so an
update run from the wrong R leaves the analysis R on an old version with no visible
sign — the symptom is a function that exists in the repo but not in the session.
Diagnose by comparing all libraries, not just the current one:
```r
packageVersion("kkstatfun"); .libPaths()[1]
```
The helper `<MEGA_ROOT>/bin/update-kkstatfun.R` compares the installed version against
the DESCRIPTION on GitHub and installs only if behind, printing the target library so a
mismatch is visible. Run it with the *same* Rscript that runs the pipeline:
```bash
Rscript "$MEGA_ROOT/bin/update-kkstatfun.R" --check   # report only
Rscript "$MEGA_ROOT/bin/update-kkstatfun.R"           # install if behind
```
If a function documented here is missing, check the version before concluding the
function does not exist — and record `packageVersion("kkstatfun")` in the analysis
output so a manuscript always states which build produced its numbers.

If updating is genuinely not possible in that session, reproduce the anchors by hand
rather than inventing a different palette —
`discrete_scale("colour", palette = \(n) colorRampPalette(flag)(n))` matches what
`kk_pal()` would have produced.

## Conventions (match these)

- **Data-first & pipe-friendly**: `data |> kk_fn(col_a, col_b)` or `data %>% kk_fn(...)`.
- **Bare column names or strings** both work for column arguments — with four exceptions
  that take **strings only**: `kk_time_series()`, `kk_time_metrics()`, `kkonehot()` and the
  `univariate_*()` plots. Passing a bare name to those gives `object 'x' not found`.
- **`group_by()` is honoured (1.0.0+)**: pipe a grouped frame in and the analysis runs once
  per group, with the grouping columns prefixed to the result. This is real stratification,
  and it is the idiomatic way to get stratum-specific estimates:
  ```r
  d %>% group_by(site) %>% kk_twobytwo(expo, out)   # one row-block per site
  d %>% group_by(site, sex) %>% kk_coxph(t, ev, predictors = "age")
  ```
  Use `kk_stratified_2x2()` when you want a **pooled Mantel-Haenszel** estimate plus a
  homogeneity test; use `group_by()` when you want the strata reported separately.
  Plotting helpers and the column-wise utilities (`kkonehot()`, `mutate_round()`,
  `format_tibble()`) are deliberately group-agnostic.
- Modelling functions (`kk_reg`, `kk_coxph`, `kk_rate_reg`, `kk_rr_reg`, `kk_firth`) take
  the outcome/time/status as a name and `predictors = c("a", "b")` as a character vector,
  and return **univariate + multivariable** results in one tidy tibble.
- Most functions **return tibbles**; some attach extras as attributes (e.g. `kk_smd` →
  `attr(x, "group_labels")`, `kk_calibration_table` → `attr(x, "brier")`/`"hosmer_lemeshow"`,
  `kk_cuminc` → `attr(x, "gray_test")` (columns `cause`/`stat`/`pv`/`df`, not broom names),
  `kk_iptw` → `attr(x, "weights")`, `kk_dose_response` → `attr(x, "nonlinearity_test")`,
  `prop_trend_test` → `attr(x, "htest")`).
- **The quasi-experimental functions (1.3.0) put the design's diagnostic on an attribute,
  and it is not optional reading** — the point estimate alone cannot tell you whether the
  design holds: `kk_did` → `"cells"` (the raw 2×2 means), `kk_event_study` → `"pretrend"`
  (joint Wald test of the leads), `kk_rdd`/`kk_rkd` → `"bw_sensitivity"` + `"binned"`,
  `kk_its` → `"counterfactual"`, `"impact"`, `"diagnostics"`, `kk_synth` → `"weights"`,
  `"path"`, `"placebo"`, `kk_iv` → `"first_stage"`, `"diagnostics"`.
- **The modelling functions keep their fitted model (1.1.0+).** `kk_reg()`, `kk_coxph()`,
  `kk_rr_reg()`, `kk_rate_reg()` and `kk_firth()` stash the `lm`/`glm`/`coxph`/`brglmFit`
  they fitted; `kk_model()` gets it back. Never refit a model by hand to run `emmeans`,
  `anova()`, `predict()` or `marginaleffects` — pull it out instead:
  ```r
  fit <- kk_reg(d, sbp, c("age", "arm"))
  kk_emmeans(fit, ~ arm)                        # tidy tibble of adjusted means
  kk_emmeans(fit, ~ arm, contrast = "pairwise") # and the contrast
  emmeans::joint_tests(kk_model(fit))           # anything emmeans can do
  emmeans::emtrends(kk_model(fit), ~ arm, var = "age")
  ```
  `kk_model(fit, "univariate", "age")` for a single-predictor model,
  `kk_model(fit, group = "A")` when the fit came from grouped data, and
  `kk_model_data(fit)` if `emmeans` ever needs `data =` passed explicitly.
  Report **model-adjusted** marginal means in results tables rather than raw group means;
  that is the whole point of reaching for `emmeans` here.
- A handful return a **named list of tibbles** rather than one tibble:
  `kk_stratified_2x2()` (`pooled_or`, `pooled_rr`, `breslow_day`, `homogeneity_test`,
  `stratum_specific`), `kk_reclassification()` (`summary`, `reclass_events`, ...),
  `kk_apc()` (`rates`, `net_drift`, `models`), `kk_markov()`/`kk_partsa()`
  (`trace`, `summary`), `kk_did_staggered()` (`att_gt`, `dynamic`, `group`, `calendar`,
  `overall`). Reach into the element you need; don't expect one flat table.
- **Discrete colour scales `scale_fill_kk()` / `scale_colour_kk()` take no palette
  argument** — they read the registered anchors via `kk_pal()` and the number of levels
  from the data. Do **not** pass `kk_pal(...)` into them, and remember a `fill =`/`colour =`
  aesthetic must actually be mapped for the scale to do anything.

## Gotchas (verified against 1.0.0 — do not rediscover these)

**1. `kk_reg()` dispatches on the *class* of the outcome.** Pass a **factor** for logistic.
Since 1.0.0 an integer 0/1 outcome raises a clear error rather than silently fitting a
linear probability model, so this no longer produces a wrong published number — but the
requirement stands:

```r
kk_reg(d, "hcv_bin", "pwid")  # Error: outcome is numeric 0/1; convert with factor()
kk_reg(d, "hcv",     "pwid")  # outcome_type "binary", odds_ratio
```

Note the *opposite* requirement for `kk_rr_reg()`, which wants the numeric 0/1 column — so
a project reporting both aOR and aPR must carry both representations. Keeping `outcome`
(factor) and `outcome_bin` (integer) side by side in the analysis frame is the cheapest fix.

**2. `kk_reri()` needs numeric 0/1 exposures**, not factors, or it aborts with a message
about `names(coef(model))`. Build `pwid_b = as.integer(pwid == "Yes")` before fitting the
interaction model you hand it. (Still true in 1.0.0.)

**3. Two calibration functions, deliberately.** `kk_calibration(data, outcome, pred_prob,
g =)` returns a **one-row summary** (Hosmer-Lemeshow, O/E ratio, calibration slope and
intercept). `kk_calibration_table(data, truth, predicted, groups =)` returns the
**per-group table** for plotting a calibration curve, with `attr(x, "brier")` and
`attr(x, "hosmer_lemeshow")`. Before 1.0.0 these were two functions with the same name and
one silently shadowed the other, so older notes referring to `kk_calibration(df, truth =,
predicted =)` describe the shadowed version — use `kk_calibration_table()` for that call.

**4. `ggsave(..., type = "cairo")` is a no-op** — ragg wins and warns once per figure
(*"Using ragg device as default. Ignoring `type` and `antialias` argument"*). Drop the
`type` argument; `ggsave()` already goes through AGG, which is the RStudio default device
and renders the registered families correctly.

**5. Read kkstatfun attributes with `exact = TRUE`.** `attr()` partial-matches by default,
and several results carry attributes sharing a prefix (`model`, `models`, `model_data`,
`group_models`). `attr(x, "models")` on a grouped result silently returns `group_models` —
a list keyed by group, not the model list you asked for. Use
`attr(x, "models", exact = TRUE)`, or just call `kk_model()`, which handles it.

**6. Grouping vs. pooling is now your explicit choice, so make it deliberately.** Pre-1.0.0
code that relied on grouped input being ignored will change its numbers on upgrade. If you
want a single pooled estimate from stratified data, either don't group or use
`kk_stratified_2x2()` for a proper Mantel-Haenszel pooled OR with a homogeneity test.

### Fixed in 1.0.0 — ignore older notes claiming these are broken

These were real defects in 0.1.x/0.2.x and are commonly repeated in older project notes.
All are verified fixed; do **not** write workarounds for them:

- **Breslow-Day is correct.** The old bug returned BD ≈ 141, p ≈ 1e-32 for two nearly
  identical stratum ORs. Verified: stratum ORs 1.95 / 1.32 → BD = 0.67, p = 0.41.
- **Loop variables work.** `kk_rr_reg()`, `kk_firth()` and `kk_trend_test()` now accept a
  variable holding a column name; the `do.call(...)` splicing workaround is unnecessary.
- **`kk_trend_test()` no longer returns `NaN`** for the Cochran-Armitage statistic when a
  dose group has zero events.
- **`kk_firth()` does have a `model_type` column** (univariate + multivariable rows), so it
  can be filtered like `kk_reg()`/`kk_rr_reg()`.
- **patchwork merges the theme.** `plot_annotation(theme = ...)` after `set_plot_font()`
  no longer aborts with *"Only elements of the same class can be merged."*
- **`odds_ratio()` / `risk_ratio()` accept bare column names.** They previously only worked
  when the columns were literally named `exposure`/`outcome`.
- **`survival_plot()` works** (it called a non-existent `survival::survfit2`).
- **`kk_runs_test()` / `kk_random_seq()` accept `threshold = "mean"`.**
- **`compare_proportions_kk_glm()` returns `conf_low`/`conf_high`.** It silently omitted
  the CI columns entirely, which also broke `plot_proportion_comparisons()`.
- **`kk_confusion_matrix()`** accepts its documented `label`/`value` tibble input, and its
  MCC no longer overflows to `NA` on realistic counts (`boot = TRUE` no longer crashes).

## Function selection guide

### Measures of association & 2×2 tables
- `kk_twobytwo(data, exposure, outcome)` — full 2×2: OR, RR, RD, AF, PAF, PF, NNT, phi, Yule's Q/Y.
- `kk_epi_stats(data, exposure, outcome)` — quick OR + RR with CIs.
- `odds_ratio()` / `risk_ratio()` — single measure with CI.
- `kk_stratified_2x2(data, exposure, outcome, stratum)` — stratum ORs, CMH pooled OR, Breslow-Day.
- `kk_reri(model_or_data, exp1, exp2, outcome)` — additive interaction (RERI, AP, S).
- `kk_nnt(estimate, ...)` — NNT/NNH with ARR/RRR.
- `kk_trend_test(data, dose, outcome)` / `prop_trend_test()` — Cochran-Armitage trend.
- `kk_sensitivity_analysis(...)` — E-value / unmeasured-confounding sensitivity.

### Rates, standardization, APC & count/rate regression
- `kk_incidence_rate(data, cases, person_time, by=)` — rates + exact Poisson CIs, rate ratios.
- `kk_std_rates(data, count, pop, std_pop)` — **direct** standardization.
- `kk_smr(data, observed, pop, ref_rate)` — **indirect** standardization / SMR.
- `kk_apc(data, age, period, count, pop)` — age-period-cohort analysis (rates, drifts, estimable functions).
- `kk_rate_reg(data, outcome, predictors, person_time=)` / `kk_poisson()` — Poisson/NB IRR regression.
- `kk_rr_reg(data, outcome, predictors)` — modified-Poisson **adjusted risk ratios** (Zou, robust SE).

### Diagnostic tests, ROC, calibration & clinical utility
- `kk_diagnostic(data, truth, prediction)` / `diagnostic_summary()` — sens/spec/PPV/NPV/LR/AUC.
- `kk_confusion_matrix(x)` / `confusion_metrics_ci()` — full metric panel from TP/FP/FN/TN (+ bootstrap CIs).
- `kk_diagnostic_lrt(pre_test_prob, sensitivity, specificity)` — likelihood ratios + post-test probability (Fagan).
- `kk_roc(data, truth, predictor)` — AUC + DeLong CI + Youden cutoff.
- `kk_compare_roc(data, truth, p1, p2)` — DeLong test comparing two markers.
- `kk_calibration(data, outcome, pred_prob, g=)` — one-row calibration summary: Hosmer-Lemeshow, O/E ratio, calibration slope & intercept.
- `kk_calibration_table(data, truth, predicted, groups=)` — per-group calibration table for the calibration curve; Brier + HL as attributes.
- `kk_decision_curve(data, truth, predictor, thresholds=)` — decision-curve analysis / net benefit for a prediction model.

### Agreement & reliability
- `kk_kappa(data, r1, r2)` — Cohen's kappa. `kk_agreement(..., weights=)` — weighted kappa.
- `kk_icc(data, raters)` — intraclass correlation (continuous, 6 Shrout-Fleiss forms).
- `kk_bland_altman(m1, m2)` — limits of agreement. `kk_mcnemar(data, t1, t2)` — paired binary.
- `kk_reliability(data, items)` — Cronbach's alpha + per-item stats.

### Group comparisons, Table 1 & non-parametric
- `kk_table1(data, by, variables)` / `table1_summary()` — publication Table 1.
- `kk_compare_groups_table(data, group, variables)` / `compare_groups_table()` — two-group diffs, CIs, effect sizes (tidyselect + group_by aware).
- `kk_median_test()`, `kk_vdw_test()` — k-sample non-parametric. `kk_chisq_test()` — r×c chi-square + Cramér's V.
- `compare_proportions()` / `compare_proportions_by()` / `compare_proportions_kk_glm()` — proportion comparisons.
- `pcit(data)` — binomial CIs for proportions. `power_proportions()` — power/sample size.

### Regression
- `kk_reg(data, outcome, predictors)` / `regression_analysis()` (`krk_reg` alias) — auto linear/logistic/ordinal, uni + multivariable.
- `kk_firth(data, outcome, predictors)` — Firth-penalized logistic for separation / rare events.
- (see also `kk_rr_reg`, `kk_rate_reg` above for RR and rate models).
- `kk_model(x, which=, predictor=, group=)` — retrieve the fitted model from any of the above.
- `kk_emmeans(x, specs, contrast=, type=)` — tidy estimated marginal means / contrasts via `emmeans`; `type = "response"` back-transforms to ORs, RRs or probabilities. `kk_model_data(x)` — the fitting data.

### Survival & time-to-event
- `kk_survival_plot(data, time, status, group)` / `survival_plot()` — Kaplan-Meier with risk table.
- `kk_coxph(data, time, status, predictors)` — Cox HRs + Schoenfeld PH test.
- `kk_logrank(data, time, status, group)` — log-rank / Peto-Peto.
- `kk_rmst(data, time, status, group, tau)` — restricted mean survival time (use when PH fails).
- `kk_cuminc(data, time, status, group, cause)` — competing-risks CIF + Gray's test.

### Causal inference / confounding control
- `kk_iptw(data, treatment, outcome, covariates)` — propensity scores + IPTW effect.
- `kk_smd(data, treatment, variables)` / `kk_balance_table()` — standardized mean differences (balance).
- `kk_tmle(data, outcome, treatment, covariates)` — targeted maximum likelihood ATE (doubly robust).
- `kk_causal_mediation(data, exposure, mediator, outcome, confounders=)` — natural direct/indirect effects with bootstrap CIs.
- `kk_simex(model, variable, error_sd)` — SIMEX correction for measurement error in a fitted model.

### Quasi-experimental designs (1.3.0)
Everything in the section above identifies an effect by **adjusting for measured
confounders** and collapses if an important one is unmeasured. These identify it from the
**structure of how the intervention was assigned** — a threshold rule, a difference in
timing, an instrument, a donor pool — so they survive confounders nobody measured. What
they cannot survive is their own identifying assumption, which is why each returns the
diagnostic for it. **Report the diagnostic with the estimate, always.**

- `kk_did(data, outcome, treated, post, unit=, time=, covariates=, family=)` — difference-in-differences. The canonical 2×2 interaction, or two-way fixed effects when `unit`/`time` are given (standard errors then cluster on `unit`). Returns `unadjusted`, the raw 2×2 difference of cell means, beside the regression estimate — check they agree before adding covariates. `family = "poisson"/"binomial"` for counts and binary outcomes.
- `kk_event_study(data, outcome, unit, time, treat_time, leads=, lags=, reference=)` — one coefficient per period relative to adoption. **This is the parallel-trends check a 2×2 DiD cannot provide**: flat leads are the evidence, and `attr(x, "pretrend")` is the joint Wald test over them. Never-treated units are marked `Inf`/`NA` in `treat_time`.
- `kk_did_staggered(data, outcome, unit, time, treat_time, control=, n_boot=)` — Callaway-Sant'Anna group-time ATT. **Use this, not `kk_did()`, whenever units adopt at different times**: two-way fixed effects then uses already-treated units as controls, and with a time-varying effect the estimate can shrink to zero or flip sign. Returns `att_gt`, `dynamic` (event study by time since adoption), `group`, `calendar`, `overall`; `band.low`/`band.high` in `att_gt` are simultaneous bands — use those when reading a table of many cells for the largest one.
- `kk_rdd(data, outcome, running, cutoff=, treatment=, bandwidth=, kernel=, degree=)` — regression discontinuity. Sharp by default; passing `treatment` makes it fuzzy (2SLS on the cutoff indicator, estimand = complier effect at the cutoff). Bandwidth defaults to Imbens-Kalyanaraman. The estimate is local to the cutoff and licenses no claim about units far from it.
- `kk_rkd(...)` — regression kink design: the effect from a **change in slope** at the threshold rather than a jump, as when a benefit formula bends at an income limit. Noisier than an RD — supply your own `bandwidth` rather than trusting the rule of thumb.
- `kk_rd_density(data, running, cutoff=)` — McCrary manipulation test. **Run it with every `kk_rdd()`.** A significant jump in the density means units sorted themselves across the cutoff and the design has failed; a non-significant one is a precondition for reading the RD estimate causally.
- `kk_its(data, outcome, time, intervention_time, offset=, family=, harmonic=, period=, transition=)` — interrupted time series. Separates the immediate `level_change` from the `trend_change`; Newey-West errors by default because serial correlation is how ITS analyses go wrong. `attr(x, "counterfactual")` is plot-ready, `attr(x, "impact")` gives the cumulative effect with an interval.
- `kk_synth(data, outcome, unit, time, treated_unit, intervention_time, predictors=)` — synthetic control for **one** treated unit against a donor pool. Weights are non-negative and sum to one, so it interpolates rather than extrapolates. Read `attr(x, "weights")` before the estimate: one donor at 0.9 is a two-unit comparison in disguise. The placebo p-value cannot go below `1/(1 + n_donors)`.
- `kk_iv(data, outcome, treatment, instruments, covariates=, cluster=)` — 2SLS. Estimand is the **complier** effect (LATE), not the population ATE. Returns `ols` for comparison, `first_stage_f` (below 10 = weak), and the Anderson-Rubin interval `ar.low`/`ar.high`, which stays valid however weak the instrument — quote that one when `weak_instrument` is `TRUE`.
- `kk_ancova(data, outcome, treatment, baseline, covariates=)` — pretest-posttest baseline adjustment. Beats both the comparison of follow-up means and the comparison of change scores, and is robust to regression to the mean; returns `change_score` and `posttest_only` beside it so the difference is visible.

**Choosing between them.** Treated and control groups, before and after → `kk_did()`
(→ `kk_event_study()` for the pre-trend check). Units adopting at different times →
`kk_did_staggered()`. One series, no control group → `kk_its()`. One treated unit, several
comparators → `kk_synth()`. Assignment by a threshold → `kk_rdd()` (+ `kk_rd_density()`).
Non-compliance or an encouragement/lottery/natural instrument → `kk_iv()`. A randomised
or observational pre-post measurement on the same people → `kk_ancova()`.

### Infectious-disease dynamics & surveillance
- `kk_seir(beta, gamma, sigma=, S0, I0, ...)` — deterministic SIR/SEIR compartmental model (add `sigma` for SEIR; `mu`/`nu` for demography).
- `kk_r0(method=c("params","growth","final_size"), ...)` — basic reproduction number from parameters, growth rate, or attack rate.
- `kk_final_size(R0)` — final epidemic size from R0.
- `kk_sero_incidence(data, age, positive, method=c("cohort","catalytic"))` — force of infection / incidence from seroprevalence surveys.
- `kk_nb_scan(data, region, time, count, expected)` — negative-binomial space-time scan statistic (cluster detection).
- `kk_gsf(model_fn, theta, times)` — generalized sensitivity functions (parameter identifiability for ODE/inverse problems).

### Health-economic evaluation
- `kk_icer(data, cost, effect=, strategy=)` — ICERs + efficiency frontier (dominance / extended dominance).
- `kk_nmb(data, cost, effect=, wtp=)` — net monetary & net health benefit at a willingness-to-pay threshold.
- `kk_ceac(data, sim, strategy, cost, effect, wtp=)` — cost-effectiveness acceptability curve from PSA draws.
- `kk_evpi(data, sim, ...)` — expected value of perfect information.
- `kk_markov(transition, costs, utilities, cycles, ...)` — Markov cohort model (half-cycle correction, discounting).
- `kk_partsa(pfs, os, times, state_costs, state_utilities, ...)` — partitioned survival model (oncology 3-state).
- `kk_discount(x, rate=0.03, times=)` — discount a stream of costs or effects.

### Cycles, rhythms & events in one long record (1.4.0, after CycleLab / Stefanov 2026)
One series measured over time (glucose sensor, temperature, BP diary, monthly admissions). Credit **CycleLab (Stefanov R, 2026, https://cyclelab.raredis.org)** and the **T-R periodogram of B. Komitov** when reporting. Start with `kk_cycle_analysis()` (runs everything that suits the data, writes `methods_text`/`results_text`, lists established vs not established); then the single steps:
- **Describe / trend:** `kk_ts_describe()` (distribution, gaps, outliers, moving average); `kk_ts_trend(form = )` — effective-N trend test for the line and 15 two-parameter forms (+ `"verhulst"`); `p.value` is the test of *no trend* on the linearising scale, `test` names the null (`A = 1` for `exponential_base`). Never pick the form by R² alone.
- **Cycles of unknown period:** `kk_periodogram()` — T-R periodogram + max-T test against AR(1) red noise (pilot + independent reference surrogates; keep `n_sim = 500` for reporting). `R_SR >= 1.96` is only a heuristic; `p_fixed` is the exact F test for a period chosen in advance. `kk_harmonic_model(periods = )` — trend + cycles, PrIdx, rolling-origin check vs naive (report a forecast only if `better_than_naive`). `kk_cycle_power()` — how long/how often to measure.
- **Daily rhythm (known period):** `kk_cosinor()` (MESOR, amplitude, acrophase; HAC SEs; `by_day = TRUE` with a coverage rule), `kk_cosinor_population()` (subjects or days as units), `kk_cosinor_compare(group = )` — `method = "units"` (coefficient-level, Bingham; default when ≥3 days/subjects per group) or `"pooled"`. Acrophase limits are `NA` when no rhythm is detected — do not report a peak time then. `kk_day_parts()` — morning/midday/evening/night with day as block.
- **Events:** `kk_epoch_response(data, y, time, events, event_time, event_type = , covariates = )` — superposed epochs with clock-matched controls; `attr(, "summary")$difference` / `p.adjusted` compare events **with controls** (never test a rise-to-peak against 0: a maximum is positive in pure noise). `kk_distributed_lag()` (lagged effect of a dose/exposure; needs gap-free regular data), `kk_its()` for one lasting intervention.
- **N-of-1:** `kk_nof1_schedule()` (block randomisation; record the seed) → `kk_nof1()` (paired t + exact randomisation test).
- **Several series:** `kk_ts_compare()` (prewhitened CCF, Granger with common trend/rhythm, joint-covariance phase shift, common cycles, coherence), `kk_wavelet()`, `kk_wavelet_coherence()` (pointwise significance; regular data only).
- **Plots** (kkplot style, finding as title): `kk_plot_periodogram()`, `kk_plot_cosinor(type = "fit"|"ellipse")`, `kk_plot_epoch()`, `kk_plot_wavelet()`, `kk_plot_model()`, `kk_plot_day_parts()`. **Examples with known truth:** `kk_cycle_example("hypertension"|"pneumonia"|"emergency"|"temperature"|"nurse"|"prediabetes"|"nof1"|"chronobiology"|"subjects")`. Guide: `vignette("cycles-and-rhythms")`.
- Time policy: POSIXct clock time keeps the UTC offset of the first measurement (no DST duplicates); row-offset methods stop on gaps — fill/interpolate first.

### Correlations, randomness, time series, misc
- `kk_fullcorplot()`, `kk_compare_independent_correlations()`, `kk_compare_dependent_correlations()`.
- `kk_runs_test()` (`kk_random_seq` alias), `kk_frequency_test()`, `kk_mssd_test()`, `kk_butler_ks()` — sequence randomness / serial independence / symmetry.
- `kk_time_series()` / `time_series_analysis()`, `kk_time_metrics()` — surveillance time series.
- `kk_summary()` / `comprehensive_summary()` — rich descriptive stats (group_by aware).
- `extract_egn_info()` / `extract_age_from_egn()` — Bulgarian EGN → DOB, sex, age, region.
- Wrangling: `kkonehot()` / `one_hot_encode()`, `mutate_round()`, `format_tibble()`.

### Plotting & theming
- `kkplot()` — themed `ggplot()` starting point; `univariate_plot()` (+ `univariate_cat_plot()` / `univariate_cont_plot()` and their `_categorical_`/`_continuous_` aliases).
- `kk_risk_plot(data)` — forest-style plot of risk/effect estimates. `plot_proportion_comparisons(results)` — plot `compare_proportions*()` output.
- `set_plot_font(family, size=)` — register the default ggplot font. `set_plot_colors(colors, scheme=, n=, space=)` — register **1–12** anchor "flag" colours as the default discrete fill/colour (and, with `continuous=TRUE`, the default gradient). `scheme=` first expands the seed(s) into a derived palette via `kk_gen_palettes()`; `scheme=NULL` (default) registers the colours as given.
- `kk_pal(n, colors=)` — build `n` colours from the anchors (interpolated when `n` exceeds the anchor count).
- `kk_gen_palettes(colors, n=, space=, plot=)` — derive a catalogue of named palettes from 1–12 seeds: ramps (`sequential`, `monochromatic`, `tints`, `shades`) and qualitative colour-theory schemes (`analogous`, `complementary`, `split_complementary`, `triadic`, `tetradic`, `spectral`), plus `custom` for multi-seed input. Ramps suit ordered/continuous quantities; qualitative schemes suit unordered categories. `kk_show_palettes(pals)` — preview them as labelled hex swatches.
- **`space=`** (1.2.0) — `"hsv"` (default, unchanged output) or `"oklch"`, which builds the schemes in perceptually uniform OKLCh: hue rotations hold apparent lightness, ramps are perceptually even, and out-of-gamut colours are gamut-mapped by reducing chroma instead of clipping RGB. Prefer `"oklch"` for new figures. Expect OKLCh output from a **dark, low-chroma seed** (e.g. `#003049`) to look muted — it matches the seed's lightness and chroma rather than jumping to a vivid hue. That is correct, not a bug.
- `scale_fill_kk()` / `scale_colour_kk()` — apply the discrete flag palette to one plot (**no arguments**). `scale_fill_kk_c()` / `scale_colour_kk_c()` — continuous gradient from the anchors (heatmaps, density fills).

### Figure accessibility & colour vision (1.2.0)
Check a palette **before** building the figure — journals and university accessibility
policies increasingly require it, and ~8% of men have a red–green deficiency.
- `kk_pal_check(colors, background=, min_dist=, type=, severity=)` — the one to reach for. One row per colour × vision type: `simulated` hex, WCAG `contrast` against the background, `graphic` (≥3:1), `nearest` palette colour and `min_dist` (OKLab ΔE) to it, `distinct` (≥ `min_dist`, default 0.10). **An empty `filter(!distinct | !graphic)` means it passes.**
- `kk_show_cvd(colors, type=, severity=)` — swatch grid, one row per vision type. The visual companion to the audit.
- `kk_cvd(colors, type=, severity=)` — simulate `"deutan"`, `"protan"`, `"tritan"`, `"achroma"` (Machado et al. 2009, applied in linear RGB). `severity=` 0–1; values below 1 give anomalous trichromacy, the common mild form. Returns a tibble, **not** a colour vector.
- `kk_pal_safe(n, seed_colors=, background=, min_contrast=, type=)` — build a categorical palette distinguishable under normal + deutan + protan + tritan simultaneously (deterministic, so figures stay reproducible). `seed_colors=` pins institutional colours at the front. Achieved separation in `attr(x, "min_dist")`.
- `kk_contrast(colors, background=)` — WCAG 2.1 ratios with AA/AAA flags (4.5 text, 3 large text and graphical objects, 7 AAA). `kk_color_convert(colors)` — OKLab/OKLCh coordinates + relative luminance.

**Gotchas.** `kk_cvd()`/`kk_pal_check()` take **colour vectors, not ggplot objects** — they
cannot simulate a rendered figure. `kk_pal_check()` defaults to all five vision types
including `achroma`, while `kk_pal_safe()` optimises for four; auditing a safe palette with
the default `type` will therefore show greyscale failures. That is expected — separating on
lightness alone caps a palette at three or four colours. For photocopy-safe figures either
ask for fewer categories with `type = c("normal","deutan","protan","tritan","achroma")`, or
keep the colours and add a redundant `linetype`/`shape`/direct-label cue.

**The default flag palette fails its own audit**: `#F77F00` sits at 2.6:1 on white (under
the 3:1 graphical threshold), and `#D62828`/`#F77F00` fall to ΔE 0.155 under deuteranopia.
Fine for a 2-colour figure using navy vs one other; for 3+ categories prefer
`kk_pal_safe(n, seed_colors = "#003049")`.

## Typical workflow

1. **Describe**: `kk_summary()` for continuous vars; `kk_table1()` for baseline by group.
2. **Associate**: `kk_twobytwo()` / `kk_epi_stats()`; stratify with `kk_stratified_2x2()`.
3. **Model**: `kk_reg()` (OR/coef), `kk_rr_reg()` (adjusted RR), `kk_rate_reg()` (IRR),
   `kk_coxph()` (HR). For rare events / separation use `kk_firth()`. Then report
   **adjusted marginal means and contrasts** with `kk_emmeans(fit, ~ group)` rather than
   raw group means — the fitted model is already stored, so nothing is refitted.
4. **Validate a prediction model**: `kk_roc()` (discrimination) + `kk_calibration()`
   (calibration statistics) or `kk_calibration_table()` (curve) + `kk_decision_curve()`
   (net benefit). Compare two models with `kk_reclassification()` (NRI/IDI).
5. **Confounding**: `kk_iptw()` / `kk_smd()` to weight and check balance; for doubly robust
   effects use `kk_tmle()`; decompose pathways with `kk_causal_mediation()`.
5b. **Evaluating an intervention or policy rather than an exposure**: pick the design from
   how the intervention was assigned, not from the outcome type — `kk_did()` +
   `kk_event_study()`, `kk_did_staggered()` under staggered rollout, `kk_its()` for a
   single series, `kk_synth()` for one treated unit, `kk_rdd()` + `kk_rd_density()` for a
   threshold rule, `kk_iv()` for non-compliance. Report the design's diagnostic alongside
   the estimate: leads, bandwidth sensitivity, manipulation test, first-stage F, placebo
   distribution. An estimate published without it is not evidence, it is an assertion.
6. **Survival**: `kk_survival_plot()` + `kk_logrank()`; `kk_coxph()` and check its Schoenfeld PH
   test — if PH fails, report `kk_rmst()`; for competing events use `kk_cuminc()`.
7. **Agreement/reliability**: `kk_kappa()`/`kk_icc()`/`kk_bland_altman()`; scales via `kk_reliability()`.
8. **Transmission modelling**: `kk_r0()` → `kk_seir()` for the epidemic curve, `kk_final_size()`
   for the attack rate; estimate force of infection from serosurveys with `kk_sero_incidence()`.
9. **Health-economic evaluation**: `kk_markov()` / `kk_partsa()` for the model, `kk_discount()`
   for time preference, `kk_icer()` + `kk_nmb()` for the base case, then `kk_ceac()` / `kk_evpi()`
   over PSA draws.
10. **Before the figures ship**: audit the palette with `kk_pal_check()` (empty
    `filter(!distinct | !graphic)` = pass). If a manuscript figure separates groups by colour
    alone, build the palette with `kk_pal_safe(n, seed_colors = ...)` instead of hand-picking.

## Notes
- If `kkstatfun` is not installed, offer `pak::pak("kostadinoff/kkstatfun")`.
- If a requested analysis has no `kk_*` equivalent, use the appropriate specialized package,
  but keep the tidy, pipe-friendly, tibble-returning style consistent with kkstatfun.
- Cite with `citation("kkstatfun")`. The Zenodo concept DOI
  [10.5281/zenodo.18936019](https://doi.org/10.5281/zenodo.18936019) always resolves to the
  latest release; take a version-specific DOI from the record when a manuscript needs to
  pin the exact build.
- `kk_cuminc()`'s Gray test needs **cmprsk** installed; without it the CIF is still
  returned but `attr(x, "gray_test")` is absent. Remember each R version has its own
  library — install into the R that runs the analysis.
- As of 1.0.0 the package passes `R CMD check` with 0 errors and 0 warnings, and every one
  of its exported functions (170 as of 1.4.0) is covered by the test suite. If a `kk_*` call
  errors, the first hypothesis should be a stale installed build, not a package bug.
