# kkstatfun 1.0.0

First stable release. This version makes `group_by()` support real across the
package, fixes eight bugs, and brings the package to a clean `R CMD check`
(0 errors, 0 warnings) with the test suite wired into `check`.

## Breaking changes

Three changes alter results or return types from 0.2.9. All three are
corrections; scripts written against the old behaviour should be re-run.

* **`group_by()` is now honoured instead of silently ignored.** Previously,
  piping a grouped data frame into most analysis functions returned the
  **pooled** estimate with no error and no warning — a confounded result that
  looked like a stratified one. It now runs the analysis once per group and
  prefixes the grouping columns.

  ```r
  # 0.2.9: OR 3.16 (pooled, grouping discarded)
  # 1.0.0: site A OR 9.33, site B OR 0.96
  d %>% group_by(site) %>% kk_twobytwo(expo, out)
  ```

  59 functions gained group support, plus the aliases `kk_balance_table()`,
  `kk_poisson()`, `krk_reg()`, `regression_analysis()` and
  `time_series_analysis()`. Plotting helpers and the column-wise utilities
  (`kkonehot()`, `mutate_round()`, `format_tibble()`) remain group-agnostic by
  design.

* **`prop_trend_test()` returns a tibble**, not a base `htest`. The original
  object is retained as `attr(x, "htest")` for anyone who needs it.

* **`set_plot_font()` default `search_sources` is now
  `c("system", "local", "google")`** (was `c("google", "system", "local")`).
  The system and local sources yield fonts that the AGG/ragg device can use
  directly; Google Fonts is tried last. See "Font resolution" below.

* **`kk_calibration()` no longer has two conflicting definitions.** The
  implementation that returns the per-group calibration table (with the Brier
  score and Hosmer-Lemeshow test as attributes) is now exported as
  **`kk_calibration_table()`**. `kk_calibration()` keeps the one-row summary
  form (Hosmer-Lemeshow, O/E ratio, calibration slope and intercept) that the
  README and tests already used.

## Bug fixes

* `kk_calibration()` was defined twice, in `model_validation.R` and
  `reclassification.R`. R sources files alphabetically, so the second
  definition silently shadowed the first and the documented
  `kk_calibration(df, truth = , predicted = )` call errored. Split into two
  functions (see above).
* `survival_plot()` was entirely non-functional — it called
  `survival::survfit2()`, which does not exist (`survfit2` is from
  **ggsurvfit**).
* `kk_runs_test()` and `kk_random_seq()` errored with `threshold = "mean"`,
  which called the non-existent `stats::mean` instead of `base::mean`.
* `compare_proportions_kk_glm()` silently returned **no confidence intervals**.
  `emmeans` names its interval columns `asymp.LCL`/`asymp.UCL` for a GLM, not
  `lower.CL`/`upper.CL`, and `tibble()` dropped the resulting `NULL`s without
  complaint. This also broke its documented companion
  `plot_proportion_comparisons()`. The extraction now accepts either naming and
  errors loudly if neither is present.
* `compare_proportions_kk_glm()` rejected a bare column name for `by`
  (e.g. `by = strat`), because `is.null(by)` forced the promise before
  `ensym()` could capture it.
* `kk_confusion_matrix()`'s Matthews correlation coefficient overflowed to `NA`
  for realistic epidemiological counts — the denominator multiplied all four
  marginals. It now multiplies their square roots. The same overflow crashed
  `boot = TRUE` outright.
* `kk_confusion_matrix()` rejected its own documented `label`/`value` tibble
  input: a lower-case-only character-class strip ran before `tolower()`, erasing
  upper-case labels such as `"TP"`.
* `odds_ratio()` and `risk_ratio()` rejected bare column names, only working
  when the column happened to be named `exposure`/`outcome`. They now forward
  captured expressions to `kk_twobytwo()`.
* `kk_compare_independent_correlations()` gained a default for `data`, so the
  documented keyword form no longer errors on a missing argument.

## Font resolution

Two font registries exist and are not interchangeable: AGG (`ragg`) and cairo
resolve families through **systemfonts**, while `showtext` uses **sysfonts**.
`set_plot_font()` previously checked only sysfonts, with two consequences:

* The `"system"` source could **never** find an installed OS font —
  `sysfonts::font_families()` returns only `sans`, `serif`, `mono` plus explicit
  additions — so it silently fell back to Arial even with the requested font
  installed.
* The `"google"` source reported success for fonts AGG cannot see. With
  `enable_showtext = FALSE` (the default), AGG silently substituted a default
  face while the function printed a checkmark.

Both are fixed. System fonts are now checked against `systemfonts`, and a
resolved family is registered with **both** registries, so Google fonts are
genuinely available to AGG. If a font cannot be mirrored into systemfonts, the
function now says so rather than claiming success.

## Testing

* Added the missing `tests/testthat.R`. The suite previously never ran under
  `R CMD check` at all.
* Coverage went from 11 of 126 exported functions (9%) to **126 of 126 (100%)**,
  and from 29 to 265 assertions across 5 files.
* New files: `test-regressions.R` (one test per bug above, each failing on the
  old code), `test-group-by.R`, `test-epi-measures.R` (numeric validation
  against `mantelhaen.test`, `coxph`, `survdiff`, `pROC`, `psych`, `cmprsk`),
  `test-smoke-all-exports.R`, and `helper-fixtures.R`.

## Documentation

* Six exported functions had no help page at all
  (`kk_dose_response`, `kk_std_rates_ci`, `kk_survival_nnt`,
  `comprehensive_summary`, `extract_egn_info`, `one_hot_encode`); `man/` was
  stale relative to `R/`. All regenerated.
* Fixed 17 malformed Rd math macros (`$\chi$` and friends used outside
  `\eqn{}`), which rendered broken in `?help`.
* `\dontrun{}` reduced from 34 examples to 3 (only `kk_setup()`,
  `set_plot_font()` and the scale helpers, which need global state or network
  access). `R CMD check` now actually executes the rest — two were broken and
  are fixed.
* `kk_table1()` had no documented parameters; `kk_bland_altman()` documented
  three arguments that are absorbed by `...`.

## Packaging

* `R CMD check`: **0 errors, 0 warnings**, 2 notes (down from 7 warnings and
  3 notes).
* Added the missing `LICENSE` file referenced by `License: MIT + file LICENSE`.
* Moved `CITATION.bib` to the standard `inst/CITATION`, so `citation("kkstatfun")`
  works and reports the installed version.
* Removed 9 declared but unused `Imports` (`MKinfer`, `brms`, `epiR`,
  `epitools`, `finalfit`, `forecast`, `ggstats`, `monochromeR`, `quantreg`);
  declared `tidyselect`, and `ragg`/`systemfonts`/`cmprsk` in `Suggests`.
* Escaped all non-ASCII characters in R code (portability warning).
* Declared missing base imports and NSE column names, clearing ~120
  "no visible binding" notes.
* `.Rbuildignore` now excludes `scratch/`, `examples/`, `README.html` and the
  rendered sources, which were being shipped in the tarball.

## Known limitations

* `Depends:` still attaches 19 packages, which `R CMD check` notes as
  excessive. Moving them to `Imports:` is correct R practice but would stop
  `library(kkstatfun)` from attaching the tidyverse for you — deferred as a
  deliberate design decision.
* Argument conventions are not fully uniform: `kk_time_series()`,
  `kk_time_metrics()`, `kkonehot()` and the `univariate_*()` plots take column
  names as **strings**, while the rest of the package takes bare names.
* `attr(kk_cuminc(...), "gray_test")` uses the column names `stat`/`pv`/`df`
  rather than the broom-style `statistic`/`p.value` used elsewhere.
