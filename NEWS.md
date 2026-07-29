# kkstatfun 1.2.0

## Colour science for accessible figures (`R/color_science.R`)

New functions for checking that a figure's colours survive print, greyscale and
colour-blind readers — the checks journals and accessibility policies
increasingly ask for.

* `kk_cvd()` simulates colour vision deficiency (deuteranopia, protanopia,
  tritanopia, achromatopsia) using the physiologically-based transforms of
  Machado, Oliveira & Fernandes (2009), applied in linear-light RGB with
  interpolated severity for anomalous trichromacy. Output is identical to
  `colorspace::deutan()` and friends, without taking the extra dependency.
* `kk_show_cvd()` previews a palette as a swatch grid, one row per vision type,
  so categories that collapse into one another are obvious at a glance.
* `kk_pal_check()` audits a whole palette: for each colour and each vision type
  it reports the perceptual distance (OKLab delta-E) to the nearest other
  colour, plus WCAG contrast against the plotting background.
* `kk_pal_safe()` builds a categorical palette that stays distinguishable for
  normal, deuteranopic, protanopic *and* tritanopic vision simultaneously, by
  deterministic farthest-point sampling in OKLab. It can be anchored on your own
  institutional colours via `seed_colors`.
* `kk_contrast()` gives WCAG 2.1 contrast ratios with the AA/AAA flags, and
  `kk_color_convert()` exposes OKLab/OKLCh coordinates and relative luminance.

## Perceptually uniform palette generation

* `kk_gen_palettes()` and `set_plot_colors()` gain `space = "oklch"`, which
  builds the colour-theory schemes in the perceptually uniform OKLCh space
  instead of HSV. Hue rotations then preserve apparent lightness (the HSV
  triadic turns `#D62828` into a far brighter `#28D628`; the OKLCh one gives
  `#008F33` at matched lightness), lightness ramps are evenly spaced to the eye,
  and out-of-gamut colours are mapped by reducing chroma rather than clipping
  RGB, which shifts hue.
* The default remains `space = "hsv"`, so palettes in existing scripts and
  published figures do not change. `"oklch"` is the better choice for new work.

# kkstatfun 1.1.0

Zenodo DOI for this exact version: [10.5281/zenodo.21629660](https://doi.org/10.5281/zenodo.21629660).

`R CMD check` now passes with **0 errors, 0 warnings and 0 notes**.

## Breaking change

**`library(kkstatfun)` no longer attaches the tidyverse.** The 19 packages
previously listed under `Depends:` moved to `Imports:` (those the package
actually uses) or `Suggests:` (those it never did). Attach what your scripts
need, as usual:

```r
library(kkstatfun)
library(dplyr)     # and ggplot2, survival, ... as needed
```

`%>%` is still re-exported by kkstatfun, so the pipe keeps working on its own.

This also fixes a latent bug rather than being pure tidying. `Depends:` only
attaches on `library()`, but a namespace can be *loaded without being attached*
— which is exactly what `kkstatfun::kk_coxph(...)` does. On that path nothing
was attached, so the package's own bare calls had nothing to resolve against.
It happened to work only when the caller had already attached those packages.
Verified: every exported function now runs from a completely bare search path.

Two consequences of the move, both fixed here:

* `survival::clogit()` builds a `coxph()`/`Surv()` call and evaluates it in the
  caller's namespace, so `kk_matched_case_control()` and `kk_case_crossover()`
  need those names imported. Results are numerically unchanged (verified
  against `clogit()` directly).
* Examples that used `aes()` and the `faithfuld` dataset bare now attach
  ggplot2 explicitly.

Packages moved to `Suggests:` because the package never called them:
`easystats`, `haven`, `marginaleffects`, `modelsummary`, `readxl`, `rstatix`,
`tidymodels`. Install them yourself if your scripts use them.

## New features — emmeans support

The modelling functions previously discarded the model they fitted, so
estimated marginal means meant refitting by hand. They now keep it.

* **`kk_model(x)`** returns the fitted model from `kk_reg()`, `kk_coxph()`,
  `kk_rr_reg()`, `kk_rate_reg()` and `kk_firth()`. Because these are ordinary
  `lm`/`glm`/`coxph`/`brglmFit` objects, every `emmeans` verb works unchanged
  — as do `anova()`, `predict()`, `plot()` and `marginaleffects`.

  ```r
  fit <- kk_reg(d, sbp, c("age", "arm"))
  emmeans::emmeans(kk_model(fit), ~ arm)
  emmeans::joint_tests(kk_model(fit))
  emmeans::emtrends(kk_model(fit), ~ arm, var = "age")
  ```

  `kk_model(fit, "univariate", "age")` gets a single-predictor model;
  `kk_model(fit, "all")` the full named list. For results built from grouped
  data there is one model per group: `kk_model(fit, group = "A")`.

* **`kk_emmeans(x, specs)`** is a tidy wrapper: it runs `emmeans` on the stored
  model and returns a **tibble**, keeping the package's tidy-out contract.
  `contrast = "pairwise"` (or any `emmeans` method) computes contrasts in the
  same call. Confidence limits are always named `conf.low`/`conf.high`,
  normalising over `emmeans`' own `lower.CL` vs `asymp.LCL` split. The
  underlying `emmGrid` is kept as `attr(x, "emmGrid")`.

* **`kk_model_data(x)`** returns the data the models were fitted to, for the
  rare case where `emmeans` needs it passed explicitly.

No `recover_data()`/`emm_basis()` methods were needed: kkstatfun wraps model
classes `emmeans` already supports rather than defining new ones. Estimated
marginal means are verified identical to those from a hand-fitted model.

## Bug fix

* Results built from grouped data silently inherited the **first group's**
  fitted model through `bind_rows()`, so it could masquerade as an overall fit.
  Per-group models are now kept separately under `attr(x, "group_models")` and
  reached with `kk_model(x, group = )`. The root cause was `attr()`'s default
  *partial* matching: `attr(x, "models")` was resolving to `models_by_group`.
  All internal attribute reads now use `exact = TRUE`, and the attribute was
  renamed so the prefix cannot collide for users either.

## Testing

* Coverage holds at **100% of exported functions** (129 of 129, up from 126),
  now **323 assertions** across 6 files.
* New `test-emmeans.R`: model retrieval for all five modelling families,
  `emmeans` on each, downstream verbs, tidy-output and CI-name normalisation,
  per-group models, and the leak regression above.

# kkstatfun 1.0.1

Zenodo DOI for this exact version: [10.5281/zenodo.21599749](https://doi.org/10.5281/zenodo.21599749).

Metadata-only release. **This is the version to cite** — no code, documentation or
test changes from 1.0.0.

* Set the maintainer address to `drkostadinkostadinov@gmail.com`. 1.0.0 shipped with a
  `kostadinov@example.com` placeholder, and Zenodo archives are immutable, so the
  correction needed its own release.
* Added `.zenodo.json`. The 1.0.0 Zenodo record was created with Zenodo's default
  licence, **CC BY 4.0**, which contradicts the package's **MIT** licence — a real
  inconsistency on a citable software artifact. `.zenodo.json` pins the licence, author
  name and affiliation, `upload_type` and keywords for this and future releases. It is
  `.Rbuildignore`d and does not ship in the tarball.

The concept DOI [10.5281/zenodo.18936019](https://doi.org/10.5281/zenodo.18936019) always
resolves to the newest release, so the README badge needs no change.

# kkstatfun 1.0.0

Zenodo DOI for this exact version: [10.5281/zenodo.21599314](https://doi.org/10.5281/zenodo.21599314).
Superseded by 1.0.1 for citation purposes (placeholder maintainer email, and the record
is labelled CC BY 4.0 rather than MIT).

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
