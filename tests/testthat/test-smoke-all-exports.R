library(dplyr)

# Smoke coverage: every exported function is called at least once with valid
# input and must return without error. This is the safety net that catches
# broken call paths such as survival::survfit2 or stats::mean.

d <- kk_fixture_cohort()
psa <- kk_fixture_psa()

test_that("epidemiological measures run", {
  expect_s3_class(kk_twobytwo(d, expo, event), "tbl_df")
  expect_s3_class(odds_ratio(d, expo, event), "tbl_df")
  expect_s3_class(risk_ratio(d, expo, event), "tbl_df")
  expect_s3_class(kk_epi_stats(d, expo, event), "tbl_df")
  expect_type(kk_stratified_2x2(d, expo, event, sex), "list")
  expect_s3_class(kk_paf(d, event, expo), "tbl_df")
  expect_s3_class(kk_trend_test(d, event, dose), "tbl_df")
  expect_s3_class(prop_trend_test(d, event, group = dose), "tbl_df")
  expect_s3_class(kk_incidence_rate(d, event, person_time), "tbl_df")
  expect_s3_class(kk_nnt(0.12, type = "risk_diff"), "tbl_df")
  expect_s3_class(kk_sensitivity_analysis(or_observed = 2.5, p_bias = 0.2), "tbl_df")

  mcc <- d %>% mutate(set_id = rep(seq_len(n() / 2), each = 2),
                      case = rep(c(1, 0), n() / 2))
  expect_s3_class(kk_matched_case_control(mcc, set_id, case, expo), "tbl_df")
  cco <- d %>% mutate(id = rep(seq_len(n() / 2), each = 2),
                      ev2 = rep(c(1, 0), n() / 2))
  expect_s3_class(kk_case_crossover(cco, id, ev2, expo), "tbl_df")
})

test_that("standardisation and rate regression run", {
  agg <- tibble(
    age_group = c("0-14", "15-64", "65+"),
    cases = c(2, 45, 120), pop = c(10000, 50000, 15000),
    std_pop = c(12000, 60000, 20000), ref_rate = c(0.0002, 0.0009, 0.008)
  )
  expect_s3_class(kk_std_rates(agg, cases, pop, std_pop), "tbl_df")
  expect_s3_class(kk_std_rates_ci(agg, cases, pop, std_pop), "tbl_df")
  expect_s3_class(kk_smr(agg, cases, pop, ref_rate), "tbl_df")
  expect_s3_class(kk_rate_reg(d, event, c("age", "arm"), person_time = person_time), "tbl_df")
  expect_s3_class(kk_poisson(d, event, c("age"), person_time = person_time), "tbl_df")
  expect_s3_class(kk_rr_reg(d, event, c("expo", "age")), "tbl_df")
  apc <- kk_apc(
    tibble(age = rep(seq(20, 60, 10), 4), period = rep(seq(2000, 2015, 5), each = 5),
           count = stats::rpois(20, 30), pop = 10000),
    age, period, count, pop
  )
  expect_type(apc, "list")
  expect_named(apc, c("rates", "net_drift", "models"))
})

test_that("diagnostic accuracy and agreement run", {
  expect_s3_class(kk_diagnostic(d, event, expo), "tbl_df")
  expect_s3_class(diagnostic_summary(d, event, expo), "tbl_df")
  expect_s3_class(kk_roc(d, event, p_new), "tbl_df")
  expect_s3_class(kk_compare_roc(d, event, p_old, p_new), "tbl_df")
  expect_s3_class(kk_calibration(d, event, p_new, g = 5), "tbl_df")
  expect_s3_class(kk_calibration_table(d, event, p_new, groups = 5), "tbl_df")
  expect_s3_class(kk_decision_curve(d, event, p_new), "tbl_df")
  expect_s3_class(kk_diagnostic_lrt(0.2, 0.85, 0.95), "tbl_df")
  expect_s3_class(kk_confusion_matrix(c(tp = 85, fp = 10, fn = 15, tn = 890)), "tbl_df")
  expect_s3_class(confusion_metrics_ci(c(tp = 85, fp = 10, fn = 15, tn = 890)), "tbl_df")
  expect_s3_class(kk_kappa(d, expo, event), "tbl_df")
  expect_s3_class(kk_agreement(d, expo, event), "tbl_df")
  expect_s3_class(kk_mcnemar(d, expo, event), "tbl_df")
  expect_s3_class(kk_icc(d, c("p_old", "p_new")), "tbl_df")
  expect_s3_class(kk_reliability(d[, c("p_old", "p_new", "chol")]), "tbl_df")
  # Returns the plot by default, the statistics with plot = FALSE.
  expect_s3_class(kk_bland_altman(d$p_old, d$p_new), "ggplot")
  expect_s3_class(kk_bland_altman(d$p_old, d$p_new, plot = FALSE), "tbl_df")
  expect_s3_class(kk_reclassification(d, event, p_old, p_new,
                                      risk_thresholds = c(0.2, 0.4))$summary, "tbl_df")
})

test_that("group comparisons and non-parametric tests run", {
  expect_s3_class(kk_table1(d, by = "arm", variables = c("age", "bmi", "sex")), "tbl_df")
  expect_s3_class(table1_summary(d, by = "arm", variables = c("age", "bmi")), "tbl_df")
  expect_s3_class(kk_compare_groups_table(d, arm, c("age", "bmi")), "tbl_df")
  expect_s3_class(compare_groups_table(d, arm, c("age", "bmi")), "tbl_df")
  expect_s3_class(kk_summary(d, sbp), "tbl_df")
  expect_s3_class(comprehensive_summary(d, sbp), "tbl_df")
  expect_s3_class(kk_chisq_test(d, expo, event), "tbl_df")
  expect_s3_class(kk_median_test(d, sbp, arm), "tbl_df")
  expect_s3_class(kk_vdw_test(d, sbp, arm), "tbl_df")
  expect_s3_class(kk_jonckheere_test(d, sbp, dose), "tbl_df")
  expect_s3_class(kk_frequency_test(d, expo), "tbl_df")
  expect_s3_class(kk_mssd_test(d, sbp), "tbl_df")
  expect_s3_class(kk_runs_test(d, sbp), "tbl_df")
  expect_s3_class(kk_random_seq(d, sbp), "tbl_df")
  expect_s3_class(kk_butler_ks(d$sbp - d$chol), "tbl_df")
})

test_that("regression and proportions run", {
  expect_s3_class(kk_reg(d, sbp, c("age", "bmi")), "tbl_df")
  expect_s3_class(krk_reg(d, sbp, c("age")), "tbl_df")
  expect_s3_class(regression_analysis(d, sbp, c("age")), "tbl_df")
  expect_s3_class(kk_firth(d, event, c("age", "expo")), "tbl_df")

  props <- tibble(group = c("A", "B", "C"), proportion = c(.2, .35, .5),
                  trials = c(100, 100, 100))
  expect_s3_class(compare_proportions(props), "tbl_df")
  # pcit() works from integer counts, not proportions.
  expect_s3_class(pcit(tibble(group = c("A", "B"), x = c(20L, 35L), n = c(100L, 100L))), "tbl_df")
  glm_df <- data.frame(group = c("A", "B", "C"), x = c(20, 35, 50), n = c(100, 100, 100))
  expect_s3_class(suppressWarnings(compare_proportions_kk_glm(glm_df, group, x, n)), "tbl_df")
  expect_s3_class(power_proportions(n = 100, p1 = .2, p2 = .35), "power.htest")
  expect_s3_class(kk_sample_size_epi(design = "cohort", p0 = .1, rr_or = 1.8), "tbl_df")
  expect_s3_class(kk_compare_independent_correlations(c(.65, .40), c(50, 60)), "tbl_df")
  expect_s3_class(kk_compare_dependent_correlations(rxz = .72, ryz = .35, rxy = .28, n = 50), "tbl_df")
})

test_that("trial designs and dose-response run", {
  cl <- d %>% mutate(cluster_id = rep(seq_len(10), length.out = n()),
                     trt = as.integer(arm == "Treatment"))
  expect_s3_class(kk_cluster_trial(cl, sbp, trt, cluster_id), "tbl_df")
  co <- tibble(pid = 1:40, sequence = rep(c("AB", "BA"), each = 20),
               period1 = stats::rnorm(40, 50, 5), period2 = stats::rnorm(40, 52, 5))
  expect_s3_class(kk_crossover_trial(co, pid, sequence, period1, period2), "tbl_df")
  expect_s3_class(kk_trend_nonlinearity(d, dose, event, family = "binomial"), "tbl_df")
  expect_s3_class(kk_dose_response(d, bmi, event, family = "binomial", ref_value = 22), "tbl_df")
})

test_that("survival and competing risks run", {
  skip_if_not_installed("survival")
  expect_s3_class(kk_coxph(d, time, status, predictors = c("age")), "tbl_df")
  expect_s3_class(kk_logrank(d, time, status, arm), "tbl_df")
  expect_s3_class(kk_rmst(d, time, status, arm, tau = 20), "tbl_df")
  expect_s3_class(kk_survival_nnt(d, time, status, arm, times = c(10, 20)), "tbl_df")
  cr <- tibble(time = stats::rexp(200, .1),
               status = sample(0:2, 200, TRUE),
               arm = rep(c("A", "B"), each = 100),
               age = stats::rnorm(200, 60, 10))
  expect_s3_class(kk_cuminc(cr, time, status, arm, cause = 1), "tbl_df")
  expect_s3_class(kk_finegray(cr, time = "time", status = "status",
                              predictors = c("age"), cause = 1), "tbl_df")
})

test_that("causal inference and measurement error run", {
  expect_s3_class(kk_smd(d, arm, c("age", "bmi")), "tbl_df")
  expect_s3_class(kk_balance_table(d, arm, c("age", "bmi")), "tbl_df")
  expect_s3_class(kk_iptw(d, expo, event, covariates = c("age", "bmi")), "tbl_df")
  expect_s3_class(kk_tmle(d, event, expo, covariates = c("age", "bmi")), "tbl_df")
  expect_s3_class(kk_causal_mediation(d, expo, sbp, event, boot_reps = 50), "tbl_df")
  # kk_reri needs 0/1 numeric exposures, not factors.
  d01 <- d %>% mutate(sm = as.integer(smoker == "Yes"))
  m <- stats::glm(event ~ expo * sm, data = d01, family = stats::binomial())
  expect_s3_class(kk_reri(m, expo, sm), "tbl_df")
  naive <- stats::lm(sbp ~ bmi, data = d)
  expect_s3_class(kk_simex(naive, bmi, error_sd = 0.8, B = 20), "tbl_df")
})

test_that("health economics run", {
  ce <- tibble(strategy = c("A", "B", "C"), cost = c(5000, 9000, 14000),
               effect = c(6.0, 6.8, 7.1))
  expect_s3_class(kk_icer(ce, cost, effect, strategy), "tbl_df")
  expect_s3_class(kk_nmb(ce, cost, effect, wtp = 50000, strategy = strategy), "tbl_df")
  expect_s3_class(kk_ceac(psa, sim, strategy, cost, effect,
                          wtp = seq(0, 1e5, by = 25000)), "tbl_df")
  expect_s3_class(kk_evpi(psa, sim, strategy, cost, effect,
                          wtp = seq(0, 1e5, by = 25000)), "tbl_df")
  disc <- kk_discount(rep(1000, 5), rate = 0.03)
  expect_s3_class(disc, "tbl_df")
  expect_true(all(disc$present_value <= disc$undiscounted))

  tp <- matrix(c(.7, .2, .1, 0, .8, .2, 0, 0, 1), nrow = 3, byrow = TRUE)
  mk <- kk_markov(tp, costs = c(1000, 3000, 0), utilities = c(.8, .5, 0), cycles = 10)
  expect_named(mk, c("trace", "summary"))
  expect_s3_class(mk$trace, "tbl_df")

  ps <- kk_partsa(pfs = exp(-0.1 * 0:10), os = exp(-0.05 * 0:10),
                  times = 0:10, state_costs = c(1000, 2000, 0),
                  state_utilities = c(.8, .6, 0))
  expect_named(ps, c("trace", "summary"))
  expect_s3_class(ps$trace, "tbl_df")
})

test_that("infectious disease models run", {
  expect_s3_class(kk_seir(beta = .5, gamma = .2, sigma = .3, S0 = 9999, I0 = 1), "tbl_df")
  expect_s3_class(kk_r0(method = "params", beta = .5, gamma = .2), "tbl_df")
  expect_s3_class(kk_final_size(2.5), "tbl_df")
  sero <- tibble(age = rep(1:10, 2), year = rep(c(2010, 2015), each = 10),
                 positive = stats::rbinom(20, 50, .3), total = 50)
  expect_s3_class(kk_sero_incidence(sero, age, positive, year, total), "tbl_df")
})

test_that("time series run", {
  ts_df <- tibble(date = seq(as.Date("2020-01-01"), by = "month", length.out = 36),
                  value = 100 + cumsum(stats::rnorm(36, 2, 5)))
  # These take the value column as a string, not a bare name.
  expect_s3_class(kk_time_series(ts_df, "value", "date"), "tbl_df")
  expect_s3_class(time_series_analysis(ts_df, "value", "date"), "tbl_df")
  expect_s3_class(kk_time_metrics(ts_df, "value", "date"), "tbl_df")
})

test_that("utilities and EGN parsing run", {
  # kkonehot takes the column as a string.
  expect_s3_class(kkonehot(d, "arm"), "data.frame")
  expect_s3_class(one_hot_encode(d, "arm"), "data.frame")
  expect_s3_class(mutate_round(d, 1), "data.frame")
  expect_s3_class(format_tibble(tibble(Value = c(10.567, 2.3, NA))), "tbl_df")
  egn <- c("9201014321", "8812128765")
  expect_s3_class(extract_egn_info(egn), "data.frame")
  expect_s3_class(extract_age_from_egn(egn), "data.frame")
})

test_that("plotting functions build ggplot objects", {
  expect_s3_class(kkplot(d, aes(bmi, sbp)) + ggplot2::geom_point(), "ggplot")
  expect_s3_class(univariate_plot(d, age), "ggplot")
  expect_s3_class(univariate_cont_plot(d, "age"), "ggplot")
  expect_s3_class(univariate_cat_plot(d, "arm"), "ggplot")
  expect_s3_class(univariate_continuous_plot(d, "age"), "ggplot")
  expect_s3_class(univariate_categorical_plot(d, "arm"), "ggplot")
  expect_s3_class(kk_fullcorplot(d[, c("age", "bmi", "sbp", "chol")]), "ggplot")
  expect_s3_class(kk_risk_plot(kk_twobytwo(d, expo, event)), "ggplot")
  skip_if_not_installed("ggsurvfit")
  expect_s3_class(kk_survival_plot(d, time, status, arm), "ggplot")
  expect_s3_class(survival_plot(d, "time", "status", "arm"), "ggplot")
})

test_that("scan statistics and sensitivity functions run", {
  # Space-time negative-binomial scan on a small grid.
  set.seed(3)
  grid <- expand.grid(x = 1:4, y = 1:4)
  sc <- tibble(
    region = rep(seq_len(nrow(grid)), each = 10),
    time = rep(1:10, times = nrow(grid)),
    x = rep(grid$x, each = 10),
    y = rep(grid$y, each = 10),
    expected = 5
  )
  sc$count <- stats::rnbinom(nrow(sc), size = 5, mu = sc$expected)
  expect_s3_class(
    kk_nb_scan(sc, region, time, count, expected, n_sim = 19),
    "data.frame"
  )

  # Generalized sensitivity function. model_fn takes theta alone and returns the
  # model output at the observation times.
  obs_times <- seq_len(20)
  decay <- function(theta) theta[["beta"]] * exp(-theta[["gamma"]] * obs_times)
  expect_s3_class(
    kk_gsf(decay, theta = c(beta = 0.6, gamma = 0.2), times = obs_times),
    "data.frame"
  )
})

test_that("stratified proportion comparison runs", {
  props <- tibble(
    group = rep(c("A", "B"), 2),
    subgroup = rep(c("S1", "S2"), each = 2),
    proportion = c(.2, .35, .25, .30),
    trials = c(100, 100, 90, 95)
  )
  expect_s3_class(compare_proportions_by(props), "data.frame")
})

test_that("session and palette setters run", {
  # These mutate global state, so snapshot and restore around them.
  old_theme <- ggplot2::theme_get()
  old_colors <- getOption("kkstatfun.colors")
  old_opts <- options()
  on.exit({
    ggplot2::theme_set(old_theme)
    options(kkstatfun.colors = old_colors)
    options(old_opts["scipen"])
  }, add = TRUE)

  expect_silent(suppressMessages(set_plot_colors(c("#D62828", "#003049", "#F77F00"))))
  expect_type(getOption("kkstatfun.colors"), "character")

  # set_plot_font with a guaranteed-present family and no network lookup.
  res <- suppressMessages(set_plot_font("sans", size = 11,
                                       search_sources = "system",
                                       update_theme = FALSE))
  expect_type(res, "list")
  expect_true(all(c("requested", "used", "loaded") %in% names(res)))

  expect_message(kk_setup(cores = 1), "kkstatfun")
  expect_equal(getOption("mc.cores"), 1)
})

test_that("palette and scale helpers run", {
  expect_type(kk_pal(5), "character")
  expect_length(kk_pal(5), 5L)
  expect_type(kk_gen_palettes(c("#D62828", "#003049"), n = 6), "list")
  expect_s3_class(kk_show_palettes(kk_gen_palettes("#003049", n = 6)), "ggplot")
  p <- ggplot2::ggplot(d, aes(arm, fill = arm)) + ggplot2::geom_bar()
  expect_s3_class(p + scale_fill_kk(), "ggplot")
  expect_s3_class(p + scale_colour_kk(), "ggplot")
  expect_s3_class(p + scale_color_kk(), "ggplot")
  q <- ggplot2::ggplot(d, aes(bmi, sbp, colour = age)) + ggplot2::geom_point()
  expect_s3_class(q + scale_colour_kk_c(), "ggplot")
  expect_s3_class(q + scale_color_kk_c(), "ggplot")
  expect_s3_class(ggplot2::ggplot(d, aes(bmi, sbp, fill = age)) +
                    ggplot2::geom_point() + scale_fill_kk_c(), "ggplot")
})
