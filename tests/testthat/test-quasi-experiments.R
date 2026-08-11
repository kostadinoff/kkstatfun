library(dplyr)

# Quasi-experimental designs. Each test builds data with a known effect and
# asks whether the estimator recovers it: a causal function that runs but
# returns the wrong number is worse than one that errors.

make_panel <- function(seed = 101, n = 500, effect = -3) {
  set.seed(seed)
  treated_unit <- rbinom(n, 1, 0.5)
  d <- tibble(
    unit = rep(seq_len(n), each = 2),
    post = rep(c(0, 1), times = n),
    treated = rep(treated_unit, each = 2),
    site = rep(sample(c("A", "B"), n, TRUE), each = 2)
  )
  d$y <- 10 + 2 * d$post + 1.5 * d$treated +
    effect * d$treated * d$post + rnorm(nrow(d))
  d
}

test_that("kk_did recovers a known 2x2 effect and matches the raw cell means", {
  d <- make_panel()
  res <- kk_did(d, y, treated, post)
  expect_s3_class(res, "tbl_df")
  expect_equal(res$estimate, -3, tolerance = 0.15)
  # With no covariates the regression estimate is the 2x2 identity exactly.
  expect_equal(res$estimate, res$unadjusted, tolerance = 1e-8)
  expect_true(res$conf.low < -3 && res$conf.high > -3)
  expect_equal(nrow(attr(res, "cells")), 4L)
  expect_s3_class(kk_model(res), "lm")
})

test_that("kk_did two-way fixed effects agrees with the 2x2 form", {
  d <- make_panel()
  a <- kk_did(d, y, treated, post)
  b <- kk_did(d, y, treated, post, unit = unit, time = post)
  expect_equal(a$estimate, b$estimate, tolerance = 1e-6)
  # Clustering on unit is what changes, not the point estimate.
  expect_equal(b$n_clusters, 500L)
})

test_that("kk_did handles count outcomes on the ratio scale", {
  set.seed(102)
  n <- 4000
  d <- tibble(
    treated = rbinom(n, 1, 0.5),
    post = rbinom(n, 1, 0.5)
  )
  d$y <- rpois(n, exp(1 + 0.3 * d$post + 0.2 * d$treated -
                        0.5 * d$treated * d$post))
  res <- kk_did(d, y, treated, post, family = "poisson")
  expect_equal(res$estimate, -0.5, tolerance = 0.15)
  expect_equal(res$ratio, exp(res$estimate))
})

test_that("kk_did refuses data where treatment or period does not vary", {
  d <- make_panel()
  expect_error(kk_did(filter(d, post == 1), y, treated, post), "must vary")
  expect_error(kk_did(d, y, treated, nope), "not found")
})

test_that("kk_did respects group_by", {
  d <- make_panel()
  g <- d %>% group_by(site) %>% kk_did(y, treated, post)
  expect_true("site" %in% names(g))
  expect_setequal(g$site, c("A", "B"))
  manual <- d %>%
    filter(site == "A") %>%
    kk_did(y, treated, post)
  expect_equal(g$estimate[g$site == "A"], manual$estimate, tolerance = 1e-10)
})

make_event <- function(seed = 103, units = 80, periods = 8, adopt_at = 5,
                       effect = 2) {
  set.seed(seed)
  d <- expand.grid(unit = seq_len(units), time = seq_len(periods))
  d$treat_time <- ifelse(d$unit <= units / 2, adopt_at, Inf)
  d$y <- 5 + 0.3 * d$time + rnorm(nrow(d), 0, 0.4) +
    effect * (d$time >= d$treat_time)
  tibble::as_tibble(d)
}

test_that("kk_event_study finds flat leads and the true post effect", {
  d <- make_event()
  res <- kk_event_study(d, y, unit, time, treat_time)
  expect_true(all(c("rel_time", "period") %in% names(res)))
  expect_equal(sum(res$period == "ref"), 1L)
  post <- res$estimate[res$period == "post"]
  expect_true(all(abs(post - 2) < 0.3))
  pre <- res$estimate[res$period == "pre"]
  expect_true(all(abs(pre) < 0.3))
  # The leads are flat by construction, so the pre-trend test should pass.
  expect_gt(attr(res, "pretrend")$p.value, 0.05)
})

test_that("kk_event_study detects a violated parallel trend", {
  set.seed(104)
  d <- make_event()
  # Treated units drift upward before adoption: parallel trends fails.
  d$y <- d$y + ifelse(is.finite(d$treat_time), 0.4 * d$time, 0)
  res <- kk_event_study(d, y, unit, time, treat_time)
  expect_lt(attr(res, "pretrend")$p.value, 0.01)
})

test_that("kk_event_study bins endpoints and validates the reference", {
  d <- make_event()
  res <- kk_event_study(d, y, unit, time, treat_time, leads = 2, lags = 2)
  expect_equal(min(res$rel_time), -2)
  expect_equal(max(res$rel_time), 2)
  expect_error(kk_event_study(d, y, unit, time, treat_time, reference = -99),
               "does not occur")
})

test_that("kk_did_staggered recovers a dynamic effect under staggered adoption", {
  set.seed(105)
  units <- 120
  d <- expand.grid(unit = seq_len(units), time = 1:6)
  d$treat_time <- rep(c(3, 5, Inf), each = units / 3)[d$unit]
  # Effect grows by 1.5 for each period since adoption.
  d$y <- 4 + 0.2 * d$time + rnorm(nrow(d), 0, 0.3) +
    1.5 * pmax(0, d$time - d$treat_time + 1)

  res <- kk_did_staggered(d, y, unit, time, treat_time, n_boot = 60)
  expect_named(res, c("att_gt", "dynamic", "group", "calendar", "overall"))

  dyn <- res$dynamic
  expect_equal(dyn$estimate[dyn$rel_time == 0], 1.5, tolerance = 0.2)
  expect_equal(dyn$estimate[dyn$rel_time == 1], 3.0, tolerance = 0.2)
  expect_equal(dyn$estimate[dyn$rel_time == 2], 4.5, tolerance = 0.3)
  # Pre-period cells should be null.
  expect_true(all(abs(dyn$estimate[dyn$rel_time < 0]) < 0.3))
  # Simultaneous bands are never narrower than the pointwise ones.
  expect_true(all(res$att_gt$band.low <= res$att_gt$conf.low + 1e-8))
  expect_true(all(res$overall$estimate > 0))
})

test_that("kk_did_staggered honours the never-treated control option", {
  set.seed(106)
  units <- 90
  d <- expand.grid(unit = seq_len(units), time = 1:5)
  d$treat_time <- rep(c(3, 4, Inf), each = units / 3)[d$unit]
  d$y <- 2 + 0.1 * d$time + rnorm(nrow(d), 0, 0.3) +
    1 * (d$time >= d$treat_time)
  res <- kk_did_staggered(d, y, unit, time, treat_time,
                          control = "nevertreated", n_boot = 40)
  expect_equal(res$overall$estimate, 1, tolerance = 0.2)
  expect_equal(res$overall$control, "nevertreated")

  no_never <- d[is.finite(d$treat_time), ]
  expect_error(
    kk_did_staggered(no_never, y, unit, time, treat_time,
                     control = "nevertreated", n_boot = 10),
    "no never-treated"
  )
})

test_that("kk_its separates level and trend change exactly on noiseless data", {
  d <- data.frame(month = 1:60)
  d$cases <- 100 - 0.5 * d$month - 15 * (d$month >= 37) -
    0.8 * pmax(0, d$month - 36)
  res <- suppressWarnings(kk_its(d, cases, month, intervention_time = 37))
  est <- setNames(res$estimate, res$term)
  expect_equal(unname(est[["level_change"]]), -15, tolerance = 1e-6)
  expect_equal(unname(est[["trend_change"]]), -0.8, tolerance = 1e-6)
  expect_equal(unname(est[["baseline_trend"]]), -0.5, tolerance = 1e-6)

  cf <- attr(res, "counterfactual")
  expect_equal(nrow(cf), 60L)
  # Before the intervention the counterfactual is the fit itself.
  pre <- cf$period == "pre"
  expect_equal(cf$fitted[pre], cf$counterfactual[pre], tolerance = 1e-8)
  # After it, the counterfactual is the extrapolated pre-period trend.
  expect_true(all(cf$counterfactual[!pre] > cf$fitted[!pre]))
  expect_lt(attr(res, "impact")$cumulative_difference, 0)
})

test_that("kk_its supports counts with an offset and a transition window", {
  set.seed(107)
  d <- data.frame(month = 1:72, pop = 1e5)
  d$cases <- rpois(72, 1e5 * exp(-8 - 0.01 * d$month - 0.3 * (d$month >= 37)))
  res <- kk_its(d, cases, month, 37, offset = pop, family = "poisson",
                harmonic = 1, period = 12)
  est <- setNames(res$estimate, res$term)
  expect_equal(unname(est[["level_change"]]), -0.3, tolerance = 0.15)
  expect_true(all(c("ratio", "ratio.low", "ratio.high") %in% names(res)))

  trimmed <- kk_its(d, cases, month, 37, offset = pop, family = "poisson",
                    transition = 3)
  expect_equal(sum(attr(trimmed, "counterfactual")$in_model), 69L)
})

test_that("kk_its rejects an intervention outside the observed range", {
  d <- data.frame(month = 1:40, y = rnorm(40))
  expect_error(kk_its(d, y, month, intervention_time = 99), "outside")
})

test_that("kk_rdd recovers a sharp discontinuity and picks a sane bandwidth", {
  set.seed(108)
  n <- 4000
  x <- runif(n, -1, 1)
  y <- 1 + 0.8 * x + 0.5 * (x >= 0) + rnorm(n, 0, 0.3)
  d <- data.frame(x = x, y = y)

  res <- kk_rdd(d, y, x, cutoff = 0)
  expect_equal(res$estimate, 0.5, tolerance = 0.08)
  expect_equal(res$design, "sharp")
  expect_gt(res$bandwidth, 0)
  expect_equal(res$bandwidth_source, "Imbens-Kalyanaraman")
  expect_equal(nrow(attr(res, "bw_sensitivity")), 6L)
  expect_true(all(c("side", "x", "y") %in% names(attr(res, "binned"))))

  # A user bandwidth restricts the sample used.
  narrow <- kk_rdd(d, y, x, bandwidth = 0.2)
  expect_equal(narrow$bandwidth, 0.2)
  expect_lt(narrow$n_left + narrow$n_right, res$n_left + res$n_right)
  expect_equal(narrow$estimate, 0.5, tolerance = 0.15)
})

test_that("kk_rdd finds no effect where there is none", {
  set.seed(109)
  n <- 3000
  x <- runif(n, -1, 1)
  y <- 1 + 0.8 * x + rnorm(n, 0, 0.3)
  res <- kk_rdd(data.frame(x = x, y = y), y, x)
  expect_gt(res$p.value, 0.05)
  expect_true(res$conf.low < 0 && res$conf.high > 0)
})

test_that("kk_rdd fuzzy design recovers the complier effect", {
  set.seed(110)
  n <- 5000
  x <- runif(n, -1, 1)
  d <- rbinom(n, 1, ifelse(x >= 0, 0.8, 0.2))
  y <- 1 + 0.5 * x + 1.0 * d + rnorm(n, 0, 0.3)
  res <- kk_rdd(data.frame(x = x, y = y, d = d), y, x, treatment = d)
  expect_equal(res$design, "fuzzy")
  expect_equal(res$estimate, 1.0, tolerance = 0.15)
  expect_equal(res$first_stage, 0.6, tolerance = 0.1)
  expect_equal(res$estimate, res$reduced_form / res$first_stage,
               tolerance = 1e-6)
})

test_that("kk_rkd recovers a known slope change", {
  set.seed(111)
  n <- 6000
  x <- runif(n, -1, 1)
  y <- 1 + 0.5 * x + 1.5 * pmax(0, x) + rnorm(n, 0, 0.25)
  res <- kk_rkd(data.frame(x = x, y = y), y, x, bandwidth = 0.6)
  expect_equal(res$estimate, 1.5, tolerance = 0.15)
  expect_equal(res$design, "sharp kink")
  expect_equal(nrow(attr(res, "bw_sensitivity")), 6L)
})

test_that("kk_rd_density passes a smooth density and flags a manipulated one", {
  set.seed(112)
  clean <- kk_rd_density(data.frame(x = rnorm(3000)), x)
  expect_gt(clean$p.value, 0.05)

  x <- rnorm(3000)
  # Push everyone just below the cutoff over to just above it. The hole this
  # leaves drives the left-hand local linear intercept negative, so the
  # estimator falls back to the kernel-weighted mean density and says so -
  # that fallback is conservative, understating the jump rather than taking
  # the log of a negative number.
  bump <- x > -0.1 & x < 0
  x[bump] <- abs(x[bump])
  expect_warning(manipulated <- kk_rd_density(data.frame(x = x), x),
                 "not positive")
  expect_lt(manipulated$p.value, 0.01)
  expect_gt(manipulated$estimate, 0)
})

test_that("kk_iv recovers the causal effect that OLS misses", {
  set.seed(113)
  n <- 4000
  z <- rbinom(n, 1, 0.5)
  u <- rnorm(n)
  d <- rbinom(n, 1, plogis(-0.5 + 1.5 * z + u))
  y <- 1 + 2 * d + u + rnorm(n)
  df <- data.frame(y = y, d = d, z = z)

  res <- kk_iv(df, y, d, instruments = "z")
  expect_equal(res$estimate, 2, tolerance = 0.35)
  # OLS is biased upward here because the confounder raises both.
  expect_gt(res$ols, res$estimate)
  expect_gt(res$first_stage_f, 10)
  expect_false(res$weak_instrument)
  expect_true(res$ar.low < 2 && res$ar.high > 2)
  expect_equal(nrow(attr(res, "diagnostics")), 3L)
  # Endogeneity is real in this design, so Wu-Hausman should reject.
  diag <- attr(res, "diagnostics")
  expect_lt(diag$p.value[diag$test == "Wu-Hausman endogeneity"], 0.05)
})

test_that("kk_iv warns about a weak instrument", {
  set.seed(114)
  n <- 600
  z <- rnorm(n)
  u <- rnorm(n)
  d <- 0.02 * z + u + rnorm(n)
  y <- 1 + 2 * d + u + rnorm(n)
  expect_warning(res <- kk_iv(data.frame(y = y, d = d, z = z), y, d, "z",
                              ar_ci = FALSE),
                 "weak")
  expect_true(res$weak_instrument)
})

test_that("kk_iv runs the over-identification test with several instruments", {
  set.seed(115)
  n <- 3000
  z1 <- rnorm(n); z2 <- rnorm(n)
  u <- rnorm(n)
  d <- 0.8 * z1 + 0.6 * z2 + u + rnorm(n)
  y <- 1 + 2 * d + u + rnorm(n)
  res <- kk_iv(data.frame(y = y, d = d, z1 = z1, z2 = z2), y, d,
               instruments = c("z1", "z2"), ar_ci = FALSE)
  expect_equal(res$n_instruments, 2L)
  sargan <- attr(res, "diagnostics")
  sargan <- sargan[sargan$test == "Sargan over-identification", ]
  # Both instruments are valid here, so the test should not reject.
  expect_gt(sargan$p.value, 0.01)
})

test_that("kk_ancova matches lm and reports the alternative estimators", {
  set.seed(116)
  n <- 400
  base <- rnorm(n, 140, 15)
  arm <- rbinom(n, 1, 0.5)
  post <- 0.6 * base + 56 - 8 * arm + rnorm(n, 0, 8)
  d <- data.frame(base = base, arm = arm, post = post)

  res <- kk_ancova(d, post, arm, base)
  expect_equal(res$estimate,
               unname(coef(lm(post ~ arm + base, data = d))[["arm"]]),
               tolerance = 1e-10)
  expect_equal(res$estimate, -8, tolerance = 1.5)
  expect_equal(res$ey1 - res$ey0, res$estimate, tolerance = 1e-8)
  expect_true(is.finite(res$change_score))
  expect_true(is.finite(res$posttest_only))
})

test_that("kk_synth builds a convex donor weighting and recovers the effect", {
  set.seed(117)
  units <- paste0("region", 1:15)
  d <- expand.grid(unit = units, time = 1:24, stringsAsFactors = FALSE)
  base <- setNames(runif(15, 8, 12), units)
  d$y <- base[d$unit] + 0.15 * d$time + rnorm(nrow(d), 0, 0.25)
  hit <- d$unit == "region1" & d$time >= 18
  d$y[hit] <- d$y[hit] - 2

  res <- kk_synth(d, y, unit, time, "region1", intervention_time = 18)
  expect_equal(res$att, -2, tolerance = 0.5)
  expect_equal(res$n_donors, 14L)

  w <- attr(res, "weights")
  # The simplex constraint is the whole point: no negative or runaway weights.
  expect_equal(sum(w$weight), 1, tolerance = 1e-6)
  expect_true(all(w$weight >= -1e-8))
  expect_equal(nrow(w), 14L)

  path <- attr(res, "path")
  expect_equal(nrow(path), 24L)
  expect_equal(path$gap, path$observed - path$synthetic, tolerance = 1e-10)
  # The pre-period fit should be much tighter than the post-period gap.
  expect_lt(res$pre_rmspe, res$post_rmspe)
  expect_true(res$p.value <= 1 && res$p.value > 0)
  expect_equal(nrow(attr(res, "placebo")), 15L)
})

test_that("kk_synth validates the treated unit and the pre-period", {
  set.seed(118)
  units <- paste0("u", 1:6)
  d <- expand.grid(unit = units, time = 1:10, stringsAsFactors = FALSE)
  d$y <- rnorm(nrow(d))
  expect_error(kk_synth(d, y, unit, time, "nope", 5), "not found")
  expect_error(kk_synth(d, y, unit, time, "u1", 2), "three pre-intervention")
  expect_error(kk_synth(d, y, unit, time, "u1", 99), "no post-intervention")
})

test_that("the quasi-experimental functions all support group_by", {
  d <- make_panel()
  expect_s3_class(d %>% group_by(site) %>% kk_did(y, treated, post), "tbl_df")

  set.seed(119)
  n <- 2000
  rd <- data.frame(
    x = runif(n, -1, 1),
    g = rep(c("a", "b"), length.out = n)
  )
  rd$y <- 1 + 0.8 * rd$x + 0.5 * (rd$x >= 0) + rnorm(n, 0, 0.3)
  gres <- rd %>% group_by(g) %>% kk_rdd(y, x)
  expect_true("g" %in% names(gres))
  expect_equal(nrow(gres), 2L)

  ivd <- data.frame(z = rbinom(n, 1, 0.5), g = rep(c("a", "b"), length.out = n))
  ivd$d <- rbinom(n, 1, plogis(-0.5 + 1.5 * ivd$z))
  ivd$y <- 1 + 2 * ivd$d + rnorm(n)
  expect_s3_class(ivd %>% group_by(g) %>% kk_iv(y, d, "z", ar_ci = FALSE),
                  "tbl_df")
})
