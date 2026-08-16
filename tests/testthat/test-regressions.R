library(dplyr)

# Regression tests. Each one fails on the code as it stood before the
# corresponding fix, so they guard against the bug coming back.

test_that("kk_calibration and kk_calibration_table are distinct functions", {
  # kk_calibration was defined twice (model_validation.R and reclassification.R);
  # the second definition silently shadowed the first.
  expect_true(is.function(kk_calibration))
  expect_true(is.function(kk_calibration_table))
  expect_false(identical(kk_calibration, kk_calibration_table))

  set.seed(42)
  df <- data.frame(y = rbinom(400, 1, 0.3))
  df$p <- plogis(qlogis(0.3) + 0.5 * scale(df$y) + rnorm(400))

  # The per-group table form carries Brier and Hosmer-Lemeshow as attributes.
  tab <- kk_calibration_table(df, y, p)
  expect_s3_class(tab, "tbl_df")
  expect_true(all(c("n", "observed_rate", "predicted_rate") %in% names(tab)))
  expect_type(attr(tab, "brier"), "double")
  expect_true(attr(tab, "brier") >= 0 && attr(tab, "brier") <= 1)
  expect_s3_class(attr(tab, "hosmer_lemeshow"), "tbl_df")

  # The summary form returns one row of calibration statistics.
  summ <- kk_calibration(df, y, p, g = 10)
  expect_s3_class(summ, "kk_calibration")
  expect_equal(nrow(summ), 1L)
  expect_true(all(c("hl_chi2", "oe_ratio", "calib_slope") %in% names(summ)))
})

test_that("survival_plot builds a plot (survfit2 comes from ggsurvfit)", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggsurvfit")
  lung <- survival::lung
  # Previously called survival::survfit2, which does not exist.
  expect_s3_class(survival_plot(lung, "time", "status", "sex"), "ggplot")
  expect_s3_class(kk_survival_plot(lung, time, status, sex), "ggplot")
})

test_that("runs tests accept threshold = 'mean' as well as 'median'", {
  set.seed(5)
  d <- tibble(x = rnorm(60))
  # `mean` lives in base, not stats; stats::mean errored at runtime.
  expect_s3_class(kk_runs_test(d, x, threshold = "mean"), "tbl_df")
  expect_s3_class(kk_runs_test(d, x, threshold = "median"), "tbl_df")
  expect_s3_class(kk_runs_test(d, x, threshold = 0), "tbl_df")
  expect_s3_class(kk_random_seq(d, x, threshold = "mean"), "tbl_df")
})

test_that("compare_proportions_kk_glm returns confidence intervals", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("sandwich")
  df <- data.frame(group = c("A", "B", "C"), x = c(20, 35, 50), n = c(100, 100, 100))
  res <- suppressWarnings(compare_proportions_kk_glm(df, group, x, n))

  # emmeans returns asymp.LCL/asymp.UCL for a glm; reading only lower.CL/upper.CL
  # yielded NULL, which tibble() dropped silently.
  expect_true(all(c("conf_low", "conf_high") %in% names(res)))
  expect_false(anyNA(res$conf_low))
  expect_false(anyNA(res$conf_high))
  expect_true(all(res$conf_low <= res$estimate))
  expect_true(all(res$conf_high >= res$estimate))

  # And its documented companion plot accepts that output.
  expect_s3_class(plot_proportion_comparisons(res), "ggplot")
})

test_that("compare_proportions_kk_glm accepts a bare column name for `by`", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("sandwich")
  df <- data.frame(
    group = rep(c("A", "B"), 2), strat = rep(c("S1", "S2"), each = 2),
    x = c(20, 35, 18, 30), n = c(100, 100, 90, 95)
  )
  # `is.null(by)` used to force the promise and error on an unbound symbol.
  res <- suppressWarnings(compare_proportions_kk_glm(df, group, x, n, by = strat))
  expect_true("strat" %in% names(res))
  expect_true(all(c("conf_low", "conf_high") %in% names(res)))
})

test_that("kk_confusion_matrix accepts every documented input form", {
  counts <- c(tp = 85, fp = 10, fn = 15, tn = 890)
  expect_s3_class(kk_confusion_matrix(counts), "tbl_df")
  expect_s3_class(kk_confusion_matrix(data.frame(tp = 85, fp = 10, fn = 15, tn = 890)), "tbl_df")

  # Upper-case labels were erased by a lower-case-only strip before tolower().
  kv <- tibble(label = c("TP", "FP", "FN", "TN"), value = c(85, 10, 15, 890))
  expect_s3_class(kk_confusion_matrix(kv), "tbl_df")

  # All three forms must agree.
  pick <- function(x) x$estimate[x$metric == "sensitivity (TPR)"]
  expect_equal(pick(kk_confusion_matrix(counts)), pick(kk_confusion_matrix(kv)))
})

test_that("kk_confusion_matrix MCC survives large counts and bootstrapping", {
  # The MCC denominator multiplied four marginals, overflowing to NA.
  big <- c(tp = 85000L, fp = 10000L, fn = 15000L, tn = 890000L)
  res <- kk_confusion_matrix(big)
  mcc <- res$estimate[grepl("MCC", res$metric)]
  expect_false(is.na(mcc))

  tp <- 85000; fp <- 10000; fn <- 15000; tn <- 890000
  ref <- (tp * tn - fp * fn) /
    (sqrt(tp + fp) * sqrt(tp + fn) * sqrt(tn + fp) * sqrt(tn + fn))
  expect_equal(mcc, ref, tolerance = 1e-8)

  # The bootstrap path hit the same overflow via integer rmultinom draws.
  expect_s3_class(
    kk_confusion_matrix(c(tp = 85, fp = 10, fn = 15, tn = 890), boot = TRUE, B = 200),
    "tbl_df"
  )
})

test_that("kk_reg works with include_diagnostics = FALSE", {
  # calculate_diagnostics() returned a 0x0 tibble, and bind_cols() cannot
  # recycle that against the n-row coefficient table.
  res <- kk_reg(mtcars, "mpg", c("wt", "hp"), include_diagnostics = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_false("r_squared" %in% names(res))

  # Diagnostics columns still appear by default.
  res_diag <- kk_reg(mtcars, "mpg", c("wt", "hp"))
  expect_true("r_squared" %in% names(res_diag))
  expect_gt(ncol(res_diag), ncol(res))

  # Logistic path too.
  d <- mtcars
  d$am <- factor(d$am)
  expect_s3_class(
    kk_reg(d, "am", c("wt", "hp"), include_diagnostics = FALSE),
    "tbl_df"
  )
})
