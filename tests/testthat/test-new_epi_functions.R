library(testthat)
library(dplyr)
pkgload::load_all(".", quiet = TRUE)

test_that("kk_matched_case_control works as expected", {
  set.seed(42)
  df <- tibble(
    set_id = rep(1:30, each = 2),
    case = rep(c(1, 0), 30),
    exposure = rbinom(60, 1, 0.4)
  )
  res <- kk_matched_case_control(df, set_id, case, exposure)
  expect_s3_class(res, "kk_matched_case_control")
  expect_equal(res$matched_sets, 30)
  expect_true(!is.na(res$odds_ratio))
})

test_that("kk_case_crossover works as expected", {
  set.seed(123)
  df <- tibble(
    id = rep(1:20, each = 2),
    event = rep(c(1, 0), 20),
    exposure = rbinom(40, 1, 0.5)
  )
  res <- kk_case_crossover(df, id, event, exposure)
  expect_s3_class(res, "kk_case_crossover")
  expect_equal(res$n_patients, 20)
})

test_that("kk_sample_size_epi computes reasonable sample sizes", {
  res_cohort <- kk_sample_size_epi(design = "cohort", p0 = 0.10, rr_or = 1.8, power = 0.80)
  expect_s3_class(res_cohort, "kk_sample_size_epi")
  expect_true(res_cohort$n_total > 50)

  res_cluster <- kk_sample_size_epi(design = "cluster_trial", p0 = 0.15, rr_or = 0.7, m = 20, icc = 0.05)
  expect_true(!is.na(res_cluster$n_clusters))
})

test_that("kk_reclassification and kk_calibration work as expected", {
  set.seed(99)
  df <- tibble(
    outcome = rbinom(100, 1, 0.3),
    p_old = runif(100, 0.1, 0.5),
    p_new = runif(100, 0.05, 0.6)
  )
  res_reclass <- kk_reclassification(df, outcome, p_old, p_new, risk_thresholds = c(0.2, 0.4))
  expect_s3_class(res_reclass, "kk_reclassification")
  expect_true("summary" %in% names(res_reclass))

  res_calib <- kk_calibration(df, outcome, p_new, g = 5)
  expect_s3_class(res_calib, "kk_calibration")
  expect_equal(res_calib$g_groups, 5)
})

test_that("kk_cluster_trial and kk_crossover_trial work as expected", {
  set.seed(111)
  df_clust <- tibble(
    cluster_id = rep(1:10, each = 10),
    treatment = rep(c(1, 0), each = 50),
    outcome = rnorm(100, mean = 10)
  )
  res_clust <- kk_cluster_trial(df_clust, outcome, treatment, cluster_id)
  expect_s3_class(res_clust, "kk_cluster_trial")
  expect_equal(res_clust$n_clusters, 10)

  df_cross <- tibble(
    patient_id = 1:20,
    sequence = rep(c("AB", "BA"), each = 10),
    period1 = rnorm(20, 50, 5),
    period2 = rnorm(20, 52, 5)
  )
  res_cross <- kk_crossover_trial(df_cross, patient_id, sequence, period1, period2)
  expect_s3_class(res_cross, "kk_crossover_trial")
  expect_equal(res_cross$n_patients, 20)
})

test_that("kk_trend_nonlinearity works as expected", {
  set.seed(42)
  df_dose <- tibble(
    dose = rep(0:3, each = 25),
    outcome = rbinom(100, 1, 0.1 + 0.15 * rep(0:3, each = 25))
  )
  res_trend <- kk_trend_nonlinearity(df_dose, dose, outcome, family = "binomial")
  expect_s3_class(res_trend, "kk_trend_nonlinearity")
  expect_equal(res_trend$exposure_levels, 4)
})
