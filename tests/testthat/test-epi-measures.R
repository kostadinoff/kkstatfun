library(dplyr)

# Numeric correctness against independent reference implementations, so a
# refactor cannot quietly change a published estimate.

test_that("kk_twobytwo matches hand-computed 2x2 measures", {
  d <- data.frame(
    expo = c(rep(1, 80), rep(0, 120)),
    out  = c(rep(1, 30), rep(0, 50), rep(1, 10), rep(0, 110))
  )
  k <- kk_twobytwo(d, expo, out)
  pick <- function(m, col = "Estimate") k[[col]][k$Metric == m]

  tab <- table(factor(d$expo, c(1, 0)), factor(d$out, c(1, 0)))
  r_exp <- tab[1, 1] / sum(tab[1, ])
  r_unexp <- tab[2, 1] / sum(tab[2, ])

  expect_equal(pick("Odds Ratio"), (tab[1, 1] * tab[2, 2]) / (tab[1, 2] * tab[2, 1]))
  expect_equal(pick("Relative Risk"), r_exp / r_unexp)
  expect_equal(pick("Risk Difference"), r_exp - r_unexp)
  expect_equal(pick("Risk in Exposed"), r_exp)
  expect_equal(pick("Risk in Unexposed"), r_unexp)

  # The OR p-value is Fisher's exact; the interval is the Woolf (logit) one.
  fe <- fisher.test(tab)
  expect_equal(pick("Odds Ratio", "P_Value"), fe$p.value)
  se <- sqrt(sum(1 / as.numeric(tab)))
  or <- (tab[1, 1] * tab[2, 2]) / (tab[1, 2] * tab[2, 1])
  expect_equal(pick("Odds Ratio", "Lower"), exp(log(or) - 1.96 * se), tolerance = 1e-3)
  expect_equal(pick("Odds Ratio", "Upper"), exp(log(or) + 1.96 * se), tolerance = 1e-3)
})

test_that("kk_stratified_2x2 matches mantelhaen.test", {
  d <- data.frame(
    coffee = c(rep(1, 50), rep(0, 50), rep(1, 40), rep(0, 60)),
    chd = c(rep(1, 15), rep(0, 35), rep(1, 5), rep(0, 45),
            rep(1, 10), rep(0, 30), rep(1, 12), rep(0, 48)),
    smoking = c(rep("S", 100), rep("N", 100))
  )
  s <- kk_stratified_2x2(d, coffee, chd, smoking)
  arr <- table(factor(d$coffee, c(1, 0)), factor(d$chd, c(1, 0)), d$smoking)
  mh <- mantelhaen.test(arr, correct = FALSE)

  expect_equal(s$pooled_or$Estimate[1], unname(mh$estimate), tolerance = 1e-6)
  expect_equal(s$pooled_or$Lower[1], mh$conf.int[1], tolerance = 1e-6)
  expect_equal(s$pooled_or$Upper[1], mh$conf.int[2], tolerance = 1e-6)
})

test_that("kk_coxph matches survival::coxph", {
  skip_if_not_installed("survival")
  lung <- survival::lung
  k <- kk_coxph(lung, time, status, predictors = c("age", "sex", "ph.ecog"))
  ref <- summary(survival::coxph(
    survival::Surv(time, status) ~ age + sex + ph.ecog, data = lung
  ))
  mv <- k[grepl("^multivar", k$model_type), ]
  for (v in c("age", "sex", "ph.ecog")) {
    expect_equal(
      mv$hazard_ratio[mv$term == v],
      unname(ref$coefficients[v, "exp(coef)"]),
      tolerance = 1e-6, info = v
    )
  }
  expect_equal(k$n_events[1], sum(lung$status == 2))
})

test_that("kk_logrank matches survival::survdiff", {
  skip_if_not_installed("survival")
  lung <- survival::lung
  k <- kk_logrank(lung, time, status, sex)
  ref <- survival::survdiff(survival::Surv(time, status) ~ sex, data = lung)
  expect_equal(k$chisq[1], ref$chisq, tolerance = 1e-6)
  expect_equal(sort(k$observed), sort(unname(ref$obs)), tolerance = 1e-6)
  expect_equal(sort(k$expected), sort(unname(ref$exp)), tolerance = 1e-6)
})

test_that("kk_roc matches pROC", {
  skip_if_not_installed("pROC")
  set.seed(3)
  d <- tibble(y = rbinom(200, 1, .4), x = rnorm(200))
  d$x <- d$x + d$y
  k <- kk_roc(d, y, x)
  ref <- pROC::roc(d$y, d$x, quiet = TRUE)
  expect_equal(k$auc, as.numeric(pROC::auc(ref)), tolerance = 1e-8)
  expect_true(k$auc_low <= k$auc && k$auc >= 0.5)
})

test_that("kk_kappa matches psych::cohen.kappa", {
  skip_if_not_installed("psych")
  set.seed(3)
  d <- tibble(r1 = sample(1:3, 100, TRUE), r2 = sample(1:3, 100, TRUE))
  k <- kk_kappa(d, r1, r2)
  ref <- suppressWarnings(psych::cohen.kappa(as.matrix(d)))
  expect_equal(k$kappa[1], ref$kappa, tolerance = 1e-6)
  expect_true(k$conf_lower[1] <= k$kappa[1])
  expect_true(k$conf_upper[1] >= k$kappa[1])
})

test_that("kk_cuminc Gray's test matches cmprsk", {
  skip_if_not_installed("cmprsk")
  set.seed(11)
  df <- tibble(
    time = rexp(300, 0.1),
    status = sample(0:2, 300, TRUE, prob = c(.3, .4, .3)),
    arm = rep(c("A", "B"), each = 150)
  )
  res <- kk_cuminc(df, time, status, arm, cause = 1)
  gt <- attr(res, "gray_test")
  expect_false(is.null(gt))

  ref <- cmprsk::cuminc(df$time, df$status, group = df$arm)
  expect_equal(gt$stat[gt$cause == "1"], unname(ref$Tests[1, "stat"]), tolerance = 1e-6)
  expect_equal(gt$pv[gt$cause == "1"], unname(ref$Tests[1, "pv"]), tolerance = 1e-6)
})

test_that("kk_incidence_rate computes rates and ratios correctly", {
  d <- tibble(cases = c(12, 20), pt = c(1000, 1500), arm = factor(c("A", "B")))
  k <- kk_incidence_rate(d, cases, pt, by = arm)
  expect_s3_class(k, "tbl_df")
  expect_equal(nrow(k), 2L)
})
