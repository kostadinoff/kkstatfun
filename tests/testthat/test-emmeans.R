library(dplyr)

# The modelling functions keep their fitted model so it can be handed to
# emmeans. Before 1.1.0 the model was discarded and had to be refitted by hand.

make_d <- function(n = 300, seed = 9) {
  set.seed(seed)
  d <- tibble(
    age  = rnorm(n, 60, 10),
    arm  = factor(sample(c("Control", "Treatment"), n, TRUE)),
    sex  = factor(sample(c("F", "M"), n, TRUE)),
    site = rep(c("A", "B"), length.out = n),
    pyrs = runif(n, .5, 5)
  )
  d$sbp  <- 100 + .4 * d$age - 6 * (d$arm == "Treatment") + rnorm(n, 0, 8)
  d$evt  <- rbinom(n, 1, plogis(-2 + .03 * d$age + .6 * (d$arm == "Treatment")))
  d$evtf <- factor(d$evt, labels = c("No", "Yes"))
  d$cnt  <- rpois(n, 2)
  d$time <- rexp(n, .05)
  d$status <- rbinom(n, 1, .7)
  d
}

test_that("modelling functions attach a retrievable fitted model", {
  d <- make_d()
  cases <- list(
    kk_reg_lm    = list(kk_reg(d, sbp, c("age", "arm")), "lm"),
    kk_reg_logit = list(kk_reg(d, evtf, c("age", "arm")), "glm"),
    kk_rr_reg    = list(kk_rr_reg(d, evt, c("age", "arm")), "glm"),
    kk_rate_reg  = list(kk_rate_reg(d, cnt, c("age", "arm"), person_time = pyrs), "glm"),
    kk_coxph     = list(kk_coxph(d, time, status, predictors = c("age", "arm")), "coxph")
  )
  for (nm in names(cases)) {
    fit <- cases[[nm]][[1]]
    expect_s3_class(kk_model(fit), cases[[nm]][[2]])
    expect_setequal(names(kk_model(fit, "all")$univariate), c("age", "arm"))
    expect_s3_class(kk_model(fit, "univariate", "arm"), cases[[nm]][[2]])
    expect_s3_class(kk_model_data(fit), "data.frame")
  }
})

test_that("kk_firth attaches its model without losing the separation attribute", {
  skip_if_not_installed("brglm2")
  d <- make_d()
  fit <- suppressWarnings(kk_firth(d, evtf, c("age", "arm")))
  expect_s3_class(kk_model(fit), "glm")
  expect_type(attr(fit, "separation", exact = TRUE), "logical")
})

test_that("emmeans works on the retrieved models", {
  skip_if_not_installed("emmeans")
  d <- make_d()
  fit <- kk_reg(d, sbp, c("age", "arm"))
  em <- emmeans::emmeans(kk_model(fit), ~ arm)
  expect_s4_class(em, "emmGrid")

  # Identical to a model fitted by hand.
  ref <- emmeans::emmeans(lm(sbp ~ age + arm, data = d), ~ arm)
  expect_equal(summary(em)$emmean, summary(ref)$emmean, tolerance = 1e-10)

  # Downstream verbs.
  expect_s4_class(emmeans::contrast(em, "pairwise"), "emmGrid")
  expect_s3_class(emmeans::joint_tests(kk_model(fit)), "summary_emm")
  expect_s4_class(emmeans::emtrends(kk_model(fit), ~ arm, var = "age"), "emmGrid")
})

test_that("emmeans works for the non-Gaussian families too", {
  skip_if_not_installed("emmeans")
  d <- make_d()
  expect_s4_class(
    emmeans::emmeans(kk_model(kk_reg(d, evtf, c("age", "arm"))), ~ arm, type = "response"),
    "emmGrid"
  )
  expect_s4_class(
    emmeans::emmeans(kk_model(kk_rate_reg(d, cnt, c("age", "arm"), person_time = pyrs)),
                     ~ arm, type = "response"),
    "emmGrid"
  )
  expect_s4_class(
    emmeans::emmeans(kk_model(kk_coxph(d, time, status, predictors = c("age", "arm"))), ~ arm),
    "emmGrid"
  )
})

test_that("kk_emmeans returns a tibble with normalised CI columns", {
  skip_if_not_installed("emmeans")
  d <- make_d()

  # Finite df: emmeans names the columns lower.CL/upper.CL
  r <- kk_emmeans(kk_reg(d, sbp, c("age", "arm")), ~ arm)
  expect_s3_class(r, "tbl_df")
  expect_true(all(c("conf.low", "conf.high", "conf.level") %in% names(r)))
  expect_false(any(c("lower.CL", "upper.CL") %in% names(r)))
  expect_equal(nrow(r), 2L)

  # Asymptotic: emmeans names them asymp.LCL/asymp.UCL
  rg <- kk_emmeans(kk_reg(d, evtf, c("age", "arm")), ~ arm, type = "response")
  expect_true(all(c("conf.low", "conf.high") %in% names(rg)))
  expect_false(any(c("asymp.LCL", "asymp.UCL") %in% names(rg)))

  expect_true(all(r$conf.low <= r$emmean))
  expect_true(all(r$conf.high >= r$emmean))
})

test_that("kk_emmeans computes contrasts and keeps the emmGrid", {
  skip_if_not_installed("emmeans")
  d <- make_d()
  fit <- kk_reg(d, sbp, c("age", "arm"))
  ct <- kk_emmeans(fit, ~ arm, contrast = "pairwise")
  expect_s3_class(ct, "tbl_df")
  expect_equal(nrow(ct), 1L)
  expect_true("contrast" %in% names(ct))
  expect_s4_class(attr(ct, "emmGrid", exact = TRUE), "emmGrid")

  ref <- summary(emmeans::contrast(emmeans::emmeans(kk_model(fit), ~ arm), "pairwise"))
  expect_equal(ct$estimate, ref$estimate, tolerance = 1e-10)
})

test_that("grouped fits keep one model per group and never leak the first", {
  skip_if_not_installed("emmeans")
  d <- make_d()
  gf <- d %>% group_by(site) %>% kk_reg(sbp, c("age", "arm"))

  # The overall slots must be empty -- a leaked first-group model would look
  # like an overall fit.
  expect_null(attr(gf, "model", exact = TRUE))
  expect_null(attr(gf, "models", exact = TRUE))
  expect_setequal(names(attr(gf, "group_models", exact = TRUE)), c("A", "B"))

  # Asking without a group points at the groups rather than returning one.
  expect_error(kk_model(gf), "one model per group")

  mA <- kk_model(gf, group = "A")
  expect_s3_class(mA, "lm")
  ref <- lm(sbp ~ age + arm, data = dplyr::filter(d, site == "A"))
  expect_equal(
    summary(emmeans::emmeans(mA, ~ arm))$emmean,
    summary(emmeans::emmeans(ref, ~ arm))$emmean,
    tolerance = 1e-10
  )
  expect_s3_class(kk_emmeans(gf, ~ arm, group = "A"), "tbl_df")
  expect_error(kk_model(gf, group = "Z"), "No models for group")
})

test_that("kk_model errors helpfully on objects with no model", {
  d <- make_d()
  expect_error(kk_model(kk_twobytwo(d, evt, evt)), "No fitted model attached")
  expect_null(kk_model_data(kk_twobytwo(d, evt, evt)))
  expect_error(kk_model(kk_reg(d, sbp, c("age", "arm")), "univariate"),
               "`predictor` is required")
  expect_error(kk_model(kk_reg(d, sbp, c("age", "arm")), "univariate", "nope"),
               "No univariate model")
})
