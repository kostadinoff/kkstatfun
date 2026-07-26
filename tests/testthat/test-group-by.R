library(dplyr)

# group_by() support. Before .kk_by_group() these functions silently returned
# the pooled estimate, which is the dangerous failure mode: a confounded
# result that looks like a stratified one.

make_data <- function(seed = 7, n = 400) {
  set.seed(seed)
  d <- tibble(
    site = rep(c("A", "B"), each = n / 2),
    expo = c(rbinom(n / 2, 1, .6), rbinom(n / 2, 1, .2)),
    out = NA_integer_
  )
  # Strong exposure effect at site A, none at site B.
  half <- n / 2
  d$out[seq_len(half)] <- rbinom(half, 1, ifelse(d$expo[seq_len(half)] == 1, .5, .1))
  d$out[(half + 1):n] <- rbinom(half, 1, .2)
  d %>% mutate(
    x = rnorm(n, 50, 10), y = rnorm(n, 100, 15),
    arm = factor(sample(c("C", "T"), n, TRUE)),
    t = rexp(n, .05), ev = rbinom(n, 1, .7)
  )
}

test_that("grouped kk_twobytwo stratifies instead of pooling", {
  d <- make_data()

  pooled <- kk_twobytwo(d, expo, out)
  pooled_or <- pooled$Estimate[pooled$Metric == "Odds Ratio"]

  grouped <- d %>% group_by(site) %>% kk_twobytwo(expo, out)
  expect_true("site" %in% names(grouped))
  expect_setequal(unique(grouped$site), c("A", "B"))

  g_or <- grouped$Estimate[grouped$Metric == "Odds Ratio"]

  # Must equal a manual per-site analysis, not the pooled value.
  manual <- d %>%
    group_split(site) %>%
    vapply(function(z) kk_twobytwo(z, expo, out)$Estimate[1], numeric(1))
  expect_equal(g_or, manual, tolerance = 1e-10)
  expect_false(isTRUE(all.equal(g_or[1], pooled_or)))
})

test_that("grouping columns are prefixed and every group is represented", {
  d <- make_data()
  cases <- list(
    kk_epi_stats      = function(z) kk_epi_stats(z, expo, out),
    kk_reg            = function(z) kk_reg(z, y, c("x")),
    kk_roc            = function(z) kk_roc(z, out, x),
    kk_incidence_rate = function(z) kk_incidence_rate(z, ev, t),
    kk_smd            = function(z) kk_smd(z, arm, c("x", "y")),
    kk_diagnostic     = function(z) kk_diagnostic(z, out, expo),
    kk_mcnemar        = function(z) kk_mcnemar(z, expo, out),
    kk_paf            = function(z) kk_paf(z, expo, out)
  )
  for (nm in names(cases)) {
    res <- cases[[nm]](d %>% group_by(site))
    expect_s3_class(res, "data.frame")
    expect_true("site" %in% names(res), info = nm)
    expect_setequal(unique(res$site), c("A", "B"))
    # Grouping keys come first.
    expect_equal(names(res)[1], "site", info = nm)
  }
})

test_that("survival functions honour group_by", {
  skip_if_not_installed("survival")
  d <- make_data()
  g <- d %>% group_by(site)
  expect_true("site" %in% names(kk_logrank(g, t, ev, arm)))
  expect_true("site" %in% names(kk_coxph(g, t, ev, predictors = "x")))
  expect_true("site" %in% names(kk_rmst(g, t, ev, arm, tau = 20)))
})

test_that("aliases inherit group support from their target", {
  d <- make_data()
  g <- d %>% group_by(site)
  # kk_balance_table <- kk_smd, so the guard is inherited.
  expect_true("site" %in% names(kk_balance_table(g, arm, c("x", "y"))))
})

test_that("multiple grouping variables are supported", {
  d <- make_data() %>% mutate(era = rep(c("early", "late"), length.out = n()))
  res <- d %>% group_by(site, era) %>% kk_twobytwo(expo, out)
  expect_true(all(c("site", "era") %in% names(res)))
  expect_equal(names(res)[1:2], c("site", "era"))
  expect_equal(
    nrow(dplyr::distinct(res[, c("site", "era")])),
    nrow(dplyr::distinct(d[, c("site", "era")]))
  )
})

test_that("ungrouped behaviour is unchanged", {
  d <- make_data()
  res <- kk_twobytwo(d, expo, out)
  expect_s3_class(res, "tbl_df")
  expect_false("site" %in% names(res))
  # A grouped-then-ungrouped frame takes the ordinary path.
  expect_equal(kk_twobytwo(d %>% group_by(site) %>% ungroup(), expo, out), res)
})
