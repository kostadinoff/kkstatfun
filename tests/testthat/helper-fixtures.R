# Shared fixtures for the smoke tests. Individual-level data, as the package
# is designed around.

kk_fixture_cohort <- function(n = 300, seed = 42) {
  set.seed(seed)
  age <- round(stats::rnorm(n, 62, 11))
  bmi <- round(stats::rnorm(n, 27, 4.5), 1)
  arm <- factor(sample(c("Control", "Treatment"), n, replace = TRUE))
  sex <- factor(sample(c("Female", "Male"), n, replace = TRUE))
  smoker <- factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.7, 0.3)))
  sbp <- round(100 + 0.45 * age + 0.4 * bmi - 8 * (arm == "Treatment") +
                 stats::rnorm(n, 0, 10))
  chol <- round(3.5 + 0.02 * age + 0.05 * bmi + stats::rnorm(n, 0, 0.7), 1)
  lp <- -6 + 0.05 * age + 0.9 * (smoker == "Yes") - 0.4 * (arm == "Treatment")
  event <- stats::rbinom(n, 1, stats::plogis(lp))
  tibble::tibble(
    arm = arm, sex = sex, smoker = smoker, age = age, bmi = bmi,
    sbp = sbp, chol = chol, event = event,
    expo = stats::rbinom(n, 1, 0.4),
    site = rep(c("A", "B"), length.out = n),
    time = stats::rexp(n, 0.05),
    status = stats::rbinom(n, 1, 0.7),
    dose = rep(0:3, length.out = n),
    p_old = stats::runif(n, 0.10, 0.50),
    p_new = stats::runif(n, 0.05, 0.60),
    person_time = stats::runif(n, 0.5, 5)
  )
}

kk_fixture_psa <- function(n_sim = 200, seed = 1) {
  set.seed(seed)
  tibble::tibble(
    sim = rep(seq_len(n_sim), 2),
    strategy = rep(c("Usual care", "New drug"), each = n_sim),
    cost = c(stats::rnorm(n_sim, 5000, 800), stats::rnorm(n_sim, 9000, 1200)),
    effect = c(stats::rnorm(n_sim, 6.0, 0.5), stats::rnorm(n_sim, 6.8, 0.6))
  )
}

# A call either returns something, or the test explains what it needed.
expect_returns <- function(expr, label) {
  res <- tryCatch(force(expr), error = function(e) e)
  testthat::expect_false(inherits(res, "error"),
    info = paste0(label, ": ", if (inherits(res, "error")) conditionMessage(res) else "")
  )
  invisible(res)
}
