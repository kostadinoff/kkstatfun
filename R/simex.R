# ============================================================
# SIMULATION EXTRAPOLATION (SIMEX) FOR MEASUREMENT ERROR
# ============================================================

#' Simulation Extrapolation (SIMEX) for Measurement Error (KK)
#'
#' @description Corrects the bias that classical additive measurement error in a
#'   continuous covariate induces in a fitted regression, using the SIMEX
#'   algorithm of Cook and Stefanski. Increasing amounts of extra error are
#'   simulated onto the mismeasured covariate, the model is refitted at each
#'   error level, and the trend in the coefficients is extrapolated back to the
#'   (hypothetical) zero-error condition. Works with any fitted `lm`/`glm`
#'   (e.g. linear, logistic, Poisson).
#'
#' @param model A fitted `lm` or `glm` object.
#' @param variable Name (bare or string) of the error-prone covariate.
#' @param error_sd Known/assumed standard deviation of the additive measurement
#'   error (`sigma_u`).
#' @param lambda Grid of error-inflation factors (default `c(0.5, 1, 1.5, 2)`).
#'   The naive fit (`lambda = 0`) is always prepended.
#' @param B Number of simulated datasets per `lambda` (default 100).
#' @param degree Degree of the polynomial extrapolant (2 = quadratic default,
#'   1 = linear, 3 = cubic).
#' @param seed Optional integer seed for reproducibility.
#'
#' @return Tibble with one row per model coefficient: the `naive` estimate, the
#'   bias-corrected `simex` estimate, its SIMEX standard error (from the
#'   variance-extrapolation of Carroll et al.), and a 95% confidence interval.
#'   Assumes the covariate enters the formula untransformed.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' u <- rnorm(500, 0, 1)          # true latent exposure
#' y <- 1 + 1.5 * u + rnorm(500)  # true relationship
#' x <- u + rnorm(500, 0, 0.8)    # observed, measured with error (sd 0.8)
#' naive <- lm(y ~ x)             # slope attenuated below 1.5
#' kk_simex(naive, x, error_sd = 0.8)
#' }
#'
#' @export
kk_simex <- function(model, variable, error_sd, lambda = c(0.5, 1, 1.5, 2),
                     B = 100, degree = 2, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (!inherits(model, c("lm", "glm"))) {
    stop("`model` must be a fitted lm or glm object.")
  }
  if (!is.numeric(error_sd) || error_sd <= 0) {
    stop("`error_sd` must be a positive number.")
  }
  variable <- rlang::as_name(rlang::ensym(variable))

  mf <- stats::model.frame(model)
  if (!variable %in% names(mf)) {
    stop("`variable` '", variable, "' not found among the model's terms. ",
      "SIMEX requires the covariate to enter the formula untransformed.")
  }

  coef_names <- names(stats::coef(model))
  p <- length(coef_names)
  lambdas <- c(0, lambda)
  L <- length(lambdas)
  n <- nrow(mf)
  x0 <- mf[[variable]]

  est <- matrix(NA_real_, L, p, dimnames = list(NULL, coef_names))
  var_model <- matrix(NA_real_, L, p) # mean model-based variance
  var_between <- matrix(NA_real_, L, p) # variance across the B simulations

  for (li in seq_len(L)) {
    lam <- lambdas[li]
    if (lam == 0) {
      est[li, ] <- stats::coef(model)
      var_model[li, ] <- diag(stats::vcov(model))
      var_between[li, ] <- 0
      next
    }
    bmat <- matrix(NA_real_, B, p)
    vmat <- matrix(NA_real_, B, p)
    for (b in seq_len(B)) {
      mfb <- mf
      mfb[[variable]] <- x0 + sqrt(lam) * stats::rnorm(n, 0, error_sd)
      fb <- stats::update(model, data = mfb)
      bmat[b, ] <- stats::coef(fb)
      vmat[b, ] <- diag(stats::vcov(fb))
    }
    est[li, ] <- colMeans(bmat)
    var_model[li, ] <- colMeans(vmat)
    var_between[li, ] <- apply(bmat, 2, stats::var)
  }

  simex_est <- numeric(p)
  simex_var <- numeric(p)
  for (k in seq_len(p)) {
    d_est <- data.frame(lam = lambdas, b = est[, k])
    fit_b <- stats::lm(b ~ stats::poly(lam, degree, raw = TRUE), data = d_est)
    simex_est[k] <- stats::predict(fit_b, newdata = data.frame(lam = -1))
    # Carroll et al. variance extrapolation: s2(lambda) = mean model var - between var
    s2 <- var_model[, k] - var_between[, k]
    d_var <- data.frame(lam = lambdas, s2 = s2)
    fit_v <- stats::lm(s2 ~ stats::poly(lam, degree, raw = TRUE), data = d_var)
    simex_var[k] <- stats::predict(fit_v, newdata = data.frame(lam = -1))
  }

  se <- sqrt(pmax(simex_var, 0))
  tibble::tibble(
    term = coef_names,
    naive = est[1, ],
    simex = simex_est,
    std.error = se,
    conf.low = simex_est - stats::qnorm(0.975) * se,
    conf.high = simex_est + stats::qnorm(0.975) * se
  )
}
