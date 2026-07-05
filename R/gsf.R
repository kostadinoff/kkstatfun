# ============================================================
# GENERALIZED SENSITIVITY FUNCTIONS (GSF) FOR INVERSE PROBLEMS
# ============================================================

#' Generalized Sensitivity Functions for Inverse Problems (KK)
#'
#' @description Computes the generalized sensitivity functions (GSF) of
#'   Thomaseth and Cobelli for a mechanistic model whose parameters are
#'   estimated by least squares from time-indexed data (e.g. an SEIR model fit
#'   to surveillance counts). Whereas an ordinary sensitivity function asks how
#'   the model *output* responds to a parameter, the GSF asks how much each
#'   *estimated parameter* depends on the observation at each time point -
#'   identifying the time windows that carry the information for estimating a
#'   given parameter, and hence where surveillance is most valuable.
#'
#'   Each GSF is a cumulative curve that rises from 0 to 1 across the observation
#'   window; a steep rise over an interval means that interval is highly
#'   informative for that parameter. (The endpoint equalling 1 for every
#'   parameter is a built-in numerical check.)
#'
#' @param model_fn Function of the parameter vector returning the model output
#'   at the observation times, i.e. `model_fn(theta)` yields a numeric vector of
#'   length `length(times)`.
#' @param theta Named (or unnamed) numeric vector of estimated parameters.
#' @param times Numeric vector of observation times (used for labelling and
#'   output order); must match the length of `model_fn(theta)`.
#' @param sigma Measurement-error standard deviation: a scalar (homoscedastic,
#'   the default 1) or a vector of length `length(times)`. Constant `sigma`
#'   cancels out of the GSF.
#' @param delta Relative step for the central finite-difference Jacobian
#'   (default 1e-4).
#'
#' @return Tibble with a `time` column and one cumulative-GSF column per
#'   parameter. The attribute `fisher_information` holds the estimated Fisher
#'   information matrix.
#'
#' @examples
#' # SEIR: how informative is each week for estimating beta vs gamma?
#' \dontrun{
#' obs_times <- 0:120
#' seir_I <- function(theta) {
#'   out <- kk_seir(beta = theta[1], gamma = theta[2], sigma = 0.2,
#'                  S0 = 9999, I0 = 1, R0_init = 0, times = obs_times)
#'   out$I
#' }
#' kk_gsf(seir_I, theta = c(beta = 0.6, gamma = 0.2), times = obs_times)
#' }
#'
#' @examples
#' # Simple check on a linear model f(t) = a + b t (GSFs end at 1)
#' f_lin <- function(theta) theta[1] + theta[2] * (1:20)
#' kk_gsf(f_lin, theta = c(a = 1, b = 0.5), times = 1:20)
#'
#' @export
kk_gsf <- function(model_fn, theta, times, sigma = 1, delta = 1e-4) {
  if (!is.function(model_fn)) stop("`model_fn` must be a function of theta.")
  f0 <- model_fn(theta)
  n <- length(f0)
  if (length(times) != n) {
    stop("`times` (", length(times), ") must match the length of model_fn(theta) (", n, ").")
  }
  p <- length(theta)
  par_names <- names(theta)
  if (is.null(par_names)) par_names <- paste0("theta", seq_len(p))

  sigma_vec <- if (length(sigma) == 1) rep(sigma, n) else sigma
  if (length(sigma_vec) != n) stop("`sigma` must be length 1 or length(times).")

  # Sensitivity Jacobian J[j, k] = d f(t_j) / d theta_k via central differences
  J <- matrix(NA_real_, n, p)
  for (k in seq_len(p)) {
    h <- delta * max(abs(theta[k]), delta)
    tp <- theta
    tm <- theta
    tp[k] <- theta[k] + h
    tm[k] <- theta[k] - h
    J[, k] <- (model_fn(tp) - model_fn(tm)) / (2 * h)
  }

  # Fisher information F = sum_j (1/sigma_j^2) g_j g_j^T, with g_j the j-th row of J
  w <- 1 / sigma_vec^2
  Fmat <- crossprod(J * sqrt(w)) # = sum_j w_j g_j g_j^T
  Finv <- tryCatch(solve(Fmat), error = function(e) {
    stop("Fisher information matrix is singular - parameters are not ",
      "jointly identifiable from these observations.")
  })

  # Per-time contribution (Hadamard of (F^{-1} g_j) and g_j, weighted), then cumulate
  contrib <- matrix(NA_real_, n, p)
  for (j in seq_len(n)) {
    gj <- J[j, ]
    contrib[j, ] <- w[j] * (Finv %*% gj) * gj
  }
  gsf <- apply(contrib, 2, cumsum)
  if (is.null(dim(gsf))) gsf <- matrix(gsf, ncol = p)
  colnames(gsf) <- par_names

  out <- tibble::tibble(time = times)
  for (k in seq_len(p)) out[[par_names[k]]] <- gsf[, k]
  attr(out, "fisher_information") <- Fmat
  out
}
