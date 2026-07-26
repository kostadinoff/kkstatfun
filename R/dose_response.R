# ============================================================
# DOSE-RESPONSE MODELLING VIA SPLINES
# ============================================================

#' Non-Linear Dose-Response Modelling via Splines
#'
#' Fits non-linear dose-response curves for continuous exposures using restricted cubic
#' splines (natural splines), reporting predicted Odds Ratios, Risk Ratios, or linear effects
#' relative to a reference exposure level, along with tests for overall association and non-linearity.
#'
#' @param data A data frame or tibble containing patient-level data.
#' @param exposure Column name (quoted or unquoted) for the continuous exposure variable.
#' @param outcome Column name (quoted or unquoted) for the outcome variable.
#' @param confounders Character vector of column names for adjustment variables (optional).
#' @param family Model family: `"binomial"` (logistic/ORs), `"gaussian"` (linear/diffs), or `"poisson"` (rate ratios). Default `"binomial"`.
#' @param ref_value Reference exposure value for comparisons (default `NULL`, which uses the median exposure).
#' @param n_knots Number of spline knots (default `3`).
#' @param alpha Significance level for confidence intervals (default `0.05`).
#'
#' @return A tidy tibble of class `kk_dose_response` containing:
#'   \describe{
#'     \item{exposure}{Exposure values across a 100-point prediction grid}
#'     \item{estimate}{Predicted OR, RR, or linear difference relative to `ref_value`}
#'     \item{conf.low}{Lower bound of 95% CI for predicted effect}
#'     \item{conf.high}{Upper bound of 95% CI for predicted effect}
#'     \item{ref_value}{Reference exposure level used for scaling}
#'   }
#'
#' @details
#' Follows methods in Woodward (2014) *Epidemiology: Study Design and Data Analysis*, 3rd Edition, Chapter 10.
#' Fits restricted cubic splines using `splines::ns()`. Deviations from a simple linear model are evaluated
#' using a Likelihood Ratio Test comparing the spline fit against a simple linear model.
#'
#' @export
#' @examples
#' library(dplyr)
#' set.seed(42)
#' sim_df <- tibble(
#'   bmi = runif(300, 18, 38),
#'   disease = rbinom(300, 1, prob = plogis(-4 + 0.15 * (bmi - 25) + 0.01 * (bmi - 25)^2))
#' )
#' kk_dose_response(sim_df, bmi, disease, family = "binomial", ref_value = 22)
kk_dose_response <- function(data, exposure, outcome, confounders = NULL, family = c("binomial", "gaussian", "poisson"), ref_value = NULL, n_knots = 3, alpha = 0.05) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }

  validate_data_frame(data)
  family <- match.arg(family)
  
  exp_col <- rlang::as_name(rlang::enquo(exposure))
  out_col <- rlang::as_name(rlang::enquo(outcome))
  
  cols_needed <- c(exp_col, out_col, confounders)
  if (!all(cols_needed %in% names(data))) {
    stop("One or more specified columns were not found in data.")
  }
  
  df <- data %>%
    dplyr::select(dplyr::all_of(cols_needed)) %>%
    stats::na.omit()
  
  x_vals <- df[[exp_col]]
  if (!is.numeric(x_vals)) {
    stop("Exposure variable must be numeric for spline dose-response analysis.")
  }
  
  ref_x <- if (is.null(ref_value)) stats::median(x_vals, na.rm = TRUE) else as.numeric(ref_value)
  df_spline <- max(2, n_knots - 1)
  
  fam_fun <- switch(family,
    binomial = stats::binomial(),
    gaussian = stats::gaussian(),
    poisson = stats::poisson()
  )
  
  # Basis matrix for splines
  spline_basis <- splines::ns(x_vals, df = df_spline)
  knots_attr <- attr(spline_basis, "knots")
  Boundary_knots <- attr(spline_basis, "Boundary.knots")
  
  df_model <- df
  for (i in 1:df_spline) {
    df_model[[paste0("ns_basis_", i)]] <- spline_basis[, i]
  }
  
  basis_cols <- paste0("ns_basis_", 1:df_spline)
  cov_terms <- if (!is.null(confounders) && length(confounders) > 0) paste("+", paste(confounders, collapse = " + ")) else ""
  
  f_spline <- stats::as.formula(paste(out_col, "~", paste(basis_cols, collapse = " + "), cov_terms))
  f_lin <- stats::as.formula(paste(out_col, "~", exp_col, cov_terms))
  f_null <- stats::as.formula(paste(out_col, "~ 1", cov_terms))
  
  m_spline <- stats::glm(f_spline, data = df_model, family = fam_fun)
  m_lin <- stats::glm(f_lin, data = df_model, family = fam_fun)
  m_null <- stats::glm(f_null, data = df_model, family = fam_fun)
  
  # Non-linearity LRT
  dev_lin <- stats::deviance(m_lin)
  dev_spline <- stats::deviance(m_spline)
  chi2_nonlin <- max(0, dev_lin - dev_spline)
  df_nonlin <- df_spline - 1
  p_nonlin <- stats::pchisq(chi2_nonlin, df = df_nonlin, lower.tail = FALSE)
  
  # Overall association LRT
  dev_null <- stats::deviance(m_null)
  chi2_assoc <- max(0, dev_null - dev_spline)
  p_assoc <- stats::pchisq(chi2_assoc, df = df_spline, lower.tail = FALSE)
  
  # Grid prediction
  grid_x <- seq(min(x_vals), max(x_vals), length.out = 100)
  grid_basis <- splines::ns(grid_x, knots = knots_attr, Boundary.knots = Boundary_knots)
  ref_basis <- splines::ns(ref_x, knots = knots_attr, Boundary.knots = Boundary_knots)
  
  coefs <- stats::coef(m_spline)
  vcov_m <- stats::vcov(m_spline)
  
  # Extract indices for basis terms in coefficient vector
  basis_indices <- match(basis_cols, names(coefs))
  b_coefs <- coefs[basis_indices]
  b_vcov <- vcov_m[basis_indices, basis_indices, drop = FALSE]
  
  z <- stats::qnorm(1 - alpha / 2)
  
  pred_estimates <- numeric(length(grid_x))
  pred_low <- numeric(length(grid_x))
  pred_high <- numeric(length(grid_x))
  
  for (j in seq_along(grid_x)) {
    # Contrast vector relative to ref_x
    contrast <- grid_basis[j, ] - ref_basis[1, ]
    delta_log <- sum(contrast * b_coefs)
    var_delta <- as.numeric(t(contrast) %*% b_vcov %*% contrast)
    se_delta <- sqrt(max(0, var_delta))
    
    ci_log_low <- delta_log - z * se_delta
    ci_log_high <- delta_log + z * se_delta
    
    if (family %in% c("binomial", "poisson")) {
      pred_estimates[j] <- exp(delta_log)
      pred_low[j] <- exp(ci_log_low)
      pred_high[j] <- exp(ci_log_high)
    } else {
      pred_estimates[j] <- delta_log
      pred_low[j] <- ci_log_low
      pred_high[j] <- ci_log_high
    }
  }
  
  res <- tibble::tibble(
    exposure = grid_x,
    estimate = pred_estimates,
    conf.low = pred_low,
    conf.high = pred_high,
    ref_value = rep(ref_x, length(grid_x))
  )
  
  nonlin_info <- list(
    p_association = p_assoc,
    p_nonlinearity = p_nonlin,
    chi2_nonlinearity = chi2_nonlin,
    df_nonlinearity = df_nonlin,
    knots = knots_attr,
    ref_value = ref_x,
    family = family
  )
  
  attr(res, "nonlinearity_test") <- nonlin_info
  class(res) <- c("kk_dose_response", class(res))
  return(res)
}
