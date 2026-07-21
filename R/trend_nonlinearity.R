#' Dose-Response Linearity and Departure from Linearity Test
#'
#' Decomposes exposure-outcome associations across ordered exposure levels into a 1-df linear
#' trend component and a departure-from-linearity (non-linearity) component on patient-level data.
#'
#' @param data A data frame or tibble containing patient-level data.
#' @param exposure Column name (quoted or unquoted) for multi-level ordinal or continuous exposure.
#' @param outcome Column name (quoted or unquoted) for outcome variable (binary 1/0, continuous, or count).
#' @param family Regression model family (`"binomial"`, `"gaussian"`, or `"poisson"`). Default `"binomial"`.
#' @param alpha Significance level for confidence intervals (default `0.05`).
#'
#' @return A tidy tibble of class `kk_trend_nonlinearity` containing:
#'   \item{exposure_levels}{Number of unique exposure levels evaluated}
#'   \item{linear_effect}{Linear slope coefficient estimate per unit exposure}
#'   \item{conf.low}{Lower bound of 95% CI for linear effect}
#'   \item{conf.high}{Upper bound of 95% CI for linear effect}
#'   \item{chi2_total}{Total association Chi-square / Deviance}
#'   \item{chi2_trend}{1-df Linear trend Chi-square / Likelihood Ratio statistic}
#'   \item{p_trend}{P-value for linear trend}
#'   \item{chi2_nonlinearity}{Departure-from-linearity Chi-square statistic}
#'   \item{df_nonlinearity}{Degrees of freedom for departure from linearity}
#'   \item{p_nonlinearity}{P-value for non-linearity (lack of fit)}
#'   \item{interpretation}{Human-readable summary of dose-response shape}
#'
#' @details
#' Follows methods in Woodward (2014) *Epidemiology: Study Design and Data Analysis*, 3rd Edition, Chapter 3.6.3.
#' Decomposes total exposure deviance into a 1-df linear trend component and a $(k-2)$-df non-linear departure
#' component. A significant $p_{\text{trend}}$ with a non-significant $p_{\text{nonlinearity}}$ indicates a consistent
#' monotonic linear dose-response relationship.
#'
#' @export
#' @examples
#' library(dplyr)
#' set.seed(42)
#' dose_df <- tibble(
#'   dose = rep(0:4, each = 40),
#'   outcome = rbinom(200, 1, prob = 0.10 + 0.12 * rep(0:4, each = 40))
#' )
#' kk_trend_nonlinearity(dose_df, dose, outcome, family = "binomial")
kk_trend_nonlinearity <- function(data, exposure, outcome, family = c("binomial", "gaussian", "poisson"), alpha = 0.05) {
  family <- match.arg(family)
  exp_col <- rlang::as_name(rlang::enquo(exposure))
  out_col <- rlang::as_name(rlang::enquo(outcome))

  df <- data %>%
    dplyr::select(dplyr::all_of(c(exp_col, out_col))) %>%
    stats::na.omit()

  exp_vals <- sort(unique(df[[exp_col]]))
  k_levels <- length(exp_vals)

  if (k_levels < 3) {
    stop("Testing departure from linearity requires at least 3 distinct exposure levels.")
  }

  fam_fun <- switch(family,
    binomial = stats::binomial(),
    gaussian = stats::gaussian(),
    poisson = stats::poisson()
  )

  # Fit 3 models: Null, Linear, Factor
  f_null <- stats::as.formula(paste0(out_col, " ~ 1"))
  f_lin <- stats::as.formula(paste0(out_col, " ~ ", exp_col))
  f_fact <- stats::as.formula(paste0(out_col, " ~ factor(", exp_col, ")"))

  m_null <- stats::glm(f_null, data = df, family = fam_fun)
  m_lin <- stats::glm(f_lin, data = df, family = fam_fun)
  m_fact <- stats::glm(f_fact, data = df, family = fam_fun)

  # Linear effect estimate
  lin_coef <- stats::coef(m_lin)[2]
  lin_se <- summary(m_lin)$coefficients[2, 2]
  z <- stats::qnorm(1 - alpha / 2)
  ci_low <- lin_coef - z * lin_se
  ci_high <- lin_coef + z * lin_se

  # Deviance / Chi-square LRT decomposition
  dev_null <- stats::deviance(m_null)
  dev_lin <- stats::deviance(m_lin)
  dev_fact <- stats::deviance(m_fact)

  chi2_tot <- dev_null - dev_fact
  df_tot <- k_levels - 1

  chi2_tr <- dev_null - dev_lin
  p_tr <- 1 - stats::pchisq(chi2_tr, df = 1)

  chi2_nonlin <- dev_lin - dev_fact
  df_nonlin <- k_levels - 2
  p_nonlin <- 1 - stats::pchisq(chi2_nonlin, df = df_nonlin)

  interp <- paste0(
    "Dose-Response Shape Analysis (", k_levels, " exposure levels):\n",
    "1. Linear Trend: Chi2(1) = ", sprintf("%.2f", chi2_tr), ", p = ", format.pval(p_tr, digits = 3), "\n",
    "2. Non-linearity (Departure): Chi2(", df_nonlin, ") = ", sprintf("%.2f", chi2_nonlin), ", p = ", format.pval(p_nonlin, digits = 3), "\n",
    "Interpretation: ",
    if (p_tr < alpha && p_nonlin >= alpha) {
      "Significant linear dose-response trend with no evidence of non-linear departure (simple linear association)."
    } else if (p_nonlin < alpha) {
      "Statistically significant non-linear curvature/departure from linearity detected."
    } else {
      "No statistically significant linear trend or dose-response association detected."
    }
  )

  res <- tibble::tibble(
    exposure_levels = k_levels,
    linear_effect = lin_coef,
    conf.low = ci_low,
    conf.high = ci_high,
    chi2_total = chi2_tot,
    df_total = df_tot,
    chi2_trend = chi2_tr,
    p_trend = p_tr,
    chi2_nonlinearity = chi2_nonlin,
    df_nonlinearity = df_nonlin,
    p_nonlinearity = p_nonlin,
    method = paste0("Dose-Response Deviance Decomposition (", family, ")"),
    interpretation = interp
  )

  class(res) <- c("kk_trend_nonlinearity", class(res))
  return(res)
}
