# ============================================================
# AGE-STANDARDIZED RATES WITH EXACT GAMMA CIs
# ============================================================

#' Directly Age-Standardized Rates with Exact Gamma Confidence Intervals
#'
#' Calculates directly age-standardized incidence or mortality rates with exact Gamma confidence intervals
#' using the Fay-Feuer (1997) or Tiwari et al. (2006) modified Gamma distribution methods (SEER standard).
#'
#' @param data A data frame or tibble containing count, population, and standard population variables.
#' @param count Column name (quoted or unquoted) for observed counts/events in each stratum.
#' @param pop Column name (quoted or unquoted) for population size in each stratum.
#' @param std_pop Column name (quoted or unquoted) for standard population weights or counts.
#' @param multiplier Rate multiplier (default `100000` for rate per 100,000 person-years).
#' @param method CI estimation method: `"tiwari"` (default, Tiwari et al. 2006 modified gamma), `"fay_feuer"` (Fay & Feuer 1997 gamma), or `"normal"` (Wald normal approximation).
#' @param conf.level Confidence level for intervals (default `0.95`).
#'
#' @return A tidy tibble of class `kk_std_rates_ci` containing:
#'   \describe{
#'     \item{crude_rate}{Unadjusted crude rate per `multiplier`}
#'     \item{std_rate}{Directly age-standardized rate per `multiplier`}
#'     \item{se}{Standard error of standardized rate}
#'     \item{conf.low}{Lower bound of standardized rate CI}
#'     \item{conf.high}{Upper bound of standardized rate CI}
#'     \item{multiplier}{Rate multiplier used}
#'     \item{method}{CI calculation method applied}
#'     \item{conf.level}{Confidence level}
#'   }
#'
#' @details
#' Follows Szklo & Nieto (2014) *Epidemiology: Beyond the Basics*, Chapter 3, and NCI SEER methodology
#' (Tiwari et al. 2006; Fay & Feuer 1997). Exact Gamma CIs provide valid coverage even when stratum event
#' counts are small or zero.
#'
#' @export
#' @examples
#' library(dplyr)
#' std_df <- tibble(
#'   age_group = c("0-14", "15-64", "65+"),
#'   cases = c(2, 45, 120),
#'   pop = c(10000, 50000, 15000),
#'   std_pop = c(12000, 60000, 20000)
#' )
#' kk_std_rates_ci(std_df, cases, pop, std_pop, method = "tiwari")
kk_std_rates_ci <- function(data, count, pop, std_pop, multiplier = 100000, method = c("tiwari", "fay_feuer", "normal"), conf.level = 0.95) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }

  validate_data_frame(data)
  method <- match.arg(method)
  
  cnt_col <- rlang::as_name(rlang::enquo(count))
  pop_col <- rlang::as_name(rlang::enquo(pop))
  std_col <- rlang::as_name(rlang::enquo(std_pop))
  
  cols_needed <- c(cnt_col, pop_col, std_col)
  if (!all(cols_needed %in% names(data))) {
    stop("One or more specified columns were not found in data.")
  }
  
  df <- data %>%
    dplyr::select(dplyr::all_of(cols_needed)) %>%
    stats::na.omit()
  
  d <- df[[cnt_col]]
  n <- df[[pop_col]]
  W <- df[[std_col]]
  
  if (any(n <= 0) || any(W <= 0) || any(d < 0)) {
    stop("Counts must be non-negative and populations must be strictly positive.")
  }
  
  w <- W / sum(W)
  total_cases <- sum(d)
  total_pop <- sum(n)
  crude_r <- total_cases / total_pop
  
  r_i <- d / n
  std_r <- sum(w * r_i)
  
  v <- sum((w^2 * d) / (n^2))
  se <- sqrt(v)
  
  alpha <- 1 - conf.level
  z <- stats::qnorm(1 - alpha / 2)
  
  if (method == "normal") {
    low <- std_r - z * se
    high <- std_r + z * se
  } else if (method == "fay_feuer") {
    if (total_cases == 0) {
      low <- 0
      w_n_ratio <- sum((w / n)^2)
      high <- 0.5 * stats::qchisq(1 - alpha / 2, df = 2) * (w_n_ratio)
    } else {
      low <- (v / (2 * std_r)) * stats::qchisq(alpha / 2, df = (2 * std_r^2) / v)
      w_max <- max(w / n)
      num_df <- 2 * (std_r + w_max)^2
      den_df <- v + w_max^2
      high <- (den_df / (2 * (std_r + w_max))) * stats::qchisq(1 - alpha / 2, df = num_df / den_df)
    }
  } else if (method == "tiwari") {
    if (total_cases == 0) {
      low <- 0
      w_n_ratio <- sum((w / n)^2)
      high <- 0.5 * stats::qchisq(1 - alpha / 2, df = 2) * (w_n_ratio)
    } else {
      low <- (v / (2 * std_r)) * stats::qchisq(alpha / 2, df = (2 * std_r^2) / v)
      w_max <- max(w / n)
      y <- std_r + w_max
      v_star <- v + w_max^2
      high <- (v_star / (2 * y)) * stats::qchisq(1 - alpha / 2, df = (2 * y^2) / v_star)
    }
  }
  
  res <- tibble::tibble(
    crude_rate = crude_r * multiplier,
    std_rate = std_r * multiplier,
    se = se * multiplier,
    conf.low = max(0, low * multiplier),
    conf.high = high * multiplier,
    multiplier = multiplier,
    method = paste("Direct Standardization -", switch(method, tiwari = "Tiwari Gamma", fay_feuer = "Fay-Feuer Gamma", normal = "Normal")),
    conf.level = conf.level
  )
  
  class(res) <- c("kk_std_rates_ci", class(res))
  return(res)
}
