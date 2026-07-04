# ============================================================
# PEARSON CORRELATION COMPARISON TESTS
# ============================================================

#' Compare Independent Correlations
#'
#' @description Evaluates whether two or $k$ independent correlation coefficients differ significantly
#'   using Fisher's z-transformation. If $k = 2$, it performs a standard normal Z-test. If $k > 2$,
#'   it performs a Chi-Square test of homogeneity.
#'
#' @param data A data frame or a vector. If a data frame is provided, can specify either raw variables
#'   (`x`, `y`, `group`) or pre-calculated fields (`r`, `n`).
#' @param x If `data` is a data frame and raw data is used, the column representing the first continuous variable.
#'   If `data` is a vector, the sample sizes (`n`) must be provided here.
#' @param y If `data` is a data frame and raw data is used, the column representing the second continuous variable.
#' @param group If `data` is a data frame and raw data is used, the column defining the independent groups.
#' @param r If `data` is a data frame and pre-calculated statistics are used, the column name containing correlations.
#' @param n If `data` is a data frame and pre-calculated statistics are used, the column name containing sample sizes.
#' @param alternative Character string specifying the alternative hypothesis for $k = 2$ tests.
#'   Must be one of `"two.sided"` (default), `"less"`, or `"greater"`.
#' @param conf.level Confidence level for the interval (default: 0.95).
#'
#' @return A tibble with the following columns:
#'   \itemize{
#'     \item \code{method}: The test method used.
#'     \item \code{alternative}: The alternative hypothesis.
#'     \item \code{statistic_name}: Name of the test statistic ("Z" or "Chi-Square").
#'     \item \code{statistic}: The test statistic value.
#'     \item \code{df}: The degrees of freedom for the homogeneity test (or \code{NA} for $k = 2$).
#'     \item \code{p_value}: The p-value of the test.
#'     \item \code{common_r}: The weighted estimate of the common population correlation.
#'     \item \code{group_summaries}: A summary string of the correlation and sample size for each group.
#'   }
#'
#' @examples
#' # Example 28c: Comparing two independent correlations
#' r_vals <- c(0.955, 0.765)
#' n_vals <- c(5, 5)
#' kk_compare_independent_correlations(r_vals, n_vals)
#'
#' # Example 28d: Homogeneity of three independent correlations
#' r_three <- c(0.955, 0.765, 0.845)
#' n_three <- c(5, 5, 5)
#' kk_compare_independent_correlations(r_three, n_three)
#'
#' @export
kk_compare_independent_correlations <- function(data, x = NULL, y = NULL, group = NULL,
                                                r = NULL, n = NULL,
                                                alternative = c("two.sided", "less", "greater"),
                                                conf.level = 0.95) {
  
  alternative <- match.arg(alternative)
  
  # Handle grouped data frame
  if (dplyr::is_grouped_df(data)) {
    x_enquo <- rlang::enquo(x)
    y_enquo <- rlang::enquo(y)
    group_enquo <- rlang::enquo(group)
    r_enquo <- rlang::enquo(r)
    n_enquo <- rlang::enquo(n)
    
    res <- data %>%
      dplyr::group_modify(function(grp_data, grp_keys) {
        rlang::inject(kk_compare_independent_correlations(
          data = grp_data,
          x = !!x_enquo,
          y = !!y_enquo,
          group = !!group_enquo,
          r = !!r_enquo,
          n = !!n_enquo,
          alternative = alternative,
          conf.level = conf.level
        ))
      })
    return(res)
  }
  
  # Handle raw data vs pre-calculated input
  if (is.data.frame(data)) {
    x_enquo <- rlang::enquo(x)
    y_enquo <- rlang::enquo(y)
    group_enquo <- rlang::enquo(group)
    r_enquo <- rlang::enquo(r)
    n_enquo <- rlang::enquo(n)
    
    if (!rlang::quo_is_null(x_enquo) && !rlang::quo_is_null(y_enquo) && !rlang::quo_is_null(group_enquo)) {
      # Calculate correlation and sample size for each group level
      summary_df <- data %>%
        dplyr::group_by(!!group_enquo) %>%
        dplyr::summarize(
          r_val = stats::cor(!!x_enquo, !!y_enquo, use = "pairwise.complete.obs", method = "pearson"),
          n_val = sum(!is.na(!!x_enquo) & !is.na(!!y_enquo)),
          .groups = "drop"
        )
      r_vec <- summary_df$r_val
      n_vec <- summary_df$n_val
      group_names <- as.character(summary_df[[rlang::as_name(group_enquo)]])
    } else if (!rlang::quo_is_null(r_enquo) && !rlang::quo_is_null(n_enquo)) {
      r_vec <- data %>% dplyr::pull(!!r_enquo)
      n_vec <- data %>% dplyr::pull(!!n_enquo)
      group_names <- as.character(seq_along(r_vec))
    } else {
      stop("Must specify either (x, y, group) for raw data or (r, n) for pre-computed correlations.")
    }
  } else {
    r_vec <- data
    n_vec <- x
    if (is.null(n_vec)) {
      stop("Must specify sample sizes as the second argument when the first argument is a vector of correlations.")
    }
    group_names <- as.character(seq_along(r_vec))
  }
  
  # Remove groups with missing values
  valid <- !is.na(r_vec) & !is.na(n_vec)
  r_vec <- r_vec[valid]
  n_vec <- n_vec[valid]
  group_names <- group_names[valid]
  
  k <- length(r_vec)
  if (k < 2) {
    stop("Must have at least 2 groups/independent correlations to compare.")
  }
  
  if (any(n_vec < 4)) {
    stop("Sample size for each group must be at least 4 to calculate Fisher transformed correlations.")
  }
  
  # Clamp correlations to avoid infinity at exactly -1 or 1
  r_vec_clamped <- pmax(pmin(r_vec, 0.99999), -0.99999)
  z_r <- atanh(r_vec_clamped)
  
  if (k == 2) {
    # Two independent correlations comparison (Z-test, Equation 28.22)
    z_stat <- (z_r[1] - z_r[2]) / sqrt(1/(n_vec[1] - 3) + 1/(n_vec[2] - 3))
    
    if (alternative == "two.sided") {
      p_val <- 2 * stats::pnorm(-abs(z_stat))
    } else if (alternative == "less") {
      p_val <- stats::pnorm(z_stat)
    } else {
      p_val <- stats::pnorm(z_stat, lower.tail = FALSE)
    }
    
    # Common correlation (Equation 28.23)
    num_common <- (n_vec[1] - 3) * z_r[1] + (n_vec[2] - 3) * z_r[2]
    den_common <- (n_vec[1] - 3) + (n_vec[2] - 3)
    z_r_common <- num_common / den_common
    r_common <- tanh(z_r_common)
    
    return(tibble::tibble(
      method = "Comparison of Two Independent Correlations",
      alternative = alternative,
      statistic_name = "Z",
      statistic = z_stat,
      df = NA_integer_,
      p_value = p_val,
      common_r = r_common,
      group_summaries = sprintf("%s: r=%.3f (n=%d); %s: r=%.3f (n=%d)", 
                                group_names[1], r_vec[1], n_vec[1],
                                group_names[2], r_vec[2], n_vec[2])
    ))
  } else {
    # Homogeneity of k independent correlations (Chi-Square test, Equation 28.24)
    w_j <- n_vec - 3
    sum_w_z <- sum(w_j * z_r)
    sum_w <- sum(w_j)
    
    chi2_val <- sum(w_j * (z_r^2)) - (sum_w_z^2) / sum_w
    df_val <- k - 1
    p_val <- stats::pchisq(chi2_val, df = df_val, lower.tail = FALSE)
    
    # Common correlation (Equation 28.25)
    z_r_common <- sum_w_z / sum_w
    r_common <- tanh(z_r_common)
    
    group_parts <- sapply(seq_along(r_vec), function(i) {
      sprintf("%s: r=%.3f (n=%d)", group_names[i], r_vec[i], n_vec[i])
    })
    group_summaries <- paste(group_parts, collapse = "; ")
    
    return(tibble::tibble(
      method = "Homogeneity Test of k Independent Correlations",
      alternative = "two.sided",
      statistic_name = "Chi-Square",
      statistic = chi2_val,
      df = as.integer(df_val),
      p_value = p_val,
      common_r = r_common,
      group_summaries = group_summaries
    ))
  }
}

#' Compare Dependent Correlations
#'
#' @description Evaluates whether two variables ($X, Y$) correlate differently with a common third
#'   criterion variable ($Z$) within the same sample, using Steiger's t-statistic.
#'
#' @param data A data frame or a single correlation coefficient ($r_{XZ}$). If a data frame is provided,
#'   can specify raw variables (`x`, `y`, `z`) or pre-calculated fields (`rxz`, `ryz`, `rxy`, `n`).
#' @param x If `data` is a data frame and raw data is used, the column representing variable $X$.
#'   If pre-calculated values are used, the correlation coefficient $r_{YZ}$.
#' @param y If `data` is a data frame and raw data is used, the column representing variable $Y$.
#'   If pre-calculated values are used, the correlation coefficient $r_{XY}$.
#' @param z If `data` is a data frame and raw data is used, the column representing variable $Z$.
#'   If pre-calculated values are used, the sample size $n$.
#' @param rxz If `data` is a data frame and pre-calculated statistics are used, the column containing $r_{XZ}$.
#' @param ryz If `data` is a data frame and pre-calculated statistics are used, the column containing $r_{YZ}$.
#' @param rxy If `data` is a data frame and pre-calculated statistics are used, the column containing $r_{XY}$.
#' @param n If `data` is a data frame and pre-calculated statistics are used, the column containing sample size $n$.
#' @param alternative Character string specifying the alternative hypothesis.
#'   Must be one of `"two.sided"` (default), `"less"`, or `"greater"`.
#'
#' @return A tibble with the following columns:
#'   \itemize{
#'     \item \code{method}: The test method used.
#'     \item \code{alternative}: The alternative hypothesis.
#'     \item \code{statistic_name}: Name of the test statistic ("t").
#'     \item \code{statistic}: Steiger's t-statistic.
#'     \item \code{df}: The degrees of freedom ($n - 3$).
#'     \item \code{p_value}: The p-value of the test.
#'     \item \code{rxz}: Pearson correlation between $X$ and $Z$.
#'     \item \code{ryz}: Pearson correlation between $Y$ and $Z$.
#'     \item \code{rxy}: Pearson correlation between $X$ and $Y$.
#'     \item \code{n}: The sample size.
#'   }
#'
#' @examples
#' # Example 28e: Dependent correlations (sugar vs cavities vs salt)
#' kk_compare_dependent_correlations(rxz = 0.955, ryz = 0.52, rxy = 0.37, n = 5)
#'
#' @export
kk_compare_dependent_correlations <- function(data, x = NULL, y = NULL, z = NULL,
                                              rxz = NULL, ryz = NULL, rxy = NULL, n = NULL,
                                              alternative = c("two.sided", "less", "greater")) {
  
  alternative <- match.arg(alternative)
  
  # Handle grouped data frame
  if (dplyr::is_grouped_df(data)) {
    x_enquo <- rlang::enquo(x)
    y_enquo <- rlang::enquo(y)
    z_enquo <- rlang::enquo(z)
    rxz_enquo <- rlang::enquo(rxz)
    ryz_enquo <- rlang::enquo(ryz)
    rxy_enquo <- rlang::enquo(rxy)
    n_enquo <- rlang::enquo(n)
    
    res <- data %>%
      dplyr::group_modify(function(grp_data, grp_keys) {
        rlang::inject(kk_compare_dependent_correlations(
          data = grp_data,
          x = !!x_enquo,
          y = !!y_enquo,
          z = !!z_enquo,
          rxz = !!rxz_enquo,
          ryz = !!ryz_enquo,
          rxy = !!rxy_enquo,
          n = !!n_enquo,
          alternative = alternative
        ))
      })
    return(res)
  }
  
  # Handle raw data vs pre-calculated input
  if (is.data.frame(data)) {
    x_enquo <- rlang::enquo(x)
    y_enquo <- rlang::enquo(y)
    z_enquo <- rlang::enquo(z)
    rxz_enquo <- rlang::enquo(rxz)
    ryz_enquo <- rlang::enquo(ryz)
    rxy_enquo <- rlang::enquo(rxy)
    n_enquo <- rlang::enquo(n)
    
    if (!rlang::quo_is_null(x_enquo) && !rlang::quo_is_null(y_enquo) && !rlang::quo_is_null(z_enquo)) {
      x_vec <- data %>% dplyr::pull(!!x_enquo)
      y_vec <- data %>% dplyr::pull(!!y_enquo)
      z_vec <- data %>% dplyr::pull(!!z_enquo)
      
      valid <- !is.na(x_vec) & !is.na(y_vec) & !is.na(z_vec)
      n_val <- sum(valid)
      
      if (n_val < 4) {
        stop("Must have at least 4 non-missing complete observations.")
      }
      
      rxz_val <- stats::cor(x_vec[valid], z_vec[valid], method = "pearson")
      ryz_val <- stats::cor(y_vec[valid], z_vec[valid], method = "pearson")
      rxy_val <- stats::cor(x_vec[valid], y_vec[valid], method = "pearson")
    } else if (!rlang::quo_is_null(rxz_enquo) && !rlang::quo_is_null(ryz_enquo) && !rlang::quo_is_null(rxy_enquo) && !rlang::quo_is_null(n_enquo)) {
      rxz_val <- data %>% dplyr::pull(!!rxz_enquo)
      ryz_val <- data %>% dplyr::pull(!!ryz_enquo)
      rxy_val <- data %>% dplyr::pull(!!rxy_enquo)
      n_val <- data %>% dplyr::pull(!!n_enquo)
    } else {
      stop("Must specify either (x, y, z) for raw data or (rxz, ryz, rxy, n) for pre-computed correlations.")
    }
  } else {
    rxz_val <- data
    ryz_val <- x
    rxy_val <- y
    n_val <- z
    if (is.null(ryz_val) || is.null(rxy_val) || is.null(n_val)) {
      stop("For pre-computed numeric input, must specify rxz, ryz, rxy, and n.")
    }
  }
  
  if (any(n_val <= 3, na.rm = TRUE)) {
    stop("Sample size n must be greater than 3 for dependent correlation comparison.")
  }
  
  # Clamp correlations to avoid infinity issues
  rxz_val <- pmax(pmin(rxz_val, 0.99999), -0.99999)
  ryz_val <- pmax(pmin(ryz_val, 0.99999), -0.99999)
  rxy_val <- pmax(pmin(rxy_val, 0.99999), -0.99999)
  
  # Determinant |R|
  detR <- 1 - rxz_val^2 - ryz_val^2 - rxy_val^2 + 2 * rxz_val * ryz_val * rxy_val
  
  # Mean correlation
  rbar <- (rxz_val + ryz_val) / 2
  
  # Formula components
  num <- (n_val - 1) * (1 + rxy_val)
  den <- 2 * ((n_val - 1) / (n_val - 3)) * detR + (rbar^2) * ((1 - rxy_val)^3)
  
  if (any(den <= 0, na.rm = TRUE)) {
    stop("Denominator in Steiger's t-statistic is non-positive; check correlation values.")
  }
  
  t_stat <- (ryz_val - rxz_val) * sqrt(num / den)
  df_val <- n_val - 3
  
  if (alternative == "two.sided") {
    p_val <- 2 * stats::pt(-abs(t_stat), df = df_val)
  } else if (alternative == "less") {
    p_val <- stats::pt(t_stat, df = df_val)
  } else {
    p_val <- stats::pt(t_stat, df = df_val, lower.tail = FALSE)
  }
  
  return(tibble::tibble(
    method = "Comparison of Two Dependent Correlations (Steiger's T)",
    alternative = alternative,
    statistic_name = "t",
    statistic = t_stat,
    df = as.integer(df_val),
    p_value = p_val,
    rxz = rxz_val,
    ryz = ryz_val,
    rxy = rxy_val,
    n = as.integer(n_val)
  ))
}
