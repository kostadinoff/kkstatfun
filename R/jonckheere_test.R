#' Jonckheere-Terpstra Test for Ordered Alternatives
#'
#' Performs the Jonckheere-Terpstra non-parametric test to evaluate monotonic dose-response
#' trends across ordered independent groups or exposure levels for continuous outcomes.
#'
#' @param data A data frame containing the variables.
#' @param outcome Name of the continuous outcome variable.
#' @param group Name of the ordered categorical factor or numeric exposure variable.
#' @param alternative Character string specifying the alternative hypothesis:
#'   \code{"two.sided"} (default), \code{"increasing"}, or \code{"decreasing"}.
#' @param continuity_correction Logical. Apply 0.5 continuity correction to the test statistic (default TRUE).
#'
#' @return A tidy \code{tibble} with columns:
#'   \item{j_stat}{Jonckheere-Terpstra $J$ statistic (sum of pairwise Mann-Whitney U statistics)}
#'   \item{expect_j}{Expected value of $J$ under the null hypothesis}
#'   \item{var_j}{Variance of $J$ (adjusted for ties)}
#'   \item{z_stat}{Standardized $Z$ score}
#'   \item{p_value}{$p$-value for the requested alternative hypothesis}
#'   \item{alternative}{Alternative hypothesis tested}
#'   \item{n_obs}{Total number of observations}
#'   \item{n_groups}{Number of ordered groups}
#'
#' @details
#' The Jonckheere-Terpstra test is a non-parametric test for ordered alternatives ($H_0: \theta_1 = \theta_2 = \dots = \theta_k$ vs $H_1: \theta_1 \le \theta_2 \le \dots \le \theta_k$ with at least one strict inequality).
#' It evaluates pairwise Mann-Whitney $U$ count statistics $U_{ij}$ for all ordered group pairs $i < j$:
#' \deqn{J = \sum_{i=1}^{k-1} \sum_{j=i+1}^k U_{ij}}
#' Variance calculation includes exact adjustment for tied observation ranks across the full sample (Sheskin, 2000; Zar, 2010).
#'
#' @references
#' Jonckheere AR (1954). A distribution-free k-sample test against ordered alternatives. \emph{Biometrika}, 41(1/2):133-145.
#' Terpstra TJ (1952). The asymptotic normality and consistency of Kendall's test against trend, when ties are present in one variable. \emph{Indagationes Mathematicae}, 14:327-333.
#' Sheskin DJ (2000). \emph{Handbook of Parametric and Nonparametric Statistical Procedures}, 2nd Edition. Chapman & Hall/CRC.
#' Zar JH (2010). \emph{Biostatistical Analysis}, 5th Edition. Prentice Hall.
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' dat <- data.frame(
#'   dose = factor(rep(c("Control", "Low", "Medium", "High"), each = 20),
#'                 levels = c("Control", "Low", "Medium", "High"), ordered = TRUE),
#'   response = c(rnorm(20, 10, 2), rnorm(20, 12, 2), rnorm(20, 15, 2.5), rnorm(20, 19, 3))
#' )
#'
#' # Test for increasing dose-response trend
#' kk_jonckheere_test(dat, outcome = "response", group = "dose", alternative = "increasing")
kk_jonckheere_test <- function(data, outcome, group, alternative = c("two.sided", "increasing", "decreasing"), continuity_correction = TRUE) {
  
  alternative <- match.arg(alternative)
  df <- as.data.frame(data)
  
  out_name <- rlang::as_name(rlang::enquo(outcome))
  grp_name <- rlang::as_name(rlang::enquo(group))
  
  # Clean complete cases
  complete_idx <- !is.na(df[[out_name]]) & !is.na(df[[grp_name]])
  y <- df[[out_name]][complete_idx]
  g <- df[[grp_name]][complete_idx]
  
  if (!is.factor(g)) {
    g <- factor(g, ordered = TRUE)
  }
  
  group_levels <- levels(g)
  k <- length(group_levels)
  
  if (k < 2) {
    stop("Group variable must have at least 2 distinct levels.")
  }
  
  # Group sizes
  n_i <- table(g)[group_levels]
  N <- sum(n_i)
  
  # Calculate pairwise U_ij statistics for all i < j
  J <- 0
  for (i in 1:(k - 1)) {
    y_i <- y[g == group_levels[i]]
    for (j in (i + 1):k) {
      y_j <- y[g == group_levels[j]]
      # Mann-Whitney count: count pairs where y_i < y_j, plus 0.5 for ties
      u_ij <- sum(outer(y_i, y_j, function(a, b) ifelse(a < b, 1, ifelse(a == b, 0.5, 0))))
      J <- J + u_ij
    }
  }
  
  # Expected value E(J)
  expect_J <- (N^2 - sum(n_i^2)) / 4
  
  # Variance calculation with tie adjustment
  # Base variance without ties: [N^2(2N+3) - sum(n_i^2(2n_i+3))] / 72
  var_J_untied <- (N^2 * (2 * N + 3) - sum(n_i^2 * (2 * n_i + 3))) / 72
  
  # Tie correction term
  ties <- table(y)
  t_k <- ties[ties > 1]
  if (length(t_k) > 0) {
    tie_adjust <- sum(t_k * (t_k - 1) * (2 * t_k + 5)) / (72 * N * (N - 1) * (N - 2))
    # Corrected variance formula (Hollander & Wolfe 1999 / Sheskin 2000)
    term1 <- (N * (N - 1) * (2 * N + 5) - sum(n_i * (n_i - 1) * (2 * n_i + 5)) - sum(t_k * (t_k - 1) * (2 * t_k + 5))) / 72
    term2 <- (sum(n_i * (n_i - 1) * (n_i - 2)) * sum(t_k * (t_k - 1) * (t_k - 2))) / (36 * N * (N - 1) * (N - 2))
    term3 <- (sum(n_i * (n_i - 1)) * sum(t_k * (t_k - 1))) / (8 * N * (N - 1))
    var_J <- term1 + term2 + term3
    if (var_J <= 0) var_J <- var_J_untied
  } else {
    var_J <- var_J_untied
  }
  
  sd_J <- sqrt(var_J)
  
  # Standardized Z score with optional continuity correction
  diff_J <- J - expect_J
  if (continuity_correction) {
    if (diff_J > 0) {
      z <- (diff_J - 0.5) / sd_J
    } else if (diff_J < 0) {
      z <- (diff_J + 0.5) / sd_J
    } else {
      z <- 0
    }
  } else {
    z <- diff_J / sd_J
  }
  
  # Calculate p-value based on requested alternative
  if (alternative == "increasing") {
    p_val <- stats::pnorm(z, lower.tail = FALSE)
  } else if (alternative == "decreasing") {
    p_val <- stats::pnorm(z, lower.tail = TRUE)
  } else { # two.sided
    p_val <- 2 * stats::pnorm(-abs(z))
  }
  
  res <- tibble::tibble(
    j_stat = J,
    expect_j = expect_J,
    var_j = var_J,
    z_stat = z,
    p_value = p_val,
    alternative = alternative,
    n_obs = N,
    n_groups = k
  )
  
  return(res)
}
