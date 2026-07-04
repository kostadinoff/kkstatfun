# ============================================================
# VAN DER WAERDEN NORMAL-SCORES TEST FOR K INDEPENDENT SAMPLES
# ============================================================

#' van der Waerden Normal-Scores Test
#'
#' @description Performs the van der Waerden Normal-Scores Test to evaluate if $k$ independent
#'   samples are derived from identical population distributions. It transforms rank orders into
#'   standard normal quantiles (normal scores) and computes a Chi-Square statistic.
#'   Supports raw data frames, grouped data frames, and automatic post-hoc pairwise comparisons.
#'
#' @param data A data frame or a vector. If a data frame is provided, the column
#'   to analyze must be specified in the `x` argument, and the grouping column in `group`.
#' @param x Column name (as symbol or string) to analyze if `data` is a data frame.
#'   Ignored if `data` is a vector.
#' @param group Column name (as symbol or string) representing the independent groups if `data` is a data frame.
#'   Must be provided as the second vector if `data` is a vector.
#' @param pairwise Logical, whether to perform post-hoc pairwise comparisons of group mean normal scores
#'   when the number of groups is greater than 2 (default: `TRUE`).
#' @param adjust_method Character string specifying the p-value adjustment method for post-hoc
#'   comparisons (default: `"bonferroni"`). Must be one of the methods accepted by \code{\link[stats]{p.adjust}}.
#'
#' @return A tibble with the following columns:
#'   \itemize{
#'     \item \code{method}: The test method used.
#'     \item \code{statistic}: Pearson's Chi-Square statistic ($\chi^2_{\text{vdw}}$).
#'     \item \code{df}: The degrees of freedom.
#'     \item \code{p_value}: The p-value of the test.
#'     \item \code{variance_normal_scores}: The variance of the normal scores ($s^2$).
#'     \item \code{group_summaries}: A summary string of the counts and mean normal scores for each group.
#'     \item \code{pairwise_results}: A list column containing a tibble of pairwise comparisons
#'       (or \code{"not performed"} if not applicable).
#'   }
#'
#' @examples
#' # Example 23.1: Noise learning performance (3 groups)
#' scores <- c(8, 10, 9, 10, 9, 7, 8, 5, 8, 5, 4, 8, 7, 5, 7)
#' groups <- c(rep("No Noise", 5), rep("Moderate Noise", 5), rep("Extreme Noise", 5))
#' kk_vdw_test(x = scores, group = groups)
#'
#' # Tidy evaluation with a data frame
#' df <- data.frame(scores = scores, groups = groups)
#' kk_vdw_test(df, scores, groups)
#'
#' @export
kk_vdw_test <- function(data, x = NULL, group = NULL,
                        pairwise = TRUE,
                        adjust_method = "bonferroni") {
  
  # Handle grouped data frame
  if (dplyr::is_grouped_df(data)) {
    x_enquo <- rlang::enquo(x)
    group_enquo <- rlang::enquo(group)
    res <- data %>%
      dplyr::group_modify(function(grp_data, grp_keys) {
        rlang::inject(kk_vdw_test(
          data = grp_data,
          x = !!x_enquo,
          group = !!group_enquo,
          pairwise = pairwise,
          adjust_method = adjust_method
        ))
      })
    return(res)
  }
  
  # Handle data frame vs vector input
  x_enquo <- rlang::enquo(x)
  group_enquo <- rlang::enquo(group)
  
  if (is.data.frame(data)) {
    if (rlang::quo_is_null(x_enquo)) {
      stop("Argument 'x' must be specified when 'data' is a data frame.")
    }
    if (rlang::quo_is_null(group_enquo)) {
      stop("Argument 'group' must be specified when 'data' is a data frame.")
    }
    vec <- data %>% dplyr::pull(!!x_enquo)
    grp <- data %>% dplyr::pull(!!group_enquo)
  } else {
    vec <- data
    grp <- group
    if (is.null(grp)) {
      stop("Argument 'group' must be specified when 'data' is a vector.")
    }
  }
  
  # Remove NA values pairwise
  valid_indices <- !is.na(vec) & !is.na(grp)
  vec <- vec[valid_indices]
  grp <- as.character(grp[valid_indices])
  
  N <- length(vec)
  if (N < 3) {
    stop("Input sequence must have at least 3 non-missing observations.")
  }
  
  group_levels <- sort(unique(grp))
  k <- length(group_levels)
  
  if (k < 2) {
    stop("Input must contain at least 2 distinct groups.")
  }
  
  # Convert ranks to normal scores
  ranks <- rank(vec, ties.method = "average")
  z <- stats::qnorm(ranks / (N + 1))
  
  # Variance of normal scores (Handbook Equation 23.1)
  s2 <- sum(z^2) / (N - 1)
  
  if (s2 == 0) {
    stop("Normal scores have zero variance; cannot perform van der Waerden test.")
  }
  
  mz <- tapply(z, grp, mean)
  n_grp <- tapply(z, grp, length)
  
  # Test statistic (Handbook Equation 23.2)
  X2_vdw <- sum(n_grp * mz^2) / s2
  df_vdw <- k - 1
  p_val <- stats::pchisq(X2_vdw, df = df_vdw, lower.tail = FALSE)
  
  # Form group summary string
  summary_parts <- sapply(group_levels, function(g) {
    sprintf("%s: n=%d, mean_z=%.3f", g, n_grp[g], mz[g])
  })
  group_summaries <- paste(summary_parts, collapse = "; ")
  
  # Pairwise comparisons
  pairwise_df <- NULL
  if (pairwise && k > 2) {
    row_combos <- utils::combn(group_levels, 2, simplify = FALSE)
    pairwise_list <- list()
    for (combo in row_combos) {
      g1 <- combo[1]
      g2 <- combo[2]
      
      diff_mean <- mz[g1] - mz[g2]
      se_diff <- sqrt(s2 * (1 / n_grp[g1] + 1 / n_grp[g2]))
      z_stat <- diff_mean / se_diff
      p_val_raw <- 2 * stats::pnorm(-abs(z_stat))
      
      pairwise_list[[length(pairwise_list) + 1]] <- tibble::tibble(
        comparison = sprintf("%s vs %s", g1, g2),
        mean_difference = diff_mean,
        z_statistic = z_stat,
        p_value_raw = p_val_raw
      )
    }
    pairwise_df <- dplyr::bind_rows(pairwise_list)
    pairwise_df$p_value_adj <- stats::p.adjust(pairwise_df$p_value_raw, method = adjust_method)
  }
  
  pairwise_col <- if (is.null(pairwise_df)) list("not performed") else list(pairwise_df)
  
  return(tibble::tibble(
    method = "van der Waerden Normal-Scores Test",
    statistic = X2_vdw,
    df = as.integer(df_vdw),
    p_value = p_val,
    variance_normal_scores = s2,
    group_summaries = group_summaries,
    pairwise_results = pairwise_col
  ))
}
