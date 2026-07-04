# ============================================================
# MEDIAN TEST FOR INDEPENDENT SAMPLES
# ============================================================

#' Median Test for Independent Samples
#'
#' @description Performs the Median Test for Independent Samples to evaluate if $k$ independent
#'   groups differ in their central tendency. It compares the proportions of observations in
#'   each group that fall above versus below/equal to the composite median of all groups combined.
#'   When $k = 2$, it also returns Fisher's Exact Test and the proportions Z-Test.
#'
#' @param data A data frame or a vector. If a data frame is provided, the column
#'   to analyze must be specified in the `x` argument, and the grouping column in `group`.
#' @param x Column name (as symbol or string) to analyze if `data` is a data frame.
#'   Ignored if `data` is a vector.
#' @param group Column name (as symbol or string) representing the independent groups if `data` is a data frame.
#'   Must be provided as the second vector if `data` is a vector.
#' @param tie_action Character string specifying how to handle observations equal to the composite median.
#'   Must be one of `"below"` (default, includes ties in the below/equal category),
#'   `"drop"` (removes observations equal to the median), or
#'   `"split"` (assigns half of the tie observations to above and half to below/equal).
#' @param alternative Character string specifying the alternative hypothesis for $k = 2$ tests.
#'   Must be one of `"two.sided"` (default), `"less"`, or `"greater"`.
#' @param continuity_correction Logical, whether to apply continuity correction in the Chi-Square test
#'   and Z-Test (default: `TRUE`).
#' @param conf.level Confidence level for the interval (default: 0.95).
#'
#' @return A tibble with the following columns:
#'   \itemize{
#'     \item \code{method}: The test method used.
#'     \item \code{alternative}: The alternative hypothesis.
#'     \item \code{statistic_name}: Name of the test statistic ("Chi-Square", "Fisher's p-value", or "Z").
#'     \item \code{statistic}: The test statistic value.
#'     \item \code{df}: The degrees of freedom for the Chi-Square test (or \code{NA} for others).
#'     \item \code{p_value}: The p-value of the test.
#'     \item \code{composite_median}: The calculated composite median value.
#'     \item \code{table_summary}: A summary string of the counts above and below the median for each group.
#'   }
#'
#' @examples
#' # Example 16.4: Standardized coordination scores (Males vs Females)
#' scores <- c(rep(10, 30), rep(2, 70), rep(10, 60), rep(2, 40))
#' gender <- c(rep("Males", 100), rep("Females", 100))
#' kk_median_test(x = scores, group = gender)
#'
#' # Tidy evaluation with a data frame
#' df <- data.frame(scores = scores, gender = gender)
#' kk_median_test(df, scores, gender)
#'
#' @export
kk_median_test <- function(data, x = NULL, group = NULL,
                           tie_action = c("below", "drop", "split"),
                           alternative = c("two.sided", "less", "greater"),
                           continuity_correction = TRUE,
                           conf.level = 0.95) {
  
  tie_action <- match.arg(tie_action)
  alternative <- match.arg(alternative)
  
  # Handle grouped data frame
  if (dplyr::is_grouped_df(data)) {
    x_enquo <- rlang::enquo(x)
    group_enquo <- rlang::enquo(group)
    res <- data %>%
      dplyr::group_modify(function(grp_data, grp_keys) {
        rlang::inject(kk_median_test(
          data = grp_data,
          x = !!x_enquo,
          group = !!group_enquo,
          tie_action = tie_action,
          alternative = alternative,
          continuity_correction = continuity_correction,
          conf.level = conf.level
        ))
      })
    return(res)
  }
  
  x_enquo <- rlang::enquo(x)
  group_enquo <- rlang::enquo(group)
  
  # Handle data frame vs vector input
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
  
  if (length(vec) < 3) {
    stop("Input sequence must have at least 3 non-missing observations.")
  }
  
  # Calculate composite median
  comp_median <- stats::median(vec)
  
  group_levels <- sort(unique(grp))
  k <- length(group_levels)
  
  if (k < 2) {
    stop("Input must contain at least 2 distinct groups.")
  }
  
  above_counts <- numeric(k)
  below_counts <- numeric(k)
  names(above_counts) <- group_levels
  names(below_counts) <- group_levels
  
  for (i in seq_along(group_levels)) {
    g <- group_levels[i]
    v_g <- vec[grp == g]
    
    a_g <- sum(v_g > comp_median)
    b_g <- sum(v_g < comp_median)
    t_g <- sum(v_g == comp_median)
    
    if (tie_action == "below") {
      above_counts[i] <- a_g
      below_counts[i] <- b_g + t_g
    } else if (tie_action == "drop") {
      above_counts[i] <- a_g
      below_counts[i] <- b_g
    } else if (tie_action == "split") {
      above_counts[i] <- a_g + t_g / 2
      below_counts[i] <- b_g + t_g / 2
    }
  }
  
  # Build contingency table
  tbl <- matrix(c(above_counts, below_counts), nrow = k, ncol = 2)
  rownames(tbl) <- group_levels
  colnames(tbl) <- c("Above", "Below")
  
  # Form table summary string
  summary_parts <- sapply(group_levels, function(g) {
    sprintf("%s: Above=%s, Below=%s", g, as.character(above_counts[g]), as.character(below_counts[g]))
  })
  table_summary <- paste(summary_parts, collapse = "; ")
  
  # Chi-Square test
  chi_res <- suppressWarnings(stats::chisq.test(tbl, correct = continuity_correction))
  
  res_chi <- tibble::tibble(
    method = "Median Test (Chi-Square)",
    alternative = "two.sided",
    statistic_name = "Chi-Square",
    statistic = as.numeric(chi_res$statistic),
    df = as.integer(chi_res$parameter),
    p_value = as.numeric(chi_res$p.value),
    composite_median = comp_median,
    table_summary = table_summary
  )
  
  if (k == 2) {
    # Round table values for Fisher exact test
    tbl_int <- round(tbl)
    fish_res <- stats::fisher.test(tbl_int, alternative = alternative, conf.level = conf.level)
    
    res_fisher <- tibble::tibble(
      method = "Median Test (Fisher's Exact)",
      alternative = alternative,
      statistic_name = "Fisher's p-value",
      statistic = as.numeric(fish_res$p.value),
      df = NA_integer_,
      p_value = as.numeric(fish_res$p.value),
      composite_median = comp_median,
      table_summary = table_summary
    )
    
    # Proportions Z-Test
    x_prop <- above_counts
    n_prop <- above_counts + below_counts
    
    # Avoid division by zero
    if (all(n_prop > 0)) {
      prop_res <- suppressWarnings(stats::prop.test(x = x_prop, n = n_prop, alternative = alternative, correct = continuity_correction))
      z_stat <- sign(x_prop[1]/n_prop[1] - x_prop[2]/n_prop[2]) * sqrt(as.numeric(prop_res$statistic))
      
      res_z <- tibble::tibble(
        method = "Median Test (Binomial Proportions Z-Test)",
        alternative = alternative,
        statistic_name = "Z",
        statistic = z_stat,
        df = NA_integer_,
        p_value = as.numeric(prop_res$p.value),
        composite_median = comp_median,
        table_summary = table_summary
      )
      
      return(dplyr::bind_rows(res_chi, res_fisher, res_z))
    } else {
      return(dplyr::bind_rows(res_chi, res_fisher))
    }
  } else {
    return(res_chi)
  }
}
