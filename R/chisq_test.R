# ============================================================
# CHI-SQUARE TEST FOR R X C TABLES
# ============================================================

#' Chi-Square Test for r x c Contingency Tables
#'
#' @description Performs Pearson's Chi-Square Test of Independence or Homogeneity on categorical data.
#'   Supports raw data frames, aggregated tables/matrices, grouped data frames, and automatic
#'   post-hoc pairwise comparisons for tables larger than $2 \times 2$.
#'
#' @param data A data frame, table, or matrix. If a data frame is provided, the row
#'   variable must be specified in `r` and the column variable in `c`.
#' @param r Column name (as symbol or string) representing rows if `data` is a data frame.
#'   Ignored if `data` is a table or matrix.
#' @param c Column name (as symbol or string) representing columns if `data` is a data frame.
#'   Ignored if `data` is a table or matrix.
#' @param correct Logical, whether to apply continuity correction for $2 \times 2$ tables (default: `TRUE`).
#' @param pairwise Logical, whether to perform post-hoc pairwise comparisons of rows when the table
#'   has more than 2 rows (default: `TRUE`).
#' @param adjust_method Character string specifying the p-value adjustment method for post-hoc
#'   comparisons (default: `"bonferroni"`). Must be one of the methods accepted by \code{\link[stats]{p.adjust}}.
#'
#' @return A tibble with the following columns:
#'   \itemize{
#'     \item \code{method}: The test method used.
#'     \item \code{statistic}: Pearson's Chi-Square statistic ($\chi^2$).
#'     \item \code{df}: The degrees of freedom.
#'     \item \code{p_value}: The p-value of the test.
#'     \item \code{observed}: Observed frequencies formatted as a string.
#'     \item \code{expected}: Expected frequencies formatted as a string.
#'     \item \code{pairwise_results}: A list column containing a tibble of pairwise comparisons
#'       (or \code{"not performed"} if not applicable).
#'   }
#'
#' @examples
#' # Example 16.5: Biting frequency among species (4 x 3 table)
#' species <- c(rep("Mice", 60), rep("Gerbils", 50), rep("Hamsters", 90), rep("Guinea Pigs", 80))
#' biting <- c(rep("Not", 20), rep("Mild", 16), rep("Flagrant", 24),
#'             rep("Not", 30), rep("Mild", 10), rep("Flagrant", 10),
#'             rep("Not", 50), rep("Mild", 30), rep("Flagrant", 10),
#'             rep("Not", 19), rep("Mild", 11), rep("Flagrant", 50))
#' df <- data.frame(species = species, biting = biting)
#' kk_chisq_test(df, species, biting)
#'
#' # Matrix input
#' tbl <- matrix(c(20, 16, 24, 30, 10, 10, 50, 30, 10, 19, 11, 50), nrow = 4, byrow = TRUE)
#' rownames(tbl) <- c("Mice", "Gerbils", "Hamsters", "Guinea Pigs")
#' colnames(tbl) <- c("Not", "Mild", "Flagrant")
#' kk_chisq_test(tbl)
#'
#' @export
kk_chisq_test <- function(data, r = NULL, c = NULL,
                          correct = TRUE,
                          pairwise = TRUE,
                          adjust_method = "bonferroni") {
  
  # Handle grouped data frame
  if (dplyr::is_grouped_df(data)) {
    r_enquo <- rlang::enquo(r)
    c_enquo <- rlang::enquo(c)
    res <- data %>%
      dplyr::group_modify(function(grp_data, grp_keys) {
        rlang::inject(kk_chisq_test(
          data = grp_data,
          r = !!r_enquo,
          c = !!c_enquo,
          correct = correct,
          pairwise = pairwise,
          adjust_method = adjust_method
        ))
      })
    return(res)
  }
  
  # Construct contingency table
  if (is.matrix(data) || is.table(data)) {
    tbl <- data
  } else if (is.data.frame(data)) {
    r_enquo <- rlang::enquo(r)
    c_enquo <- rlang::enquo(c)
    if (rlang::quo_is_null(r_enquo)) {
      stop("Argument 'r' must be specified when 'data' is a data frame.")
    }
    if (rlang::quo_is_null(c_enquo)) {
      stop("Argument 'c' must be specified when 'data' is a data frame.")
    }
    r_vec <- data %>% dplyr::pull(!!r_enquo)
    c_vec <- data %>% dplyr::pull(!!c_enquo)
    
    # Remove pairwise NA
    valid_indices <- !is.na(r_vec) & !is.na(c_vec)
    r_vec <- r_vec[valid_indices]
    c_vec <- c_vec[valid_indices]
    
    tbl <- table(r_vec, c_vec)
  } else {
    stop("Input 'data' must be a data frame, table, or matrix.")
  }
  
  if (nrow(tbl) < 2 || ncol(tbl) < 2) {
    stop("Contingency table must be at least 2 x 2.")
  }
  
  # Chi-Square test
  chi_res <- suppressWarnings(stats::chisq.test(tbl, correct = correct))
  
  # Format observed and expected strings
  row_names <- rownames(tbl)
  if (is.null(row_names)) {
    row_names <- as.character(seq_len(nrow(tbl)))
    rownames(tbl) <- row_names
  }
  col_names <- colnames(tbl)
  if (is.null(col_names)) {
    col_names <- as.character(seq_len(ncol(tbl)))
    colnames(tbl) <- col_names
  }
  
  obs_parts <- character()
  exp_parts <- character()
  for (i in seq_len(nrow(tbl))) {
    row_obs <- sapply(seq_len(ncol(tbl)), function(j) sprintf("%s=%d", col_names[j], tbl[i, j]))
    obs_parts <- append(obs_parts, sprintf("%s[%s]", row_names[i], paste(row_obs, collapse = ",")))
    
    row_exp <- sapply(seq_len(ncol(tbl)), function(j) sprintf("%s=%.2f", col_names[j], chi_res$expected[i, j]))
    exp_parts <- append(exp_parts, sprintf("%s[%s]", row_names[i], paste(row_exp, collapse = ",")))
  }
  obs_str <- paste(obs_parts, collapse = "; ")
  exp_str <- paste(exp_parts, collapse = "; ")
  
  # Pairwise comparisons
  pairwise_df <- NULL
  if (pairwise && nrow(tbl) > 2) {
    row_combos <- utils::combn(row_names, 2, simplify = FALSE)
    pairwise_list <- list()
    for (combo in row_combos) {
      r1 <- combo[1]
      r2 <- combo[2]
      sub_tbl <- tbl[base::c(r1, r2), , drop = FALSE]
      sub_chi <- suppressWarnings(stats::chisq.test(sub_tbl, correct = correct))
      
      pairwise_list[[length(pairwise_list) + 1]] <- tibble::tibble(
        comparison = sprintf("%s vs %s", r1, r2),
        statistic = as.numeric(sub_chi$statistic),
        df = as.integer(sub_chi$parameter),
        p_value_raw = as.numeric(sub_chi$p.value)
      )
    }
    pairwise_df <- dplyr::bind_rows(pairwise_list)
    pairwise_df$p_value_adj <- stats::p.adjust(pairwise_df$p_value_raw, method = adjust_method)
  }
  
  pairwise_col <- if (is.null(pairwise_df)) list("not performed") else list(pairwise_df)
  
  return(tibble::tibble(
    method = "Chi-Square Test of Independence",
    statistic = as.numeric(chi_res$statistic),
    df = as.integer(chi_res$parameter),
    p_value = as.numeric(chi_res$p.value),
    observed = obs_str,
    expected = exp_str,
    pairwise_results = pairwise_col
  ))
}
