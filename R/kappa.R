# ============================================================
# COHEN'S KAPPA AGREEMENT TEST
# ============================================================

#' Cohen's Kappa Agreement Test
#'
#' @description Computes Cohen's Kappa coefficient ($\kappa$) to evaluate the agreement
#'   between two raters or judges categorizing subjects into mutually exclusive categories.
#'   It includes the standard error, Z-statistic, p-value, and confidence interval limits
#'   matching textbook formulas.
#'
#' @param data A data frame, table, or square matrix. If a data frame is provided, the columns
#'   for the two raters must be specified in `rater1` and `rater2`.
#' @param rater1 Column name (as symbol or string) representing the first rater's ratings.
#'   Ignored if `data` is a table or matrix.
#' @param rater2 Column name (as symbol or string) representing the second rater's ratings.
#'   Ignored if `data` is a table or matrix.
#' @param conf.level Confidence level for the interval (default: 0.95).
#'
#' @return A tibble with the following columns:
#'   \itemize{
#'     \item \code{method}: The test method used.
#'     \item \code{kappa}: Cohen's Kappa coefficient ($\kappa$).
#'     \item \code{std_error}: The standard error ($\sigma_\kappa$).
#'     \item \code{z_statistic}: The standard normal Z-statistic.
#'     \item \code{p_value}: Two-sided p-value.
#'     \item \code{conf_lower}: Lower limit of the confidence interval.
#'     \item \code{conf_upper}: Upper limit of the confidence interval.
#'     \item \code{n_obs}: Total number of non-missing observations.
#'     \item \code{observed_agreement}: Observed agreement rate ($P_o$).
#'     \item \code{chance_agreement}: Expected chance agreement rate ($P_e$).
#'   }
#'
#' @examples
#' # Example 16.6: Doctor diagnoses on psychiatric patients (3 x 3 table)
#' doc1 <- c(rep("Schiz", 31), rep("Bipolar", 4), rep("Other", 2),
#'           rep("Schiz", 6), rep("Bipolar", 29), rep("Other", 8),
#'           rep("Schiz", 10), rep("Bipolar", 7), rep("Other", 3))
#' doc2 <- c(rep("Schiz", 37), rep("Bipolar", 43), rep("Other", 20))
#' # Note: We can form the data frame of ratings
#' df <- data.frame(
#'   doc1 = c(rep("Schiz", 31), rep("Bipolar", 6), rep("Other", 10),
#'            rep("Schiz", 4), rep("Bipolar", 29), rep("Other", 7),
#'            rep("Schiz", 2), rep("Bipolar", 8), rep("Other", 3)),
#'   doc2 = c(rep("Schiz", 37), rep("Bipolar", 43), rep("Other", 20))
#' )
#' # Directly using matrix input from Example 16.6
#' tbl <- matrix(c(31, 4, 2, 6, 29, 8, 10, 7, 3), nrow = 3, byrow = TRUE)
#' rownames(tbl) <- c("Schiz", "Bipolar", "Other")
#' colnames(tbl) <- c("Schiz", "Bipolar", "Other")
#' kk_kappa(tbl)
#'
#' @export
kk_kappa <- function(data, rater1 = NULL, rater2 = NULL, conf.level = 0.95) {
  
  # Handle grouped data frame
  if (dplyr::is_grouped_df(data)) {
    rater1_enquo <- rlang::enquo(rater1)
    rater2_enquo <- rlang::enquo(rater2)
    res <- data %>%
      dplyr::group_modify(function(grp_data, grp_keys) {
        rlang::inject(kk_kappa(
          data = grp_data,
          rater1 = !!rater1_enquo,
          rater2 = !!rater2_enquo,
          conf.level = conf.level
        ))
      })
    return(res)
  }
  
  # Construct contingency table
  if (is.matrix(data) || is.table(data)) {
    tbl <- data
    if (nrow(tbl) != ncol(tbl)) {
      stop("Contingency table must be square for Cohen's Kappa.")
    }
  } else if (is.data.frame(data)) {
    rater1_enquo <- rlang::enquo(rater1)
    rater2_enquo <- rlang::enquo(rater2)
    if (rlang::quo_is_null(rater1_enquo)) {
      stop("Argument 'rater1' must be specified when 'data' is a data frame.")
    }
    if (rlang::quo_is_null(rater2_enquo)) {
      stop("Argument 'rater2' must be specified when 'data' is a data frame.")
    }
    r1_vec <- data %>% dplyr::pull(!!rater1_enquo)
    r2_vec <- data %>% dplyr::pull(!!rater2_enquo)
    
    # Remove pairwise NA
    valid_indices <- !is.na(r1_vec) & !is.na(r2_vec)
    r1_vec <- r1_vec[valid_indices]
    r2_vec <- r2_vec[valid_indices]
    
    # Combine levels to ensure square table
    all_levels <- sort(unique(c(as.character(r1_vec), as.character(r2_vec))))
    r1_fac <- factor(r1_vec, levels = all_levels)
    r2_fac <- factor(r2_vec, levels = all_levels)
    
    tbl <- table(r1_fac, r2_fac)
  } else {
    stop("Input 'data' must be a data frame, table, or matrix.")
  }
  
  n <- sum(tbl)
  if (n < 2) {
    stop("Kappa calculation requires at least 2 non-missing observations.")
  }
  
  row_sums <- rowSums(tbl)
  col_sums <- colSums(tbl)
  
  EO <- sum(diag(tbl))
  EE <- sum(row_sums * col_sums) / n
  
  P_o <- EO / n
  P_e <- EE / n
  
  if (P_e == 1) {
    kappa <- 1.0
    se_kappa <- 0.0
    z_stat <- NA_real_
    p_val <- NA_real_
    conf_lower <- 1.0
    conf_upper <- 1.0
  } else {
    kappa <- (P_o - P_e) / (1 - P_e)
    
    # Standard error of kappa (Fleiss/Zar/Handbook Equation 16.31)
    se_kappa <- sqrt(P_o * (1 - P_o) / (n * (1 - P_e)^2))
    
    if (se_kappa == 0) {
      z_stat <- NA_real_
      p_val <- NA_real_
      conf_lower <- kappa
      conf_upper <- kappa
    } else {
      z_stat <- kappa / se_kappa
      p_val <- 2 * stats::pnorm(-abs(z_stat))
      
      z_crit <- stats::qnorm(1 - (1 - conf.level)/2)
      conf_lower <- kappa - z_crit * se_kappa
      conf_upper <- kappa + z_crit * se_kappa
    }
  }
  
  return(tibble::tibble(
    method = "Cohen's Kappa",
    kappa = kappa,
    std_error = se_kappa,
    z_statistic = z_stat,
    p_value = p_val,
    conf_lower = conf_lower,
    conf_upper = conf_upper,
    n_obs = n,
    observed_agreement = P_o,
    chance_agreement = P_e
  ))
}
