# ============================================================
# RELIABILITY & AGREEMENT (ICC, INTERNAL CONSISTENCY)
# ============================================================

#' Intraclass Correlation Coefficient (KK)
#'
#' @description Computes intraclass correlation coefficients (ICC) for the
#'   agreement of continuous measurements made by several raters on the same
#'   subjects. Returns all six Shrout-Fleiss forms (ICC1, ICC2, ICC3 and their
#'   average-rater counterparts) with F-tests and confidence intervals.
#'
#' @param data Data frame in wide format: one row per subject, one column per
#'   rater. If `raters` is NULL all columns are treated as raters.
#' @param raters Optional character vector of the rater columns to use.
#' @param conf.level Confidence level (default 0.95).
#'
#' @return Tibble of ICC types with point estimates, confidence intervals,
#'   F statistic, and p-value.
#'
#' @examples
#' \dontrun{
#' # Three raters scoring the same 10 subjects
#' ratings <- data.frame(
#'   r1 = c(9, 6, 8, 7, 10, 6, 8, 7, 9, 5),
#'   r2 = c(8, 6, 7, 8, 9, 5, 8, 6, 9, 6),
#'   r3 = c(9, 5, 8, 8, 9, 6, 7, 7, 8, 5)
#' )
#' kk_icc(ratings)
#' }
#'
#' @export
kk_icc <- function(data, raters = NULL, conf.level = 0.95) {
              validate_data_frame(data)
              if (!requireNamespace("psych", quietly = TRUE)) {
                            stop("Package 'psych' is required for kk_icc().")
              }
              if (!is.null(raters)) {
                            missing_cols <- setdiff(raters, names(data))
                            if (length(missing_cols) > 0) {
                                          stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
                            }
                            data <- data[, raters, drop = FALSE]
              }
              mat <- as.matrix(data[, vapply(data, is.numeric, logical(1)), drop = FALSE])
              if (ncol(mat) < 2) stop("Need at least two numeric rater columns.")

              res <- psych::ICC(mat, lmer = FALSE, alpha = 1 - conf.level)
              tibble::as_tibble(res$results) %>%
                            dplyr::transmute(
                                          type = .data$type,
                                          icc = .data$ICC,
                                          conf.low = .data$`lower bound`,
                                          conf.high = .data$`upper bound`,
                                          F_stat = .data$F,
                                          df1 = .data$df1,
                                          df2 = .data$df2,
                                          p.value = .data$p,
                                          conf.level = conf.level
                            )
}

#' Internal-Consistency Reliability (Cronbach's Alpha) (KK)
#'
#' @description Computes Cronbach's alpha (and the standardized alpha) for a set
#'   of items measuring a single construct, together with per-item statistics
#'   including alpha-if-item-dropped, useful for scale and questionnaire
#'   validation.
#'
#' @param data Data frame of item responses (one column per item). If `items`
#'   is NULL all numeric columns are used.
#' @param items Optional character vector of item columns to use.
#'
#' @return Tibble of per-item statistics carrying the overall raw and
#'   standardized alpha as the attribute `alpha` (a one-row tibble).
#'
#' @examples
#' \dontrun{
#' items <- data.frame(
#'   q1 = c(4, 5, 3, 4, 5), q2 = c(4, 4, 3, 5, 5),
#'   q3 = c(5, 5, 2, 4, 4), q4 = c(4, 5, 3, 4, 5)
#' )
#' kk_reliability(items)
#' }
#'
#' @export
kk_reliability <- function(data, items = NULL) {
              validate_data_frame(data)
              if (!requireNamespace("psych", quietly = TRUE)) {
                            stop("Package 'psych' is required for kk_reliability().")
              }
              if (!is.null(items)) {
                            missing_cols <- setdiff(items, names(data))
                            if (length(missing_cols) > 0) {
                                          stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
                            }
                            data <- data[, items, drop = FALSE]
              }
              mat <- data[, vapply(data, is.numeric, logical(1)), drop = FALSE]
              if (ncol(mat) < 2) stop("Need at least two numeric item columns.")

              a <- suppressWarnings(psych::alpha(mat, warnings = FALSE))

              per_item <- tibble::as_tibble(a$item.stats, rownames = "item") %>%
                            dplyr::bind_cols(
                                          tibble::as_tibble(a$alpha.drop)["raw_alpha"] %>%
                                                        dplyr::rename(alpha_if_dropped = "raw_alpha")
                            )

              attr(per_item, "alpha") <- tibble::tibble(
                            raw_alpha = a$total$raw_alpha,
                            std_alpha = a$total$std.alpha,
                            n_items = ncol(mat),
                            mean = a$total$mean,
                            sd = a$total$sd
              )
              per_item
}
