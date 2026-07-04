# ============================================================
# COVARIATE BALANCE (STANDARDIZED MEAN DIFFERENCES)
# ============================================================

#' Standardized Mean Differences for Covariate Balance (KK)
#'
#' @description Computes standardized mean differences (SMD) between two groups
#'   for each covariate, the standard diagnostic for baseline balance in matched,
#'   weighted, or propensity-score analyses. Continuous variables use the pooled
#'   standard deviation; binary and categorical variables use the proportion-based
#'   SMD (one row per non-reference level). An absolute SMD above `threshold`
#'   (conventionally 0.1) flags meaningful imbalance.
#'
#' @param data Data frame.
#' @param treatment Two-level grouping column (bare name or string).
#' @param variables Character vector of covariate columns. If NULL, all columns
#'   except `treatment` are used.
#' @param threshold Absolute SMD above which a covariate is flagged as imbalanced
#'   (default 0.1).
#'
#' @return Tibble with one row per covariate (or per level for categoricals):
#'   the summary in each group, the SMD, and an `imbalanced` flag.
#'
#' @examples
#' df <- data.frame(
#'   arm = rep(c("treated", "control"), each = 50),
#'   age = c(rnorm(50, 55, 8), rnorm(50, 50, 8)),
#'   smoker = rbinom(100, 1, 0.4)
#' )
#' kk_smd(df, arm, variables = c("age", "smoker"))
#'
#' @export
kk_smd <- function(data, treatment, variables = NULL, threshold = 0.1) {
              validate_data_frame(data)

              treat_name <- .kk_colname(rlang::enquo(treatment))
              if (!treat_name %in% names(data)) {
                            stop("Treatment column '", treat_name, "' not found in data.")
              }

              if (is.null(variables)) {
                            variables <- setdiff(names(data), treat_name)
              }
              missing_cols <- setdiff(variables, names(data))
              if (length(missing_cols) > 0) {
                            stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
              }

              grp <- data[[treat_name]]
              grp_levels <- if (is.factor(grp)) levels(grp) else sort(unique(stats::na.omit(grp)))
              if (length(grp_levels) != 2) {
                            stop("`treatment` must have exactly two levels; found ", length(grp_levels), ".")
              }
              g1 <- grp == grp_levels[1]
              g2 <- grp == grp_levels[2]

              smd_continuous <- function(x) {
                            m1 <- mean(x[g1], na.rm = TRUE)
                            m2 <- mean(x[g2], na.rm = TRUE)
                            v1 <- stats::var(x[g1], na.rm = TRUE)
                            v2 <- stats::var(x[g2], na.rm = TRUE)
                            pooled_sd <- sqrt((v1 + v2) / 2)
                            smd <- if (pooled_sd == 0) 0 else (m1 - m2) / pooled_sd
                            tibble::tibble(
                                          group1 = m1, group2 = m2, smd = smd
                            )
              }

              smd_proportion <- function(p1, p2) {
                            denom <- sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2)
                            if (is.na(denom) || denom == 0) 0 else (p1 - p2) / denom
              }

              rows <- purrr::map_dfr(variables, function(v) {
                            x <- data[[v]]
                            if (is.numeric(x) && length(unique(stats::na.omit(x))) > 2) {
                                          smd_continuous(x) %>%
                                                        dplyr::mutate(variable = v, level = NA_character_,
                                                                      type = "continuous", .before = 1)
                            } else {
                                          # Categorical / binary: one row per non-reference level
                                          xf <- factor(x)
                                          levs <- levels(xf)
                                          if (length(levs) == 2) levs <- levs[2] # binary: just the "1" level
                                          purrr::map_dfr(levs, function(lv) {
                                                        p1 <- mean(xf[g1] == lv, na.rm = TRUE)
                                                        p2 <- mean(xf[g2] == lv, na.rm = TRUE)
                                                        tibble::tibble(
                                                                      variable = v, level = lv, type = "categorical",
                                                                      group1 = p1, group2 = p2,
                                                                      smd = smd_proportion(p1, p2)
                                                        )
                                          })
                            }
              })

              rows %>%
                            dplyr::mutate(
                                          abs_smd = abs(.data$smd),
                                          imbalanced = .data$abs_smd > threshold
                            ) %>%
                            dplyr::select("variable", "level", "type", "group1", "group2",
                                          "smd", "abs_smd", "imbalanced") %>%
                            structure(group_labels = grp_levels)
}

#' @rdname kk_smd
#' @export
kk_balance_table <- kk_smd
