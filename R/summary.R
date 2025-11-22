# ============================================================
# SUMMARY STATISTICS
# ============================================================

#' Comprehensive Summary Statistics
#'
#' @description Calculates extensive descriptive statistics for numeric
#'   or categorical variables with grouping support
#'
#' @param data Data frame
#' @param col Variable to summarize (unquoted)
#' @param var_name Optional name for variable
#' @param verbose Level: "full", "basic", or "custom"
#' @param stats Custom statistics to include (for verbose="custom")
#' @param pairwise Calculate pairwise comparisons
#' @param chi_probs Expected probabilities for chi-square test
#'
#' @return Tibble with summary statistics
#'
#' @export
comprehensive_summary <- function(data, col, var_name = NULL,
                                  verbose = c("full", "basic", "custom"),
                                  stats = NULL, pairwise = TRUE,
                                  chi_probs = NULL) {
              verbose <- match.arg(verbose)
              validate_data_frame(data)

              col_quo <- rlang::enquo(col)
              col_name <- rlang::quo_name(col_quo)
              if (is.null(var_name)) var_name <- col_name

              x <- dplyr::pull(data, {{ col }})

              compute_stats <- function(x, var_name) {
                            if (is.null(x)) stop("Input cannot be NULL")
                            if (is.data.frame(x) || is.matrix(x)) {
                                          stop("Input must be vector or factor, not data frame/matrix")
                            }

                            result <- list()
                            n_miss <- sum(is.na(x))
                            result$var_name <- var_name
                            result$type <- if (is.numeric(x)) "numeric" else "categorical"
                            result$n_total <- length(x)
                            result$n_miss <- n_miss
                            result$n_valid <- length(x) - n_miss
                            result$miss_pct <- (n_miss / length(x)) * 100

                            if (result$n_valid == 0) {
                                          result$note <- "All values missing"
                                          return(result)
                            }

                            if (verbose == "basic") {
                                          stats_to_use <- if (result$type == "numeric") {
                                                        c("mean", "median", "sd", "min", "max")
                                          } else {
                                                        c("n_unique", "mode")
                                          }
                            } else if (verbose == "custom" && !is.null(stats)) {
                                          stats_to_use <- stats
                            } else {
                                          stats_to_use <- if (result$type == "numeric") {
                                                        c(
                                                                      "mean", "median", "sd", "var", "min", "max", "range",
                                                                      "q1", "q3", "iqr", "mad", "cv", "skewness", "kurtosis",
                                                                      "ci_mean_low", "ci_mean_up", "outlier_count"
                                                        )
                                          } else {
                                                        c("n_unique", "mode", "entropy", "gini_index", "chi_p")
                                          }
                            }

                            if (result$type == "numeric") {
                                          x_clean <- stats::na.omit(x)

                                          if ("mean" %in% stats_to_use) result$mean <- mean(x_clean)
                                          if ("median" %in% stats_to_use) result$median <- stats::median(x_clean)
                                          if ("sd" %in% stats_to_use) result$sd <- stats::sd(x_clean)
                                          if ("var" %in% stats_to_use) result$var <- stats::var(x_clean)
                                          if ("min" %in% stats_to_use) result$min <- min(x_clean)
                                          if ("max" %in% stats_to_use) result$max <- max(x_clean)
                                          if ("range" %in% stats_to_use) result$range <- max(x_clean) - min(x_clean)

                                          if (any(c("q1", "q3", "iqr", "mad", "outlier_count") %in% stats_to_use)) {
                                                        q1_val <- stats::quantile(x_clean, 0.25)
                                                        q3_val <- stats::quantile(x_clean, 0.75)
                                                        if ("q1" %in% stats_to_use) result$q1 <- q1_val
                                                        if ("q3" %in% stats_to_use) result$q3 <- q3_val
                                                        if ("iqr" %in% stats_to_use) result$iqr <- q3_val - q1_val

                                                        if ("outlier_count" %in% stats_to_use) {
                                                                      iqr <- q3_val - q1_val
                                                                      outliers <- sum(x_clean < (q1_val - CONSTANTS$OUTLIER_IQR_MULTIPLIER * iqr) |
                                                                                    x_clean > (q3_val + CONSTANTS$OUTLIER_IQR_MULTIPLIER * iqr))
                                                                      result$outlier_count <- outliers
                                                        }
                                          }

                                          if ("mad" %in% stats_to_use) {
                                                        result$mad <- stats::mad(x_clean, constant = 1.4826)
                                          }

                                          if ("cv" %in% stats_to_use) {
                                                        mean_val <- mean(x_clean)
                                                        if (mean_val != 0) {
                                                                      result$cv <- stats::sd(x_clean) / abs(mean_val)
                                                        }
                                          }

                                          if ("skewness" %in% stats_to_use && length(x_clean) >= 3) {
                                                        result$skewness <- moments::skewness(x_clean)
                                          }

                                          if ("kurtosis" %in% stats_to_use && length(x_clean) >= 4) {
                                                        result$kurtosis <- moments::kurtosis(x_clean)
                                          }

                                          if (any(c("ci_mean_low", "ci_mean_up") %in% stats_to_use)) {
                                                        t_test <- stats::t.test(x_clean, conf.level = CONSTANTS$DEFAULT_CONF_LEVEL)
                                                        result$ci_mean_low <- t_test$conf.int[1]
                                                        result$ci_mean_up <- t_test$conf.int[2]
                                          }
                            } else {
                                          x_cat <- as.factor(x)
                                          x_cat <- x_cat[!is.na(x_cat)]
                                          freq_table <- table(x_cat)

                                          if (any(c("n_unique", "mode", "entropy", "gini_index") %in% stats_to_use)) {
                                                        result$n_unique <- length(unique(x_cat))
                                          }

                                          if ("mode" %in% stats_to_use) {
                                                        result$mode <- names(sort(freq_table, decreasing = TRUE))[1]
                                          }

                                          if ("entropy" %in% stats_to_use) {
                                                        freqs <- prop.table(freq_table)
                                                        result$entropy <- entropy::entropy(freqs, unit = "log2")
                                          }

                                          if ("gini_index" %in% stats_to_use) {
                                                        p <- prop.table(freq_table)
                                                        result$gini_index <- 1 - sum(p^2)
                                          }

                                          if ("chi_p" %in% stats_to_use && result$n_unique > 1) {
                                                        expected_prob <- if (!is.null(chi_probs)) {
                                                                      if (length(chi_probs) != result$n_unique || sum(chi_probs) != 1) {
                                                                                    stop("chi_probs must match levels and sum to 1")
                                                                      }
                                                                      chi_probs
                                                        } else {
                                                                      rep(1 / result$n_unique, result$n_unique)
                                                        }

                                                        chi_test <- stats::chisq.test(freq_table, p = expected_prob)
                                                        result$chi_p <- chi_test$p.value
                                                        result$chi_significant <- chi_test$p.value <= 0.05
                                          }
                            }

                            return(result)
              }

              if (dplyr::is_grouped_df(data)) {
                            result <- data %>%
                                          dplyr::group_by(dplyr::across(dplyr::all_of(dplyr::group_vars(.)))) %>%
                                          dplyr::summarise(
                                                        stats = list(compute_stats({{ col }}, var_name)),
                                                        .groups = "keep"
                                          ) %>%
                                          tidyr::unnest_wider(stats)
              } else {
                            result <- tibble::as_tibble(compute_stats(x, var_name))
              }

              return(result)
}
