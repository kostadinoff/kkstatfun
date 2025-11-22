# ============================================================
# SUMMARY STATISTICS
# ============================================================

#' Comprehensive Summary Statistics (KK Summary)
#'
#' @description Computes full summary statistics with grouped data support.
#'
#' @param data Data frame
#' @param col Column to analyze
#' @param var_name Optional variable name
#' @param verbose Verbosity level ("full", "basic", "custom")
#' @param stats List of stats to include for custom verbose
#' @param pairwise Compute pairwise comparisons?
#' @param chi_probs Expected probabilities for chi-square
#'
#' @return Tibble with summary statistics
#'
#' @export
kk_summary <- function(data, col, var_name = NULL,
                       verbose = c("full", "basic", "custom"),
                       stats = NULL, pairwise = TRUE, chi_probs = NULL) {
              verbose <- match.arg(verbose)
              if (!is.data.frame(data)) stop("Input 'data' must be a data frame")

              col_quo <- rlang::enquo(col)
              col_name <- rlang::quo_name(col_quo)
              if (is.null(var_name)) var_name <- col_name

              x <- dplyr::pull(data, {{ col }})

              compute_stats <- function(x, var_name) {
                            if (is.null(x)) stop("Input cannot be NULL")
                            if (is.data.frame(x) || is.matrix(x)) stop("Input must be a vector, not a data frame or matrix")
                            if (!is.vector(x) && !is.factor(x)) stop("Input must be a vector or factor")

                            if (is.factor(x)) x <- as.character(x)

                            result <- list()

                            n_miss <- sum(is.na(x))
                            result$var_name <- var_name
                            result$type <- ifelse(is.numeric(x), "numeric", "categorical")
                            result$n_total <- length(x)
                            result$n_miss <- n_miss
                            result$n_valid <- length(x) - n_miss
                            result$miss_pct <- (n_miss / length(x)) * 100

                            if (result$n_valid == 0) {
                                          result$note <- "All values are missing"
                                          return(result)
                            }

                            if (verbose == "basic") {
                                          stats <- if (result$type == "numeric") {
                                                        c("mean", "median", "sd", "min", "max")
                                          } else {
                                                        c("level_summary", "mode")
                                          }
                            } else if (verbose == "custom" && !is.null(stats)) {
                                          stats <- stats
                            } else {
                                          stats <- if (result$type == "numeric") {
                                                        c(
                                                                      "mean", "huber_mean", "trim_mean", "geometric_mean", "median", "min", "max", "range",
                                                                      "variance", "sd", "se", "cv_pct", "mad", "iqr", "q1", "q3", "skewness",
                                                                      "kurtosis", "pct_5_95", "ci_mean_low", "ci_mean_up", "shapiro_p",
                                                                      "shapiro_int", "ks_p", "ks_int", "n_outliers", "outlier_values"
                                                        )
                                          } else {
                                                        c(
                                                                      "levels", "n_unique", "mode", "level_summary", "entropy", "evenness",
                                                                      "gini_index", "chi_p", "chi_int", "chi_note", "cramer_v", "pairwise_p", "pairwise_note"
                                                        )
                                          }
                            }

                            if (result$type == "numeric") {
                                          x <- stats::na.omit(x)
                                          if ("mean" %in% stats) result$mean <- mean(x)
                                          if ("huber_mean" %in% stats) {
                                                        huber_m <- function(x, c = 1.345, max_iter = 100, tol = 1e-6) {
                                                                      mu <- stats::median(x)
                                                                      sigma <- stats::mad(x, constant = 1.4826)
                                                                      for (i in 1:max_iter) {
                                                                                    residuals <- x - mu
                                                                                    weights <- pmin(c / abs(residuals / sigma), 1)
                                                                                    weights[is.na(weights)] <- 1
                                                                                    mu_new <- sum(weights * x) / sum(weights)
                                                                                    if (abs(mu_new - mu) < tol) break
                                                                                    mu <- mu_new
                                                                      }
                                                                      return(mu)
                                                        }
                                                        result$huber_mean <- huber_m(x)
                                          }
                                          if ("trim_mean" %in% stats) result$trim_mean <- mean(x, trim = 0.1)
                                          if ("geometric_mean" %in% stats) result$geometric_mean <- if (all(x > 0)) exp(mean(log(x))) else NA
                                          if ("median" %in% stats) result$median <- stats::median(x)
                                          if ("min" %in% stats) result$min <- min(x)
                                          if ("max" %in% stats) result$max <- max(x)
                                          if ("range" %in% stats) result$range <- max(x) - min(x)
                                          if ("variance" %in% stats) result$variance <- stats::var(x)
                                          if ("sd" %in% stats) result$sd <- stats::sd(x)
                                          if ("se" %in% stats) result$se <- stats::sd(x) / sqrt(length(x))
                                          if ("cv_pct" %in% stats) result$cv_pct <- stats::sd(x) / mean(x) * 100
                                          if ("mad" %in% stats) result$mad <- stats::mad(x, constant = 1.4826)
                                          if ("iqr" %in% stats) result$iqr <- stats::IQR(x)
                                          if ("q1" %in% stats) result$q1 <- stats::quantile(x, 0.25)
                                          if ("q3" %in% stats) result$q3 <- stats::quantile(x, 0.75)
                                          if ("skewness" %in% stats) result$skewness <- moments::skewness(x)
                                          if ("kurtosis" %in% stats) result$kurtosis <- moments::kurtosis(x)
                                          if ("pct_5_95" %in% stats) result$pct_5_95 <- list(stats::quantile(x, probs = c(0.05, 0.95)))
                                          if ("ci_mean_low" %in% stats || "ci_mean_up" %in% stats) {
                                                        t_test <- stats::t.test(x, conf.level = 0.95)
                                                        result$ci_mean_low <- t_test$conf.int[1]
                                                        result$ci_mean_up <- t_test$conf.int[2]
                                          }
                                          if ("shapiro_p" %in% stats || "shapiro_int" %in% stats) {
                                                        if (length(x) >= 3 && length(x) <= 5000) {
                                                                      shapiro_test <- stats::shapiro.test(x)
                                                                      result$shapiro_p <- shapiro_test$p.value
                                                                      result$shapiro_int <- ifelse(shapiro_test$p.value > 0.05,
                                                                                    "normal (p > 0.05)",
                                                                                    "non-normal (p <= 0.05)"
                                                                      )
                                                        } else {
                                                                      result$shapiro_p <- NA
                                                                      result$shapiro_int <- "sample size out of Shapiro-Wilk range (3-5000)"
                                                        }
                                          }
                                          if ("ks_p" %in% stats || "ks_int" %in% stats) {
                                                        if (length(x) > 5000) {
                                                                      ks_test <- tryCatch(
                                                                                    {
                                                                                                  suppressWarnings(stats::ks.test(x, "pnorm", mean(x), stats::sd(x)))
                                                                                    },
                                                                                    error = function(e) NULL
                                                                      )
                                                                      if (!is.null(ks_test)) {
                                                                                    result$ks_p <- ks_test$p.value
                                                                                    result$ks_int <- ifelse(ks_test$p.value > 0.05,
                                                                                                  "normal (KS p > 0.05)",
                                                                                                  "non-normal (KS p <= 0.05)"
                                                                                    )
                                                                      } else {
                                                                                    result$ks_p <- NA
                                                                                    result$ks_int <- "KS test failed"
                                                                      }
                                                        } else {
                                                                      result$ks_p <- NA
                                                                      result$ks_int <- "KS test not performed (n <= 5000)"
                                                        }
                                          }
                                          if ("n_outliers" %in% stats || "outlier_values" %in% stats) {
                                                        q1 <- stats::quantile(x, 0.25)
                                                        q3 <- stats::quantile(x, 0.75)
                                                        iqr <- q3 - q1
                                                        lower_fence <- q1 - 1.5 * iqr
                                                        upper_fence <- q3 + 1.5 * iqr
                                                        outliers <- x[x < lower_fence | x > upper_fence]
                                                        result$n_outliers <- length(outliers)
                                                        result$outlier_values <- list(outliers)
                                          }
                            } else {
                                          x <- as.factor(x)
                                          x <- x[!is.na(x)]
                                          freq_table <- table(x)
                                          n_valid <- sum(freq_table)
                                          low_counts <- all(freq_table < 5) && n_valid < 10
                                          if (low_counts && any(c("chi_p", "pairwise_p") %in% stats)) {
                                                        result$note <- "Warning: Very low counts; chi-square and pairwise tests may be unreliable"
                                          }
                                          if (any(c("n_unique", "evenness", "chi_p", "chi_int", "chi_note", "cramer_v", "pairwise_p") %in% stats)) {
                                                        result$n_unique <- length(unique(x))
                                          }
                                          if ("levels" %in% stats) result$levels <- list(levels(x))
                                          if ("mode" %in% stats) result$mode <- names(sort(table(x), decreasing = TRUE))[1]
                                          if ("level_summary" %in% stats) {
                                                        proportions <- prop.table(freq_table) * 100
                                                        ci_list <- lapply(names(freq_table), function(level) {
                                                                      binom_result <- stats::binom.test(freq_table[level], n_valid, conf.level = 0.95)
                                                                      tibble::tibble(
                                                                                    level = level,
                                                                                    count = as.numeric(freq_table[level]),
                                                                                    prop_pct = proportions[level],
                                                                                    ci_low_pct = binom_result$conf.int[1] * 100,
                                                                                    ci_up_pct = binom_result$conf.int[2] * 100
                                                                      )
                                                        })
                                                        result$level_summary <- list(dplyr::bind_rows(ci_list))
                                          }
                                          if ("entropy" %in% stats || "evenness" %in% stats) {
                                                        freqs <- prop.table(freq_table)
                                                        result$entropy <- entropy::entropy(freqs, unit = "log2")
                                          }
                                          if ("evenness" %in% stats) {
                                                        result$evenness <- if (!is.null(result$n_unique) && result$n_unique > 1) {
                                                                      result$entropy / log2(result$n_unique)
                                                        } else {
                                                                      0
                                                        }
                                          }
                                          if ("gini_index" %in% stats) {
                                                        p <- prop.table(freq_table)
                                                        result$gini_index <- 1 - sum(p^2)
                                          }
                                          if (any(c("chi_p", "chi_int", "chi_note", "cramer_v") %in% stats) && result$n_unique > 1) {
                                                        expected_prob <- if (!is.null(chi_probs)) {
                                                                      if (length(chi_probs) != result$n_unique || sum(chi_probs) != 1) {
                                                                                    stop("chi_probs must match number of levels and sum to 1")
                                                                      }
                                                                      chi_probs
                                                        } else {
                                                                      rep(1 / result$n_unique, result$n_unique)
                                                        }
                                                        if (low_counts) {
                                                                      result$chi_p <- NA
                                                                      result$chi_int <- "chi-square test skipped due to very low counts"
                                                                      result$chi_note <- "no test performed"
                                                                      result$cramer_v <- NA
                                                        } else if (any(freq_table < 5) || n_valid < 10) {
                                                                      chi_test <- stats::chisq.test(freq_table, p = expected_prob, simulate.p.value = TRUE, B = 2000)
                                                                      result$chi_note <- "simulated p-value used due to low counts or small sample size"
                                                                      result$chi_p <- chi_test$p.value
                                                                      result$chi_int <- ifelse(chi_test$p.value > 0.05,
                                                                                    "uniform distribution (p > 0.05)",
                                                                                    "non-uniform distribution (p <= 0.05)"
                                                                      )
                                                                      result$cramer_v <- sqrt(chi_test$statistic / (n_valid * (result$n_unique - 1)))
                                                        } else {
                                                                      chi_test <- stats::chisq.test(freq_table, p = expected_prob)
                                                                      result$chi_note <- "standard chi-square test"
                                                                      result$chi_p <- chi_test$p.value
                                                                      result$chi_int <- ifelse(chi_test$p.value > 0.05,
                                                                                    "uniform distribution (p > 0.05)",
                                                                                    "non-uniform distribution (p <= 0.05)"
                                                                      )
                                                                      result$cramer_v <- sqrt(chi_test$statistic / (n_valid * (result$n_unique - 1)))
                                                        }
                                          } else if (any(c("chi_p", "chi_int", "chi_note", "cramer_v") %in% stats)) {
                                                        result$chi_p <- NA
                                                        result$chi_int <- "chi-square test not applicable (single level)"
                                                        result$chi_note <- "no test performed"
                                                        result$cramer_v <- NA
                                          }
                                          if ("pairwise_p" %in% stats && pairwise && result$n_unique > 2 && all(freq_table >= 1) && result$n_unique <= 10) {
                                                        level_names <- names(freq_table)
                                                        pairwise_results <- tibble::tibble(level1 = character(), level2 = character(), p_value = numeric())
                                                        for (i in 1:(length(level_names) - 1)) {
                                                                      for (j in (i + 1):length(level_names)) {
                                                                                    test_result <- stats::binom.test(c(freq_table[i], freq_table[j]),
                                                                                                  n = c(freq_table[i] + freq_table[j], freq_table[i] + freq_table[j]),
                                                                                                  p = 0.5
                                                                                    )
                                                                                    pairwise_results <- dplyr::add_row(pairwise_results,
                                                                                                  level1 = level_names[i],
                                                                                                  level2 = level_names[j],
                                                                                                  p_value = test_result$p.value
                                                                                    )
                                                                      }
                                                        }
                                                        pairwise_results$p_value <- stats::p.adjust(pairwise_results$p_value, method = "holm")
                                                        result$pairwise_p <- list(pairwise_results)
                                                        result$pairwise_note <- "Holm-adjusted p-values from exact binomial tests"
                                          } else if ("pairwise_p" %in% stats) {
                                                        reason <- if (!pairwise) {
                                                                      "pairwise comparisons disabled"
                                                        } else if (result$n_unique <= 2) {
                                                                      "2 or fewer levels"
                                                        } else if (any(freq_table < 1)) {
                                                                      "zero counts in some levels"
                                                        } else {
                                                                      "too many levels (>10)"
                                                        }
                                                        result$pairwise_p <- list("not performed")
                                                        result$pairwise_note <- paste("pairwise comparisons skipped:", reason)
                                          }
                            }

                            return(result)
              }

              if (dplyr::is_grouped_df(data)) {
                            result <- data %>%
                                          dplyr::group_by(dplyr::across(dplyr::all_of(dplyr::group_vars(.)))) %>%
                                          dplyr::summarise(stats = list(compute_stats({{ col }}, var_name)), .groups = "keep") %>%
                                          tidyr::unnest_wider(stats)
              } else {
                            result <- tibble::as_tibble(compute_stats(x, var_name))
              }
              return(result)
}

#' @export
comprehensive_summary <- kk_summary
