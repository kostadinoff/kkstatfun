# ============================================================
# GROUP COMPARISON TABLE
# ============================================================

#' Compare Groups with Differences and Confidence Intervals
#'
#' @description Creates a comparison table showing group statistics, differences,
#'   confidence intervals, and p-values for both categorical and numerical variables.
#'   Now includes sample size per group for each variable, degrees of freedom, effect size, and a total column.
#'
#' @param data A data frame or a grouped data frame (from \code{dplyr::group_by}).
#' @param group Grouping variable (can be unquoted or a string). If \code{data} is grouped
#'   and \code{group} is missing, the last grouping variable is used as the comparison group.
#' @param variables Variables to compare (tidyselect supported, e.g., \code{c(age, smoking)} or \code{starts_with("score")}).
#'   If \code{NULL} (default), all columns except the \code{group} and grouping variables are compared.
#' @param nonparametric Logical, whether to use nonparametric tests (Wilcoxon) for continuous variables (default: \code{FALSE}).
#' @param correct Logical, whether to apply continuity correction in Chi-square and proportion tests (default: \code{FALSE}).
#' @param adjust_method P-value adjustment method for categorical variables with >2 levels (default: \code{"holm"}).
#' @param conf.level Confidence level for intervals (default: \code{0.95}).
#'
#' @return A tibble with comparison results, including counts, means/proportions, differences, CIs, p-values, and effect sizes.
#'
#' @examples
#' # Basic usage with tidy evaluation
#' kk_compare_groups_table(mtcars, am, c(mpg, hp))
#'
#' # Automatic variable selection
#' mtcars |>
#'               dplyr::select(am, mpg, hp, cyl) |>
#'               kk_compare_groups_table(am)
#'
#' # Stratified analysis using group_by
#' mtcars |>
#'               dplyr::group_by(vs) |>
#'               kk_compare_groups_table(am, c(mpg, hp))
#'
#' # Automatic group detection from grouped data
#' mtcars |>
#'               dplyr::group_by(am) |>
#'               kk_compare_groups_table()
#'
#' @export
kk_compare_groups_table <- function(data, group, variables = NULL,
                                    nonparametric = FALSE,
                                    correct = FALSE,
                                    adjust_method = "holm",
                                    conf.level = 0.95) {
              # Handle grouped data frames
              if (dplyr::is_grouped_df(data)) {
                            # Get grouping variables
                            group_vars <- dplyr::group_vars(data)

                            # If group is missing, try to use the last grouping variable
                            # and use the rest as grouping strata
                            if (missing(group)) {
                                          if (length(group_vars) == 0) {
                                                        stop("Data is grouped but has no grouping variables. Please specify 'group'.")
                                          }
                                          group_name <- group_vars[length(group_vars)]
                                          strata_vars <- group_vars[-length(group_vars)]
                                          # Re-group by strata only for recursive call
                                          if (length(strata_vars) > 0) {
                                                        data <- dplyr::group_by(data, !!!rlang::syms(strata_vars))
                                          } else {
                                                        data <- dplyr::ungroup(data)
                                          }
                                          group_expr <- rlang::sym(group_name)
                            } else {
                                          group_expr <- rlang::enquo(group)
                                          group_name <- rlang::as_name(rlang::ensym(group))
                                          strata_vars <- group_vars
                            }

                            # Get group keys for the strata
                            group_keys <- dplyr::group_keys(data)

                            # Process each stratum separately
                            results <- data %>%
                                          dplyr::group_map(~ {
                                                        # Auto-detect variables for this stratum
                                                        vars_to_use <- if (is.null(variables)) {
                                                                      setdiff(names(.x), c(group_name, strata_vars))
                                                        } else {
                                                                      # Support tidyselect on .x
                                                                      names(tidyselect::eval_select(rlang::enquo(variables), .x))
                                                        }

                                                        # Run comparison on this subset
                                                        kk_compare_groups_table(
                                                                      data = .x,
                                                                      group = !!group_expr,
                                                                      variables = vars_to_use,
                                                                      nonparametric = nonparametric,
                                                                      correct = correct,
                                                                      adjust_method = adjust_method,
                                                                      conf.level = conf.level
                                                        )
                                          }, .keep = TRUE)

                            # Combine all results and add grouping columns
                            if (length(results) == 0) {
                                          return(tibble::tibble())
                            }

                            # Add strata columns to each result
                            if (length(strata_vars) > 0) {
                                          for (i in seq_along(results)) {
                                                        for (j in seq_along(strata_vars)) {
                                                                      results[[i]] <- tibble::add_column(
                                                                                    results[[i]],
                                                                                    !!strata_vars[j] := group_keys[[j]][i],
                                                                                    .before = 1
                                                                      )
                                                        }
                                          }
                            }

                            # Combine all results
                            return(dplyr::bind_rows(results))
              }

              # Resolve group
              if (missing(group)) {
                            stop("Argument 'group' is missing with no default (and data is not grouped)")
              }
              group_name <- rlang::as_name(rlang::ensym(group))

              if (!group_name %in% names(data)) {
                            stop(sprintf("Group variable '%s' not found in data", group_name))
              }

              # Resolve variables (tidyselect)
              if (is.null(variables)) {
                            variables <- setdiff(names(data), group_name)
              } else {
                            variables <- names(tidyselect::eval_select(rlang::enquo(variables), data))
              }

              # Check that group has exactly 2 levels
              group_levels <- unique(stats::na.omit(data[[group_name]]))
              if (length(group_levels) != 2) {
                            stop(sprintf("Group variable must have exactly 2 levels, found %d", length(group_levels)))
              }

              # Split data by group
              group1_name <- as.character(group_levels[1])
              group2_name <- as.character(group_levels[2])

              data1 <- data[data[[group_name]] == group_levels[1], ]
              data2 <- data[data[[group_name]] == group_levels[2], ]

              n1_total <- nrow(data1)
              n2_total <- nrow(data2)
              n_all <- nrow(data)

              # Process each variable
              results_list <- list()

              for (var in variables) {
                            var_data <- data[[var]]
                            var_data1 <- data1[[var]]
                            var_data2 <- data2[[var]]

                            n_valid_1 <- sum(!is.na(var_data1))
                            n_valid_2 <- sum(!is.na(var_data2))
                            n_valid_all <- sum(!is.na(var_data))

                            # Determine variable type
                            # Check if numeric variable is actually binary (0/1 only)
                            is_binary_numeric <- FALSE
                            if (is.numeric(var_data)) {
                                          unique_vals <- unique(stats::na.omit(var_data))
                                          is_binary_numeric <- all(unique_vals %in% c(0, 1)) && length(unique_vals) <= 2
                            }

                            is_numeric <- is.numeric(var_data) && !is_binary_numeric
                            is_categorical <- is.factor(var_data) || is.character(var_data) || is_binary_numeric

                            if (is_numeric) {
                                          # Numerical variable
                                          diff <- mean(var_data1, na.rm = TRUE) - mean(var_data2, na.rm = TRUE)

                                          # Initialize results with defaults
                                          test_name <- if (nonparametric) "Wilcoxon" else "t-test"
                                          ci_lower <- NA_real_
                                          ci_upper <- NA_real_
                                          test_stat <- "NA"
                                          df_val <- NA_character_
                                          p_value <- NA_real_
                                          effect_size_str <- "NA"

                                          # Statistical test and Effect size
                                          if (nonparametric) {
                                                        # Defensive check for wilcox.test
                                                        can_run_wilcox <- n_valid_1 > 0 && n_valid_2 > 0

                                                        test_result <- NULL
                                                        if (can_run_wilcox) {
                                                                      test_result <- tryCatch(
                                                                                    stats::wilcox.test(var_data1, var_data2, conf.int = TRUE, conf.level = conf.level),
                                                                                    error = function(e) NULL
                                                                      )
                                                        }

                                                        if (!is.null(test_result)) {
                                                                      p_value <- test_result$p.value
                                                                      test_stat <- sprintf("W=%.2f", test_result$statistic)
                                                                      if (!is.null(test_result$conf.int)) {
                                                                                    ci_lower <- test_result$conf.int[1]
                                                                                    ci_upper <- test_result$conf.int[2]
                                                                      }
                                                        } else {
                                                                      # If failed or skipped, but identical and not all NA
                                                                      if (n_valid_1 > 0 && n_valid_2 > 0 && isTRUE(all.equal(var_data1, var_data2))) {
                                                                                    p_value <- 1.0
                                                                                    test_stat <- "W=0.00"
                                                                                    ci_lower <- 0
                                                                                    ci_upper <- 0
                                                                      }
                                                        }

                                                        # Effect size (r)
                                                        n_obs <- n_valid_1 + n_valid_2
                                                        if (n_obs > 0 && n_valid_1 > 0 && n_valid_2 > 0) {
                                                                      mu_W <- (as.numeric(n_valid_1) * as.numeric(n_valid_2)) / 2
                                                                      sigma_W <- sqrt((as.numeric(n_valid_1) * as.numeric(n_valid_2) * (as.numeric(n_valid_1) + as.numeric(n_valid_2) + 1)) / 12)
                                                                      if (sigma_W > 0 && !is.null(test_result)) {
                                                                                    z_score <- (as.numeric(test_result$statistic) - mu_W) / sigma_W
                                                                                    effect_size_val <- z_score / sqrt(n_obs)
                                                                                    effect_size_str <- sprintf("r=%.2f", abs(effect_size_val))
                                                                      } else if (isTRUE(all.equal(var_data1, var_data2))) {
                                                                                    effect_size_str <- "r=0.00"
                                                                      }
                                                        }

                                                        # Median (IQR)
                                                        calc_median_iqr <- function(x) {
                                                                      if (sum(!is.na(x)) == 0) {
                                                                                    return("NA")
                                                                      }
                                                                      med <- median(x, na.rm = TRUE)
                                                                      q1 <- quantile(x, 0.25, na.rm = TRUE)
                                                                      q3 <- quantile(x, 0.75, na.rm = TRUE)
                                                                      sprintf("%.2f [%.2f, %.2f]", med, q1, q3)
                                                        }

                                                        total_val <- calc_median_iqr(var_data)
                                                        group1_val <- calc_median_iqr(var_data1)
                                                        group2_val <- calc_median_iqr(var_data2)
                                          } else {
                                                        # Defensive check for t.test
                                                        v1 <- if (n_valid_1 > 1) stats::var(var_data1, na.rm = TRUE) else 0
                                                        v2 <- if (n_valid_2 > 1) stats::var(var_data2, na.rm = TRUE) else 0

                                                        # t.test fails if variance is 0 in BOTH groups
                                                        # Also check if we have enough data
                                                        can_run_t_test <- (n_valid_1 > 0 && n_valid_2 > 0) &&
                                                                      !((is.na(v1) || v1 == 0) && (is.na(v2) || v2 == 0))

                                                        test_result <- NULL
                                                        if (can_run_t_test) {
                                                                      test_result <- tryCatch(
                                                                                    stats::t.test(var_data1, var_data2, conf.level = conf.level),
                                                                                    error = function(e) NULL
                                                                      )
                                                        }

                                                        if (!is.null(test_result)) {
                                                                      p_value <- test_result$p.value
                                                                      ci_lower <- test_result$conf.int[1]
                                                                      ci_upper <- test_result$conf.int[2]
                                                                      test_stat <- sprintf("t=%.2f", test_result$statistic)
                                                                      df_val <- sprintf("%.1f", test_result$parameter)
                                                        } else {
                                                                      # If failed or skipped, but identical and not all NA
                                                                      if (n_valid_1 > 0 && n_valid_2 > 0 && isTRUE(all.equal(mean(var_data1, na.rm = TRUE), mean(var_data2, na.rm = TRUE)))) {
                                                                                    p_value <- 1.0
                                                                                    test_stat <- "t=0.00"
                                                                                    ci_lower <- 0
                                                                                    ci_upper <- 0
                                                                      }
                                                        }

                                                        # Effect size (Cohen's d)
                                                        mean1 <- mean(var_data1, na.rm = TRUE)
                                                        sd1 <- stats::sd(var_data1, na.rm = TRUE)
                                                        mean2 <- mean(var_data2, na.rm = TRUE)
                                                        sd2 <- stats::sd(var_data2, na.rm = TRUE)

                                                        s1 <- if (is.na(sd1)) 0 else sd1
                                                        s2 <- if (is.na(sd2)) 0 else sd2

                                                        denom <- (n_valid_1 + n_valid_2 - 2)
                                                        if (denom > 0) {
                                                                      pooled_sd <- sqrt(((n_valid_1 - 1) * s1^2 + (n_valid_2 - 1) * s2^2) / denom)
                                                                      if (pooled_sd > 0) {
                                                                                    d_val <- (mean1 - mean2) / pooled_sd
                                                                                    effect_size_str <- sprintf("d=%.2f", abs(d_val))
                                                                      } else if (isTRUE(all.equal(mean1, mean2))) {
                                                                                    effect_size_str <- "d=0.00"
                                                                      }
                                                        }

                                                        # Mean (SD)
                                                        calc_mean_sd <- function(x) {
                                                                      if (length(stats::na.omit(x)) == 0) {
                                                                                    return("NA")
                                                                      }
                                                                      mn <- mean(x, na.rm = TRUE)
                                                                      s <- stats::sd(x, na.rm = TRUE)
                                                                      sprintf("%.2f (%.2f)", mn, ifelse(is.na(s), 0, s))
                                                        }

                                                        total_val <- calc_mean_sd(var_data)
                                                        group1_val <- calc_mean_sd(var_data1)
                                                        group2_val <- calc_mean_sd(var_data2)
                                          }

                                          results_list[[length(results_list) + 1]] <- tibble::tibble(
                                                        Characteristic = var,
                                                        n_Total = as.character(n_valid_all),
                                                        Total = total_val,
                                                        n1 = as.character(n_valid_1),
                                                        Group1 = group1_val,
                                                        n2 = as.character(n_valid_2),
                                                        Group2 = group2_val,
                                                        Difference = sprintf("%.2f", diff),
                                                        ci_95 = if (is.na(ci_lower)) "NA" else sprintf("%.2f, %.2f", ci_lower, ci_upper),
                                                        p_value = format_pvalue(p_value),
                                                        Test = test_name,
                                                        Statistic = test_stat,
                                                        df = ifelse(is.na(df_val), "NA", df_val),
                                                        effect_size = effect_size_str
                                          )
                            } else if (is_categorical) {
                                          # Categorical variable
                                          var_factor1 <- factor(var_data1)
                                          var_factor2 <- factor(var_data2)
                                          var_factor_all <- factor(var_data)

                                          # Get all levels
                                          all_levels <- unique(c(levels(var_factor1), levels(var_factor2)))

                                          # Create contingency table
                                          tbl <- table(data[[group_name]], data[[var]], useNA = "no")

                                          # Determine test
                                          test_name <- "Chi-square" # Default
                                          test_stat <- "NA"
                                          df_val <- "NA"
                                          effect_size_str <- "NA"
                                          overall_p <- NA_real_

                                          # Check if we have at least 2x2. If not, the test is not applicable.
                                          if (nrow(tbl) >= 2 && ncol(tbl) >= 2) {
                                                        # Effect size (Cramer's V) using tryCatch for the test
                                                        chisq_res_for_es <- tryCatch(
                                                                      suppressWarnings(stats::chisq.test(tbl, correct = correct)),
                                                                      error = function(e) NULL
                                                        )

                                                        if (!is.null(chisq_res_for_es)) {
                                                                      chisq_stat <- chisq_res_for_es$statistic
                                                                      N_total_tbl <- sum(tbl)
                                                                      k <- min(dim(tbl))
                                                                      if (k > 1 && N_total_tbl > 0) {
                                                                                    v_val <- sqrt(chisq_stat / (N_total_tbl * (k - 1)))
                                                                                    effect_size_str <- sprintf("V=%.2f", v_val)
                                                                      }
                                                        }

                                                        if (any(tbl < 5)) {
                                                                      test_result <- tryCatch(
                                                                                    stats::fisher.test(tbl),
                                                                                    error = function(e) NULL
                                                                      )
                                                                      test_name <- "Fisher"
                                                                      if (!is.null(test_result)) {
                                                                                    overall_p <- test_result$p.value
                                                                                    test_stat <- ""
                                                                                    df_val <- ""
                                                                      }
                                                        } else {
                                                                      test_result <- tryCatch(
                                                                                    stats::chisq.test(tbl, correct = correct),
                                                                                    error = function(e) NULL
                                                                      )
                                                                      test_name <- "Chi-square"
                                                                      if (!is.null(test_result)) {
                                                                                    overall_p <- test_result$p.value
                                                                                    test_stat <- sprintf("χ²=%.2f", test_result$statistic)
                                                                                    df_val <- as.character(test_result$parameter)
                                                                      }
                                                        }
                                          } else {
                                                        # Constant data or only one group present for this variable
                                                        # If it's 2x1 (constant across groups), p-value is 1.0
                                                        if (nrow(tbl) >= 2 && ncol(tbl) == 1) {
                                                                      overall_p <- 1.0
                                                                      test_name <- "Chi-square"
                                                                      test_stat <- "χ²=0.00"
                                                                      df_val <- "0"
                                                                      effect_size_str <- "V=0.00"
                                                        }
                                          }

                                          # For each level, calculate proportions and differences
                                          level_results <- list()
                                          level_pvalues <- c()

                                          levels_to_show <- all_levels
                                          if (length(all_levels) == 2) {
                                                        # Prioritize showing "positive" level
                                                        positive_candidates <- c("1", "Yes", "True", "T", "Y", "Positive", "Present")

                                                        pos_idx <- which(all_levels %in% positive_candidates)
                                                        if (length(pos_idx) > 0) {
                                                                      levels_to_show <- all_levels[pos_idx[1]]
                                                        } else {
                                                                      # Default to first level if no specific positive level found
                                                                      levels_to_show <- all_levels[1]
                                                        }
                                          }

                                          for (level in levels_to_show) {
                                                        count_all <- sum(var_data == level, na.rm = TRUE)
                                                        count1 <- sum(var_data1 == level, na.rm = TRUE)
                                                        count2 <- sum(var_data2 == level, na.rm = TRUE)

                                                        prop_all <- if (n_valid_all > 0) count_all / n_valid_all else 0
                                                        prop1 <- if (n_valid_1 > 0) count1 / n_valid_1 else 0
                                                        prop2 <- if (n_valid_2 > 0) count2 / n_valid_2 else 0

                                                        diff_prop <- prop1 - prop2

                                                        # Confidence interval
                                                        se_diff <- sqrt(prop1 * (1 - prop1) / max(1, n_valid_1) + prop2 * (1 - prop2) / max(1, n_valid_2))
                                                        z_crit <- stats::qnorm(1 - (1 - conf.level) / 2)
                                                        ci_lower <- diff_prop - z_crit * se_diff
                                                        ci_upper <- diff_prop + z_crit * se_diff

                                                        # Pairwise test
                                                        if (length(all_levels) > 2) {
                                                                      binary_p <- NA_real_
                                                                      binary_tbl <- matrix(c(count1, n1_total - count1, count2, n2_total - count2), nrow = 2)
                                                                      if (all(dim(binary_tbl) == c(2, 2))) {
                                                                                    if (any(binary_tbl < 5)) {
                                                                                                  level_test <- tryCatch(stats::fisher.test(binary_tbl), error = function(e) NULL)
                                                                                                  binary_p <- if (!is.null(level_test)) level_test$p.value else NA_real_
                                                                                    } else {
                                                                                                  level_test <- tryCatch(stats::prop.test(c(count1, count2), c(n_valid_1, n_valid_2), correct = correct), error = function(e) NULL)
                                                                                                  binary_p <- if (!is.null(level_test)) level_test$p.value else NA_real_
                                                                                    }
                                                                      }
                                                                      level_pvalues <- c(level_pvalues, binary_p)
                                                        }

                                                        level_results[[length(level_results) + 1]] <- tibble::tibble(
                                                                      Characteristic = paste0(var, " - ", level),
                                                                      n_Total = as.character(n_valid_all),
                                                                      Total = sprintf("%d (%.1f%%)", count_all, prop_all * 100),
                                                                      n1 = as.character(n_valid_1),
                                                                      Group1 = sprintf("%d (%.1f%%)", count1, prop1 * 100),
                                                                      n2 = as.character(n_valid_2),
                                                                      Group2 = sprintf("%d (%.1f%%)", count2, prop2 * 100),
                                                                      Difference = sprintf("%.1f%%", diff_prop * 100),
                                                                      ci_95 = sprintf("%.1f%%, %.1f%%", ci_lower * 100, ci_upper * 100),
                                                                      p_value = NA_character_,
                                                                      Test = test_name,
                                                                      Statistic = test_stat,
                                                                      df = df_val,
                                                                      effect_size = effect_size_str
                                                        )
                                          }

                                          # Adjust p-values
                                          if (length(all_levels) > 2) {
                                                        if (all(is.na(level_pvalues))) {
                                                                      for (i in seq_along(level_results)) {
                                                                                    level_results[[i]]$p_value <- NA_character_
                                                                      }
                                                        } else {
                                                                      adjusted_p <- stats::p.adjust(level_pvalues, method = adjust_method)
                                                                      for (i in seq_along(level_results)) {
                                                                                    level_results[[i]]$p_value <- format_pvalue(adjusted_p[i])
                                                                      }
                                                        }
                                          } else {
                                                        level_results[[1]]$p_value <- format_pvalue(overall_p)
                                          }

                                          results_list <- c(results_list, level_results)
                            }
              }

              # Combine all results
              result_df <- dplyr::bind_rows(results_list)

              # Rename columns
              names(result_df)[names(result_df) == "n1"] <- paste0("n_", group1_name)
              names(result_df)[names(result_df) == "Group1"] <- group1_name
              names(result_df)[names(result_df) == "n2"] <- paste0("n_", group2_name)
              names(result_df)[names(result_df) == "Group2"] <- group2_name

              return(result_df)
}

#' Format p-value for display
#'
#' @param p P-value
#' @return Formatted string
#' @noRd
format_pvalue <- function(p) {
              if (is.na(p)) {
                            return(NA_character_)
              }
              if (p < 0.001) {
                            return("<0.001")
              }
              if (p > 0.9) {
                            return(">0.9")
              }
              return(sprintf("%.3f", p))
}

#' @rdname kk_compare_groups_table
#' @export
compare_groups_table <- kk_compare_groups_table
