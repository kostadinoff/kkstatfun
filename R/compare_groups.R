# ============================================================
# GROUP COMPARISON TABLE
# ============================================================

#' Compare Groups with Differences and Confidence Intervals
#'
#' @description Creates a comparison table showing group statistics, differences,
#'   confidence intervals, and p-values for both categorical and numerical variables.
#'   Now includes sample size per group for each variable, degrees of freedom, effect size, and a total column.
#'
#' @param data Data frame
#' @param group Grouping variable (must have exactly 2 levels)
#' @param variables Vector of variable names to compare. If NULL (default),
#'   all columns except the group variable will be compared.
#' @param nonparametric Use nonparametric tests for continuous variables (default: FALSE)
#' @param adjust_method P-value adjustment method for categorical variables with >2 levels (default: "holm")
#' @param conf.level Confidence level (default: 0.95)
#'
#' @return Tibble with comparison results
#'
#' @export
kk_compare_groups_table <- function(data, group, variables = NULL,
                                    nonparametric = FALSE,
                                    adjust_method = "holm",
                                    conf.level = 0.95) {
              # Handle grouped data frames
              if (dplyr::is_grouped_df(data)) {
                            # Get grouping variables and keys
                            group_vars <- dplyr::group_vars(data)
                            group_keys <- dplyr::group_keys(data)

                            # Get the group variable name for exclusion
                            group_sym <- rlang::ensym(group)
                            group_name <- rlang::as_name(group_sym)

                            # Process each group separately
                            results <- data %>%
                                          dplyr::group_map(~ {
                                                        # Exclude grouping variables from comparison
                                                        # Since .x is ungrouped, we need to explicitly set variables
                                                        vars_to_use <- if (is.null(variables)) {
                                                                      # Auto-detect: all columns except group var and grouping vars
                                                                      setdiff(names(.x), c(group_name, group_vars))
                                                        } else {
                                                                      # Explicit: remove grouping vars from provided list
                                                                      setdiff(variables, group_vars)
                                                        }

                                                        # Run comparison on this subset
                                                        kk_compare_groups_table(
                                                                      data = .x,
                                                                      group = {{ group }},
                                                                      variables = vars_to_use,
                                                                      nonparametric = nonparametric,
                                                                      adjust_method = adjust_method,
                                                                      conf.level = conf.level
                                                        )
                                          }, .keep = TRUE)

                            # Add grouping columns to each result
                            for (i in seq_along(results)) {
                                          for (j in seq_along(group_vars)) {
                                                        results[[i]] <- tibble::add_column(
                                                                      results[[i]],
                                                                      !!group_vars[j] := group_keys[[j]][i],
                                                                      .before = 1
                                                        )
                                          }
                            }

                            # Combine all results
                            return(dplyr::bind_rows(results))
              }

              # Validate inputs
              if (!is.data.frame(data)) stop("'data' must be a data frame")

              group_sym <- rlang::ensym(group)
              group_name <- rlang::as_name(group_sym)

              if (!group_name %in% names(data)) {
                            stop(sprintf("Group variable '%s' not found in data", group_name))
              }

              # Check that group has exactly 2 levels
              group_levels <- unique(stats::na.omit(data[[group_name]]))
              if (length(group_levels) != 2) {
                            stop(sprintf("Group variable must have exactly 2 levels, found %d", length(group_levels)))
              }

              # If variables not specified, use all columns except group
              if (is.null(variables)) {
                            variables <- setdiff(names(data), group_name)
              }

              # Ensure variables exist
              missing_vars <- setdiff(variables, names(data))
              if (length(missing_vars) > 0) {
                            stop(sprintf("Variables not found: %s", paste(missing_vars, collapse = ", ")))
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

                                          # Statistical test
                                          if (nonparametric) {
                                                        test_result <- stats::wilcox.test(var_data1, var_data2, conf.int = TRUE, conf.level = conf.level)
                                                        test_name <- "Wilcoxon"
                                                        ci_lower <- test_result$conf.int[1]
                                                        ci_upper <- test_result$conf.int[2]
                                                        test_stat <- sprintf("W=%.2f", test_result$statistic)
                                                        df_val <- NA_character_

                                                        # Effect size (r)
                                                        n_obs <- n_valid_1 + n_valid_2
                                                        mu_W <- n_valid_1 * n_valid_2 / 2
                                                        sigma_W <- sqrt((n_valid_1 * n_valid_2 * (n_valid_1 + n_valid_2 + 1)) / 12)
                                                        z_score <- (test_result$statistic - mu_W) / sigma_W
                                                        effect_size_val <- z_score / sqrt(n_obs)
                                                        effect_size_str <- sprintf("r=%.2f", abs(effect_size_val))

                                                        # Median (IQR)
                                                        calc_median_iqr <- function(x) {
                                                                      med <- median(x, na.rm = TRUE)
                                                                      q1 <- quantile(x, 0.25, na.rm = TRUE)
                                                                      q3 <- quantile(x, 0.75, na.rm = TRUE)
                                                                      sprintf("%.2f [%.2f, %.2f]", med, q1, q3)
                                                        }

                                                        total_val <- calc_median_iqr(var_data)
                                                        group1_val <- calc_median_iqr(var_data1)
                                                        group2_val <- calc_median_iqr(var_data2)
                                          } else {
                                                        test_result <- stats::t.test(var_data1, var_data2, conf.level = conf.level)
                                                        test_name <- "t-test"
                                                        ci_lower <- test_result$conf.int[1]
                                                        ci_upper <- test_result$conf.int[2]
                                                        test_stat <- sprintf("t=%.2f", test_result$statistic)
                                                        df_val <- sprintf("%.1f", test_result$parameter)

                                                        # Effect size (Cohen's d)
                                                        mean1 <- mean(var_data1, na.rm = TRUE)
                                                        sd1 <- stats::sd(var_data1, na.rm = TRUE)
                                                        mean2 <- mean(var_data2, na.rm = TRUE)
                                                        sd2 <- stats::sd(var_data2, na.rm = TRUE)

                                                        pooled_sd <- sqrt(((n_valid_1 - 1) * sd1^2 + (n_valid_2 - 1) * sd2^2) / (n_valid_1 + n_valid_2 - 2))
                                                        d_val <- (mean1 - mean2) / pooled_sd
                                                        effect_size_str <- sprintf("d=%.2f", abs(d_val))

                                                        # Mean (SD)
                                                        calc_mean_sd <- function(x) {
                                                                      mn <- mean(x, na.rm = TRUE)
                                                                      s <- stats::sd(x, na.rm = TRUE)
                                                                      sprintf("%.2f (%.2f)", mn, s)
                                                        }

                                                        total_val <- calc_mean_sd(var_data)
                                                        group1_val <- calc_mean_sd(var_data1)
                                                        group2_val <- calc_mean_sd(var_data2)
                                          }

                                          p_value <- test_result$p.value

                                          results_list[[length(results_list) + 1]] <- tibble::tibble(
                                                        Characteristic = var,
                                                        n_Total = as.character(n_valid_all),
                                                        Total = total_val,
                                                        n1 = as.character(n_valid_1),
                                                        Group1 = group1_val,
                                                        n2 = as.character(n_valid_2),
                                                        Group2 = group2_val,
                                                        Difference = sprintf("%.2f", diff),
                                                        CI_95 = sprintf("%.2f, %.2f", ci_lower, ci_upper),
                                                        p_value = format_pvalue(p_value),
                                                        Test = test_name,
                                                        Statistic = test_stat,
                                                        df = df_val,
                                                        Effect_Size = effect_size_str
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
                                          test_stat <- NA_character_
                                          df_val <- NA_character_
                                          effect_size_str <- NA_character_

                                          chisq_res_for_es <- suppressWarnings(stats::chisq.test(tbl))
                                          chisq_stat <- chisq_res_for_es$statistic
                                          N_total_tbl <- sum(tbl)
                                          k <- min(dim(tbl))
                                          v_val <- sqrt(chisq_stat / (N_total_tbl * (k - 1)))
                                          effect_size_str <- sprintf("V=%.2f", v_val)

                                          if (any(tbl < 5)) {
                                                        test_result <- stats::fisher.test(tbl)
                                                        test_name <- "Fisher"
                                                        overall_p <- test_result$p.value
                                                        test_stat <- ""
                                                        df_val <- ""
                                          } else {
                                                        test_result <- stats::chisq.test(tbl)
                                                        test_name <- "Chi-square"
                                                        overall_p <- test_result$p.value
                                                        test_stat <- sprintf("χ²=%.2f", test_result$statistic)
                                                        df_val <- as.character(test_result$parameter)
                                          }

                                          # For each level, calculate proportions and differences
                                          level_results <- list()
                                          level_pvalues <- c()

                                          levels_to_show <- all_levels
                                          if (length(all_levels) == 2) {
                                                        # Prioritize showing "positive" level
                                                        positive_candidates <- c("1", "Yes", "True", "T", "Y", "Positive", "Present", "Low")
                                                        # "Low" added just in case? No, remove "Low".
                                                        # Actually let's stick to standard positive ones.
                                                        # Removing "Low" to avoid confusion.

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

                                                        prop_all <- count_all / n_all
                                                        prop1 <- count1 / n1_total
                                                        prop2 <- count2 / n2_total

                                                        diff_prop <- prop1 - prop2

                                                        # Confidence interval
                                                        se_diff <- sqrt(prop1 * (1 - prop1) / n1_total + prop2 * (1 - prop2) / n2_total)
                                                        z_crit <- stats::qnorm(1 - (1 - conf.level) / 2)
                                                        ci_lower <- diff_prop - z_crit * se_diff
                                                        ci_upper <- diff_prop + z_crit * se_diff

                                                        # Pairwise test
                                                        if (length(all_levels) > 2) {
                                                                      binary_tbl <- matrix(c(count1, n1_total - count1, count2, n2_total - count2), nrow = 2)
                                                                      if (any(binary_tbl < 5)) {
                                                                                    level_test <- stats::fisher.test(binary_tbl)
                                                                      } else {
                                                                                    level_test <- stats::prop.test(c(count1, count2), c(n1_total, n2_total), correct = FALSE)
                                                                      }
                                                                      level_pvalues <- c(level_pvalues, level_test$p.value)
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
                                                                      CI_95 = sprintf("%.1f%%, %.1f%%", ci_lower * 100, ci_upper * 100),
                                                                      p_value = NA_character_,
                                                                      Test = test_name,
                                                                      Statistic = test_stat,
                                                                      df = df_val,
                                                                      Effect_Size = effect_size_str
                                                        )
                                          }

                                          # Adjust p-values
                                          if (length(all_levels) > 2) {
                                                        adjusted_p <- stats::p.adjust(level_pvalues, method = adjust_method)
                                                        for (i in seq_along(level_results)) {
                                                                      level_results[[i]]$p_value <- format_pvalue(adjusted_p[i])
                                                        }
                                          } else {
                                                        level_results[[1]]$p_value <- format_pvalue(overall_p)
                                          }

                                          results_list <- c(results_list, level_results)
                            }
              }

              # Combine all results
              result_df <- dplyr::bind_rows(results_list)

              # Combine all results
              result_df <- dplyr::bind_rows(results_list)

              # Rename columns
              names(result_df)[names(result_df) == "n1"] <- paste0("n_", group1_name)
              names(result_df)[names(result_df) == "Group1"] <- group1_name
              names(result_df)[names(result_df) == "n2"] <- paste0("n_", group2_name)
              names(result_df)[names(result_df) == "Group2"] <- group2_name
              names(result_df)[names(result_df) == "CI_95"] <- "95% CI"
              names(result_df)[names(result_df) == "p_value"] <- "p-value"
              names(result_df)[names(result_df) == "Effect_Size"] <- "Effect Size"
              # Clean up Total n name to be more descriptive if needed, or leave as n_Total

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

#' @export
compare_groups_table <- kk_compare_groups_table
