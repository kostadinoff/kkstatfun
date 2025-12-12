# ============================================================
# GROUP COMPARISON TABLE
# ============================================================

#' Compare Groups with Differences and Confidence Intervals
#'
#' @description Creates a comparison table showing group statistics, differences,
#'   confidence intervals, and p-values for both categorical and numerical variables.
#'
#' @param data Data frame
#' @param group Grouping variable (must have exactly 2 levels)
#' @param variables Vector of variable names to compare
#' @param nonparametric Use nonparametric tests for continuous variables (default: FALSE)
#' @param adjust_method P-value adjustment method for categorical variables with >2 levels (default: "holm")
#' @param conf.level Confidence level (default: 0.95)
#'
#' @return Tibble with comparison results
#'
#' @export
kk_compare_groups_table <- function(data, group, variables,
                                    nonparametric = FALSE,
                                    adjust_method = "holm",
                                    conf.level = 0.95) {
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

              n1 <- nrow(data1)
              n2 <- nrow(data2)

              # Process each variable
              results_list <- list()

              for (var in variables) {
                            var_data <- data[[var]]
                            var_data1 <- data1[[var]]
                            var_data2 <- data2[[var]]

                            # Determine variable type
                            is_numeric <- is.numeric(var_data)
                            is_categorical <- is.factor(var_data) || is.character(var_data)

                            if (is_numeric) {
                                          # Numerical variable
                                          mean1 <- mean(var_data1, na.rm = TRUE)
                                          sd1 <- stats::sd(var_data1, na.rm = TRUE)
                                          mean2 <- mean(var_data2, na.rm = TRUE)
                                          sd2 <- stats::sd(var_data2, na.rm = TRUE)

                                          diff <- mean1 - mean2

                                          # Statistical test
                                          if (nonparametric) {
                                                        test_result <- stats::wilcox.test(var_data1, var_data2, conf.int = TRUE, conf.level = conf.level)
                                                        test_name <- "Wilcoxon"
                                                        ci_lower <- test_result$conf.int[1]
                                                        ci_upper <- test_result$conf.int[2]
                                          } else {
                                                        test_result <- stats::t.test(var_data1, var_data2, conf.level = conf.level)
                                                        test_name <- "t-test"
                                                        ci_lower <- test_result$conf.int[1]
                                                        ci_upper <- test_result$conf.int[2]
                                          }

                                          p_value <- test_result$p.value

                                          results_list[[length(results_list) + 1]] <- tibble::tibble(
                                                        Characteristic = var,
                                                        Group1 = sprintf("%.2f (%.2f)", mean1, sd1),
                                                        Group2 = sprintf("%.2f (%.2f)", mean2, sd2),
                                                        Difference = sprintf("%.2f", diff),
                                                        CI_95 = sprintf("%.2f, %.2f", ci_lower, ci_upper),
                                                        p_value = format_pvalue(p_value),
                                                        Test = test_name
                                          )
                            } else if (is_categorical) {
                                          # Categorical variable
                                          var_factor1 <- factor(var_data1)
                                          var_factor2 <- factor(var_data2)

                                          # Get all levels
                                          all_levels <- unique(c(levels(var_factor1), levels(var_factor2)))

                                          # Create contingency table
                                          tbl <- table(data[[group_name]], data[[var]], useNA = "no")

                                          # Determine test
                                          if (any(tbl < 5)) {
                                                        # Use Fisher's exact test for small counts
                                                        test_result <- stats::fisher.test(tbl)
                                                        test_name <- "Fisher"
                                                        overall_p <- test_result$p.value
                                          } else {
                                                        # Use Chi-square test
                                                        test_result <- stats::chisq.test(tbl)
                                                        test_name <- "Chi-square"
                                                        overall_p <- test_result$p.value
                                          }

                                          # For each level, calculate proportions and differences
                                          level_results <- list()
                                          level_pvalues <- c()

                                          for (level in all_levels) {
                                                        count1 <- sum(var_data1 == level, na.rm = TRUE)
                                                        count2 <- sum(var_data2 == level, na.rm = TRUE)

                                                        prop1 <- count1 / n1
                                                        prop2 <- count2 / n2

                                                        diff_prop <- prop1 - prop2

                                                        # Confidence interval for difference in proportions
                                                        # Using normal approximation
                                                        se_diff <- sqrt(prop1 * (1 - prop1) / n1 + prop2 * (1 - prop2) / n2)
                                                        z_crit <- stats::qnorm(1 - (1 - conf.level) / 2)
                                                        ci_lower <- diff_prop - z_crit * se_diff
                                                        ci_upper <- diff_prop + z_crit * se_diff

                                                        # Pairwise test for this level
                                                        if (length(all_levels) > 2) {
                                                                      # Binary comparison for this level vs others
                                                                      binary_tbl <- matrix(c(count1, n1 - count1, count2, n2 - count2), nrow = 2)

                                                                      if (any(binary_tbl < 5)) {
                                                                                    level_test <- stats::fisher.test(binary_tbl)
                                                                      } else {
                                                                                    level_test <- stats::prop.test(c(count1, count2), c(n1, n2), correct = FALSE)
                                                                      }
                                                                      level_pvalues <- c(level_pvalues, level_test$p.value)
                                                        }

                                                        level_results[[length(level_results) + 1]] <- tibble::tibble(
                                                                      Characteristic = if (length(all_levels) > 2) paste0(var, " - ", level) else var,
                                                                      Group1 = sprintf("%d (%.1f%%)", count1, prop1 * 100),
                                                                      Group2 = sprintf("%d (%.1f%%)", count2, prop2 * 100),
                                                                      Difference = sprintf("%.1f%%", diff_prop * 100),
                                                                      CI_95 = sprintf("%.1f%%, %.1f%%", ci_lower * 100, ci_upper * 100),
                                                                      p_value = NA_character_, # Will be filled later
                                                                      Test = test_name
                                                        )
                                          }

                                          # Adjust p-values if multiple levels
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

              # Rename columns with group names
              names(result_df)[2] <- sprintf("%s\nN = %d", group1_name, n1)
              names(result_df)[3] <- sprintf("%s\nN = %d", group2_name, n2)
              names(result_df)[5] <- "95% CI"
              names(result_df)[6] <- "p-value"

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
