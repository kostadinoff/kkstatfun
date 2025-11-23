# ============================================================
# PROPORTION ANALYSIS
# ============================================================

#' Calculate Confidence Intervals for Proportions (PCIT)
#'
#' @description Calculates binomial confidence intervals for proportions based on
#'   the first two numeric columns (successes, trials).
#'
#' @param data Data frame
#' @param conf.level Confidence level (default: 0.95)
#'
#' @return Tibble with results
#'
#' @export
pcit <- function(data, conf.level = 0.95) {
              # Identify numeric columns
              numeric_cols <- sapply(data, is.numeric)

              # Ensure there are at least two numeric columns
              if (sum(numeric_cols) < 2) {
                            stop("The dataset must contain at least two numeric columns.")
              }

              # Get indices of the first and second numeric columns
              numeric_col_indices <- which(numeric_cols)

              if (length(numeric_col_indices) < 2) {
                            stop("Not enough numeric columns found.")
              }

              # Extract the first and second numeric columns
              x <- data[[numeric_col_indices[1]]]
              n <- data[[numeric_col_indices[2]]]

              # Ensure that n >= x for all cases
              if (any(n < x)) {
                            stop("Each trial count must be greater than or equal to the success count.")
              }

              # Initialize vectors for confidence intervals
              lower <- numeric(nrow(data))
              upper <- numeric(nrow(data))

              # Calculate confidence intervals for each row
              for (i in seq_len(nrow(data))) {
                            ci <- stats::binom.test(
                                          x = x[i],
                                          n = n[i],
                                          conf.level = conf.level
                            )$conf.int
                            lower[i] <- ci[1]
                            upper[i] <- ci[2]
              }

              # Identify non-numeric columns for grouping
              non_numeric_cols <- names(data)[!numeric_cols]

              # Create a tibble including all original columns and calculated results
              result <- tibble::tibble(
                            # Include non-numeric columns
                            data[, non_numeric_cols, drop = FALSE],

                            # Calculated columns with distinct names
                            successes = x,
                            trials = n,
                            proportion = x / n,
                            lower = lower,
                            upper = upper,
                            conf.level = conf.level
              )

              return(result)
}

#' Compare Proportions using GLM
#'
#' @description Compares proportions between groups using logistic regression
#'   with robust standard errors.
#'
#' @param data Data frame
#' @param group Grouping variable
#' @param x Success count variable
#' @param n Trial count variable
#' @param by Optional stratification variable
#' @param covariates Optional covariates
#' @param adjust Adjustment method (default: "holm")
#' @param conf.level Confidence level (default: 0.95)
#' @param vcov_type Robust variance type (default: "HC3")
#' @param drop_empty Drop empty groups (default: TRUE)
#'
#' @export
compare_proportions_kk_glm <- function(data, group, x, n,
                                       by = NULL, covariates = NULL,
                                       adjust = "holm", conf.level = 0.95,
                                       vcov_type = "HC3", drop_empty = TRUE) {
              for (pkg in c("emmeans", "sandwich")) {
                            if (!requireNamespace(pkg, quietly = TRUE)) {
                                          stop(sprintf("Package '%s' is required. Install it first.", pkg))
                            }
              }

              group_sym <- dplyr::ensym(group)
              x_sym <- dplyr::ensym(x)
              n_sym <- dplyr::ensym(n)
              by_sym <- if (is.null(by)) NULL else dplyr::ensym(by)

              group_name <- rlang::as_string(group_sym)
              by_name <- if (is.null(by_sym)) NULL else rlang::as_string(by_sym)

              df <- data |>
                            dplyr::mutate(
                                          .x = !!x_sym,
                                          .n = !!n_sym,
                                          .fail = .n - .x
                            )

              if (any(df$.n < df$.x, na.rm = TRUE)) {
                            stop("Each 'n' must be >= 'x'.")
              }

              rhs_terms <- group_name
              if (!is.null(by_name)) rhs_terms <- paste(rhs_terms, by_name, sep = " * ")
              if (!is.null(covariates) && length(covariates)) {
                            rhs_terms <- paste(rhs_terms, paste(covariates, collapse = " + "), sep = " + ")
              }
              fml <- stats::as.formula(paste0("cbind(.x, .fail) ~ ", rhs_terms))

              fit <- stats::glm(fml, family = stats::binomial("logit"), data = df)

              # robust VCOV with safe fallback if leverage ~1
              V <- tryCatch(sandwich::vcovHC(fit, type = vcov_type), error = function(e) stats::vcov(fit))
              hat <- tryCatch(stats::hatvalues(fit), error = function(e) rep(0, nrow(df)))
              if (any(is.finite(hat) & hat > 0.99)) V <- stats::vcov(fit)

              if (is.null(by_name)) {
                            emm <- emmeans::emmeans(fit, specs = group_name, vcov. = V)
                            emm_resp <- emmeans::regrid(emm, transform = "response")
                            cmp <- emmeans::contrast(emm_resp, method = "pairwise", adjust = adjust)
                            out <- as.data.frame(summary(cmp, infer = TRUE, level = conf.level)) # <- no emmeans:: here
                            tibble::tibble(
                                          group1     = sub(" - .*", "", out$contrast),
                                          group2     = sub(".* - ", "", out$contrast),
                                          estimate   = out$estimate,
                                          conf_low   = out$lower.CL,
                                          conf_high  = out$upper.CL,
                                          p_value    = out$p.value,
                                          adjust     = adjust,
                                          conf_level = conf.level
                            )
              } else {
                            emm <- emmeans::emmeans(fit, specs = group_name, by = by_name, vcov. = V)
                            emm_resp <- emmeans::regrid(emm, transform = "response")
                            cmp <- emmeans::contrast(emm_resp, method = "pairwise", by = by_name, adjust = adjust)
                            tmp <- as.data.frame(summary(cmp, infer = TRUE, level = conf.level)) # <- no emmeans::
                            out <- tibble::tibble(
                                          !!by_name := tmp[[by_name]],
                                          group1     = sub(" - .*", "", tmp$contrast),
                                          group2     = sub(".* - ", "", tmp$contrast),
                                          estimate   = tmp$estimate,
                                          conf_low   = tmp$lower.CL,
                                          conf_high  = tmp$upper.CL,
                                          p_value    = tmp$p.value,
                                          adjust     = adjust,
                                          conf_level = conf.level
                            )
                            if (drop_empty) {
                                          out <- out |>
                                                        dplyr::group_by(.data[[by_name]]) |>
                                                        dplyr::filter(dplyr::n() > 0) |>
                                                        dplyr::ungroup()
                            }
                            out
              }
}

#' Compare Proportions (Simple)
#'
#' @description Pairwise comparison of proportions using normal approximation
#'
#' @param data Data frame with 'proportion' and 'trials' columns
#' @param conf.level Confidence level
#' @param method Adjustment method
#'
#' @export
compare_proportions <- function(data,
                                conf.level = 0.95,
                                method = "holm") {
              # Validate the input data
              required_cols <- c("proportion", "trials")
              if (!all(required_cols %in% names(data))) {
                            stop("The input data must contain 'proportion' and 'trials' columns.")
              }
              # Identify group columns dynamically (assuming non-numeric columns are groups)
              non_numeric_cols <- names(data)[!sapply(data, is.numeric)]
              if (length(non_numeric_cols) == 0) {
                            stop("No non-numeric columns found for grouping.")
              }
              # Get all combinations of non-numeric columns for group identification
              group_cols <- non_numeric_cols
              # Ensure there are at least two groups to compare
              if (nrow(data) < 2) {
                            stop("The dataset must contain at least two groups for comparison.")
              }
              # Create a grid of pairwise combinations of the groups
              combinations <- utils::combn(seq_len(nrow(data)), 2, simplify = FALSE)
              # Function to calculate differences, p-values, and confidence intervals for each pair
              calculate_difference <- function(index_pair) {
                            i <- index_pair[1]
                            j <- index_pair[2]

                            group1 <- data[i, ]
                            group2 <- data[j, ]

                            # Proportion difference
                            prop_diff <- group1$proportion - group2$proportion
                            # Standard error for the difference in proportions
                            pooled_se <- sqrt(
                                          group1$proportion * (1 - group1$proportion) / group1$trials +
                                                        group2$proportion * (1 - group2$proportion) / group2$trials
                            )

                            small_constant <- 1e-10
                            pooled_se <- max(pooled_se, small_constant)

                            # Prevent division by zero
                            if (pooled_se == 0) {
                                          stop("Standard error is zero; cannot compute z-score and p-value.")
                            }
                            z_score <- prop_diff / pooled_se
                            # Calculate p-value for the proportion difference
                            p_value <- 2 * (1 - stats::pnorm(abs(z_score)))
                            # Calculate confidence interval for the proportion difference
                            z_critical <- stats::qnorm(1 - (1 - conf.level) / 2)
                            ci_lower <- prop_diff - z_critical * pooled_se
                            ci_upper <- prop_diff + z_critical * pooled_se

                            result <- tibble::tibble(
                                          !!!stats::setNames(
                                                        lapply(group_cols, function(col) {
                                                                      group1[[col]]
                                                        }),
                                                        paste0("gr1_", group_cols)
                                          ),
                                          !!!stats::setNames(
                                                        lapply(group_cols, function(col) {
                                                                      group2[[col]]
                                                        }),
                                                        paste0("gr2_", group_cols)
                                          ),
                                          prop_diff = prop_diff,
                                          z_score = z_score,
                                          p_value = p_value,
                                          ci_lower = ci_lower,
                                          ci_upper = ci_upper
                            )

                            return(result)
              }

              # Apply the calculate_difference function to each pair
              results <- purrr::map_dfr(combinations, calculate_difference)

              # Adjust p-values for multiple comparisons
              results <- results %>%
                            dplyr::mutate(adj_p_value = stats::p.adjust(p_value, method = method))

              return(results)
}

#' Compare Proportions Stratified by Group
#'
#' @description Compares proportions within groups (stratified analysis)
#'
#' @param data Data frame
#' @param conf.level Confidence level
#' @param method Adjustment method
#'
#' @export
compare_proportions_by <- function(data,
                                   conf.level = 0.95,
                                   method = "holm") {
              # Validate the input data
              required_cols <- c("proportion", "trials")
              if (!all(required_cols %in% names(data))) {
                            stop("The input data must contain 'proportion' and 'trials' columns.")
              }

              # Automatically determine group and subgroup variables
              group_var <- names(data)[1]
              subgroup_var <- names(data)[2]

              # Split the data by group
              grouped_data <- split(data, data[[group_var]])

              # Function to calculate differences, p-values, and confidence intervals for each subgroup
              calculate_difference_within_group <- function(df) {
                            # Generate pairwise comparisons for subgroups
                            pairwise_comparisons <- utils::combn(unique(df[[subgroup_var]]), 2, simplify = FALSE)

                            # Initialize an empty result dataframe
                            result_list <- list()

                            for (pair in pairwise_comparisons) {
                                          subgroup1 <- df %>% dplyr::filter(!!rlang::sym(subgroup_var) == pair[1])
                                          subgroup2 <- df %>% dplyr::filter(!!rlang::sym(subgroup_var) == pair[2])

                                          # Ensure there is exactly one row in each subgroup
                                          if (nrow(subgroup1) != 1 || nrow(subgroup2) != 1) {
                                                        warning("Each subgroup should have exactly one entry for comparison.")
                                                        next
                                          }

                                          # Calculate proportion difference
                                          prop_diff <- subgroup1$proportion - subgroup2$proportion

                                          # Standard error for the difference in proportions
                                          pooled_se <- sqrt(
                                                        subgroup1$proportion * (1 - subgroup1$proportion) / subgroup1$trials +
                                                                      subgroup2$proportion * (1 - subgroup2$proportion) / subgroup2$trials
                                          )
                                          small_constant <- 1e-10
                                          pooled_se <- max(pooled_se, small_constant)
                                          # Prevent division by zero
                                          if (pooled_se == 0) {
                                                        warning("Standard error is zero; cannot compute z-score and p-value.")
                                                        next
                                          }

                                          z_score <- prop_diff / pooled_se

                                          # Calculate p-value for the proportion difference
                                          p_value <- 2 * (1 - stats::pnorm(abs(z_score)))

                                          # Calculate confidence interval for the proportion difference
                                          z_critical <- stats::qnorm(1 - (1 - conf.level) / 2)
                                          ci_lower <- prop_diff - z_critical * pooled_se
                                          ci_upper <- prop_diff + z_critical * pooled_se

                                          # Append the results for this pairwise comparison
                                          result_list[[length(result_list) + 1]] <- tibble::tibble(
                                                        group = unique(df[[group_var]]),
                                                        subgroup1 = pair[1],
                                                        subgroup2 = pair[2],
                                                        prop_diff = prop_diff,
                                                        z_score = z_score,
                                                        p_value = as.numeric(p_value),
                                                        # Ensure p_value is numeric
                                                        ci_lower = ci_lower,
                                                        ci_upper = ci_upper
                                          )
                            }

                            # Combine all results into one dataframe
                            dplyr::bind_rows(result_list)
              }

              # Apply the calculation function to each group
              results <- purrr::map_dfr(grouped_data, calculate_difference_within_group)

              # Adjust p-values for multiple comparisons if p_value column is correctly numeric
              if (nrow(results) > 0) {
                            results <- results %>%
                                          dplyr::mutate(adj_p_value = stats::p.adjust(p_value, method = method)) %>%
                                          dplyr::select(
                                                        group,
                                                        subgroup1,
                                                        subgroup2,
                                                        prop_diff,
                                                        z_score,
                                                        p_value,
                                                        ci_lower,
                                                        ci_upper,
                                                        adj_p_value
                                          )
              } else {
                            results <- results %>%
                                          dplyr::mutate(adj_p_value = NA) %>%
                                          dplyr::select(
                                                        group,
                                                        subgroup1,
                                                        subgroup2,
                                                        prop_diff,
                                                        z_score,
                                                        p_value,
                                                        ci_lower,
                                                        ci_upper,
                                                        adj_p_value
                                          )
              }

              return(results)
}

#' Test for Trend in Proportions
#'
#' @description Performs the Cochran-Armitage test for trend in proportions.
#'
#' @param data Data frame
#' @param x Variable for number of successes (or binary outcome if n is NULL)
#' @param n Variable for number of trials (optional if x is binary outcome)
#' @param group Grouping variable (ordered)
#'
#' @return Result of prop.trend.test
#' @export
prop_trend_test <- function(data, x, n = NULL, group = NULL) {
              x_enquo <- rlang::enquo(x)
              n_enquo <- rlang::enquo(n)
              group_enquo <- rlang::enquo(group)

              if (!rlang::quo_is_null(n_enquo)) {
                            # Summarized data case: x=successes, n=trials, group=group

                            df <- data %>%
                                          dplyr::select(!!x_enquo, !!n_enquo, !!group_enquo) %>%
                                          dplyr::arrange(!!group_enquo)

                            x_vec <- df %>% dplyr::pull(!!x_enquo)
                            n_vec <- df %>% dplyr::pull(!!n_enquo)
              } else {
                            # Raw data case: x=outcome (binary), group=group
                            # We need to calculate successes and trials per group

                            df <- data %>%
                                          dplyr::group_by(!!group_enquo) %>%
                                          dplyr::summarise(
                                                        successes = sum(!!x_enquo, na.rm = TRUE),
                                                        trials = dplyr::n()
                                          ) %>%
                                          dplyr::arrange(!!group_enquo)

                            x_vec <- df$successes
                            n_vec <- df$trials
              }

              stats::prop.trend.test(x_vec, n_vec)
}
