# ============================================================
# PROPORTION ANALYSIS FUNCTIONS
# ============================================================

#' Calculate Confidence Intervals for Proportions
#'
#' @description Computes binomial confidence intervals for proportions
#'
#' @param data Data frame with numeric columns
#' @param conf.level Confidence level (default: 0.95)
#'
#' @return Tibble with CI columns
#'
#' @export
proportion_ci <- function(data, conf.level = CONSTANTS$DEFAULT_CONF_LEVEL) {
              validate_data_frame(data)

              required_cols <- c("successes", "trials")
              if (!all(required_cols %in% names(data))) {
                            missing <- setdiff(required_cols, names(data))
                            stop(sprintf("Data must contain: %s", paste(required_cols, collapse = ", ")))
              }

              x <- data$successes
              n <- data$trials

              if (any(n < x, na.rm = TRUE)) {
                            stop("Each trial count must be >= success count")
              }

              lower <- numeric(nrow(data))
              upper <- numeric(nrow(data))

              for (i in seq_len(nrow(data))) {
                            ci <- stats::binom.test(x = x[i], n = n[i], conf.level = conf.level)$conf.int
                            lower[i] <- ci[1]
                            upper[i] <- ci[2]
              }

              non_numeric_cols <- names(data)[!sapply(data, is.numeric)]

              tibble::tibble(
                            data[, non_numeric_cols, drop = FALSE],
                            successes = x,
                            trials = n,
                            proportion = x / n,
                            lower_ci = lower,
                            upper_ci = upper,
                            conf_level = conf.level
              )
}

#' Alias for proportion_ci (backward compatibility)
#'
#' @description Old name for proportion_ci(). Use proportion_ci() instead.
#'
#' @param data Data frame with numeric columns
#' @param conf.level Confidence level
#'
#' @return Tibble with CI columns
#'
#' @export
pcit <- proportion_ci

#' Compare Proportions Between Groups (GLM-based)
#'
#' @description Tests differences in proportions using GLM with robust SE
#'
#' @param data Data frame with group, x (successes), n (trials)
#' @param group Grouping variable name
#' @param x Success count variable name
#' @param n Trial count variable name
#' @param by Optional stratification variable
#' @param covariates Optional vector of covariate names
#' @param adjust Multiple comparison adjustment method
#' @param conf.level Confidence level
#' @param vcov_type Sandwich VCOV type
#' @param drop_empty Drop empty combinations
#'
#' @return Tibble with pairwise comparisons
#'
#' @export
compare_proportions_glm <- function(data, group, x, n,
                                    by = NULL, covariates = NULL,
                                    adjust = "holm",
                                    conf.level = CONSTANTS$DEFAULT_CONF_LEVEL,
                                    vcov_type = "HC3", drop_empty = TRUE) {
              validate_data_frame(data)

              for (pkg in c("emmeans", "sandwich")) {
                            if (!requireNamespace(pkg, quietly = TRUE)) {
                                          stop(sprintf("Package '%s' is required", pkg))
                            }
              }

              group_sym <- dplyr::ensym(group)
              x_sym <- dplyr::ensym(x)
              n_sym <- dplyr::ensym(n)
              by_sym <- if (is.null(by)) NULL else dplyr::ensym(by)

              group_name <- rlang::as_string(group_sym)
              by_name <- if (is.null(by_sym)) NULL else rlang::as_string(by_sym)

              df <- data %>%
                            dplyr::mutate(
                                          .x = !!x_sym,
                                          .n = !!n_sym,
                                          .fail = .n - .x
                            )

              if (any(df$.n < df$.x, na.rm = TRUE)) {
                            stop("Each 'n' must be >= 'x'")
              }

              rhs_terms <- group_name
              if (!is.null(by_name)) rhs_terms <- paste(rhs_terms, by_name, sep = " * ")
              if (!is.null(covariates) && length(covariates)) {
                            rhs_terms <- paste(rhs_terms, paste(covariates, collapse = " + "), sep = " + ")
              }

              fml <- stats::as.formula(paste0("cbind(.x, .fail) ~ ", rhs_terms))
              fit <- stats::glm(fml, family = stats::binomial("logit"), data = df)

              V <- tryCatch(
                            sandwich::vcovHC(fit, type = vcov_type),
                            error = function(e) stats::vcov(fit)
              )

              hat <- tryCatch(stats::hatvalues(fit), error = function(e) rep(0, nrow(df)))

              if (any(is.finite(hat) & hat > 0.99)) V <- stats::vcov(fit)

              if (is.null(by_name)) {
                            emm <- emmeans::emmeans(fit, specs = group_name, vcov. = V)
                            emm_resp <- emmeans::regrid(emm, transform = "response")
                            cmp <- emmeans::contrast(emm_resp, method = "pairwise", adjust = adjust)
                            out <- as.data.frame(summary(cmp, infer = TRUE, level = conf.level))

                            tibble::tibble(
                                          group1 = sub(" - .*", "", out$contrast),
                                          group2 = sub(".* - ", "", out$contrast),
                                          estimate = out$estimate,
                                          conf_low = out$lower.CL,
                                          conf_high = out$upper.CL,
                                          p_value = out$p.value,
                                          adjust_method = adjust,
                                          conf_level = conf.level
                            )
              } else {
                            emm <- emmeans::emmeans(fit, specs = group_name, by = by_name, vcov. = V)
                            emm_resp <- emmeans::regrid(emm, transform = "response")
                            cmp <- emmeans::contrast(emm_resp, method = "pairwise", by = by_name, adjust = adjust)
                            tmp <- as.data.frame(summary(cmp, infer = TRUE, level = conf.level))

                            out <- tibble::tibble(
                                          !!by_name := tmp[[by_name]],
                                          group1 = sub(" - .*", "", tmp$contrast),
                                          group2 = sub(".* - ", "", tmp$contrast),
                                          estimate = tmp$estimate,
                                          conf_low = tmp$lower.CL,
                                          conf_high = tmp$upper.CL,
                                          p_value = tmp$p.value,
                                          adjust_method = adjust,
                                          conf_level = conf.level
                            )

                            if (drop_empty) {
                                          out <- out %>%
                                                        dplyr::group_by(.data[[by_name]]) %>%
                                                        dplyr::filter(dplyr::n() > 0) %>%
                                                        dplyr::ungroup()
                            }

                            out
              }
}

#' Simple Proportion Comparisons
#'
#' @description Tests pairwise differences in proportions using z-tests
#'
#' @param data Data frame with proportion and trials columns
#' @param conf.level Confidence level
#' @param method P-value adjustment method
#'
#' @return Tibble with pairwise comparisons
#'
#' @export
compare_proportions_simple <- function(data,
                                       conf.level = CONSTANTS$DEFAULT_CONF_LEVEL,
                                       method = "holm") {
              validate_data_frame(data)

              required_cols <- c("proportion", "trials")
              if (!all(required_cols %in% names(data))) {
                            stop(sprintf("Data must contain: %s", paste(required_cols, collapse = ", ")))
              }

              non_numeric_cols <- names(data)[!sapply(data, is.numeric)]
              if (length(non_numeric_cols) == 0) {
                            stop("No non-numeric columns found for grouping")
              }

              if (nrow(data) < 2) {
                            stop("Data must contain at least two groups")
              }

              combinations <- utils::combn(seq_len(nrow(data)), 2, simplify = FALSE)

              calculate_difference <- function(index_pair) {
                            i <- index_pair[1]
                            j <- index_pair[2]

                            group1 <- data[i, ]
                            group2 <- data[j, ]

                            prop_diff <- group1$proportion - group2$proportion

                            pooled_se <- sqrt(
                                          group1$proportion * (1 - group1$proportion) / group1$trials +
                                                        group2$proportion * (1 - group2$proportion) / group2$trials
                            )

                            pooled_se <- max(pooled_se, CONSTANTS$SMALL_CONSTANT)

                            if (pooled_se == 0) {
                                          stop("Standard error is zero")
                            }

                            z_score <- prop_diff / pooled_se
                            p_value <- 2 * (1 - stats::pnorm(abs(z_score)))

                            z_critical <- stats::qnorm(1 - (1 - conf.level) / 2)
                            ci_lower <- prop_diff - z_critical * pooled_se
                            ci_upper <- prop_diff + z_critical * pooled_se

                            tibble::tibble(
                                          !!!setNames(
                                                        lapply(non_numeric_cols, function(col) group1[[col]]),
                                                        paste0("group1_", non_numeric_cols)
                                          ),
                                          !!!setNames(
                                                        lapply(non_numeric_cols, function(col) group2[[col]]),
                                                        paste0("group2_", non_numeric_cols)
                                          ),
                                          prop_diff = prop_diff,
                                          z_score = z_score,
                                          p_value = p_value,
                                          ci_lower = ci_lower,
                                          ci_upper = ci_upper
                            )
              }

              results <- purrr::map_dfr(combinations, calculate_difference)

              results %>%
                            dplyr::mutate(adj_p_value = stats::p.adjust(p_value, method = method))
}
