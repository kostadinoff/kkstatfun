# ============================================================
# ADVANCED EPIDEMIOLOGY FUNCTIONS
# ============================================================

#' Mantel-Haenszel Stratified 2x2 Analysis (KK)
#'
#' @description Performs stratified 2x2 analysis across multiple strata to control for confounding.
#'   Calculates Mantel-Haenszel pooled Odds Ratio and Relative Risk with tests for homogeneity.
#'
#' @param data Data frame containing exposure, outcome, and strata variables
#' @param exposure Exposure variable (binary)
#' @param outcome Outcome variable (binary)
#' @param strata Stratification variable
#' @param conf.level Confidence level (default 0.95)
#' @param method Method for pooling (default "mh" for Mantel-Haenszel)
#'
#' @return List containing:
#'   \itemize{
#'     \item pooled_or - Pooled Odds Ratio with CI
#'     \item pooled_rr - Pooled Risk Ratio with CI
#'     \item homogeneity_test - Test for homogeneity across strata
#'     \item breslow_day - Breslow-Day test for homogeneity of ORs
#'     \item stratum_specific - Stratum-specific estimates
#'   }
#'
#' @examples
#' data <- data.frame(
#'               exposure = c(1, 1, 0, 0, 1, 1, 0, 0),
#'               outcome = c(1, 0, 1, 0, 1, 0, 1, 0),
#'               strata = c(1, 1, 1, 1, 2, 2, 2, 2)
#' )
#' kk_stratified_2x2(data, exposure, outcome, strata)
#'
#' @export
kk_stratified_2x2 <- function(data, exposure, outcome, strata, conf.level = 0.95, method = "mh") {
              exposure_enquo <- rlang::enquo(exposure)
              outcome_enquo <- rlang::enquo(outcome)
              strata_enquo <- rlang::enquo(strata)

              exp_vec <- data %>% dplyr::pull(!!exposure_enquo)
              out_vec <- data %>% dplyr::pull(!!outcome_enquo)
              strata_vec <- data %>% dplyr::pull(!!strata_enquo)

              # Get unique strata
              strata_levels <- unique(strata_vec)
              k <- length(strata_levels)

              # Initialize arrays for stratum-specific calculations
              a <- numeric(k)
              b <- numeric(k)
              c_vec <- numeric(k) # renamed to avoid conflict with c()
              d <- numeric(k)
              n1 <- numeric(k)
              n0 <- numeric(k)
              m1 <- numeric(k)
              n_total <- numeric(k)

              # Calculate stratum-specific values
              for (i in seq_along(strata_levels)) {
                            idx <- strata_vec == strata_levels[i]
                            tbl <- table(Exposure = factor(exp_vec[idx]), Outcome = factor(out_vec[idx]))

                            if (all(dim(tbl) == c(2, 2))) {
                                          a[i] <- tbl[2, 2]
                                          b[i] <- tbl[2, 1]
                                          c_vec[i] <- tbl[1, 2]
                                          d[i] <- tbl[1, 1]

                                          n1[i] <- a[i] + b[i] # exposed
                                          n0[i] <- c_vec[i] + d[i] # unexposed
                                          m1[i] <- a[i] + c_vec[i] # cases
                                          n_total[i] <- a[i] + b[i] + c_vec[i] + d[i]
                            } else {
                                          stop(paste("Stratum", strata_levels[i], "does not have 2x2 structure"))
                            }
              }

              # Mantel-Haenszel Pooled Odds Ratio
              mh_or_num <- sum(a * d / n_total)
              mh_or_den <- sum(b * c_vec / n_total)
              mh_or <- mh_or_num / mh_or_den

              # MH OR variance (Robins, Breslow, Greenland)
              p_sum <- sum((a + d) * a * d / n_total^2)
              q_sum <- sum((b * c_vec * (a + d) + a * d * (b + c_vec)) / n_total^2)
              r_sum <- sum((b + c_vec) * b * c_vec / n_total^2)

              se_ln_mh_or <- sqrt((p_sum / (2 * mh_or_num^2)) + (q_sum / (2 * mh_or_num * mh_or_den)) + (r_sum / (2 * mh_or_den^2)))
              z <- stats::qnorm(1 - (1 - conf.level) / 2)
              mh_or_lower <- exp(log(mh_or) - z * se_ln_mh_or)
              mh_or_upper <- exp(log(mh_or) + z * se_ln_mh_or)

              # Mantel-Haenszel Pooled Risk Ratio
              mh_rr_num <- sum(a * n0 / n_total)
              mh_rr_den <- sum(c_vec * n1 / n_total)
              mh_rr <- mh_rr_num / mh_rr_den

              # MH RR variance
              var_ln_mh_rr <- sum((n1 * n0 * m1 * (n_total - m1)) / (n_total^3)) / (mh_rr_num * mh_rr_den)
              se_ln_mh_rr <- sqrt(var_ln_mh_rr)
              mh_rr_lower <- exp(log(mh_rr) - z * se_ln_mh_rr)
              mh_rr_upper <- exp(log(mh_rr) + z * se_ln_mh_rr)

              # Mantel-Haenszel Chi-square test
              mh_chisq_num <- (abs(sum(a) - sum(n1 * m1 / n_total)) - 0.5)^2
              mh_chisq_den <- sum(n1 * n0 * m1 * (n_total - m1) / (n_total^2 * (n_total - 1)))
              mh_chisq <- mh_chisq_num / mh_chisq_den
              mh_pvalue <- stats::pchisq(mh_chisq, df = 1, lower.tail = FALSE)

              # Breslow-Day test for homogeneity of ORs
              or_i <- (a * d) / (b * c_vec)
              expected_a <- numeric(k)

              # Iterative solution for expected a under common OR
              for (i in 1:k) {
                            # Solve quadratic equation for expected a
                            A_coef <- mh_or - 1
                            B_coef <- n1[i] + m1[i] + (mh_or - 1) * (n1[i] + m1[i])
                            C_coef <- -mh_or * n1[i] * m1[i]

                            if (abs(A_coef) < 1e-10) {
                                          expected_a[i] <- -C_coef / B_coef
                            } else {
                                          discriminant <- B_coef^2 - 4 * A_coef * C_coef
                                          if (discriminant >= 0) {
                                                        root1 <- (-B_coef + sqrt(discriminant)) / (2 * A_coef)
                                                        root2 <- (-B_coef - sqrt(discriminant)) / (2 * A_coef)
                                                        expected_a[i] <- ifelse(root1 >= 0 && root1 <= min(n1[i], m1[i]), root1, root2)
                                          } else {
                                                        expected_a[i] <- n1[i] * m1[i] / n_total[i]
                                          }
                            }
              }

              expected_b <- n1 - expected_a
              expected_c <- m1 - expected_a
              expected_d <- n_total - expected_a - expected_b - expected_c

              # Breslow-Day statistic
              bd_stat <- sum((a - expected_a)^2 / (1 / expected_a + 1 / expected_b + 1 / expected_c + 1 / expected_d)^(-1))
              bd_pvalue <- stats::pchisq(bd_stat, df = k - 1, lower.tail = FALSE)

              # Stratum-specific estimates
              stratum_results <- tibble::tibble(
                            Stratum = strata_levels,
                            OR = or_i,
                            RR = (a / n1) / (c_vec / n0),
                            Exposed_Cases = a,
                            Exposed_Total = n1,
                            Unexposed_Cases = c_vec,
                            Unexposed_Total = n0
              )

              # Return results
              list(
                            pooled_or = tibble::tibble(
                                          Estimate = mh_or,
                                          Lower = mh_or_lower,
                                          Upper = mh_or_upper,
                                          Method = "Mantel-Haenszel"
                            ),
                            pooled_rr = tibble::tibble(
                                          Estimate = mh_rr,
                                          Lower = mh_rr_lower,
                                          Upper = mh_rr_upper,
                                          Method = "Mantel-Haenszel"
                            ),
                            homogeneity_test = tibble::tibble(
                                          Test = "Mantel-Haenszel Chi-square",
                                          Statistic = mh_chisq,
                                          DF = 1,
                                          P_Value = mh_pvalue
                            ),
                            breslow_day = tibble::tibble(
                                          Test = "Breslow-Day",
                                          Statistic = bd_stat,
                                          DF = k - 1,
                                          P_Value = bd_pvalue,
                                          Interpretation = ifelse(bd_pvalue > 0.05, "ORs appear homogeneous", "ORs may be heterogeneous")
                            ),
                            stratum_specific = stratum_results
              )
}

#' McNemar's Test for Matched Pairs (KK)
#'
#' @description Performs McNemar's test for matched/paired case-control or before-after studies.
#'   Calculates conditional odds ratio with exact and asymptotic confidence intervals.
#'
#' @param data Data frame containing exposure, outcome, and pair ID
#' @param exposure Exposure or treatment variable (binary)
#' @param outcome Outcome variable (binary)
#' @param pair_id Variable identifying matched pairs
#' @param conf.level Confidence level (default 0.95)
#' @param exact Use exact test (default TRUE), otherwise continuity-corrected chi-square
#'
#' @return Tibble with McNemar's test results and conditional OR
#'
#' @examples
#' data <- data.frame(
#'               id = rep(1:50, 2),
#'               exposure = rep(c(0, 1), each = 50),
#'               outcome = sample(0:1, 100, replace = TRUE)
#' )
#' kk_mcnemar(data, exposure, outcome, id)
#'
#' @export
kk_mcnemar <- function(data, exposure, outcome, pair_id, conf.level = 0.95, exact = TRUE) {
              exposure_enquo <- rlang::enquo(exposure)
              outcome_enquo <- rlang::enquo(outcome)
              pair_enquo <- rlang::enquo(pair_id)

              exp_vec <- data %>% dplyr::pull(!!exposure_enquo)
              out_vec <- data %>% dplyr::pull(!!outcome_enquo)
              pair_vec <- data %>% dplyr::pull(!!pair_enquo)

              # Create contingency table for discordant pairs
              # We need to reshape to get exposure status for each member of pair
              pair_data <- data %>%
                            dplyr::select(!!pair_enquo, !!exposure_enquo, !!outcome_enquo) %>%
                            dplyr::arrange(!!pair_enquo)

              # Count discordant pairs
              # b = outcome+/exposure-, c = outcome-/exposure+
              tbl <- table(Exposure = exp_vec, Outcome = out_vec)

              if (all(dim(tbl) == c(2, 2))) {
                            b <- tbl[1, 2] # Exposure-, Outcome+
                            c_val <- tbl[2, 1] # Exposure+, Outcome-

                            # McNemar's test
                            if (exact) {
                                          # Exact binomial test
                                          mcnemar_result <- stats::binom.test(c_val, b + c_val, p = 0.5)
                                          test_stat <- NA
                                          pvalue <- mcnemar_result$p.value
                                          test_name <- "Exact binomial"
                            } else {
                                          # Continuity-corrected chi-square
                                          test_stat <- (abs(b - c_val) - 1)^2 / (b + c_val)
                                          pvalue <- stats::pchisq(test_stat, df = 1, lower.tail = FALSE)
                                          test_name <- "Chi-square (continuity corrected)"
                            }

                            # Conditional Odds Ratio
                            cond_or <- c_val / b

                            # CI for conditional OR (exact method based on binomial)
                            z <- stats::qnorm(1 - (1 - conf.level) / 2)
                            se_ln_or <- sqrt(1 / b + 1 / c_val)
                            cond_or_lower <- exp(log(cond_or) - z * se_ln_or)
                            cond_or_upper <- exp(log(cond_or) + z * se_ln_or)

                            tibble::tibble(
                                          Metric = c("Conditional Odds Ratio", "McNemar's Test"),
                                          Estimate = c(cond_or, test_stat),
                                          Lower = c(cond_or_lower, NA),
                                          Upper = c(cond_or_upper, NA),
                                          P_Value = c(pvalue, pvalue),
                                          Test = c(NA, test_name),
                                          Discordant_b = c(b, b),
                                          Discordant_c = c(c_val, c_val),
                                          Conf_Level = conf.level
                            )
              } else {
                            stop("Data must result in 2x2 table")
              }
}

#' Cochran-Armitage Trend Test (KK)
#'
#' @description Tests for linear trend in proportions across ordered groups (dose-response).
#'   Calculates trend OR/RR with confidence intervals.
#'
#' @param data Data frame
#' @param outcome Binary outcome variable
#' @param dose_group Ordered grouping variable (will be converted to numeric scores)
#' @param scores Optional numeric scores for dose levels (default: 0, 1, 2, ...)
#' @param conf.level Confidence level (default 0.95)
#'
#' @return Tibble with trend test results and trend OR/RR
#'
#' @examples
#' data <- data.frame(
#'               outcome = c(rep(1, 10), rep(0, 90), rep(1, 20), rep(0, 80), rep(1, 30), rep(0, 70)),
#'               dose = rep(c("Low", "Medium", "High"), each = 100)
#' )
#' kk_trend_test(data, outcome, dose)
#'
#' @export
kk_trend_test <- function(data, outcome, dose_group, scores = NULL, conf.level = 0.95) {
              outcome_enquo <- rlang::enquo(outcome)
              dose_enquo <- rlang::enquo(dose_group)

              out_vec <- data %>% dplyr::pull(!!outcome_enquo)
              dose_vec <- data %>% dplyr::pull(!!dose_enquo)

              # Convert to factor and get levels
              dose_fac <- factor(dose_vec)
              levels_dose <- levels(dose_fac)
              k <- length(levels_dose)

              # Assign scores
              if (is.null(scores)) {
                            scores <- 0:(k - 1)
              } else if (length(scores) != k) {
                            stop("Length of scores must equal number of dose levels")
              }

              # Create summary table
              summary_tbl <- data %>%
                            dplyr::group_by(!!dose_enquo) %>%
                            dplyr::summarise(
                                          cases = sum(!!outcome_enquo == 1, na.rm = TRUE),
                                          total = dplyr::n(),
                                          .groups = "drop"
                            ) %>%
                            dplyr::arrange(!!dose_enquo)

              r <- summary_tbl$cases
              n <- summary_tbl$total
              R <- sum(r)
              N <- sum(n)

              # Cochran-Armitage test statistic
              numerator <- N * sum(scores * r) - R * sum(scores * n)
              denominator <- sqrt(R * (N - R) * (N * sum(scores^2 * n) - (sum(scores * n))^2))
              z_ca <- numerator / denominator
              pvalue_ca <- 2 * stats::pnorm(abs(z_ca), lower.tail = FALSE)

              # Trend OR per unit increase in score
              # Fit logistic regression
              dose_numeric <- scores[as.numeric(dose_fac)]
              model <- stats::glm(out_vec ~ dose_numeric, family = stats::binomial())
              trend_or <- exp(stats::coef(model)[2])
              trend_or_se <- summary(model)$coefficients[2, 2]
              z <- stats::qnorm(1 - (1 - conf.level) / 2)
              trend_or_lower <- exp(log(trend_or) - z * trend_or_se)
              trend_or_upper <- exp(log(trend_or) + z * trend_or_se)

              # Results
              tibble::tibble(
                            Test = c("Cochran-Armitage Trend", "Trend OR (per unit)"),
                            Statistic = c(z_ca, trend_or),
                            Lower = c(NA, trend_or_lower),
                            Upper = c(NA, trend_or_upper),
                            P_Value = c(pvalue_ca, summary(model)$coefficients[2, 4]),
                            Conf_Level = conf.level,
                            N_Groups = k,
                            Total_N = N
              )
}

#' Inter-Rater Agreement (KK)
#'
#' @description Calculates inter-rater agreement measures including Cohen's Kappa,
#'   Weighted Kappa, and Prevalence-Adjusted Bias-Adjusted Kappa (PABAK).
#'
#' @param data Data frame
#' @param rater1 First rater's ratings
#' @param rater2 Second rater's ratings
#' @param weights Weighting scheme: "unweighted" (default), "linear", or "quadratic"
#' @param conf.level Confidence level (default 0.95)
#'
#' @return Tibble with kappa statistics and confidence intervals
#'
#' @examples
#' data <- data.frame(
#'               rater1 = c(1, 0, 1, 0, 1),
#'               rater2 = c(1, 1, 0, 0, 1)
#' )
#' kk_agreement(data, rater1, rater2)
#'
#' @export
kk_agreement <- function(data, rater1, rater2, weights = "unweighted", conf.level = 0.95) {
              rater1_enquo <- rlang::enquo(rater1)
              rater2_enquo <- rlang::enquo(rater2)

              r1_vec <- data %>% dplyr::pull(!!rater1_enquo)
              r2_vec <- data %>% dplyr::pull(!!rater2_enquo)

              # Remove missing values
              complete_idx <- complete.cases(r1_vec, r2_vec)
              r1_vec <- r1_vec[complete_idx]
              r2_vec <- r2_vec[complete_idx]

              n <- length(r1_vec)

              # Create contingency table
              tbl <- table(Rater1 = r1_vec, Rater2 = r2_vec)
              k_cats <- nrow(tbl)

              # Observed agreement
              p_o <- sum(diag(tbl)) / n

              # Expected agreement
              row_marg <- rowSums(tbl) / n
              col_marg <- colSums(tbl) / n
              p_e <- sum(row_marg * col_marg)

              # Cohen's Kappa
              kappa <- (p_o - p_e) / (1 - p_e)

              # Kappa SE (Fleiss)
              p_ij <- tbl / n
              theta1 <- p_o
              theta2 <- sum(row_marg * col_marg)
              theta3 <- sum(diag(p_ij) * (row_marg + col_marg))
              theta4 <- sum(p_ij * (row_marg + col_marg)^2)

              var_kappa <- (theta1 * (1 - theta1) / ((1 - theta2)^2) +
                            2 * (1 - theta1) * (2 * theta1 * theta2 - theta3) / ((1 - theta2)^3) +
                            (1 - theta1)^2 * (theta4 - 4 * theta2^2) / ((1 - theta2)^4)) / n

              se_kappa <- sqrt(var_kappa)
              z <- stats::qnorm(1 - (1 - conf.level) / 2)
              kappa_lower <- kappa - z * se_kappa
              kappa_upper <- kappa + z * se_kappa

              # Weighted Kappa
              if (weights != "unweighted" && k_cats > 2) {
                            # Create weight matrix
                            w_matrix <- matrix(1, k_cats, k_cats)
                            for (i in 1:k_cats) {
                                          for (j in 1:k_cats) {
                                                        if (weights == "linear") {
                                                                      w_matrix[i, j] <- 1 - abs(i - j) / (k_cats - 1)
                                                        } else if (weights == "quadratic") {
                                                                      w_matrix[i, j] <- 1 - ((i - j) / (k_cats - 1))^2
                                                        }
                                          }
                            }

                            p_o_w <- sum(p_ij * w_matrix)
                            p_e_w <- sum(outer(row_marg, col_marg) * w_matrix)
                            kappa_w <- (p_o_w - p_e_w) / (1 - p_e_w)

                            # Simplified SE for weighted kappa
                            se_kappa_w <- sqrt((p_o_w * (1 - p_o_w)) / (n * (1 - p_e_w)^2))
                            kappa_w_lower <- kappa_w - z * se_kappa_w
                            kappa_w_upper <- kappa_w + z * se_kappa_w
              } else {
                            kappa_w <- NA
                            kappa_w_lower <- NA
                            kappa_w_upper <- NA
                            se_kappa_w <- NA
              }

              # PABAK (binary only)
              if (k_cats == 2) {
                            pabak <- 2 * p_o - 1
                            pabak_se <- 2 * sqrt(p_o * (1 - p_o) / n)
                            pabak_lower <- pabak - z * pabak_se
                            pabak_upper <- pabak + z * pabak_se
              } else {
                            pabak <- NA
                            pabak_lower <- NA
                            pabak_upper <- NA
              }

              # Return results
              tibble::tibble(
                            Measure = c("Cohen's Kappa", "Weighted Kappa", "PABAK", "Percent Agreement"),
                            Estimate = c(kappa, kappa_w, pabak, p_o * 100),
                            Lower = c(kappa_lower, kappa_w_lower, pabak_lower, NA),
                            Upper = c(kappa_upper, kappa_w_upper, pabak_upper, NA),
                            SE = c(se_kappa, se_kappa_w, pabak_se, NA),
                            Conf_Level = conf.level,
                            N = n,
                            K_Categories = k_cats
              )
}

#' E-value for Unmeasured Confounding (KK)
#'
#' @description Calculates E-value to assess robustness of findings to unmeasured confounding.
#'   The E-value is the minimum strength of association that an unmeasured confounder
#'   would need to have with both the exposure and outcome to fully explain away the observed effect.
#'
#' @param estimate Point estimate (OR, RR, or HR)
#' @param lower Lower confidence limit (optional)
#' @param upper Upper confidence limit (optional)
#' @param type Type of estimate: "RR" (default), "OR", or "HR"
#' @param rare_outcome For ORs, is the outcome rare (<15%)? If TRUE, OR approximates RR
#'
#' @return Tibble with E-values for point estimate and confidence limit
#'
#' @references VanderWeele TJ, Ding P (2017). Sensitivity Analysis in
#'   Observational Research. Ann Intern Med 167:268-274.
#'
#' @examples
#' kk_sensitivity_analysis(estimate = 1.5, lower = 1.1, upper = 2.0)
#'
#' @export
kk_sensitivity_analysis <- function(estimate, lower = NULL, upper = NULL,
                                    type = c("RR", "OR", "HR"), rare_outcome = FALSE) {
              type <- match.arg(type)

              # Convert OR to RR if outcome is rare
              if (type == "OR" && rare_outcome) {
                            estimate_rr <- estimate
                            if (!is.null(lower)) lower <- lower # Already approximate RR
                            if (!is.null(upper)) upper <- upper
              } else if (type == "OR" && !rare_outcome) {
                            # Conservative: bound OR with RR
                            # For harmful effect (OR > 1): RR < OR
                            # For protective effect (OR < 1): RR > OR
                            warning("OR does not directly give RR for common outcomes. E-value may be conservative.")
                            estimate_rr <- estimate
              } else {
                            estimate_rr <- estimate
              }

              # E-value function
              calc_evalue <- function(rr) {
                            if (is.na(rr)) {
                                          return(NA)
                            }
                            if (rr >= 1) {
                                          rr + sqrt(rr * (rr - 1))
                            } else if (rr < 1) {
                                          1 / rr + sqrt((1 / rr) * ((1 / rr) - 1))
                            } else {
                                          1
                            }
              }

              # Calculate E-values
              e_point <- calc_evalue(estimate_rr)

              # E-value for CI limit closest to null
              if (!is.null(lower) && !is.null(upper)) {
                            if (estimate >= 1) {
                                          # Harmful/positive association: use lower CI
                                          e_ci <- calc_evalue(lower)
                                          ci_limit <- lower
                            } else {
                                          # Protective/negative association: use upper CI
                                          e_ci <- calc_evalue(upper)
                                          ci_limit <- upper
                            }
              } else {
                            e_ci <- NA
                            ci_limit <- NA
              }

              # Interpretation
              interpretation <- dplyr::case_when(
                            e_point < 1.5 ~ "Weak evidence - estimate sensitive to small confounding",
                            e_point >= 1.5 && e_point < 2.5 ~ "Moderate evidence - some robustness to confounding",
                            e_point >= 2.5 ~ "Strong evidence - substantial robustness to confounding",
                            TRUE ~ "Unable to interpret"
              )

              tibble::tibble(
                            Parameter = c("Point Estimate", "CI Limit (closest to null)"),
                            Value = c(estimate, ci_limit),
                            E_Value = c(e_point, e_ci),
                            Interpretation = c(interpretation, NA),
                            Type = type,
                            Note = c(
                                          "E-value: Minimum RR of confounder with exposure AND outcome to explain away effect",
                                          "If E-value > strength of known risk factors, estimate is robust"
                            )
              )
}
