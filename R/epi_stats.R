# ============================================================
# EPIDEMIOLOGY STATISTICS
# ============================================================

#' Calculate Odds Ratio and Relative Risk
#'
#' @description Computes Odds Ratio (OR) and Relative Risk (RR) from a 2x2 contingency table
#'   or from two binary vectors.
#'
#' @param data Optional data frame. If provided, `exposure` and `outcome` should be column names.
#' @param exposure Exposure variable (vector or column name). Must be binary.
#' @param outcome Outcome variable (vector or column name). Must be binary.
#' @param conf.level Confidence level (default: 0.95)
#'
#' @return Tibble with OR, RR, and their confidence intervals
#'
#' @export
kk_epi_stats <- function(data = NULL, exposure, outcome, conf.level = 0.95) {
              if (!is.null(data)) {
                            validate_data_frame(data)
                            exposure_col <- rlang::as_string(rlang::ensym(exposure))
                            outcome_col <- rlang::as_string(rlang::ensym(outcome))

                            if (!all(c(exposure_col, outcome_col) %in% names(data))) {
                                          stop("Exposure or outcome columns not found in data")
                            }

                            exp_vec <- data[[exposure_col]]
                            out_vec <- data[[outcome_col]]
              } else {
                            exp_vec <- exposure
                            out_vec <- outcome
              }

              # Ensure binary
              if (length(unique(stats::na.omit(exp_vec))) > 2 || length(unique(stats::na.omit(out_vec))) > 2) {
                            stop("Exposure and outcome must be binary variables")
              }

              # Create 2x2 table
              exp_fac <- factor(exp_vec)
              out_fac <- factor(out_vec)

              tbl <- table(Exposure = exp_fac, Outcome = out_fac)

              # Manual Calculation
              # Default table gives:
              #           0    1
              #      0    d    c
              #      1    b    a

              a <- tbl[2, 2] # Exp=1, Out=1
              b <- tbl[2, 1] # Exp=1, Out=0
              c <- tbl[1, 2] # Exp=0, Out=1
              d <- tbl[1, 1] # Exp=0, Out=0

              # Odds Ratio
              or_est <- (a * d) / (b * c)

              # SE(ln(OR))
              se_ln_or <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
              z <- stats::qnorm(1 - (1 - conf.level) / 2)

              or_low <- exp(log(or_est) - z * se_ln_or)
              or_high <- exp(log(or_est) + z * se_ln_or)

              # Relative Risk
              n1 <- a + b
              n0 <- c + d
              p1 <- a / n1
              p0 <- c / n0

              rr_est <- p1 / p0

              # SE(ln(RR))
              se_ln_rr <- sqrt((1 / a - 1 / n1) + (1 / c - 1 / n0))

              rr_low <- exp(log(rr_est) - z * se_ln_rr)
              rr_high <- exp(log(rr_est) + z * se_ln_rr)

              tibble::tibble(
                            Metric = c("Odds Ratio", "Relative Risk"),
                            Estimate = c(or_est, rr_est),
                            Lower = c(or_low, rr_low),
                            Upper = c(or_high, rr_high),
                            Conf_Level = conf.level,
                            Exposed_Event = a,
                            Exposed_Total = n1,
                            Unexposed_Event = c,
                            Unexposed_Total = n0
              )
}

#' Epidemiology 2x2 Analysis (KK)
#'
#' @description Comprehensive 2x2 table analysis including OR, RR, and Risk Difference.
#'
#' @param data Data frame or 2x2 matrix/table
#' @param exposure Exposure column (if data is data frame)
#' @param outcome Outcome column (if data is data frame)
#' @param conf.level Confidence level (default 0.95)
#' @param method Method for CIs ("wald", "small.sample", "bootstrap") - currently "wald" or "small.sample" (Fisher/Exact)
#'
#' @return Tibble with estimates
#' @export
kk_twobytwo <- function(data, exposure = NULL, outcome = NULL, conf.level = 0.95, method = "wald") {
              # Handle input types
              exposure_enquo <- rlang::enquo(exposure)
              outcome_enquo <- rlang::enquo(outcome)

              if (is.data.frame(data) && !rlang::quo_is_null(exposure_enquo) && !rlang::quo_is_null(outcome_enquo)) {
                            # Tidy evaluation

                            # Extract vectors
                            exp_vec <- data %>% dplyr::pull(!!exposure_enquo)
                            out_vec <- data %>% dplyr::pull(!!outcome_enquo)

                            # Create table
                            exp_fac <- factor(exp_vec)
                            out_fac <- factor(out_vec)

                            tbl <- table(Exposure = exp_fac, Outcome = out_fac)
              } else if (is.matrix(data) || is.table(data)) {
                            if (all(dim(data) == c(2, 2))) {
                                          tbl <- data
                            } else {
                                          stop("Input matrix/table must be 2x2")
                            }
              } else {
                            stop("Invalid input. Must be data frame with exposure/outcome or 2x2 table.")
              }

              # Check dimensions
              if (nrow(tbl) != 2 || ncol(tbl) != 2) stop("Table must be 2x2")

              a <- tbl[2, 2] # Exp+, Out+
              b <- tbl[2, 1] # Exp+, Out-
              c <- tbl[1, 2] # Exp-, Out+
              d <- tbl[1, 1] # Exp-, Out-

              n1 <- a + b # Total Exposed
              n0 <- c + d # Total Unexposed

              # Estimates
              p1 <- a / n1 # Risk in Exposed
              p0 <- c / n0 # Risk in Unexposed

              # Risk Ratio (RR)
              rr <- p1 / p0

              # Odds Ratio (OR)
              or <- (a * d) / (b * c)

              # Risk Difference (RD)
              rd <- p1 - p0

              # Confidence Intervals
              z <- stats::qnorm(1 - (1 - conf.level) / 2)

              # RR CI (Wald)
              se_ln_rr <- sqrt((1 / a - 1 / n1) + (1 / c - 1 / n0))
              rr_low <- exp(log(rr) - z * se_ln_rr)
              rr_high <- exp(log(rr) + z * se_ln_rr)

              # OR CI (Wald)
              se_ln_or <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
              or_low <- exp(log(or) - z * se_ln_or)
              or_high <- exp(log(or) + z * se_ln_or)

              # RD CI (Wald)
              se_rd <- sqrt((p1 * (1 - p1) / n1) + (p0 * (1 - p0) / n0))
              rd_low <- rd - z * se_rd
              rd_high <- rd + z * se_rd

              # Attributable Fraction among Exposed (AF_e) / Attributable Risk Percent (AR%)
              # Formula: (RR - 1) / RR
              af_e <- (rr - 1) / rr
              # CI for AF_e: 1 - 1/RR_low to 1 - 1/RR_high (if RR > 1)
              af_e_low <- (rr_low - 1) / rr_low
              af_e_high <- (rr_high - 1) / rr_high

              # Population Attributable Fraction (PAF)
              # Formula: (Risk_total - Risk_unexposed) / Risk_total
              # Risk_total = (a + c) / (n1 + n0)
              p_total <- (a + c) / (n1 + n0)
              paf <- (p_total - p0) / p_total
              # CI for PAF is complex, leaving as NA for now

              # Preventable Fraction among Exposed (PF_e)
              # Formula: 1 - RR (useful when RR < 1)
              pf_e <- 1 - rr
              pf_e_low <- 1 - rr_high
              pf_e_high <- 1 - rr_low

              # Fisher's Exact Test for p-value
              fisher <- stats::fisher.test(tbl)
              chisq <- stats::chisq.test(tbl, correct = FALSE)

              # Compile results
              res <- tibble::tibble(
                            Metric = c(
                                          "Odds Ratio", "Relative Risk", "Risk Difference",
                                          "Attributable Fraction (Exposed)", "Population Attributable Fraction", "Preventable Fraction (Exposed)"
                            ),
                            Estimate = c(or, rr, rd, af_e, paf, pf_e),
                            Lower = c(or_low, rr_low, rd_low, af_e_low, NA, pf_e_low),
                            Upper = c(or_high, rr_high, rd_high, af_e_high, NA, pf_e_high),
                            P_Value = c(fisher$p.value, chisq$p.value, chisq$p.value, NA, NA, NA),
                            Test = c("Fisher's Exact", "Chi-Square", "Chi-Square", NA, NA, NA),
                            Conf_Level = conf.level
              )

              return(res)
}
#' Diagnostic Test Accuracy (KK)
#'
#' @description Calculates sensitivity, specificity, PPV, NPV, and Accuracy.
#'
#' @param data Data frame
#' @param truth Column with true status (binary, 0/1 or No/Yes)
#' @param prediction Column with predicted status (binary) or probabilities (numeric)
#' @param cutoff Cutoff for numeric predictions (default 0.5)
#' @param positive Value indicating positive case in truth (optional, auto-detected)
#'
#' @return Tibble with metrics
#' @export
kk_diagnostic <- function(data, truth, prediction, cutoff = 0.5, positive = NULL) {
              truth_enquo <- rlang::enquo(truth)
              pred_enquo <- rlang::enquo(prediction)

              truth_vec <- data %>% dplyr::pull(!!truth_enquo)
              pred_vec <- data %>% dplyr::pull(!!pred_enquo)

              # Handle numeric predictions
              if (is.numeric(pred_vec) && length(unique(pred_vec)) > 2) {
                            pred_class <- ifelse(pred_vec >= cutoff, 1, 0)
                            auc_val <- NA
                            if (requireNamespace("pROC", quietly = TRUE)) {
                                          roc_obj <- try(pROC::roc(truth_vec, pred_vec, quiet = TRUE), silent = TRUE)
                                          if (!inherits(roc_obj, "try-error")) {
                                                        auc_val <- as.numeric(pROC::auc(roc_obj))
                                          }
                            }
              } else {
                            pred_class <- pred_vec
                            auc_val <- NA
              }

              # Simple calculation if we can identify TP, TN, FP, FN
              # Let's assume the "positive" class is the second level or '1' or 'Yes'

              levels_truth <- levels(factor(truth_vec))
              if (is.null(positive)) positive <- levels_truth[length(levels_truth)] # Assume last is positive

              tp <- sum(pred_class == positive & truth_vec == positive)
              tn <- sum(pred_class != positive & truth_vec != positive)
              fp <- sum(pred_class == positive & truth_vec != positive)
              fn <- sum(pred_class != positive & truth_vec == positive)

              sens <- tp / (tp + fn)
              spec <- tn / (tn + fp)
              ppv <- tp / (tp + fp)
              npv <- tn / (tn + fn)
              acc <- (tp + tn) / (tp + tn + fp + fn)
              f1 <- 2 * (ppv * sens) / (ppv + sens)

              tibble::tibble(
                            Metric = c("Sensitivity", "Specificity", "PPV", "NPV", "Accuracy", "F1 Score", "AUC"),
                            Value = c(sens, spec, ppv, npv, acc, f1, auc_val)
              )
}

#' Risk Plot (KK)
#'
#' @description Forest plot for Odds Ratios or Relative Risks.
#'
#' @param data Data frame containing Metric, Estimate, Lower, Upper (output of kk_epi_2x2)
#' @param title Plot title
#'
#' @return ggplot object
#' @export
kk_risk_plot <- function(data, title = "Risk Estimates") {
              if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")

              # Check columns
              req_cols <- c("Metric", "Estimate", "Lower", "Upper")
              if (!all(req_cols %in% names(data))) stop("Data must contain Metric, Estimate, Lower, Upper")

              kkplot(data, ggplot2::aes(x = .data$Estimate, y = .data$Metric, xmin = .data$Lower, xmax = .data$Upper)) +
                            ggplot2::geom_point(size = 3) +
                            ggplot2::geom_errorbar(width = 0.2) +
                            ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
                            ggplot2::labs(title = title, x = "Estimate (95% CI)", y = "") +
                            ggplot2::theme_minimal()
}


#' Calculate Risk Ratio (Relative Risk)
#'
#' @description Calculates Relative Risk (RR) with confidence intervals.
#'
#' @param data Data frame or 2x2 table
#' @param exposure Exposure variable
#' @param outcome Outcome variable
#' @param conf.level Confidence level (default: 0.95)
#'
#' @return Tibble with RR estimate and CI
#' @export
risk_ratio <- function(data, exposure = NULL, outcome = NULL, conf.level = 0.95) {
              res <- kk_twobytwo(data, exposure, outcome, conf.level)
              res %>%
                            dplyr::filter(.data$Metric == "Relative Risk") %>%
                            dplyr::select("Metric", "Estimate", "Lower", "Upper", "P_Value", "Conf_Level")
}

#' Calculate Odds Ratio
#'
#' @description Calculates Odds Ratio (OR) with confidence intervals.
#'
#' @param data Data frame or 2x2 table
#' @param exposure Exposure variable
#' @param outcome Outcome variable
#' @param conf.level Confidence level (default: 0.95)
#'
#' @return Tibble with OR estimate and CI
#' @export
odds_ratio <- function(data, exposure = NULL, outcome = NULL, conf.level = 0.95) {
              res <- kk_twobytwo(data, exposure, outcome, conf.level)
              res %>%
                            dplyr::filter(.data$Metric == "Odds Ratio") %>%
                            dplyr::select("Metric", "Estimate", "Lower", "Upper", "P_Value", "Conf_Level")
}
