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
epi_stats <- function(data = NULL, exposure, outcome, conf.level = 0.95) {
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
              # Convention: Exposure=Yes/No (Rows), Outcome=Yes/No (Cols)
              # We need to ensure factor levels are ordered correctly (Reference usually first, but for OR/RR usually we want Risk of Exposed vs Unexposed)
              # Standard Table:
              #           Outcome +  Outcome -
              # Exposed +    a          b
              # Exposed -    c          d

              exp_fac <- factor(exp_vec)
              out_fac <- factor(out_vec)

              # Reverse levels if needed so that the "Event" and "Exposure" are the first levels
              # This assumes default factor ordering is alphabetical or 0/1.
              # Usually we want 1 (Yes) vs 0 (No).
              # If levels are 0/1, 1 is usually second. We want 1 to be the "Event".
              # Let's try to detect if 0/1 or Yes/No

              tbl <- table(Exposure = exp_fac, Outcome = out_fac)

              # Use epitools if available, otherwise manual calculation
              if (requireNamespace("epitools", quietly = TRUE)) {
                            # epitools expects:
                            #           Outcome -  Outcome +
                            # Exposed -    d          c
                            # Exposed +    b          a
                            # So we might need to reorder or just use their functions carefully.
                            # epitools::riskratio and oddsratio use the "rows are exposure, cols are outcome" format but with specific ordering.

                            # Let's do manual calculation to be safe and dependency-free (except base)
              }

              # Manual Calculation
              # Assume the "positive" level is the second level if 0/1, or we ask user?
              # Let's assume standard R factor order: levels are sorted.
              # If 0/1: 0 is ref, 1 is event.
              # We want Risk(1) / Risk(0).

              # Re-orient table to:
              #           Event (1)   No Event (0)
              # Exposed (1)   a           b
              # Unexposed (0) c           d

              # Default table gives:
              #           0    1
              #      0    d    c
              #      1    b    a

              # So:
              a <- tbl[2, 2] # Exp=1, Out=1
              b <- tbl[2, 1] # Exp=1, Out=0
              c <- tbl[1, 2] # Exp=0, Out=1
              d <- tbl[1, 1] # Exp=0, Out=0

              # Odds Ratio = (a/b) / (c/d) = ad / bc
              or_est <- (a * d) / (b * c)

              # SE(ln(OR)) = sqrt(1/a + 1/b + 1/c + 1/d)
              se_ln_or <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
              z <- stats::qnorm(1 - (1 - conf.level) / 2)

              or_low <- exp(log(or_est) - z * se_ln_or)
              or_high <- exp(log(or_est) + z * se_ln_or)

              # Relative Risk = (a / (a+b)) / (c / (c+d))
              n1 <- a + b
              n0 <- c + d
              p1 <- a / n1
              p0 <- c / n0

              rr_est <- p1 / p0

              # SE(ln(RR)) = sqrt( (1/a - 1/n1) + (1/c - 1/n0) )
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
