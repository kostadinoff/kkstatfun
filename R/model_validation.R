# ============================================================
# MODEL VALIDATION & CAUSAL WEIGHTING
# ============================================================

#' Calibration of a Risk Model (KK)
#'
#' @description Assesses how well predicted probabilities from a binary risk
#'   model agree with observed event rates. Returns the Brier score, the
#'   Hosmer-Lemeshow goodness-of-fit test, and a grouped calibration table
#'   (mean predicted vs. observed risk within equal-count risk deciles) suitable
#'   for a calibration plot. This complements discrimination measures such as
#'   `kk_roc`.
#'
#' @param data Data frame.
#' @param truth Binary outcome column (bare name or string); coerced to 0/1.
#' @param predicted Predicted-probability column (bare name or string), on the
#'   0-1 scale.
#' @param groups Number of risk groups for the calibration table and
#'   Hosmer-Lemeshow test (default 10).
#'
#' @return Tibble (the per-group calibration table) with attributes `brier` and
#'   `hosmer_lemeshow` (a one-row tibble with the chi-square statistic, degrees
#'   of freedom, and p-value).
#'
#' @examples
#' \dontrun{
#' df <- data.frame(y = rbinom(500, 1, 0.3))
#' df$p <- plogis(qlogis(0.3) + 0.5 * scale(df$y) + rnorm(500))
#' kk_calibration(df, y, p)
#' }
#'
#' @export
kk_calibration <- function(data, truth, predicted, groups = 10) {
              validate_data_frame(data)

              truth_name <- .kk_colname(rlang::enquo(truth))
              pred_name <- .kk_colname(rlang::enquo(predicted))
              missing_cols <- setdiff(c(truth_name, pred_name), names(data))
              if (length(missing_cols) > 0) {
                            stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
              }

              y_raw <- data[[truth_name]]
              y <- if (is.factor(y_raw)) as.integer(y_raw) - 1L else as.numeric(y_raw)
              p <- data[[pred_name]]
              ok <- stats::complete.cases(y, p)
              y <- y[ok]; p <- p[ok]
              if (!all(y %in% c(0, 1))) stop("`truth` must be binary (0/1).")
              if (any(p < 0 | p > 1)) stop("`predicted` must be probabilities in [0, 1].")

              # Brier score
              brier <- mean((p - y)^2)

              # Risk groups by quantiles of predicted probability
              qs <- stats::quantile(p, probs = seq(0, 1, length.out = groups + 1),
                                    na.rm = TRUE, type = 7)
              qs[1] <- -Inf; qs[length(qs)] <- Inf
              grp <- cut(p, breaks = unique(qs), include.lowest = TRUE, labels = FALSE)

              cal <- tibble::tibble(grp = grp, y = y, p = p) %>%
                            dplyr::group_by(.data$grp) %>%
                            dplyr::summarise(
                                          n = dplyr::n(),
                                          observed_events = sum(.data$y),
                                          observed_rate = mean(.data$y),
                                          predicted_rate = mean(.data$p),
                                          .groups = "drop"
                            )

              # Hosmer-Lemeshow statistic
              o1 <- cal$observed_events
              e1 <- cal$predicted_rate * cal$n
              o0 <- cal$n - o1
              e0 <- (1 - cal$predicted_rate) * cal$n
              hl_stat <- sum((o1 - e1)^2 / e1 + (o0 - e0)^2 / e0, na.rm = TRUE)
              hl_df <- nrow(cal) - 2
              hl_p <- stats::pchisq(hl_stat, df = hl_df, lower.tail = FALSE)

              attr(cal, "brier") <- brier
              attr(cal, "hosmer_lemeshow") <- tibble::tibble(
                            statistic = hl_stat, df = hl_df, p.value = hl_p
              )
              cal
}

#' Inverse-Probability-of-Treatment Weighting (KK)
#'
#' @description Estimates a propensity score from the supplied covariates,
#'   constructs inverse-probability-of-treatment weights (ATE or ATT, optionally
#'   stabilized), and returns the weighted treatment effect on a binary outcome.
#'   Pair with `kk_smd` to check covariate balance before and after weighting.
#'
#' @param data Data frame.
#' @param treatment Binary treatment column (bare name or string); coerced to 0/1.
#' @param outcome Binary outcome column (bare name or string); coerced to 0/1.
#' @param covariates Character vector of covariate names for the propensity model.
#' @param estimand Target estimand: "ATE" (default) or "ATT".
#' @param stabilize Use stabilized weights (default TRUE).
#' @param conf.level Confidence level (default 0.95).
#'
#' @return A tibble with the weighted risk difference and risk ratio (with robust
#'   confidence intervals), carrying the fitted weights and propensity scores as
#'   the attributes `weights` and `propensity`.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   trt = rbinom(500, 1, 0.5),
#'   age = rnorm(500, 50, 10),
#'   sex = rbinom(500, 1, 0.5)
#' )
#' df$out <- rbinom(500, 1, plogis(-1 + 0.5 * df$trt + 0.02 * df$age))
#' kk_iptw(df, trt, out, covariates = c("age", "sex"))
#' }
#'
#' @export
kk_iptw <- function(data, treatment, outcome, covariates,
                    estimand = c("ATE", "ATT"), stabilize = TRUE,
                    conf.level = 0.95) {
              validate_data_frame(data)
              estimand <- match.arg(estimand)
              if (!requireNamespace("sandwich", quietly = TRUE)) {
                            stop("Package 'sandwich' is required for kk_iptw().")
              }
              if (!is.character(covariates) || length(covariates) < 1) {
                            stop("`covariates` must be a non-empty character vector.")
              }

              treat_name <- .kk_colname(rlang::enquo(treatment))
              out_name <- .kk_colname(rlang::enquo(outcome))
              missing_cols <- setdiff(c(treat_name, out_name, covariates), names(data))
              if (length(missing_cols) > 0) {
                            stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
              }

              to01 <- function(v) {
                            if (is.factor(v)) as.integer(v) - 1L
                            else if (is.logical(v)) as.integer(v)
                            else as.numeric(v)
              }
              a <- to01(data[[treat_name]])
              y <- to01(data[[out_name]])
              if (!all(stats::na.omit(a) %in% c(0, 1))) stop("`treatment` must be binary.")
              if (!all(stats::na.omit(y) %in% c(0, 1))) stop("`outcome` must be binary.")

              # Propensity score
              ps_fml <- stats::as.formula(paste(treat_name, "~",
                                                paste(covariates, collapse = " + ")))
              ps_data <- data
              ps_data[[treat_name]] <- a
              ps_model <- stats::glm(ps_fml, data = ps_data, family = stats::binomial())
              ps <- stats::fitted(ps_model)

              # Weights
              if (estimand == "ATE") {
                            w <- ifelse(a == 1, 1 / ps, 1 / (1 - ps))
                            if (stabilize) {
                                          pa <- mean(a)
                                          w <- ifelse(a == 1, pa / ps, (1 - pa) / (1 - ps))
                            }
              } else { # ATT
                            w <- ifelse(a == 1, 1, ps / (1 - ps))
              }

              # Weighted outcome model for risk difference (identity link) and
              # risk ratio (log link), with robust SEs.
              df_w <- data.frame(y = y, a = a, w = w)
              z <- stats::qnorm(1 - (1 - conf.level) / 2)

              fit_rd <- stats::glm(y ~ a, data = df_w, weights = w,
                                   family = stats::gaussian())
              rd <- stats::coef(fit_rd)[["a"]]
              rd_se <- sqrt(sandwich::vcovHC(fit_rd, type = "HC0")["a", "a"])

              fit_rr <- suppressWarnings(stats::glm(y ~ a, data = df_w, weights = w,
                                                    family = stats::poisson(link = "log")))
              lrr <- stats::coef(fit_rr)[["a"]]
              lrr_se <- sqrt(sandwich::vcovHC(fit_rr, type = "HC0")["a", "a"])

              res <- tibble::tibble(
                            estimand = estimand,
                            metric = c("Risk difference", "Risk ratio"),
                            estimate = c(rd, exp(lrr)),
                            conf.low = c(rd - z * rd_se, exp(lrr - z * lrr_se)),
                            conf.high = c(rd + z * rd_se, exp(lrr + z * lrr_se)),
                            p.value = c(2 * stats::pnorm(-abs(rd / rd_se)),
                                        2 * stats::pnorm(-abs(lrr / lrr_se))),
                            conf.level = conf.level
              )
              attr(res, "weights") <- w
              attr(res, "propensity") <- ps
              res
}
