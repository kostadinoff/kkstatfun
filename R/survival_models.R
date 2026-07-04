# ============================================================
# SURVIVAL MODELS & RATES
# ============================================================

# Internal: resolve a column argument that may be supplied as a bare
# symbol or as a character string (mirrors the flexible style of kk_reg).
.kk_colname <- function(quo) {
              expr <- rlang::quo_get_expr(quo)
              if (is.character(expr)) {
                            return(expr)
              }
              rlang::as_name(rlang::quo_get_expr(quo))
}

#' Cox Proportional Hazards Model (KK)
#'
#' @description Fits univariate and multivariable Cox proportional hazards models
#'   and returns a tidy hazard-ratio table together with the Schoenfeld residual
#'   test of the proportional-hazards assumption (`survival::cox.zph`).
#'
#' @param data Data frame.
#' @param time Follow-up time column (bare name or string).
#' @param status Event indicator column (bare name or string); 0/1, 1/2, logical,
#'   or a `Surv`-compatible coding.
#' @param predictors Character vector of predictor column names.
#' @param conf.level Confidence level (default 0.95).
#' @param ... Additional arguments passed to `survival::coxph`.
#'
#' @return Tibble with one row per model term containing the hazard ratio,
#'   confidence interval, p-value, model type (univariate / multivariable),
#'   the per-term Schoenfeld p-value (`ph_p`), and the global PH-test p-value
#'   (`ph_global_p`).
#'
#' @examples
#' \dontrun{
#' library(survival)
#' kk_coxph(lung, time, status, predictors = c("age", "sex", "ph.ecog"))
#' }
#'
#' @export
kk_coxph <- function(data, time, status, predictors, conf.level = 0.95, ...) {
              validate_data_frame(data)
              if (!requireNamespace("survival", quietly = TRUE)) {
                            stop("Package 'survival' is required for kk_coxph().")
              }
              if (!is.character(predictors) || length(predictors) < 1) {
                            stop("`predictors` must be a non-empty character vector.")
              }

              time_name <- .kk_colname(rlang::enquo(time))
              status_name <- .kk_colname(rlang::enquo(status))

              missing_cols <- setdiff(c(time_name, status_name, predictors), names(data))
              if (length(missing_cols) > 0) {
                            stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
              }

              lhs <- sprintf("survival::Surv(%s, %s)", time_name, status_name)
              z_mult <- conf.level

              fit_one <- function(rhs, model_type) {
                            fml <- stats::as.formula(paste(lhs, "~", rhs))
                            model <- survival::coxph(fml, data = data, ...)

                            res <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE, conf.level = z_mult)

                            # Schoenfeld residual PH test
                            ph_p_term <- rep(NA_real_, nrow(res))
                            ph_global <- NA_real_
                            zph <- tryCatch(survival::cox.zph(model), error = function(e) NULL)
                            if (!is.null(zph)) {
                                          tab <- zph$table
                                          ph_global <- tab["GLOBAL", "p"]
                                          ph_p_term <- tab[match(res$term, rownames(tab)), "p"]
                            }

                            res %>%
                                          dplyr::transmute(
                                                        term = .data$term,
                                                        model_type = model_type,
                                                        hazard_ratio = .data$estimate,
                                                        conf.low = .data$conf.low,
                                                        conf.high = .data$conf.high,
                                                        std.error = .data$std.error,
                                                        statistic = .data$statistic,
                                                        p.value = .data$p.value,
                                                        ph_p = ph_p_term,
                                                        ph_global_p = ph_global,
                                                        n = model$n,
                                                        n_events = model$nevent,
                                                        concordance = unname(summary(model)$concordance[1]),
                                                        AIC = stats::AIC(model),
                                                        conf.level = z_mult
                                          )
              }

              univariate <- purrr::map_dfr(predictors, function(p) fit_one(p, "univariate"))
              multivariable <- fit_one(paste(predictors, collapse = " + "), "multivariable")

              dplyr::bind_rows(univariate, multivariable)
}

#' Log-Rank and Related Survival Tests (KK)
#'
#' @description Compares survival distributions across groups using the family of
#'   `survival::survdiff` tests. `rho = 0` gives the standard log-rank test;
#'   `rho = 1` gives the Peto & Peto modification of the Gehan-Wilcoxon test
#'   (more weight on early events).
#'
#' @param data Data frame.
#' @param time Follow-up time column (bare name or string).
#' @param status Event indicator column (bare name or string).
#' @param group Grouping column (bare name or string).
#' @param rho Weighting parameter (default 0 = log-rank).
#'
#' @return Tibble with one row per group (N, observed, expected, O/E ratio) and
#'   the overall test statistic, degrees of freedom, and p-value.
#'
#' @examples
#' \dontrun{
#' library(survival)
#' kk_logrank(lung, time, status, sex)
#' }
#'
#' @export
kk_logrank <- function(data, time, status, group, rho = 0) {
              validate_data_frame(data)
              if (!requireNamespace("survival", quietly = TRUE)) {
                            stop("Package 'survival' is required for kk_logrank().")
              }

              time_name <- .kk_colname(rlang::enquo(time))
              status_name <- .kk_colname(rlang::enquo(status))
              group_name <- .kk_colname(rlang::enquo(group))

              missing_cols <- setdiff(c(time_name, status_name, group_name), names(data))
              if (length(missing_cols) > 0) {
                            stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
              }

              fml <- stats::as.formula(sprintf(
                            "survival::Surv(%s, %s) ~ %s", time_name, status_name, group_name
              ))
              sd <- survival::survdiff(fml, data = data, rho = rho)

              df <- length(sd$n) - 1
              chisq <- sd$chisq
              p_value <- stats::pchisq(chisq, df = df, lower.tail = FALSE)
              method <- if (rho == 0) "Log-rank" else sprintf("Peto-Peto (rho = %s)", rho)

              group_levels <- sub(paste0("^", group_name, "="), "", names(sd$n))

              tibble::tibble(
                            group = group_levels,
                            n = as.numeric(sd$n),
                            observed = as.numeric(sd$obs),
                            expected = as.numeric(sd$exp),
                            oe_ratio = as.numeric(sd$obs) / as.numeric(sd$exp),
                            chisq = chisq,
                            df = df,
                            p_value = p_value,
                            method = method
              )
}

#' Incidence Rate and Rate Ratios with Exact CIs (KK)
#'
#' @description Computes incidence rates per unit of person-time with exact
#'   (Poisson) confidence intervals. When a stratifying variable is supplied,
#'   rate ratios versus the reference (first) level are also returned.
#'
#' @param data Data frame (one or more rows; counts and person-time are summed
#'   within each stratum).
#' @param cases Column of event counts (bare name or string).
#' @param person_time Column of person-time at risk (bare name or string).
#' @param by Optional stratifying column (bare name or string).
#' @param conf.level Confidence level (default 0.95).
#' @param multiplier Rate multiplier, e.g. 1000 for per-1000 person-years
#'   (default 1000).
#'
#' @return Tibble with cases, person-time, incidence rate and exact CI per
#'   stratum. If `by` is supplied, rate ratios (with CI and p-value) versus the
#'   reference level are appended.
#'
#' @examples
#' df <- data.frame(
#'   arm = c("exposed", "unexposed"),
#'   cases = c(40, 15),
#'   pyears = c(1000, 1200)
#' )
#' kk_incidence_rate(df, cases, pyears, by = arm)
#'
#' @export
kk_incidence_rate <- function(data, cases, person_time, by = NULL,
                              conf.level = 0.95, multiplier = 1000) {
              validate_data_frame(data)

              cases_name <- .kk_colname(rlang::enquo(cases))
              pt_name <- .kk_colname(rlang::enquo(person_time))
              by_quo <- rlang::enquo(by)
              has_by <- !rlang::quo_is_null(by_quo)
              by_name <- if (has_by) .kk_colname(by_quo) else NULL

              need <- c(cases_name, pt_name, by_name)
              missing_cols <- setdiff(need, names(data))
              if (length(missing_cols) > 0) {
                            stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
              }

              alpha <- 1 - conf.level

              if (has_by) {
                            agg <- data %>%
                                          dplyr::group_by(.data[[by_name]]) %>%
                                          dplyr::summarise(
                                                        cases = sum(.data[[cases_name]], na.rm = TRUE),
                                                        person_time = sum(.data[[pt_name]], na.rm = TRUE),
                                                        .groups = "drop"
                                          ) %>%
                                          dplyr::rename(stratum = 1)
              } else {
                            agg <- tibble::tibble(
                                          stratum = "overall",
                                          cases = sum(data[[cases_name]], na.rm = TRUE),
                                          person_time = sum(data[[pt_name]], na.rm = TRUE)
                            )
              }

              agg <- agg %>%
                            dplyr::mutate(
                                          rate = .data$cases / .data$person_time * multiplier,
                                          rate_low = stats::qgamma(alpha / 2, .data$cases) /
                                                        .data$person_time * multiplier,
                                          rate_high = stats::qgamma(1 - alpha / 2, .data$cases + 1) /
                                                        .data$person_time * multiplier,
                                          multiplier = multiplier,
                                          conf.level = conf.level
                            )

              if (has_by && nrow(agg) >= 2) {
                            ref_cases <- agg$cases[1]
                            ref_pt <- agg$person_time[1]
                            z <- stats::qnorm(1 - alpha / 2)
                            rr <- (agg$cases / agg$person_time) / (ref_cases / ref_pt)
                            se_ln <- sqrt(1 / agg$cases + 1 / ref_cases)
                            agg <- agg %>%
                                          dplyr::mutate(
                                                        rate_ratio = rr,
                                                        rr_low = exp(log(rr) - z * se_ln),
                                                        rr_high = exp(log(rr) + z * se_ln),
                                                        rr_p = 2 * stats::pnorm(-abs(log(rr) / se_ln)),
                                                        reference = agg$stratum[1]
                                          )
                            agg$rate_ratio[1] <- 1
                            agg$rr_low[1] <- NA_real_
                            agg$rr_high[1] <- NA_real_
                            agg$rr_p[1] <- NA_real_
              }

              agg
}
