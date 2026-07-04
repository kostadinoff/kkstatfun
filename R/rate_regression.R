# ============================================================
# RATE & RISK-RATIO REGRESSION
# ============================================================

#' Poisson / Negative-Binomial Rate Regression (KK)
#'
#' @description Fits univariate and multivariable Poisson regression models for
#'   count or rate outcomes and returns incidence-rate ratios (IRR). Overdispersion
#'   is assessed via the Pearson dispersion statistic; when it is substantial and
#'   `MASS` is available, a negative-binomial model is fitted instead so the
#'   standard errors are not understated.
#'
#' @param data Data frame.
#' @param outcome Count outcome column (bare name or string).
#' @param predictors Character vector of predictor column names.
#' @param person_time Optional column of person-time; when supplied it enters the
#'   model as `offset(log(person_time))`, turning counts into rates.
#' @param conf.level Confidence level (default 0.95).
#' @param dispersion_threshold Pearson dispersion above which a negative-binomial
#'   model is preferred (default 1.5).
#' @param ... Additional arguments passed to the fitting function.
#'
#' @return Tibble with one row per term: the IRR, confidence interval, p-value,
#'   model type (univariate / multivariable), the fitted family, and the Pearson
#'   dispersion statistic.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   cases = rpois(100, 3),
#'   pyears = runif(100, 50, 150),
#'   age = rnorm(100, 50, 10),
#'   sex = rbinom(100, 1, 0.5)
#' )
#' kk_rate_reg(df, cases, c("age", "sex"), person_time = pyears)
#' }
#'
#' @export
kk_rate_reg <- function(data, outcome, predictors, person_time = NULL,
                        conf.level = 0.95, dispersion_threshold = 1.5, ...) {
              validate_data_frame(data)
              if (!is.character(predictors) || length(predictors) < 1) {
                            stop("`predictors` must be a non-empty character vector.")
              }

              outcome_name <- .kk_colname(rlang::enquo(outcome))
              pt_quo <- rlang::enquo(person_time)
              has_offset <- !rlang::quo_is_null(pt_quo)
              pt_name <- if (has_offset) .kk_colname(pt_quo) else NULL

              missing_cols <- setdiff(c(outcome_name, predictors, pt_name), names(data))
              if (length(missing_cols) > 0) {
                            stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
              }

              offset_term <- if (has_offset) sprintf(" + offset(log(%s))", pt_name) else ""

              fit_one <- function(rhs, model_type) {
                            fml <- stats::as.formula(paste0(outcome_name, " ~ ", rhs, offset_term))
                            pois <- stats::glm(fml, data = data, family = stats::poisson(), ...)

                            # Pearson dispersion
                            pr <- stats::residuals(pois, type = "pearson")
                            dispersion <- sum(pr^2) / stats::df.residual(pois)

                            use_nb <- dispersion > dispersion_threshold &&
                                          requireNamespace("MASS", quietly = TRUE)
                            if (use_nb) {
                                          model <- tryCatch(
                                                        MASS::glm.nb(fml, data = data, ...),
                                                        error = function(e) pois
                                          )
                                          family_used <- if (inherits(model, "negbin")) "negbin" else "poisson"
                            } else {
                                          model <- pois
                                          family_used <- "poisson"
                            }

                            broom::tidy(model, exponentiate = TRUE, conf.int = TRUE,
                                        conf.level = conf.level) %>%
                                          dplyr::transmute(
                                                        term = .data$term,
                                                        model_type = model_type,
                                                        IRR = .data$estimate,
                                                        conf.low = .data$conf.low,
                                                        conf.high = .data$conf.high,
                                                        std.error = .data$std.error,
                                                        statistic = .data$statistic,
                                                        p.value = .data$p.value,
                                                        family = family_used,
                                                        dispersion = dispersion,
                                                        AIC = stats::AIC(model),
                                                        conf.level = conf.level
                                          )
              }

              univariate <- purrr::map_dfr(predictors, function(p) fit_one(p, "univariate"))
              multivariable <- fit_one(paste(predictors, collapse = " + "), "multivariable")

              dplyr::bind_rows(univariate, multivariable) %>%
                            dplyr::filter(.data$term != "(Intercept)")
}

#' @rdname kk_rate_reg
#' @export
kk_poisson <- kk_rate_reg

#' Adjusted Risk-Ratio Regression (Modified Poisson) (KK)
#'
#' @description Estimates adjusted risk ratios for a binary outcome using the
#'   modified Poisson approach of Zou (2004): a log-link Poisson model with robust
#'   (sandwich) standard errors. This avoids the convergence problems of
#'   log-binomial regression while yielding valid risk ratios rather than odds
#'   ratios, which are preferable for common outcomes in cohort studies.
#'
#' @param data Data frame.
#' @param outcome Binary outcome column (bare name or string); coerced to 0/1.
#' @param predictors Character vector of predictor column names.
#' @param conf.level Confidence level (default 0.95).
#' @param vcov_type Sandwich estimator type passed to `sandwich::vcovHC`
#'   (default "HC0", as in Zou 2004).
#'
#' @return Tibble with one row per term: the adjusted risk ratio, robust
#'   confidence interval, robust p-value, and model type
#'   (univariate / multivariable).
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   event = rbinom(200, 1, 0.3),
#'   exposure = rbinom(200, 1, 0.5),
#'   age = rnorm(200, 50, 10)
#' )
#' kk_rr_reg(df, event, c("exposure", "age"))
#' }
#'
#' @export
kk_rr_reg <- function(data, outcome, predictors, conf.level = 0.95,
                      vcov_type = "HC0") {
              validate_data_frame(data)
              if (!requireNamespace("sandwich", quietly = TRUE)) {
                            stop("Package 'sandwich' is required for kk_rr_reg().")
              }
              if (!is.character(predictors) || length(predictors) < 1) {
                            stop("`predictors` must be a non-empty character vector.")
              }

              outcome_name <- .kk_colname(rlang::enquo(outcome))
              missing_cols <- setdiff(c(outcome_name, predictors), names(data))
              if (length(missing_cols) > 0) {
                            stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
              }

              # Coerce outcome to 0/1
              y_raw <- data[[outcome_name]]
              y <- if (is.factor(y_raw)) {
                            as.integer(y_raw) - 1L
              } else if (is.logical(y_raw)) {
                            as.integer(y_raw)
              } else {
                            as.numeric(y_raw)
              }
              if (!all(stats::na.omit(y) %in% c(0, 1))) {
                            stop("`outcome` must be binary (0/1, logical, or a two-level factor).")
              }
              data[[outcome_name]] <- y

              z <- stats::qnorm(1 - (1 - conf.level) / 2)

              fit_one <- function(rhs, model_type) {
                            fml <- stats::as.formula(paste(outcome_name, "~", rhs))
                            model <- stats::glm(fml, data = data,
                                                family = stats::poisson(link = "log"))

                            rob_vcov <- sandwich::vcovHC(model, type = vcov_type)
                            rob_se <- sqrt(diag(rob_vcov))
                            est <- stats::coef(model)

                            tibble::tibble(
                                          term = names(est),
                                          model_type = model_type,
                                          risk_ratio = exp(est),
                                          conf.low = exp(est - z * rob_se),
                                          conf.high = exp(est + z * rob_se),
                                          std.error = rob_se,
                                          statistic = est / rob_se,
                                          p.value = 2 * stats::pnorm(-abs(est / rob_se)),
                                          conf.level = conf.level
                            )
              }

              univariate <- purrr::map_dfr(predictors, function(p) fit_one(p, "univariate"))
              multivariable <- fit_one(paste(predictors, collapse = " + "), "multivariable")

              dplyr::bind_rows(univariate, multivariable) %>%
                            dplyr::filter(.data$term != "(Intercept)")
}
