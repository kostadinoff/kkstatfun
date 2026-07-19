# ============================================================
# INDIRECT STANDARDIZATION (SMR) & FIRTH LOGISTIC
# ============================================================

#' Standardized Mortality/Morbidity Ratio (KK)
#'
#' @description Performs indirect standardization: compares the observed number
#'   of events in a study population with the number expected if it experienced
#'   the age-specific rates of a reference (standard) population, yielding the
#'   standardized mortality/morbidity ratio (SMR) with an exact Poisson
#'   confidence interval. Complements the direct standardization in
#'   `kk_std_rates`.
#'
#' @param data Data frame with one row per stratum (e.g. age band).
#' @param observed Column of observed events (bare name or string). May be a
#'   single total; if per-stratum, values are summed.
#' @param pop Column of study-population person-time / counts per stratum
#'   (bare name or string).
#' @param ref_rate Column of reference (standard) stratum-specific rates
#'   (bare name or string), on the same scale as `1 / pop` (i.e. events per
#'   person). Expected = pop * ref_rate.
#' @param conf.level Confidence level (default 0.95).
#' @param multiplier Multiplier for the standardized rate (default 1000).
#'
#' @return One-row tibble with observed, expected, the SMR and its exact CI, and
#'   the indirectly standardized rate.
#'
#' @examples
#' df <- data.frame(
#'   age_group = c("0-39", "40-59", "60+"),
#'   deaths = c(5, 20, 60),
#'   pyears = c(20000, 15000, 8000),
#'   ref_rate = c(0.0002, 0.0015, 0.008)
#' )
#' kk_smr(df, deaths, pyears, ref_rate)
#'
#' @export
kk_smr <- function(data, observed, pop, ref_rate, conf.level = 0.95,
                   multiplier = 1000) {
              validate_data_frame(data)

              obs_name <- .kk_colname(rlang::enquo(observed))
              pop_name <- .kk_colname(rlang::enquo(pop))
              rate_name <- .kk_colname(rlang::enquo(ref_rate))

              missing_cols <- setdiff(c(obs_name, pop_name, rate_name), names(data))
              if (length(missing_cols) > 0) {
                            stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
              }

              observed_total <- sum(data[[obs_name]], na.rm = TRUE)
              pop_total <- sum(data[[pop_name]], na.rm = TRUE)
              expected_total <- sum(data[[pop_name]] * data[[rate_name]], na.rm = TRUE)

              smr <- observed_total / expected_total
              alpha <- 1 - conf.level

              # Exact Poisson CI for the observed count, scaled by expected
              smr_low <- stats::qgamma(alpha / 2, observed_total) / expected_total
              smr_high <- stats::qgamma(1 - alpha / 2, observed_total + 1) / expected_total

              # Indirectly standardized rate = SMR * crude reference rate
              crude_ref_rate <- expected_total / pop_total
              std_rate <- smr * crude_ref_rate * multiplier

              tibble::tibble(
                            observed = observed_total,
                            expected = expected_total,
                            smr = smr,
                            smr_low = smr_low,
                            smr_high = smr_high,
                            std_rate = std_rate,
                            multiplier = multiplier,
                            p.value = stats::poisson.test(observed_total, expected_total)$p.value,
                            conf.level = conf.level
              )
}

#' Firth-Penalized Logistic Regression (KK)
#' @description Fits univariate and multivariable logistic regressions using
#'   Firth's penalized likelihood, which removes the small-sample bias of maximum
#'   likelihood and yields finite estimates under complete or quasi-complete
#'   separation (e.g. rare events or a zero cell). Separation in the data is
#'   detected and reported. Returns a tidy odds-ratio table.
#'
#' @param data Data frame.
#' @param outcome Binary outcome column (bare name or string).
#' @param predictors Character vector of predictor column names.
#' @param conf.level Confidence level (default 0.95).
#'
#' @return Tibble of odds ratios for univariate and multivariable models with
#'   profile-penalized confidence intervals, p-values, and a `model_type`
#'   column (indicating "univariate" or "multivariable"). The tibble carries
#'   a logical attribute `separation` indicating whether separation was
#'   detected in the multivariable model.
#'
#' @examples
#' \dontrun{
#' # A predictor that perfectly predicts the outcome (separation)
#' df <- data.frame(
#'   y = c(0, 0, 0, 0, 1, 1, 1, 1),
#'   x = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   z = rnorm(8)
#' )
#' kk_firth(df, y, c("x", "z"))
#' }
#'
#' @export
kk_firth <- function(data, outcome, predictors, conf.level = 0.95) {
              validate_data_frame(data)
              if (!requireNamespace("brglm2", quietly = TRUE)) {
                            stop("Package 'brglm2' is required for kk_firth().")
              }
              if (!is.character(predictors) || length(predictors) < 1) {
                            stop("`predictors` must be a non-empty character vector.")
              }

              out_name <- .kk_colname(rlang::enquo(outcome))
              missing_cols <- setdiff(c(out_name, predictors), names(data))
              if (length(missing_cols) > 0) {
                            stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
              }

              y_raw <- data[[out_name]]
              data[[out_name]] <- if (is.factor(y_raw)) as.integer(y_raw) - 1L
                                  else if (is.logical(y_raw)) as.integer(y_raw)
                                  else as.numeric(y_raw)

              fit_one <- function(rhs, model_type) {
                            fml <- stats::as.formula(paste(out_name, "~", rhs))
                            model <- stats::glm(fml, data = data, family = stats::binomial(),
                                                method = brglm2::brglmFit, type = "AS_mean")
                            broom::tidy(model, conf.int = TRUE, conf.level = conf.level) %>%
                                          dplyr::transmute(
                                                        term = .data$term,
                                                        model_type = model_type,
                                                        odds_ratio = exp(.data$estimate),
                                                        conf.low = exp(.data$conf.low),
                                                        conf.high = exp(.data$conf.high),
                                                        std.error = .data$std.error,
                                                        statistic = .data$statistic,
                                                        p.value = .data$p.value,
                                                        conf.level = conf.level
                                          ) %>%
                                          dplyr::filter(.data$term != "(Intercept)")
              }

              univariate <- purrr::map_dfr(predictors, function(p) fit_one(p, "univariate"))
              multivariable_formula_str <- paste(predictors, collapse = " + ")
              multivariable <- fit_one(multivariable_formula_str, "multivariable")

              res <- dplyr::bind_rows(univariate, multivariable)

              # Detect separation (pass the fitter as a function, since the
              # package is loaded via requireNamespace and not attached)
              separation <- FALSE
              if (requireNamespace("detectseparation", quietly = TRUE)) {
                            fml_mv <- stats::as.formula(paste(out_name, "~", multivariable_formula_str))
                            det <- tryCatch(
                                          stats::glm(fml_mv, data = data, family = stats::binomial(),
                                                     method = detectseparation::detect_separation),
                                          error = function(e) NULL
                            )
                            if (!is.null(det)) separation <- isTRUE(det$outcome)
              }

              attr(res, "separation") <- separation
              res
}
