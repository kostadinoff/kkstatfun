# ============================================================
# REGRESSION ANALYSIS (UNIFIED FUNCTION)
# ============================================================

#' Regression Analysis - Univariate and Multivariate
#'
#' @description Performs univariate and multivariate regression with automatic
#'   model selection. Supports linear, logistic, and ordinal regression.
#'
#' @param data Data frame
#' @param outcome Outcome variable name (character)
#' @param predictors Character vector of predictor names
#' @param log_outcome Log-transform outcome for continuous models
#' @param custom_formula Optional custom formula
#' @param include_diagnostics Include model diagnostics
#' @param ... Additional arguments to glm/lm/polr
#'
#' @return Tibble with regression results
#'
#' @details
#' Outcome types detected:
#' - Binary factor (2 levels): Logistic (odds ratios)
#' - Ordered factor: Ordinal regression
#' - Numeric: Linear regression
#'
#' @export
regression_analysis <- function(data, outcome, predictors,
                                log_outcome = FALSE,
                                custom_formula = NULL,
                                include_diagnostics = TRUE, ...) {
              validate_data_frame(data)

              if (!is.character(outcome) || length(outcome) != 1) {
                            stop("'outcome' must be a single character string")
              }

              if (!is.character(predictors)) {
                            stop("'predictors' must be a character vector")
              }

              if (!is.logical(log_outcome)) {
                            stop("'log_outcome' must be TRUE or FALSE")
              }

              if (!is.logical(include_diagnostics)) {
                            stop("'include_diagnostics' must be TRUE or FALSE")
              }

              outcome_name <- as.character(outcome)

              if (!outcome_name %in% colnames(data)) {
                            stop(sprintf("Outcome variable '%s' not found in data", outcome_name))
              }

              missing_predictors <- setdiff(predictors, colnames(data))
              if (length(missing_predictors) > 0) {
                            stop(sprintf("Predictors not found: %s", paste(missing_predictors, collapse = ", ")))
              }

              if (log_outcome) {
                            if (!is.numeric(data[[outcome_name]]) || any(data[[outcome_name]] <= 0, na.rm = TRUE)) {
                                          stop("log_outcome=TRUE requires positive numeric outcome")
                            }
                            data <- data %>% dplyr::mutate(!!rlang::sym(outcome_name) := log(!!rlang::sym(outcome_name)))
              }

              outcome_type <- dplyr::case_when(
                            is.factor(data[[outcome_name]]) && nlevels(data[[outcome_name]]) == 2 ~ "binary",
                            is.ordered(data[[outcome_name]]) ~ "ordinal",
                            is.numeric(data[[outcome_name]]) ~ "continuous",
                            TRUE ~ NA_character_
              )

              if (is.na(outcome_type)) {
                            stop("Outcome must be numeric, binary factor, or ordered factor")
              }

              fit_model <- function(formula) {
                            tryCatch(
                                          {
                                                        if (outcome_type == "binary") {
                                                                      stats::glm(formula, data = data, family = stats::binomial(), ...)
                                                        } else if (outcome_type == "ordinal") {
                                                                      if (!requireNamespace("MASS", quietly = TRUE)) {
                                                                                    stop("Package 'MASS' required for ordinal regression")
                                                                      }
                                                                      MASS::polr(formula, data = data, Hess = TRUE, ...)
                                                        } else if (outcome_type == "continuous") {
                                                                      stats::lm(formula, data = data, ...)
                                                        }
                                          },
                                          error = function(e) {
                                                        stop(sprintf("Model fitting failed: %s", e$message))
                                          }
                            )
              }

              calculate_diagnostics <- function(model) {
                            if (!include_diagnostics) {
                                          return(tibble::tibble())
                            }

                            diagnostics <- list()
                            summary_model <- summary(model)

                            if (outcome_type == "continuous") {
                                          diagnostics$r_squared <- summary_model$r.squared
                                          diagnostics$adj_r_squared <- summary_model$adj.r.squared
                                          diagnostics$residual_std_error <- summary_model$sigma
                                          diagnostics$model_p_value <- stats::pf(
                                                        summary_model$fstatistic[1],
                                                        summary_model$fstatistic[2],
                                                        summary_model$fstatistic[3],
                                                        lower.tail = FALSE
                                          )
                            } else if (outcome_type == "binary") {
                                          null_model <- stats::update(model, ~1)
                                          loglik_model <- as.numeric(stats::logLik(model))
                                          loglik_null <- as.numeric(stats::logLik(null_model))
                                          diagnostics$pseudo_r_squared <- as.numeric(1 - (loglik_model / loglik_null))
                                          diagnostics$nagelkerke_r_squared <- as.numeric(
                                                        (1 - exp(-2 * (loglik_model - loglik_null))) /
                                                                      (1 - exp(2 * loglik_null / nrow(data)))
                                          )
                                          diagnostics$model_p_value <- stats::pchisq(
                                                        2 * (loglik_model - loglik_null),
                                                        df = length(stats::coef(model)) - 1,
                                                        lower.tail = FALSE
                                          )

                                          if (requireNamespace("pROC", quietly = TRUE)) {
                                                        roc_curve <- pROC::roc(model$y, stats::fitted(model), quiet = TRUE)
                                                        diagnostics$auc_roc <- as.numeric(pROC::auc(roc_curve))
                                          }
                            } else if (outcome_type == "ordinal") {
                                          null_model <- stats::update(model, ~1)
                                          loglik_model <- as.numeric(stats::logLik(model))
                                          loglik_null <- as.numeric(stats::logLik(null_model))
                                          diagnostics$pseudo_r_squared <- as.numeric(1 - (loglik_model / loglik_null))
                                          diagnostics$nagelkerke_r_squared <- as.numeric(
                                                        (1 - exp(-2 * (loglik_model - loglik_null))) /
                                                                      (1 - exp(2 * loglik_null / nrow(data)))
                                          )
                                          diagnostics$model_p_value <- stats::pchisq(
                                                        2 * (loglik_model - loglik_null),
                                                        df = length(stats::coef(model)),
                                                        lower.tail = FALSE
                                          )
                            }

                            tibble::as_tibble(diagnostics)
              }

              process_results <- function(model, model_type) {
                            estimate_label <- if (outcome_type == "binary") {
                                          "odds_ratio"
                            } else if (outcome_type == "continuous" && log_outcome) {
                                          "exp_coef"
                            } else if (outcome_type == "ordinal") {
                                          "odds_ratio"
                            } else {
                                          "coef"
                            }

                            results <- broom::tidy(model, conf.int = TRUE) %>%
                                          dplyr::mutate(
                                                        model_type = model_type,
                                                        outcome_type = outcome_type,
                                                        AIC = stats::AIC(model),
                                                        BIC = stats::BIC(model),
                                                        estimate_label = estimate_label,
                                                        coef_type = dplyr::if_else(grepl("\\|", term), "scale", "coefficient")
                                          )

                            if (outcome_type %in% c("binary", "ordinal") ||
                                          (outcome_type == "continuous" && log_outcome)) {
                                          results <- results %>%
                                                        dplyr::mutate(
                                                                      estimate = dplyr::if_else(coef_type == "coefficient", exp(estimate), estimate),
                                                                      conf.low = dplyr::if_else(coef_type == "coefficient", exp(conf.low), conf.low),
                                                                      conf.high = dplyr::if_else(coef_type == "coefficient", exp(conf.high), conf.high),
                                                                      percent_change = dplyr::if_else(
                                                                                    coef_type == "coefficient",
                                                                                    (estimate - 1) * 100,
                                                                                    NA_real_
                                                                      ),
                                                                      percent_change_low = dplyr::if_else(
                                                                                    coef_type == "coefficient",
                                                                                    (conf.low - 1) * 100,
                                                                                    NA_real_
                                                                      ),
                                                                      percent_change_high = dplyr::if_else(
                                                                                    coef_type == "coefficient",
                                                                                    (conf.high - 1) * 100,
                                                                                    NA_real_
                                                                      )
                                                        )
                            } else if (outcome_type == "continuous") {
                                          results <- results %>%
                                                        dplyr::mutate(
                                                                      percent_change = estimate * 100,
                                                                      percent_change_low = conf.low * 100,
                                                                      percent_change_high = conf.high * 100
                                                        )
                            }

                            diagnostics <- calculate_diagnostics(model)
                            results %>% dplyr::bind_cols(diagnostics)
              }

              # Univariate models
              univariate_results <- purrr::map_dfr(predictors, function(predictor) {
                            formula <- stats::as.formula(paste(outcome_name, "~", predictor))
                            model <- fit_model(formula)
                            process_results(model, "univariate") %>%
                                          dplyr::mutate(predictor = predictor)
              })

              # Multivariate model
              if (is.null(custom_formula)) {
                            multivariate_formula <- stats::as.formula(
                                          paste(outcome_name, "~", paste(predictors, collapse = " + "))
                            )
              } else {
                            multivariate_formula <- custom_formula
              }

              multivariate_model <- fit_model(multivariate_formula)
              multivariate_results <- process_results(multivariate_model, "multivariate")

              dplyr::bind_rows(univariate_results, multivariate_results)
}
