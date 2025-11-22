# ============================================================
# TIME SERIES ANALYSIS
# ============================================================

#' Time Series Metrics and Analysis
#'
#' @description Calculates comprehensive time series metrics including
#'   descriptive statistics, trends, and stationarity tests
#'
#' @param data Data frame with time series
#' @param value_col Value column name (character or unquoted)
#' @param date_col Date column name (default: "date")
#' @param group_cols Optional grouping columns
#' @param include_advanced Include advanced metrics (GARCH, Hurst, etc.)
#' @param round_digits Decimal places to round
#'
#' @return Tibble with time series metrics
#'
#' @export
time_series_analysis <- function(data, value_col = NULL,
                                 date_col = "date",
                                 group_cols = NULL,
                                 include_advanced = TRUE,
                                 round_digits = 4) {
              if (missing(data)) {
                            stop("Argument 'data' is missing")
              }

              if (is.null(value_col) && !"value" %in% names(data)) {
                            stop("Specify value_col or ensure 'value' column exists")
              }

              if (!is.null(value_col)) {
                            if (is.character(value_col)) value_col <- rlang::sym(value_col)
                            data <- dplyr::mutate(data, value = !!value_col)
              }

              validate_date_column(data, date_col)

              if (!is.null(group_cols)) {
                            group_cols <- rlang::syms(group_cols)
                            data <- dplyr::group_by(data, !!!group_cols)
              }

              date_col_sym <- rlang::sym(date_col)

              result <- data %>%
                            dplyr::arrange(!!date_col_sym) %>%
                            dplyr::filter(!is.na(value)) %>%
                            dplyr::group_map(~ {
                                          y <- .x$value
                                          n <- length(y)

                                          if (n < 2) {
                                                        stop(sprintf("Need at least 2 observations, got %d", n))
                                          }

                                          if (n < 5) {
                                                        warning(sprintf("Small sample size (n=%d). Results may be unreliable.", n))
                                          }

                                          # Basic statistics
                                          tibble::tibble(
                                                        n_obs = n,
                                                        mean = mean(y, na.rm = TRUE),
                                                        median = stats::median(y, na.rm = TRUE),
                                                        sd = stats::sd(y, na.rm = TRUE),
                                                        min = min(y, na.rm = TRUE),
                                                        max = max(y, na.rm = TRUE),
                                                        range = max(y) - min(y),
                                                        trend_slope = if (n >= 3) {
                                                                      time_idx <- as.numeric(seq_len(n))
                                                                      stats::coef(stats::lm(y ~ time_idx))[2]
                                                        } else {
                                                                      NA_real_
                                                        },
                                                        acf_lag1 = if (n >= 3) {
                                                                      stats::acf(y, lag.max = 1, plot = FALSE)$acf[2]
                                                        } else {
                                                                      NA_real_
                                                        },
                                                        adf_pvalue = if (n >= CONSTANTS$MIN_SAMPLE_FOR_TESTS$adf_kpss) {
                                                                      tryCatch(
                                                                                    tseries::adf.test(y)$p.value,
                                                                                    error = function(e) NA_real_
                                                                      )
                                                        } else {
                                                                      NA_real_
                                                        },
                                                        garch_vol = if (include_advanced && n >= CONSTANTS$MIN_SAMPLE_FOR_TESTS$garch) {
                                                                      tryCatch(
                                                                                    {
                                                                                                  y_diff <- if (n > 1) diff(y) else y
                                                                                                  spec <- rugarch::ugarchspec(
                                                                                                                mean.model = list(armaOrder = c(1, 0)),
                                                                                                                variance.model = list(model = "sGARCH", garchOrder = c(1, 1))
                                                                                                  )
                                                                                                  fit <- rugarch::ugarchfit(spec, y_diff, solver = "hybrid")
                                                                                                  mean(rugarch::sigma(fit), na.rm = TRUE)
                                                                                    },
                                                                                    error = function(e) NA_real_
                                                                      )
                                                        } else {
                                                                      NA_real_
                                                        }
                                          ) %>%
                                                        dplyr::mutate(dplyr::across(
                                                                      where(is.numeric),
                                                                      ~ round(., round_digits)
                                                        ))
                            }, .keep = TRUE) %>%
                            dplyr::bind_rows()

              result
}
