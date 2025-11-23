# ============================================================
# TIME SERIES ANALYSIS
# ============================================================

#' Time Series Analysis (KK Time Series)
#'
#' @description Comprehensive time series analysis including GARCH, Hurst, and chain/fixed base metrics.
#'
#' @param data Data frame
#' @param value_col Value column
#' @param date_col Date column
#' @param group_cols Grouping columns
#' @param wide_format Return in wide format?
#' @param skip_advanced Skip advanced metrics (GARCH, etc.)?
#' @param round_digits Rounding digits
#'
#' @return Tibble with time series metrics
#'
#' @examples
#' date <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)
#' value <- 100 + cumsum(rnorm(24, 2, 5))
#' df <- data.frame(date = date, value = value)
#' kk_time_series(df)
#'
#' @export
kk_time_series <- function(data, value_col = NULL, date_col = "date", group_cols = NULL, wide_format = FALSE, skip_advanced = FALSE, round_digits = 4) {
              # Validate data input
              if (missing(data)) {
                            stop("Argument 'data' is missing, with no default")
              }

              # Validate value column
              if (is.null(value_col) && !"value" %in% names(data)) {
                            stop("Please specify a value column using value_col parameter or ensure 'value' column exists")
              }

              # Convert value_col to symbol and extract
              if (!is.null(value_col)) {
                            if (is.character(value_col)) value_col <- rlang::sym(value_col)
                            data <- dplyr::mutate(data, value = !!value_col)
              }

              # Validate date column
              if (!date_col %in% names(data)) {
                            stop(sprintf("Date column '%s' not found in data. Available columns: %s", date_col, paste(names(data), collapse = ", ")))
              }
              if (is.character(date_col)) date_col <- rlang::sym(date_col)

              # Ensure date is proper format
              if (!inherits(data[[date_col]], c("Date", "POSIXct"))) {
                            stop(sprintf("Column '%s' must be of class Date or POSIXct. Current class: %s", date_col, class(data[[date_col]])[1]))
              }

              # Handle grouping
              if (!is.null(group_cols)) {
                            group_cols <- rlang::syms(group_cols)
                            data <- dplyr::group_by(data, !!!group_cols)
                            # Check for duplicate dates within groups
                            data <- data %>%
                                          dplyr::group_by(!!!group_cols) %>%
                                          dplyr::mutate(.dup_dates = any(duplicated(!!date_col))) %>%
                                          dplyr::ungroup()
                            if (any(data$.dup_dates)) {
                                          stop("Duplicate dates found within groups. Ensure unique dates per group or preprocess data to aggregate duplicates.")
                            }
                            data <- dplyr::select(data, -.dup_dates)
                            data <- dplyr::group_by(data, !!!group_cols)
              } else {
                            # Check for duplicate dates in ungrouped data
                            if (any(duplicated(data[[date_col]]))) {
                                          stop("Duplicate dates found in data. Ensure unique dates or preprocess data to aggregate duplicates.")
                            }
              }

              # Extract group keys for proper labeling
              group_keys <- if (!is.null(group_cols)) dplyr::group_keys(data) else NULL
              group_labels <- if (!is.null(group_keys)) {
                            apply(group_keys, 1, paste, collapse = "_")
              } else {
                            NULL
              }

              # Process time series for each group
              result <- data %>%
                            dplyr::arrange(!!date_col) %>%
                            dplyr::filter(!is.na(value)) %>%
                            dplyr::group_map(~ {
                                          # Extract values and count observations
                                          y <- .x$value
                                          n <- length(y)
                                          if (n < 2) stop("Insufficient data points for time series analysis (n < 2)")
                                          if (n < 5) warning("Sample size is small (n = ", n, "). Some metrics may be unreliable or return NA.")

                                          # Check for negative values
                                          if (any(y <= 0)) {
                                                        warning("Non-positive values detected in group. Geometric mean growth rate may be unreliable.")
                                          }

                                          # Count missing values (before filtering)
                                          missing_count <- sum(is.na(.x$value))

                                          # Create zoo object for time series
                                          ts_zoo <- try(zoo::zoo(y, order.by = .x[[as.character(date_col)]]), silent = TRUE)
                                          if (inherits(ts_zoo, "try-error")) stop("Failed to create zoo object")

                                          # Basic statistics
                                          mean_val <- mean(y, na.rm = TRUE)
                                          median_val <- stats::median(y, na.rm = TRUE)
                                          sd_val <- stats::sd(y, na.rm = TRUE)
                                          var_val <- stats::var(y, na.rm = TRUE)
                                          min_val <- min(y, na.rm = TRUE)
                                          max_val <- max(y, na.rm = TRUE)
                                          range_val <- diff(range(y, na.rm = TRUE))
                                          q1_val <- stats::quantile(y, 0.25, na.rm = TRUE)
                                          q3_val <- stats::quantile(y, 0.75, na.rm = TRUE)
                                          skewness_val <- if (requireNamespace("moments", quietly = TRUE) && n >= 3) {
                                                        moments::skewness(y, na.rm = TRUE)
                                          } else {
                                                        warning("Sample size < 3 or 'moments' package missing for skewness. Returning NA.")
                                                        NA
                                          }
                                          kurtosis_val <- if (requireNamespace("moments", quietly = TRUE) && n >= 4) {
                                                        moments::kurtosis(y, na.rm = TRUE)
                                          } else {
                                                        warning("Sample size < 4 or 'moments' package missing for kurtosis. Returning NA.")
                                                        NA
                                          }
                                          cv_val <- if (mean_val != 0 && n >= 2) {
                                                        sd_val / abs(mean_val)
                                          } else {
                                                        warning("Mean is zero or sample size < 2 for CV. Returning NA.")
                                                        NA
                                          }

                                          # Outlier count (IQR method)
                                          outlier_count <- try(
                                                        {
                                                                      iqr <- q3_val - q1_val
                                                                      sum(y < (q1_val - 1.5 * iqr) | y > (q3_val + 1.5 * iqr))
                                                        },
                                                        silent = TRUE
                                          )
                                          if (inherits(outlier_count, "try-error")) outlier_count <- NA

                                          # Chain-base metrics
                                          abs_increase_chain <- if (n > 1) diff(y) else NA
                                          t_y_chain <- if (n > 1 && all(y[1:(n - 1)] != 0)) {
                                                        (y[2:n] / y[1:(n - 1)])
                                          } else {
                                                        warning("Zero values in denominator for chain-base growth rate. Returning NA.")
                                                        rep(NA, n - 1)
                                          }
                                          t_y_star_chain <- if (n > 1 && all(y[1:(n - 1)] != 0)) {
                                                        ((y[2:n] / y[1:(n - 1)]) - 1) * 100
                                          } else {
                                                        warning("Zero values in denominator for chain-base rate of increase. Returning NA.")
                                                        rep(NA, n - 1)
                                          }

                                          # Fixed-base metrics (using first value as base)
                                          base_value <- y[1]
                                          abs_increase_fixed <- if (n > 0 && base_value != 0) {
                                                        y - base_value
                                          } else {
                                                        warning("Base value is zero for fixed-base metrics. Returning NA.")
                                                        rep(NA, n)
                                          }
                                          t_y_fixed <- if (n > 0 && base_value != 0) (y / base_value) else rep(NA, n)
                                          t_y_star_fixed <- if (n > 0 && base_value != 0) ((y / base_value) - 1) * 100 else rep(NA, n)

                                          # Geometric mean growth rate (chain-base, coefficient form)
                                          t_y_chain_coeff <- t_y_chain[!is.na(t_y_chain) & t_y_chain > 0]
                                          geom_mean_growth <- if (length(t_y_chain_coeff) > 0) {
                                                        exp(mean(log(t_y_chain_coeff), na.rm = TRUE))
                                          } else {
                                                        warning("No valid positive chain-base growth rates for geometric mean. Returning NA.")
                                                        NA
                                          }
                                          geom_mean_growth_pct <- if (!is.na(geom_mean_growth)) (geom_mean_growth - 1) * 100 else NA

                                          # Mean rate of increase (T̅′)
                                          mean_incrate <- if (!is.na(geom_mean_growth)) geom_mean_growth - 1 else NA
                                          mean_incrate_pct <- if (!is.na(geom_mean_growth)) geom_mean_growth_pct else NA

                                          # Summary statistics for chain-base and fixed-base metrics
                                          mean_absinc_chain <- if (!all(is.na(abs_increase_chain))) mean(abs_increase_chain, na.rm = TRUE) else NA
                                          mean_devrate_chain <- if (!all(is.na(t_y_chain))) mean(t_y_chain, na.rm = TRUE) else NA
                                          mean_incrate_chain <- if (!all(is.na(t_y_star_chain))) mean(t_y_star_chain, na.rm = TRUE) else NA
                                          mean_absinc_fixed <- if (!all(is.na(abs_increase_fixed))) mean(abs_increase_fixed, na.rm = TRUE) else NA
                                          mean_devrate_fixed <- if (!all(is.na(t_y_fixed))) mean(t_y_fixed, na.rm = TRUE) else NA
                                          mean_incrate_fixed <- if (!all(is.na(t_y_star_fixed))) mean(t_y_star_fixed, na.rm = TRUE) else NA

                                          sd_absinc_chain <- if (length(abs_increase_chain[!is.na(abs_increase_chain)]) > 1) {
                                                        stats::sd(abs_increase_chain, na.rm = TRUE)
                                          } else {
                                                        NA
                                          }
                                          sd_devrate_chain <- if (length(t_y_chain[!is.na(t_y_chain)]) > 1) {
                                                        stats::sd(t_y_chain, na.rm = TRUE)
                                          } else {
                                                        NA
                                          }
                                          sd_incrate_chain <- if (length(t_y_star_chain[!is.na(t_y_star_chain)]) > 1) {
                                                        stats::sd(t_y_star_chain, na.rm = TRUE)
                                          } else {
                                                        NA
                                          }
                                          sd_absinc_fixed <- if (length(abs_increase_fixed[!is.na(abs_increase_fixed)]) > 1) {
                                                        stats::sd(abs_increase_fixed, na.rm = TRUE)
                                          } else {
                                                        NA
                                          }
                                          sd_devrate_fixed <- if (length(t_y_fixed[!is.na(t_y_fixed)]) > 1) {
                                                        stats::sd(t_y_fixed, na.rm = TRUE)
                                          } else {
                                                        NA
                                          }
                                          sd_incrate_fixed <- if (length(t_y_star_fixed[!is.na(t_y_star_fixed)]) > 1) {
                                                        stats::sd(t_y_star_fixed, na.rm = TRUE)
                                          } else {
                                                        NA
                                          }

                                          # Rate of change (existing ROC, for consistency with original function)
                                          roc <- if (n > 1 && all(y[1:(n - 1)] != 0)) {
                                                        (y[2:n] - y[1:(n - 1)]) / y[1:(n - 1)] * 100
                                          } else {
                                                        warning("Zero values in denominator for ROC. Returning NA.")
                                                        rep(NA, n - 1)
                                          }
                                          roc_stats <- if (!all(is.na(roc))) {
                                                        c(
                                                                      mean(roc, na.rm = TRUE),
                                                                      stats::sd(roc, na.rm = TRUE),
                                                                      min(roc, na.rm = TRUE),
                                                                      max(roc, na.rm = TRUE)
                                                        )
                                          } else {
                                                        rep(NA, 4)
                                          }

                                          # Advanced metrics (skipped if skip_advanced = TRUE and n is small)
                                          trend_slope <- trend_strength <- NA
                                          if (!skip_advanced || n >= 10) {
                                                        # Time index for regression
                                                        time_index <- as.numeric(difftime(.x[[as.character(date_col)]], min(.x[[as.character(date_col)]]), units = "days"))

                                                        # Trend strength
                                                        trend_model <- try(stats::lm(value ~ time_index, data = .x), silent = TRUE)
                                                        if (!inherits(trend_model, "try-error")) {
                                                                      trend_slope <- try(stats::coef(trend_model)[2], silent = TRUE)
                                                                      if (!inherits(trend_slope, "try-error")) {
                                                                                    trend_fit <- try(stats::predict(trend_model), silent = TRUE)
                                                                                    if (!inherits(trend_fit, "try-error")) {
                                                                                                  detrended <- y - trend_fit
                                                                                                  trend_strength <- try(1 - stats::var(detrended) / stats::var(y), silent = TRUE)
                                                                                                  if (inherits(trend_strength, "try-error")) trend_strength <- NA
                                                                                    }
                                                                      }
                                                        }
                                          }

                                          garch_vol <- NA
                                          if (!skip_advanced || n >= 30) {
                                                        garch_vol <- try(
                                                                      {
                                                                                    if (requireNamespace("rugarch", quietly = TRUE) && n >= 30) {
                                                                                                  suppressWarnings({
                                                                                                                # Difference data if non-stationary
                                                                                                                y_diff <- if (n > 1) diff(y) else y
                                                                                                                spec <- rugarch::ugarchspec(
                                                                                                                              mean.model = list(armaOrder = c(1, 0)),
                                                                                                                              variance.model = list(model = "sGARCH", garchOrder = c(1, 1))
                                                                                                                )
                                                                                                                garch_fit <- rugarch::ugarchfit(spec, y_diff, solver = "hybrid")
                                                                                                                if (is.null(garch_fit@fit$convergence) || garch_fit@fit$convergence != 0) {
                                                                                                                              # Try simpler model
                                                                                                                              spec <- rugarch::ugarchspec(
                                                                                                                                            mean.model = list(armaOrder = c(0, 0)),
                                                                                                                                            variance.model = list(model = "sGARCH", garchOrder = c(1, 1))
                                                                                                                              )
                                                                                                                              garch_fit <- rugarch::ugarchfit(spec, y_diff, solver = "hybrid")
                                                                                                                }
                                                                                                                mean(rugarch::sigma(garch_fit), na.rm = TRUE)
                                                                                                  })
                                                                                    } else {
                                                                                                  warning("Sample size < 30 or 'rugarch' package missing for GARCH. Returning NA.")
                                                                                                  NA
                                                                                    }
                                                                      },
                                                                      silent = TRUE
                                                        )
                                                        if (inherits(garch_vol, "try-error") || is.null(garch_vol)) {
                                                                      warning("GARCH fitting failed. Returning NA.")
                                                                      garch_vol <- NA
                                                        }
                                          }

                                          dom_freq <- NA
                                          if (!skip_advanced || n >= 10) {
                                                        dom_freq <- try(
                                                                      {
                                                                                    freq <- stats::spectrum(y, plot = FALSE)
                                                                                    freq$freq[which.max(freq$spec)]
                                                                      },
                                                                      silent = TRUE
                                                        )
                                                        if (inherits(dom_freq, "try-error")) dom_freq <- NA
                                          }

                                          shannon_entropy <- NA
                                          if (!skip_advanced || n >= 5) {
                                                        shannon_entropy <- try(
                                                                      {
                                                                                    if (requireNamespace("entropy", quietly = TRUE) && n >= 5) {
                                                                                                  breaks <- pretty(range(y), n = min(10, n / 5))
                                                                                                  binned <- cut(y, breaks, include.lowest = TRUE)
                                                                                                  counts <- table(binned)
                                                                                                  entropy::entropy(counts / sum(counts))
                                                                                    } else {
                                                                                                  warning("Sample size < 5 or 'entropy' package missing for Shannon entropy. Returning NA.")
                                                                                                  NA
                                                                                    }
                                                                      },
                                                                      silent = TRUE
                                                        )
                                                        if (inherits(shannon_entropy, "try-error")) shannon_entropy <- NA
                                          }

                                          acf_lag1 <- NA
                                          pacf_lag1 <- NA
                                          if (!skip_advanced || n >= 3) {
                                                        acf_lag1 <- try(
                                                                      {
                                                                                    acf_result <- stats::acf(y, lag.max = 1, plot = FALSE, na.action = stats::na.pass)
                                                                                    acf_result$acf[2]
                                                                      },
                                                                      silent = TRUE
                                                        )
                                                        if (inherits(acf_lag1, "try-error")) acf_lag1 <- NA

                                                        pacf_lag1 <- try(
                                                                      {
                                                                                    pacf_result <- stats::pacf(y, lag.max = 1, plot = FALSE, na.action = stats::na.pass)
                                                                                    pacf_result$acf[1]
                                                                      },
                                                                      silent = TRUE
                                                        )
                                                        if (inherits(pacf_lag1, "try-error")) pacf_lag1 <- NA
                                          }

                                          ljung_pval <- NA
                                          if (!skip_advanced || n >= 3) {
                                                        ljung_pval <- try(
                                                                      {
                                                                                    if (n >= 3) {
                                                                                                  test <- stats::Box.test(y, lag = min(10, n - 1), type = "Ljung-Box")
                                                                                                  test$p.value
                                                                                    } else {
                                                                                                  warning("Sample size < 3 for Ljung-Box test. Returning NA.")
                                                                                                  NA
                                                                                    }
                                                                      },
                                                                      silent = TRUE
                                                        )
                                                        if (inherits(ljung_pval, "try-error")) ljung_pval <- NA
                                          }

                                          adf_result <- c(NA, NA)
                                          if (!skip_advanced || n >= 10) {
                                                        adf_result <- try(
                                                                      {
                                                                                    if (requireNamespace("tseries", quietly = TRUE) && n >= 10) {
                                                                                                  suppressWarnings({
                                                                                                                test <- tseries::adf.test(y)
                                                                                                                c(test$p.value, test$statistic)
                                                                                                  })
                                                                                    } else {
                                                                                                  warning("Sample size < 10 or 'tseries' package missing for ADF test. Returning NA.")
                                                                                                  c(NA, NA)
                                                                                    }
                                                                      },
                                                                      silent = TRUE
                                                        )
                                                        if (inherits(adf_result, "try-error")) adf_result <- c(NA, NA)
                                          }

                                          kpss_result <- c(NA, NA)
                                          if (!skip_advanced || n >= 10) {
                                                        kpss_result <- try(
                                                                      {
                                                                                    if (requireNamespace("tseries", quietly = TRUE) && n >= 10) {
                                                                                                  suppressWarnings({
                                                                                                                test <- tseries::kpss.test(y)
                                                                                                                c(test$p.value, test$statistic)
                                                                                                  })
                                                                                    } else {
                                                                                                  warning("Sample size < 10 or 'tseries' package missing for KPSS test. Returning NA.")
                                                                                                  c(NA, NA)
                                                                                    }
                                                                      },
                                                                      silent = TRUE
                                                        )
                                                        if (inherits(kpss_result, "try-error")) kpss_result <- c(NA, NA)
                                          }

                                          hurst <- NA
                                          if (!skip_advanced || n >= 20) {
                                                        hurst <- try(
                                                                      {
                                                                                    if (requireNamespace("pracma", quietly = TRUE) && n >= 20) {
                                                                                                  temp <- utils::capture.output({
                                                                                                                h_result <- pracma::hurstexp(y, display = FALSE)
                                                                                                  })
                                                                                                  h_result$Hs
                                                                                    } else {
                                                                                                  warning("Sample size < 20 or 'pracma' package missing for Hurst exponent. Returning NA.")
                                                                                                  NA
                                                                                    }
                                                                      },
                                                                      silent = TRUE
                                                        )
                                                        if (inherits(hurst, "try-error")) hurst <- NA
                                          }

                                          # Compile results
                                          result_tibble <- dplyr::tibble(
                                                        Metric = c(
                                                                      "Length of Series", "Mean", "Median", "Standard Deviation", "Variance", "Min", "Max", "Range",
                                                                      "Q1 (25th Percentile)", "Q3 (75th Percentile)", "Skewness", "Kurtosis", "CV (SD/Mean)",
                                                                      "Missing Count", "Outlier Count",
                                                                      "Mean Abs Increase (Chain)", "Mean Growth Rate (Chain, Coeff)", "Mean Rate of Increase (Chain, %)",
                                                                      "Mean Abs Increase (Fixed)", "Mean Growth Rate (Fixed, Coeff)", "Mean Rate of Increase (Fixed, %)",
                                                                      "SD Abs Increase (Chain)", "SD Growth Rate (Chain, Coeff)", "SD Rate of Increase (Chain, %)",
                                                                      "SD Abs Increase (Fixed)", "SD Growth Rate (Fixed, Coeff)", "SD Rate of Increase (Fixed, %)",
                                                                      "Geometric Mean Growth Rate (Coeff)", "Geometric Mean Growth Rate (%)", "Mean Rate of Increase (T̅′, Coeff)", "Mean Rate of Increase (T̅′, %)",
                                                                      "Mean Rate of Change (%)", "SD Rate of Change (%)", "Min ROC (%)", "Max ROC (%)",
                                                                      "Trend Slope", "Trend Strength", "ADF p-value", "ADF statistic",
                                                                      "KPSS p-value", "KPSS statistic", "ACF Lag 1", "PACF Lag 1", "Ljung-Box p-value",
                                                                      "Hurst Exponent", "GARCH Volatility", "Shannon Entropy", "Dominant Frequency"
                                                        ),
                                                        Value = tryCatch(
                                                                      {
                                                                                    c(
                                                                                                  n, mean_val, median_val, sd_val, var_val, min_val, max_val, range_val,
                                                                                                  q1_val, q3_val, skewness_val, kurtosis_val, cv_val,
                                                                                                  missing_count, outlier_count,
                                                                                                  mean_absinc_chain, mean_devrate_chain, mean_incrate_chain,
                                                                                                  mean_absinc_fixed, mean_devrate_fixed, mean_incrate_fixed,
                                                                                                  sd_absinc_chain, sd_devrate_chain, sd_incrate_chain,
                                                                                                  sd_absinc_fixed, sd_devrate_fixed, sd_incrate_fixed,
                                                                                                  geom_mean_growth, geom_mean_growth_pct, mean_incrate, mean_incrate_pct,
                                                                                                  roc_stats[1], roc_stats[2], roc_stats[3], roc_stats[4],
                                                                                                  trend_slope, trend_strength,
                                                                                                  adf_result[1], adf_result[2],
                                                                                                  kpss_result[1], kpss_result[2],
                                                                                                  acf_lag1, pacf_lag1, ljung_pval,
                                                                                                  hurst, garch_vol, shannon_entropy, dom_freq
                                                                                    )
                                                                      },
                                                                      error = function(e) {
                                                                                    warning("Error in calculating statistics: ", e$message)
                                                                                    rep(NA, 48)
                                                                      }
                                                        )
                                          )

                                          return(result_tibble)
                            }, .keep = TRUE)

              # Bind results with proper group labels
              if (!is.null(group_cols)) {
                            result <- dplyr::bind_rows(result, .id = "group_idx") %>%
                                          dplyr::mutate(group = group_labels[as.integer(group_idx)]) %>%
                                          dplyr::select(-group_idx)
              } else {
                            result <- dplyr::bind_rows(result)
              }

              # Convert to wide format if requested
              if (wide_format) {
                            result <- result %>%
                                          tidyr::pivot_wider(names_from = Metric, values_from = Value, names_repair = "minimal")
              }

              return(result)
}

#' Time Series Metrics (KK Time Metrics)
#'
#' @description Calculates detailed time series metrics per period.
#'
#' @param data Data frame
#' @param value_col Value column
#' @param date_col Date column
#' @param group_cols Grouping columns
#'
#' @return Tibble with metrics
#'
#' @examples
#' date <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)
#' value <- 100 + cumsum(rnorm(24, 2, 5))
#' df <- data.frame(date = date, value = value)
#' kk_time_metrics(df)
#'
#' @export
kk_time_metrics <- function(data, value_col = NULL, date_col = "date", group_cols = NULL) {
              # Validate data input
              if (missing(data)) {
                            stop("Argument 'data' is missing, with no default")
              }

              # Validate value column
              if (is.null(value_col) && !"value" %in% names(data)) {
                            stop("Please specify a value column using value_col parameter or ensure 'value' column exists")
              }

              # Convert value_col to symbol and extract
              if (!is.null(value_col)) {
                            if (is.character(value_col)) value_col <- rlang::sym(value_col)
                            data <- dplyr::mutate(data, value = !!value_col)
              }

              # Validate date column
              if (!date_col %in% names(data)) {
                            stop(sprintf("Date column '%s' not found in data. Available columns: %s", date_col, paste(names(data), collapse = ", ")))
              }
              if (is.character(date_col)) date_col <- rlang::sym(date_col)

              # Ensure date is proper format
              if (!inherits(data[[date_col]], c("Date", "POSIXct"))) {
                            stop(sprintf("Column '%s' must be of class Date or POSIXct. Current class: %s", date_col, class(data[[date_col]])[1]))
              }

              # Handle grouping
              if (!is.null(group_cols)) {
                            group_cols <- rlang::syms(group_cols)
                            data <- dplyr::group_by(data, !!!group_cols)
              }

              # Process time series for each group
              result <- data %>%
                            dplyr::arrange(!!date_col) %>%
                            dplyr::filter(!is.na(value)) %>%
                            dplyr::group_map(~ {
                                          # Extract values, time index, and period
                                          y <- .x$value
                                          n <- length(y)
                                          if (n < 2) stop("Insufficient data points for time series analysis")
                                          time_index <- 1:n
                                          period <- .x[[as.character(date_col)]][order(.x[[as.character(date_col)]])]

                                          # Autocorrelation coefficient r1
                                          r1 <- if (n > 1) {
                                                        stats::cor(y[1:(n - 1)], y[2:n], use = "pairwise.complete.obs", method = "pearson")
                                          } else {
                                                        NA
                                          }

                                          # Durbin-Watson Q_BP
                                          q_bp <- if (n > 1) {
                                                        sum((y[2:n] - y[1:(n - 1)])^2) / sum(y^2)
                                          } else {
                                                        NA
                                          }

                                          # Ljung-Box test with p-value
                                          ljung_box_p <- if (n > 1) {
                                                        test <- try(stats::Box.test(y, lag = 1, type = "Ljung-Box"), silent = TRUE)
                                                        if (!inherits(test, "try-error")) test$p.value else NA
                                          } else {
                                                        NA
                                          }

                                          # Spearman correlation for trend and p-value
                                          spearman <- if (n > 1) {
                                                        test <- try(stats::cor.test(time_index, y, method = "spearman", exact = FALSE), silent = TRUE)
                                                        if (!inherits(test, "try-error")) c(test$estimate, test$p.value) else c(NA, NA)
                                          } else {
                                                        c(NA, NA)
                                          }

                                          # Anderson-Darling test for normality with p-value
                                          ad_test <- if (n > 1 && requireNamespace("nortest", quietly = TRUE)) {
                                                        if (n < 7) {
                                                                      warning("Sample size too small for Anderson-Darling test (< 7 observations). Returning NA.")
                                                                      c(NA, NA)
                                                        } else {
                                                                      test <- try(nortest::ad.test(y), silent = TRUE)
                                                                      if (!inherits(test, "try-error")) c(test$statistic, test$p.value) else c(NA, NA)
                                                        }
                                          } else {
                                                        c(NA, NA)
                                          }

                                          # Chain base metrics per period
                                          abs_increase_chain <- if (n > 1) c(NA, diff(y)) else rep(NA, n)
                                          t_y_chain <- if (n > 1) c(NA, (y[2:n] / y[1:(n - 1)])) else rep(NA, n) # Coefficient form
                                          t_y_star_chain <- if (n > 1) c(NA, ((y[2:n] / y[1:(n - 1)]) - 1) * 100) else rep(NA, n) # Percentage form

                                          # Fixed base period metrics per period (using first non-NA value as base)
                                          base_value <- y[1]
                                          abs_increase_fixed <- if (n > 0) y - base_value else rep(NA, n)
                                          t_y_fixed <- if (n > 0) (y / base_value) else rep(NA, n) # Coefficient form
                                          t_y_star_fixed <- if (n > 0) ((y / base_value) - 1) * 100 else rep(NA, n) # Percentage form

                                          # Geometric mean growth rate (chain-base, coefficient form)
                                          t_y_chain_coeff <- t_y_chain[!is.na(t_y_chain)]
                                          geom_mean_growth <- if (length(t_y_chain_coeff) > 0) {
                                                        exp(mean(log(t_y_chain_coeff), na.rm = TRUE))
                                          } else {
                                                        NA
                                          }
                                          geom_mean_growth_pct <- (geom_mean_growth - 1) * 100 # Percentage form

                                          # Standard deviation of chain-base growth rates (in coefficient form)
                                          t_y_chain_sd <- if (length(t_y_chain_coeff) > 1) {
                                                        stats::sd(t_y_chain_coeff, na.rm = TRUE)
                                          } else {
                                                        NA
                                          }
                                          t_y_chain_sd_pct <- t_y_chain_sd * 100 # Percentage form

                                          # Mean rate of increase (T̅′) based on geometric mean growth rate
                                          mean_incrate <- geom_mean_growth - 1 # Coefficient form
                                          mean_incrate_pct <- geom_mean_growth_pct # Percentage form

                                          # Compute means for chain base metrics
                                          abs_increase_chain_mean <- if (!all(is.na(abs_increase_chain))) mean(abs_increase_chain, na.rm = TRUE) else NA
                                          t_y_chain_mean <- if (!all(is.na(t_y_chain))) mean(t_y_chain, na.rm = TRUE) else NA
                                          t_y_star_chain_mean <- if (!all(is.na(t_y_star_chain))) mean(t_y_star_chain, na.rm = TRUE) else NA

                                          # Compute means for fixed base metrics
                                          abs_increase_fixed_mean <- if (!all(is.na(abs_increase_fixed))) mean(abs_increase_fixed, na.rm = TRUE) else NA
                                          t_y_fixed_mean <- if (!all(is.na(t_y_fixed))) mean(t_y_fixed, na.rm = TRUE) else NA
                                          t_y_star_fixed_mean <- if (!all(is.na(t_y_star_fixed))) mean(t_y_star_fixed, na.rm = TRUE) else NA

                                          # Compute standard deviations for per-period metrics
                                          abs_increase_chain_sd <- if (!all(is.na(abs_increase_chain))) stats::sd(abs_increase_chain, na.rm = TRUE) else NA
                                          t_y_chain_sd <- if (!all(is.na(t_y_chain))) stats::sd(t_y_chain, na.rm = TRUE) else NA
                                          t_y_star_chain_sd <- if (!all(is.na(t_y_star_chain))) stats::sd(t_y_star_chain, na.rm = TRUE) else NA
                                          abs_increase_fixed_sd <- if (!all(is.na(abs_increase_fixed))) stats::sd(abs_increase_fixed, na.rm = TRUE) else NA
                                          t_y_fixed_sd <- if (!all(is.na(t_y_fixed))) stats::sd(t_y_fixed, na.rm = TRUE) else NA
                                          t_y_star_fixed_sd <- if (!all(is.na(t_y_star_fixed))) stats::sd(t_y_star_fixed, na.rm = TRUE) else NA

                                          # Create per-period tibbles with period as date
                                          abs_increase_chain_df <- dplyr::tibble(period = period, value = round(abs_increase_chain, 4))
                                          t_y_chain_df <- dplyr::tibble(period = period, value = round(t_y_chain, 4))
                                          t_y_star_chain_df <- dplyr::tibble(period = period, value = round(t_y_star_chain, 4))
                                          abs_increase_fixed_df <- dplyr::tibble(period = period, value = round(abs_increase_fixed, 4))
                                          t_y_fixed_df <- dplyr::tibble(period = period, value = round(t_y_fixed, 4))
                                          t_y_star_fixed_df <- dplyr::tibble(period = period, value = round(t_y_star_fixed, 4))

                                          # Create a wide-format tibble with simplified lowercase names
                                          descriptive <- dplyr::tibble(
                                                        autocorr_r1 = r1,
                                                        durbinwatson_qbp = q_bp,
                                                        ljungbox_pval = ljung_box_p,
                                                        spearman_corr = spearman[1],
                                                        spearman_pval = spearman[2],
                                                        anderson_stat = ad_test[1],
                                                        anderson_pval = ad_test[2],
                                                        mean_absinc_chain = abs_increase_chain_mean,
                                                        mean_devrate_chain = t_y_chain_mean,
                                                        mean_incrate_chain = t_y_star_chain_mean,
                                                        mean_absinc_fixed = abs_increase_fixed_mean,
                                                        mean_devrate_fixed = t_y_fixed_mean,
                                                        mean_incrate_fixed = t_y_star_fixed_mean,
                                                        sd_absinc_chain = abs_increase_chain_sd,
                                                        sd_devrate_chain = t_y_chain_sd,
                                                        sd_incrate_chain = t_y_star_chain_sd,
                                                        sd_absinc_fixed = abs_increase_fixed_sd,
                                                        sd_devrate_fixed = t_y_fixed_sd,
                                                        sd_incrate_fixed = t_y_star_fixed_sd,
                                                        geom_mean_growth = geom_mean_growth,
                                                        geom_mean_growth_pct = geom_mean_growth_pct,
                                                        mean_incrate = mean_incrate,
                                                        mean_incrate_pct = mean_incrate_pct,
                                                        per_absinc_chain = list(abs_increase_chain_df),
                                                        per_devrate_chain = list(t_y_chain_df),
                                                        per_incrate_chain = list(t_y_star_chain_df),
                                                        per_absinc_fixed = list(abs_increase_fixed_df),
                                                        per_devrate_fixed = list(t_y_fixed_df),
                                                        per_incrate_fixed = list(t_y_star_fixed_df)
                                          ) %>%
                                                        dplyr::mutate_if(is.numeric, ~ round(., 4))

                                          return(descriptive)
                            }, .keep = TRUE) %>%
                            dplyr::bind_rows(.id = "group")

              return(result)
}

#' @export
time_series_analysis <- kk_time_series
