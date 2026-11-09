# ============================================================
# TIME SERIES: DESCRIPTION, TREND FORMS, PARTS OF THE DAY
# ============================================================

# ---- two-parameter trend functions ----------------------------------------
# Each form is fitted by linearisation (u = g(X), v = h(Y), v = c0 + c1 u)
# and ranked by R-squared on the ORIGINAL scale of Y, because correlations
# in differently transformed coordinates are not comparable.
# X = time from the start plus one sampling step, so X > 0.

.kk_trend_defs <- list(
  linear = list(f = "Y = A*X + B", u = function(x) x, v = function(y) y,
                ab = function(c) c(A = c[2], B = c[1]),
                pred = function(x, A, B) A * x + B, joint = TRUE),
  exponential = list(f = "Y = B*exp(A*X)", u = function(x) x, v = log,
                     ab = function(c) c(A = c[2], B = exp(c[1])),
                     pred = function(x, A, B) B * exp(A * x), pos = TRUE),
  power = list(f = "Y = B*X^A", u = log, v = log,
               ab = function(c) c(A = c[2], B = exp(c[1])),
               pred = function(x, A, B) B * x^A, pos = TRUE),
  log = list(f = "Y = A*ln(X) + B", u = log, v = function(y) y,
             ab = function(c) c(A = c[2], B = c[1]),
             pred = function(x, A, B) A * log(x) + B, joint = TRUE),
  hyperbolic = list(f = "Y = A/X + B", u = function(x) 1 / x,
                    v = function(y) y, ab = function(c) c(A = c[2], B = c[1]),
                    pred = function(x, A, B) A / x + B, joint = TRUE),
  sqrt = list(f = "Y = A*sqrt(X) + B", u = sqrt, v = function(y) y,
              ab = function(c) c(A = c[2], B = c[1]),
              pred = function(x, A, B) A * sqrt(x) + B, joint = TRUE),
  shifted_hyperbola = list(f = "Y = B/(A + X)", u = function(x) x,
                           v = function(y) 1 / y,
                           ab = function(c) c(A = c[1] / c[2], B = 1 / c[2]),
                           pred = function(x, A, B) B / (A + x), nz = TRUE),
  hyperbolic_saturation = list(f = "Y = X/(A*X + B)", u = function(x) 1 / x,
                               v = function(y) 1 / y,
                               ab = function(c) c(A = c[1], B = c[2]),
                               pred = function(x, A, B) x / (A * x + B), nz = TRUE),
  exp_inverse = list(f = "Y = B*exp(A/X)", u = function(x) 1 / x, v = log,
                     ab = function(c) c(A = c[2], B = exp(c[1])),
                     pred = function(x, A, B) B * exp(A / x), pos = TRUE),
  inverse_exponential = list(f = "Y = 1/(A*exp(X) + B)", u = exp,
                             v = function(y) 1 / y,
                             ab = function(c) c(A = c[2], B = c[1]),
                             pred = function(x, A, B) 1 / (A * exp(x) + B),
                             nz = TRUE),
  inverse_quadratic = list(f = "Y = A/X^2 + B", u = function(x) 1 / x^2,
                           v = function(y) y,
                           ab = function(c) c(A = c[2], B = c[1]),
                           pred = function(x, A, B) A / x^2 + B, joint = TRUE),
  quadratic_nolinear = list(f = "Y = A*X^2 + B", u = function(x) x^2,
                            v = function(y) y,
                            ab = function(c) c(A = c[2], B = c[1]),
                            pred = function(x, A, B) A * x^2 + B, joint = TRUE),
  exponential_base = list(f = "Y = B*A^X", u = function(x) x, v = log,
                          ab = function(c) c(A = exp(c[2]), B = exp(c[1])),
                          pred = function(x, A, B) B * A^x, pos = TRUE),
  inverse_linear = list(f = "Y = 1/(A*X + B)", u = function(x) x,
                        v = function(y) 1 / y,
                        ab = function(c) c(A = c[2], B = c[1]),
                        pred = function(x, A, B) 1 / (A * x + B), nz = TRUE),
  michaelis_menten = list(f = "Y = B*X/(A + X)", u = function(x) 1 / x,
                          v = function(y) 1 / y,
                          ab = function(c) c(A = c[2] / c[1], B = 1 / c[1]),
                          pred = function(x, A, B) B * x / (A + x), nz = TRUE)
)

# forms that are linear in A and B, and so can be fitted jointly with cycles
.kk_trend_joint <- c("none", "linear", "log", "hyperbolic", "sqrt",
                     "inverse_quadratic", "quadratic_nolinear",
                     "quadratic", "cubic")

.kk_trend_X <- function(t, origin, step) t - origin + step

# Fit one form; returns A, B, fitted values and R2 on the original scale.
.kk_fit_trend_form <- function(y, X, form) {
  if (form == "verhulst") return(.kk_fit_verhulst(y, X))
  d <- .kk_trend_defs[[form]]
  if (is.null(d)) stop("Unknown trend form: ", form, call. = FALSE)
  if (isTRUE(d$pos) && any(y <= 0)) return(NULL)
  if (isTRUE(d$nz) && any(y == 0)) return(NULL)
  u <- suppressWarnings(d$u(X))
  v <- suppressWarnings(d$v(y))
  if (!all(is.finite(u)) || !all(is.finite(v))) return(NULL)
  lf <- stats::lm(v ~ u)
  cf <- stats::coef(lf)
  if (anyNA(cf)) return(NULL)
  ab <- d$ab(unname(cf))
  pred <- function(x) d$pred(x, ab[["A"]], ab[["B"]])
  fv <- pred(X)
  if (!all(is.finite(fv))) return(NULL)
  r2 <- 1 - sum((y - fv)^2) / sum((y - mean(y))^2)
  list(form = form, formula = d$f, A = ab[["A"]], B = ab[["B"]],
       fitted = fv, r_squared = r2, predict = pred, k = 2, lin = lf)
}

.kk_fit_verhulst <- function(y, X) {
  C0 <- min(y) - 0.05 * diff(range(y))
  F0 <- diff(range(y)) * 1.1
  st <- list(C = C0, F = F0, A = -4 / diff(range(X)), B = 2)
  f <- tryCatch(suppressWarnings(stats::nls(
    y ~ C + F / (1 + exp(A * X + B)), start = st,
    control = stats::nls.control(maxiter = 200, warnOnly = TRUE))),
    error = function(e) NULL)
  if (is.null(f) || !isTRUE(f$convInfo$isConv)) return(NULL)
  cf <- stats::coef(f)
  if (!all(is.finite(cf))) return(NULL)
  pred <- function(x) cf[["C"]] + cf[["F"]] / (1 + exp(cf[["A"]] * x + cf[["B"]]))
  fv <- pred(X)
  if (!all(is.finite(fv))) return(NULL)
  list(form = "verhulst", formula = "Y = C + F/(1 + exp(A*X + B))",
       A = cf[["A"]], B = cf[["B"]], C = cf[["C"]], F = cf[["F"]],
       fitted = fv,
       r_squared = 1 - sum((y - fv)^2) / sum((y - mean(y))^2), predict = pred,
       k = 4, fit = f)
}

# Remove a trend: F1(t) = F(t) - Y(t) + mean(F). Used by kk_periodogram().
.kk_detrend <- function(y, t, form) {
  if (form == "none") return(list(y1 = y, trend = rep(mean(y), length(y))))
  if (form %in% c("quadratic", "cubic")) {
    X <- .kk_trend_matrix_origin(t, form, min(t))
    tv <- stats::fitted(stats::lm(y ~ X))
  } else {
    step <- stats::median(diff(t))
    fit <- .kk_fit_trend_form(y, .kk_trend_X(t, min(t), step), form)
    if (is.null(fit)) stop("The ", form, " trend cannot be fitted to these data.",
                           call. = FALSE)
    tv <- fit$fitted
  }
  list(y1 = y - tv + mean(y), trend = tv)
}

# Lag-1 autocorrelation of residuals, corrected for short-series bias:
# r = r1 + (1 + 3 r1)/N + k/N (as in CycleLab).
.kk_r1_corrected <- function(e, k) {
  N <- length(e)
  r1 <- stats::acf(e, lag.max = 1, plot = FALSE)$acf[2]
  min(0.99, max(-0.99, r1 + (1 + 3 * r1) / N + k / N))
}

#' Trend of a Time Series: Autocorrelation-Corrected Test and Trend Forms (KK)
#'
#' @description Fits the chosen trend function (by default a straight line)
#'   and tests whether there is a trend at all, with a t test that uses the
#'   effective number of observations \eqn{N_{eff} = N(1 - r)/(1 + r)}, where
#'   \eqn{r} is the lag-1 autocorrelation of the residuals, corrected for its
#'   downward bias in short series as \eqn{r = r_1 + (1 + 3r_1)/N + k/N}
#'   (\eqn{k} = number of trend parameters). Without the correction
#'   autocorrelated series routinely show a spurious trend. Scaling the
#'   standard error by \eqn{\sqrt{(N-2)/(N_{eff}-2)}} is the approximation
#'   used by CycleLab, adequate for AR(1)-like residuals; it is not the exact
#'   covariance of a slope under autocorrelation.
#'
#'   **What is tested.** Every two-parameter form is fitted on a linearising
#'   scale, \eqn{v = c_0 + c_1 u} (e.g. \eqn{\ln Y = c_0 + c_1 X} for
#'   \eqn{Y = B e^{AX}}), and in every one of them the series is constant
#'   exactly when \eqn{c_1 = 0} — which is \eqn{A = 0} for most forms,
#'   \eqn{A = 1} for \eqn{Y = BA^X} and \eqn{B = 0} for
#'   \eqn{Y = X/(AX + B)}. The test of no trend is therefore the effective-N
#'   t test of \eqn{c_1} on the linearising scale (`p.value`); `test` says
#'   which null hypothesis it is. For forms linear in A and B the standard
#'   error and interval of `A` itself are also given. The Verhulst logistic
#'   is tested against a constant by the extra-sum-of-squares F test with
#'   the effective N.
#'
#'   All fifteen two-parameter trend functions are also fitted (by
#'   linearisation) and ranked by \eqn{R^2} on the original scale; a message
#'   is shown when one explains at least 0.05 more variance than the line. The
#'   form is **not** chosen automatically: picking the best of fifteen by
#'   \eqn{R^2} inflates false trends, and \eqn{R^2} says nothing about how the
#'   function behaves after the record ends. Choose a form when the nature of
#'   the process justifies it. The Verhulst logistic is fitted only on request
#'   (`form = "verhulst"`). Forms fitted on a log or reciprocal scale
#'   describe the median (geometric mean) of the original values rather than
#'   their mean.
#'
#' @param data Data frame, one row per measurement.
#' @param outcome Measured variable.
#' @param time Time column: numeric, `Date` (days) or `POSIXct` (hours).
#' @param form Trend function to test (default `"linear"`); see the
#'   `forms` attribute for the names.
#' @param conf.level Confidence level (default 0.95).
#'
#' @return One-row tibble: `form`, `formula`, `A`, `B` (and `C`, `F` for
#'   Verhulst), `r_squared`, `r_residual` (corrected lag-1 autocorrelation),
#'   `n`, `n_eff` (floored at the number of parameters plus 1;
#'   `n_eff_floored` says when), `test` (the null hypothesis of no trend),
#'   `p.value`, and for forms linear in A and B the effective-N `std.error`,
#'   `conf.low`, `conf.high` of `A`. `p.value` is `NA` when the effective
#'   number of observations is too small for inference. Attributes: `forms`
#'   (all functions ranked
#'   by R-squared; forms that cannot be fitted — e.g. logarithmic
#'   linearisations when a value is zero or negative — are omitted),
#'   `fitted`.
#'
#' @references Komitov B (1998) *Shestmerna statistika (6D STAT), version
#'   5.0. User manual.* Stara Zagora.
#'   Stefanov R (2026) *CycleLab: analysis of biomedical time series*.
#'   <https://cyclelab.raredis.org>
#'   Bayley GV, Hammersley JM (1946) The "effective" number of independent
#'   observations in an autocorrelated time series. *Supplement to the
#'   Journal of the Royal Statistical Society* 8:184-197.
#'   \doi{10.2307/2983560}
#'
#' @seealso [kk_trend_test()], [kk_periodogram()], [kk_harmonic_model()]
#'
#' @examples
#' er <- kk_cycle_example("emergency")
#' kk_ts_trend(er, visits, month)
#' head(attr(kk_ts_trend(er, visits, month), "forms"))
#'
#' @export
kk_ts_trend <- function(data, outcome, time, form = "linear",
                        conf.level = 0.95) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  y_name <- .kk_colname(rlang::enquo(outcome))
  t_name <- .kk_colname(rlang::enquo(time))
  data <- .kk_ts_prepare(data, c(y_name, t_name), numeric = y_name)
  data <- data[order(data[[t_name]]), , drop = FALSE]
  info <- .kk_ts_time(data[[t_name]])
  t <- info$t
  y <- as.numeric(data[[y_name]])
  N <- length(y)
  if (N < 6) stop("At least 6 measurements are needed.", call. = FALSE)
  if (stats::sd(y) == 0) stop("The series is constant.", call. = FALSE)
  step <- stats::median(diff(t))
  X <- .kk_trend_X(t, min(t), step)

  forms <- dplyr::bind_rows(lapply(names(.kk_trend_defs), function(f) {
    r <- .kk_fit_trend_form(y, X, f)
    if (is.null(r)) return(NULL)
    tibble::tibble(form = f, formula = r$formula, A = r$A, B = r$B,
                   r_squared = r$r_squared, joint_with_cycles = f %in% .kk_trend_joint)
  }))
  forms <- forms[order(-forms$r_squared), , drop = FALSE]

  fit <- .kk_fit_trend_form(y, X, form)
  if (is.null(fit)) stop("The ", form, " trend cannot be fitted to these data.",
                         call. = FALSE)
  kp <- fit$k
  # residuals on the scale of the test: the linearising scale for the
  # two-parameter forms, the original scale for Verhulst
  e <- if (form == "verhulst") y - fit$fitted else stats::residuals(fit$lin)
  r <- .kk_r1_corrected(e, kp)
  neff_raw <- N * (1 - r) / (1 + r)
  neff <- max(kp + 1, neff_raw)
  out <- tibble::tibble(form = form, formula = fit$formula, A = fit$A,
                        B = fit$B)
  if (form == "verhulst") {
    out$C <- fit$C
    out$F <- fit$F
  }
  out$r_squared <- fit$r_squared
  out$r_residual <- r
  out$n <- N
  out$n_eff <- neff
  out$n_eff_floored <- neff_raw < kp + 1
  null_txt <- switch(form,
    exponential_base = "A = 1 (linearised slope 0)",
    hyperbolic_saturation = "B = 0 (linearised slope 0)",
    shifted_hyperbola = "1/B = 0 (linearised slope 0)",
    michaelis_menten = "A/B = 0 (linearised slope 0)",
    verhulst = "constant (extra-sum-of-squares F, effective N)",
    "A = 0 (linearised slope 0)")
  out$test <- null_txt
  df <- neff - kp
  if (df < 1) {
    warning("The effective number of observations is too small to test the ",
            "trend.", call. = FALSE)
    out$p.value <- NA_real_
  } else if (form == "verhulst") {
    rss1 <- sum((y - fit$fitted)^2)
    rss0 <- sum((y - mean(y))^2)
    Fst <- ((rss0 - rss1) / (kp - 1)) / (rss1 / df)
    out$p.value <- stats::pf(Fst, kp - 1, df, lower.tail = FALSE)
  } else {
    sl <- summary(fit$lin)$coefficients[2, ]
    se1 <- sl[["Std. Error"]] * sqrt((N - 2) / (neff - 2))
    out$p.value <- 2 * stats::pt(-abs(sl[["Estimate"]] / se1), df)
    if (form %in% .kk_trend_joint) {
      tq <- stats::qt(1 - (1 - conf.level) / 2, df)
      out$std.error <- se1
      out$conf.low <- fit$A - tq * se1
      out$conf.high <- fit$A + tq * se1
    }
  }

  lin <- forms$r_squared[forms$form == "linear"]
  if (length(lin) && nrow(forms) && forms$form[1] != "linear" &&
      forms$r_squared[1] - lin >= 0.05) {
    message(sprintf(paste0("The %s trend (%s) explains %.2f of the variance ",
                           "against %.2f for the linear; choose it only if the ",
                           "process justifies that shape."),
                    forms$form[1], forms$formula[1], forms$r_squared[1], lin))
  }
  attr(out, "forms") <- forms
  attr(out, "fitted") <- tibble::tibble(time = data[[t_name]], observed = y,
                                        trend = fit$fitted)
  out
}

#' Describe a Time Series: Distribution, Outliers, Moving Average (KK)
#'
#' @description The first step of a series analysis. Reports the
#'   distribution (mean, SD, median, quartiles, skewness, excess kurtosis)
#'   and whether it is close to normal — by the Shapiro-Wilk test, or for
#'   N > 5000 when skewness and kurtosis are within three standard errors of
#'   zero — the sampling step, duration and gaps, and lists values more than
#'   three SD from the mean as possible entry errors. The moving average over
#'   one cycle (one day for hourly data, seven days for daily data by default)
#'   removes that cycle and shows the slower changes.
#'
#' @param data Data frame, one row per measurement.
#' @param outcome Measured variable.
#' @param time Time column: numeric, `Date` or `POSIXct`.
#' @param window Moving-average window in time units (default 24 h for
#'   `POSIXct`, 7 days for `Date`, 5 steps for numeric).
#'
#' @return One-row tibble with `n`, `n_missing`, `duration`, `step`,
#'   `max_gap`, `mean`, `sd`, `median`, `q1`, `q3`, `min`, `max`, `skewness`,
#'   `kurtosis`, `shapiro_p`, `near_normal`, `n_outliers`, `summary_measure`
#'   (`"mean (SD)"` or `"median (IQR)"`). Attributes: `outliers`,
#'   `moving_average`.
#'
#' @seealso [kk_ts_trend()], [kk_cycle_analysis()]
#'
#' @examples
#' bp <- kk_cycle_example("hypertension")
#' kk_ts_describe(bp, sbp, date)
#'
#' @export
kk_ts_describe <- function(data, outcome, time, window = NULL) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  y_name <- .kk_colname(rlang::enquo(outcome))
  t_name <- .kk_colname(rlang::enquo(time))
  validate_data_frame(data)
  n_missing <- sum(is.na(data[[y_name]]))
  data <- .kk_ts_prepare(data, c(y_name, t_name), numeric = y_name)
  if (nrow(data) < 4) stop("At least 4 measurements are needed.", call. = FALSE)
  data <- data[order(data[[t_name]]), , drop = FALSE]
  info <- .kk_ts_time(data[[t_name]])
  t <- info$t
  y <- as.numeric(data[[y_name]])
  n <- length(y)
  dts <- diff(t)
  step <- stats::median(dts)
  sk <- moments::skewness(y)
  ku <- moments::kurtosis(y) - 3
  se_sk <- sqrt(6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3)))
  se_ku <- 2 * se_sk * sqrt((n^2 - 1) / ((n - 3) * (n + 5)))
  constant <- stats::sd(y) == 0
  if (constant) warning("The series is constant.", call. = FALSE)
  sw <- if (!constant && n >= 3 && n <= 5000) stats::shapiro.test(y)$p.value else NA_real_
  near <- if (constant) FALSE else if (!is.na(sw)) sw >= 0.05 else
    abs(sk) <= 3 * se_sk && abs(ku) <= 3 * se_ku
  m <- mean(y)
  s <- stats::sd(y)
  out_idx <- if (constant) integer(0) else which(abs(y - m) > 3 * s)
  if (is.null(window)) {
    window <- switch(info$class, POSIXct = 24, Date = 7, 5 * step)
  }
  k <- max(1, round(window / step))
  ma <- as.numeric(stats::filter(y, rep(1 / k, k), sides = 2))
  q <- stats::quantile(y, c(0.25, 0.5, 0.75))
  out <- tibble::tibble(
    n = n, n_missing = n_missing, duration = diff(range(t)), step = step,
    max_gap = if (length(dts)) max(dts) else NA_real_, unit = info$unit,
    mean = m, sd = s, median = q[[2]], q1 = q[[1]], q3 = q[[3]],
    min = min(y), max = max(y), skewness = sk, kurtosis = ku,
    shapiro_p = sw, near_normal = near, n_outliers = length(out_idx),
    summary_measure = if (near) "mean (SD)" else "median (IQR)"
  )
  attr(out, "outliers") <- tibble::tibble(time = data[[t_name]][out_idx],
                                          value = y[out_idx],
                                          z = (y[out_idx] - m) / s)
  attr(out, "moving_average") <- tibble::tibble(time = data[[t_name]],
                                                observed = y,
                                                moving_average = ma,
                                                window = window)
  out
}

#' Compare Parts of the Day (KK)
#'
#' @description Averages the measurements of every day in four parts —
#'   morning 05-11 h, midday 11-16 h, evening 16-22 h and night 22-05 h — and
#'   compares them using only days with measurements in all parts. The day is
#'   the block: a repeated-measures ANOVA with "day" as a blocking factor
#'   (parametric) and the Friedman test (non-parametric), then pairwise
#'   paired t and Wilcoxon signed-rank tests with Holm-adjusted p values.
#'   Differences between days therefore do not affect the comparison, and
#'   neighbouring measurements are not counted as independent. Night hours
#'   after midnight are assigned to the night that began the previous
#'   evening.
#'
#' @param data Data frame with a `POSIXct` time column.
#' @param outcome Measured variable.
#' @param time `POSIXct` time column.
#' @param breaks Start hours of the parts (default `c(5, 11, 16, 22)`).
#' @param labels Labels of the parts (default morning, midday, evening,
#'   night).
#'
#' @return Tibble of tests (`test`, `statistic`, `df`, `p.value`). Attributes:
#'   `means` (per part: days, mean, SD), `pairwise` (paired t and Wilcoxon
#'   with Holm-adjusted p), `daily` (the day x part means, ready to plot).
#'
#' @seealso [kk_cosinor()], [kk_epoch_response()]
#'
#' @examples
#' g <- kk_cycle_example("prediabetes")$series
#' kk_day_parts(g, glucose, time)
#'
#' @export
kk_day_parts <- function(data, outcome, time, breaks = c(5, 11, 16, 22),
                         labels = c("morning", "midday", "evening", "night")) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  y_name <- .kk_colname(rlang::enquo(outcome))
  t_name <- .kk_colname(rlang::enquo(time))
  data <- .kk_ts_prepare(data, c(y_name, t_name), numeric = y_name)
  tv <- data[[t_name]]
  if (!inherits(tv, "POSIXt")) stop("`time` must be POSIXct.", call. = FALSE)
  # fixed-offset clock time and calendar day (see kk_cosinor(), Time)
  info <- .kk_ts_time(tv)
  h <- info$clock
  day <- .kk_ts_day(info)

  part <- cut(h, c(-Inf, breaks, Inf), right = FALSE,
              labels = c(labels[length(labels)], labels))
  part <- factor(as.character(part), levels = labels)
  early <- h < breaks[1]
  day[early] <- day[early] - 1
  agg <- stats::aggregate(list(value = data[[y_name]]),
                          list(day = day, part = part), mean)
  w <- tidyr::pivot_wider(agg, names_from = "part", values_from = "value")
  w <- w[stats::complete.cases(w[, intersect(labels, names(w))]), , drop = FALSE]
  if (nrow(w) < 3) stop("Fewer than 3 days have measurements in all parts.",
                        call. = FALSE)
  long <- tidyr::pivot_longer(w, dplyr::all_of(labels), names_to = "part",
                              values_to = "value")
  long$part <- factor(long$part, levels = labels)
  long$day <- factor(long$day)
  av <- stats::anova(stats::lm(value ~ part + day, data = long))
  fr <- stats::friedman.test(as.matrix(w[, labels]))
  tests <- tibble::tibble(
    test = c("repeated-measures ANOVA (day as block)", "Friedman"),
    statistic = c(av$`F value`[1], unname(fr$statistic)),
    df = c(av$Df[1], unname(fr$parameter)),
    p.value = c(av$`Pr(>F)`[1], fr$p.value)
  )
  prs <- utils::combn(labels, 2, simplify = FALSE)
  pw <- dplyr::bind_rows(lapply(prs, function(p) {
    a <- w[[p[1]]]
    b <- w[[p[2]]]
    tt <- stats::t.test(a, b, paired = TRUE)
    wx <- suppressWarnings(stats::wilcox.test(a, b, paired = TRUE, exact = FALSE))
    tibble::tibble(part1 = p[1], part2 = p[2], mean_difference = mean(a - b),
                   p_ttest = tt$p.value, p_wilcoxon = wx$p.value)
  }))
  pw$p_ttest_holm <- stats::p.adjust(pw$p_ttest, "holm")
  pw$p_wilcoxon_holm <- stats::p.adjust(pw$p_wilcoxon, "holm")
  means <- dplyr::bind_rows(lapply(labels, function(l) {
    tibble::tibble(part = l, n_days = nrow(w), mean = mean(w[[l]]),
                   sd = stats::sd(w[[l]]))
  }))
  attr(tests, "means") <- means
  attr(tests, "pairwise") <- pw
  attr(tests, "daily") <- long
  tests
}
