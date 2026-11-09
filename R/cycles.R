# ============================================================
# CYCLES: PERIODOGRAM, HARMONIC MODEL AND FORECAST, POWER
# ============================================================

# ---- fast generalised Lomb-Scargle / T-R statistic ------------------------
# For every trial frequency, the R-squared of y ~ 1 + cos + sin (the
# "floating mean" periodogram of Zechmeister & Kuerster 2009). Komitov's T-R
# periodogram is its square root: the correlation coefficient R of the fitted
# sinusoid. Y may be a matrix (one column per series) for Monte Carlo.

.kk_gls_setup <- function(t, freqs) {
  n <- length(t)
  arg <- outer(2 * pi * freqs, t)
  Cm <- cos(arg)
  Sm <- sin(arg)
  C <- rowMeans(Cm)
  S <- rowMeans(Sm)
  CC <- rowMeans(Cm^2) - C^2
  SS <- rowMeans(Sm^2) - S^2
  CS <- rowMeans(Cm * Sm) - C * S
  list(Cm = Cm, Sm = Sm, CC = CC, SS = SS, CS = CS, D = CC * SS - CS^2, n = n)
}

.kk_gls_power <- function(st, Y) {
  Y <- as.matrix(Y)
  Y <- sweep(Y, 2, colMeans(Y))
  YY <- colMeans(Y^2)
  YC <- (st$Cm %*% Y) / st$n
  YS <- (st$Sm %*% Y) / st$n
  num <- st$SS * YC^2 + st$CC * YS^2 - 2 * st$CS * YC * YS
  P <- sweep(num / st$D, 2, YY, "/")
  # degenerate frequencies (e.g. exactly the Nyquist frequency of regular
  # sampling, where the sine column vanishes) are undefined, not zero
  bad <- !is.finite(st$D) | st$D < 1e-10 * pmax(st$CC, st$SS, 1e-300)
  P[bad, ] <- NA
  P[!is.finite(P)] <- NA
  pmin(pmax(P, 0), 1)
}

# Residualise the columns of Y on the basis B (with intercept).
.kk_project_out <- function(Y, B) {
  B <- cbind(1, B)
  qrB <- qr(B)
  Y - qr.fitted(qrB, Y)
}

# AR(1) surrogates with the same lag-1 autocorrelation and variance.
.kk_ar1_sims <- function(y, n_sim) {
  n <- length(y)
  phi <- stats::acf(y, lag.max = 1, plot = FALSE, na.action = stats::na.pass)$acf[2]
  phi <- max(min(phi, 0.98), -0.98)
  s <- stats::sd(y) * sqrt(1 - phi^2)
  E <- matrix(stats::rnorm(n * n_sim, 0, s), n, n_sim)
  Y <- E
  Y[1, ] <- E[1, ] / sqrt(1 - phi^2)
  if (n > 1) for (i in 2:n) Y[i, ] <- phi * Y[i - 1, ] + E[i, ]
  attr(Y, "phi") <- phi
  Y
}

#' T-R Periodogram by Komitov with a Red-Noise Test (KK)
#'
#' @description Searches a series for cycles of unknown period with the T-R
#'   periodogram of B. Komitov. At each trial period \eqn{T} a sinusoid with
#'   a free mean is fitted by least squares; the periodogram is the
#'   correlation \eqn{R(T)} between the series and the sinusoid and the ratio
#'   \eqn{R/S_R}, where \eqn{S_R = (1 - R^2)/\sqrt{N}} is the standard error of
#'   \eqn{R}. With a floating mean this is the generalised Lomb-Scargle
#'   periodogram (\eqn{R^2} is its normalised power), so it needs neither
#'   equal spacing nor complete cycles and is computed at the actual
#'   measurement times.
#'
#'   The rule \eqn{R/S_R \ge 1.96} is CycleLab's historical heuristic. It is
#'   not an exact 5% test even for one period chosen in advance: under
#'   independent Gaussian noise the fitted sinusoid at a pre-specified
#'   period has \eqn{F = \frac{N-3}{2}\frac{R^2}{1-R^2} \sim F(2, N-3)}
#'   (returned as `p_fixed`); at \eqn{N = 100}, \eqn{R/S_R = 1.96}
#'   corresponds to p of about 0.17. Searched over hundreds of periods,
#'   random series cross it routinely, and autocorrelated ("red") noise puts
#'   spurious power at long periods. Every peak is therefore tested against
#'   AR(1) surrogates with the record's length, time points and lag-1
#'   autocorrelation, with a max-T test in the spirit of Westfall and Young:
#'   \eqn{R/S_R} is standardised at each period by its mean and SD in a
#'   *pilot* set of surrogates, and the p value for the whole range is the
#'   share of an *independent* reference set whose largest standardised
#'   value reaches the observed one. Keeping the two sets separate makes the
#'   observed and the reference maxima exchangeable, so the test holds its
#'   level; the p value is then \eqn{(1 + \#\{\max Z^* \ge \max Z\})/(B + 1)}.
#'   The surrogates go through the same detrending and the same projection
#'   on the cycles already accepted as the data. With `n_sim` = 500 (the
#'   default) the false-cycle rate of white and red noise was close to 5% in
#'   our simulations; with fewer surrogates the p value is coarse — use at
#'   least 200, and 500 or more for a reported analysis.
#'
#'   Cycles are extracted one at a time. The grid peak with the largest
#'   standardised statistic is tested; if \eqn{p < \alpha} it enters the
#'   model, its period is then refined between the neighbouring grid points
#'   (by the maximum of \eqn{R}; the refinement does not enter the test),
#'   and the residual is searched again. Each step is conditional on the
#'   cycles accepted before it; this sequential procedure is not the formal
#'   step-down algorithm of Westfall and Young.
#'   A period that does not differ from a calendar period (24 h, 7 days,
#'   12 months, 365.25 days or their harmonics) within the frequency
#'   resolution \eqn{1/\mathrm{duration}} enters the model with its calendar
#'   value; the replacement is listed in `decisions`. The trend is removed
#'   first, \eqn{F_1(t) = F(t) - Y(t) + \bar F}, so that it does not appear as
#'   a spurious long cycle.
#'
#'   Trial periods run from two sampling steps to half the record (at least
#'   two complete cycles), evenly spaced in frequency, at most `max_periods`.
#'   The sinusoid is fitted at the actual measurement times, but the AR(1)
#'   noise model of the surrogates is defined on the order of the
#'   measurements; with markedly irregular sampling the red-noise test is
#'   approximate, and a decision says so.
#'   The Fourier periodogram at the harmonics \eqn{T_p, T_p/2, T_p/3, \dots}
#'   of the record length is returned as an independent check, together with
#'   the autocorrelation function and the cycle phase \eqn{Ph(t)} (0-1, from
#'   the minimum nearest the start) of the first cycle.
#'
#' @param data Data frame, one row per measurement.
#' @param outcome Measured variable (bare name or string).
#' @param time Time column: numeric, `Date` (days) or `POSIXct` (hours).
#' @param periods Optional numeric vector of trial periods (overrides the
#'   default grid).
#' @param detrend Trend removed before the search: `"linear"` (default),
#'   `"none"`, or any two-parameter form of [kk_ts_trend()] (e.g. `"log"`,
#'   `"exponential"`).
#' @param max_cycles Maximum number of cycles to extract (default 5).
#' @param n_sim Number of AR(1) surrogates (default 500).
#' @param calendar Candidate calendar periods for the replacement. `NULL`
#'   (default) uses 24, 12, 8, 6 h and 168 h for `POSIXct`; 7, 3.5, 365.25
#'   and 182.625 days for `Date`; none for numeric time (give e.g.
#'   `c(12, 6)` for monthly data). `FALSE` switches the replacement off.
#' @param fundamental Optional fundamental period; detected periods within 5%
#'   of `fundamental / k` are flagged as its harmonics (default 24 h for
#'   `POSIXct`, 7 days for `Date`). A harmonic is not a separate rhythm: a
#'   non-sinusoidal weekly or daily pattern (e.g. workdays versus weekend)
#'   needs several harmonics to be described.
#' @param max_periods Maximum number of trial periods (default 1000).
#' @param alpha Significance level (default 0.05).
#' @param seed Random seed for the surrogates; the result is reproducible for
#'   a given seed (default 1).
#'
#' @return Tibble with one row per tested peak: `cycle`, `period`
#'   (estimated), `model_period` (calendar value if replaced), `amplitude`,
#'   `peak` (time of the maximum after the start of the record), `R`, `R_SR`,
#'   `p_fixed` (F test for that period as if chosen in advance — not a test
#'   of a searched peak), `p_value` (max-T against red noise, after removing
#'   the cycles accepted before it), `in_model`, `harmonic`, and for
#'   `POSIXct` `peak_clock` (clock hour of the first maximum). Attributes:
#'   `spectrum` (period, `R`, `R_SR`, `p_fixed`, the standardised statistic
#'   and the family-wise threshold on the `R_SR` scale), `fourier`, `acf`,
#'   `phase`, `ar1`, `detrended`, `decisions`.
#'
#' @references Komitov B (1997) The Schove's series. Centural and
#'   supercentural variations of the solar activity. *Bulgarian Geophysical
#'   Journal* 23:74-82.
#'   Komitov B (1998) *Shestmerna statistika (6D STAT), version 5.0. User
#'   manual.* Stara Zagora.
#'   Zechmeister M, Kuerster M (2009) The generalised Lomb-Scargle
#'   periodogram. *Astronomy & Astrophysics* 496:577-584.
#'   \doi{10.1051/0004-6361:200811296}
#'   Westfall PH, Young SS (1993) *Resampling-Based Multiple Testing*. Wiley.
#'   Stefanov R (2026) *CycleLab: analysis of biomedical time series*.
#'   Plovdiv: Institute for Rare Diseases. <https://cyclelab.raredis.org>
#'
#' @seealso [kk_harmonic_model()], [kk_ts_trend()], [kk_cosinor()],
#'   [kk_wavelet()], [kk_plot_periodogram()]
#'
#' @examples
#' set.seed(4)
#' d <- data.frame(month = 1:96)
#' d$rate <- 50 + 0.1 * d$month + 12 * cos(2 * pi * (d$month - 1) / 12) +
#'   rnorm(96, 0, 4)
#' res <- kk_periodogram(d, rate, month, calendar = c(12, 6), n_sim = 200)
#' res
#' attr(res, "decisions")
#'
#' @export
kk_periodogram <- function(data, outcome, time, periods = NULL,
                           detrend = "linear", max_cycles = 5, n_sim = 500,
                           calendar = NULL, fundamental = NULL,
                           max_periods = 1000, alpha = 0.05, seed = 1) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  y_name <- .kk_colname(rlang::enquo(outcome))
  t_name <- .kk_colname(rlang::enquo(time))
  data <- .kk_ts_prepare(data, c(y_name, t_name), numeric = y_name)
  data <- data[order(data[[t_name]]), , drop = FALSE]
  info <- .kk_ts_time(data[[t_name]])
  # one local coordinate for the whole function: time since the first
  # measurement (numeric time is shifted too), so peaks and phases agree
  t <- info$t - min(info$t)
  y <- as.numeric(data[[y_name]])
  n <- length(y)
  if (n < 10) stop("At least 10 measurements are needed.", call. = FALSE)
  if (stats::sd(y) == 0) stop("The series is constant.", call. = FALSE)
  if (n_sim < 19) stop("`n_sim` must be at least 19.", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)
  decisions <- character(0)

  span <- diff(range(t))
  dts <- diff(t)
  dt <- stats::median(dts)
  if (any(dts <= 0)) stop("Times must be distinct.", call. = FALSE)
  if (stats::sd(dts) > 0.1 * mean(dts)) {
    decisions <- c(decisions, paste0(
      "Sampling is irregular: the sinusoids are fitted at the actual times, ",
      "but the AR(1) surrogates assume equal correlation between neighbouring ",
      "measurements, so the red-noise test is approximate."))
  }
  if (is.null(calendar)) {
    calendar <- switch(info$class,
      POSIXct = c(24, 12, 8, 6, 168),
      Date = c(7, 3.5, 365.25, 182.625),
      numeric()
    )
  }
  if (isFALSE(calendar)) calendar <- numeric()
  if (is.null(fundamental)) {
    fundamental <- switch(info$class, POSIXct = 24, Date = 7, NULL)
  }

  if (is.null(periods)) {
    fmin <- 2 / span
    fmax <- 1 / (2 * dt)
    need <- ceiling((fmax - fmin) * span)
    nf <- min(max_periods, max(50, 3 * need))
    if (need > max_periods) {
      pmin_ok <- 1 / (fmin + max_periods / span)
      decisions <- c(decisions, sprintf(paste0(
        "%d trial periods cannot cover the range without gaps; a minimum ",
        "period of %.3g would."), max_periods, pmin_ok))
      warning(decisions[length(decisions)], call. = FALSE)
    }
    freqs <- seq(fmin, fmax, length.out = nf)
  } else {
    freqs <- sort(1 / periods)
  }

  tr <- .kk_detrend(y, t, detrend)
  y1 <- tr$y1
  if (detrend != "none") {
    decisions <- c(decisions, sprintf(
      "The %s trend was removed before the search (F1 = F - Y + mean).",
      detrend))
  }
  # the same detrending is applied to the surrogates: an exact projection
  # for trends linear in their parameters, a linear projection otherwise
  trend_basis <- switch(detrend,
    none = matrix(numeric(0), n, 0),
    quadratic = cbind(t, t^2), cubic = cbind(t, t^2, t^3),
    log = cbind(log(t + dt)), hyperbolic = cbind(1 / (t + dt)),
    sqrt = cbind(sqrt(t + dt)), inverse_quadratic = cbind(1 / (t + dt)^2),
    quadratic_nolinear = cbind((t + dt)^2),
    cbind(t))
  if (!detrend %in% c(.kk_trend_joint, "none")) {
    decisions <- c(decisions, sprintf(paste0(
      "The %s trend is not linear in its parameters; the surrogates were ",
      "detrended linearly, an approximation."), detrend))
  }

  rsr <- function(P) sqrt(P) / ((1 - P) / sqrt(n))
  pfix <- function(P) stats::pf((n - 3) / 2 * P / (1 - P), 2, n - 3,
                                lower.tail = FALSE)
  st <- .kk_gls_setup(t, freqs)
  resid <- y1
  cycles <- list()
  spectrum <- NULL
  model_cols <- matrix(numeric(0), n, 0)
  phi_used <- NA_real_
  surrogates <- function(B) {
    Y <- .kk_ar1_sims(resid, B)
    phi <- attr(Y, "phi")
    Y <- .kk_project_out(Y, cbind(trend_basis, model_cols))
    attr(Y, "phi") <- phi
    Y
  }
  for (k in seq_len(max_cycles)) {
    P_obs <- drop(.kk_gls_power(st, resid))
    S_obs <- rsr(P_obs)
    # pilot set: centre and scale of R/SR at each period
    pilot <- surrogates(n_sim)
    if (k == 1) phi_used <- attr(pilot, "phi")
    S_pil <- rsr(.kk_gls_power(st, pilot))
    mu <- rowMeans(S_pil, na.rm = TRUE)
    sdv <- apply(S_pil, 1, stats::sd, na.rm = TRUE)
    sdv[!is.finite(sdv) | sdv <= 0] <- NA
    # independent reference set: distribution of the largest standardised value
    S_ref <- rsr(.kk_gls_power(st, surrogates(n_sim)))
    maxZ <- apply((S_ref - mu) / sdv, 2, function(z) {
      if (all(is.na(z))) NA_real_ else max(z, na.rm = TRUE)
    })
    Z_obs <- (S_obs - mu) / sdv
    if (all(is.na(Z_obs))) stop("No trial period could be evaluated.", call. = FALSE)
    if (k == 1) {
      zcrit <- stats::quantile(maxZ, 1 - alpha, na.rm = TRUE)
      spectrum <- tibble::tibble(
        period = 1 / freqs, frequency = freqs,
        R = sqrt(P_obs), R_SR = S_obs, p_fixed = pfix(P_obs),
        z = Z_obs, threshold = mu + zcrit * sdv
      )
    }
    j <- which.max(Z_obs)
    # the test uses the grid statistic, exactly as for the surrogates
    pval <- (1 + sum(maxZ >= Z_obs[j], na.rm = TRUE)) / (sum(!is.na(maxZ)) + 1)
    f_best <- freqs[j]
    if (pval < alpha) {
      lo <- freqs[max(1, j - 1)]
      hi <- freqs[min(length(freqs), j + 1)]
      if (hi > lo) {
        f_ref <- stats::optimize(function(f) {
          pw <- drop(.kk_gls_power(.kk_gls_setup(t, f), resid))
          if (is.na(pw)) 0 else -pw
        }, c(lo, hi))$minimum
        if (isTRUE(drop(.kk_gls_power(.kk_gls_setup(t, f_ref), resid)) >= P_obs[j])) {
          f_best <- f_ref
        }
      }
    }
    p_best <- drop(.kk_gls_power(.kk_gls_setup(t, f_best), resid))
    s_best <- rsr(p_best)
    per <- 1 / f_best

    model_per <- per
    used <- vapply(cycles, function(cc) if (isTRUE(cc$in_model)) cc$model_period else NA_real_,
                   numeric(1))
    used <- used[!is.na(used)]
    if (length(calendar)) {
      dif <- abs(1 / calendar - f_best)
      jc <- which.min(dif)
      if (dif[jc] < 1 / span && !any(abs(used - calendar[jc]) < 1e-9)) {
        if (calendar[jc] <= span / 2) {
          model_per <- calendar[jc]
          decisions <- c(decisions, sprintf(
            "Period %.4g replaced by the calendar period %g (within the resolution 1/%.4g).",
            per, calendar[jc], span))
        } else {
          decisions <- c(decisions, sprintf(paste0(
            "Period %.4g was not replaced by the calendar period %g: the record ",
            "does not contain two complete cycles of it."), per, calendar[jc]))
        }
      }
    }
    cs <- cos(2 * pi * t / model_per)
    sn <- sin(2 * pi * t / model_per)
    cf <- stats::coef(stats::lm(resid ~ cs + sn))
    amp <- sqrt(cf[2]^2 + cf[3]^2)
    peak <- (atan2(cf[3], cf[2]) %% (2 * pi)) / (2 * pi) * model_per
    harm <- NA
    if (!is.null(fundamental)) {
      kk <- round(fundamental / per)
      harm <- kk >= 2 &&
        abs(per - fundamental / kk) / (fundamental / kk) < 0.05
    }
    dup <- length(used) && any(abs(1 / used - 1 / model_per) < 1 / span)
    if (dup) {
      decisions <- c(decisions, sprintf(paste0(
        "The peak at %.4g lies within the frequency resolution of a cycle ",
        "already in the model (residual of a changing rhythm); the search ",
        "stopped."), per))
    }
    sig <- pval < alpha && per <= span / 2 && !dup
    cycles[[k]] <- tibble::tibble(
      cycle = k, period = per, model_period = model_per,
      amplitude = unname(amp), peak = unname(peak),
      R = sqrt(p_best), R_SR = s_best, p_fixed = pfix(p_best),
      p_value = pval, in_model = sig, harmonic = harm
    )
    if (!sig) break
    model_cols <- cbind(model_cols, cs, sn)
    resid <- y1 - stats::fitted(stats::lm(y1 ~ model_cols)) + mean(y1)
  }
  out <- dplyr::bind_rows(cycles)
  if (is.null(fundamental)) out$harmonic <- NULL
  if (info$class == "POSIXct") {
    out$peak_clock <- (.kk_clock_hours(info$origin) + out$peak) %% 24
  }

  # Fourier periodogram at the harmonics of the record length
  Tp <- span + dt
  kmax <- floor(Tp / (2 * dt))
  fper <- Tp / seq_len(max(1, kmax))
  Pf <- drop(.kk_gls_power(.kk_gls_setup(t, 1 / fper), y1))
  Pf[is.na(Pf)] <- 0

  fourier <- tibble::tibble(harmonic = seq_along(fper), period = fper,
                            R = sqrt(Pf), R_SR = rsr(Pf))

  # cycle phase of the first cycle: 0 at the minimum nearest the start
  phase <- NULL
  if (nrow(out) && out$in_model[1]) {
    P1 <- out$model_period[1]
    tmin <- (out$peak[1] + P1 / 2) %% P1
    phase <- tibble::tibble(time = data[[t_name]], t = t,
                            phase = ((t - tmin) / P1) %% 1,
                            value = y1)
  }

  ac <- stats::acf(y1, lag.max = min(n - 1, floor(n / 2)), plot = FALSE)
  attr(out, "spectrum") <- spectrum
  attr(out, "fourier") <- fourier
  attr(out, "acf") <- tibble::tibble(lag = drop(ac$lag), acf = drop(ac$acf),
                                     bound = 1.96 / sqrt(n))
  attr(out, "phase") <- phase
  attr(out, "ar1") <- phi_used
  attr(out, "detrended") <- tibble::tibble(time = data[[t_name]], t = t,
                                           observed = y, trend = tr$trend,
                                           detrended = y1)
  attr(out, "decisions") <- decisions
  attr(out, "time_unit") <- info$unit
  out
}

# ---- trend + cycles model and forecast -------------------------------------

# Trend columns on a fixed origin so new times are placed correctly.
.kk_trend_matrix_origin <- function(t, trend, origin, step = 1) {
  t0 <- t - origin
  X <- t0 + step
  switch(trend,
    none = NULL,
    linear = cbind(trend_t = t0),
    quadratic = cbind(trend_t = t0, trend_t2 = t0^2),
    cubic = cbind(trend_t = t0, trend_t2 = t0^2, trend_t3 = t0^3),
    log = cbind(trend_log = log(X)),
    hyperbolic = cbind(trend_inv = 1 / X),
    sqrt = cbind(trend_sqrt = sqrt(X)),
    inverse_quadratic = cbind(trend_inv2 = 1 / X^2),
    quadratic_nolinear = cbind(trend_t2 = X^2),
    stop("Trend ", trend, " is not linear in its parameters.", call. = FALSE)
  )
}

#' Trend-Plus-Cycles Model with a Validated Forecast (KK)
#'
#' @description The trigonometric approximation: the accepted cycles and the
#'   trend combined in one model,
#'   \deqn{y(t) = \mathrm{trend}(t) + \sum_k \left[A_k\cos(2\pi t/T_k) +
#'   B_k\sin(2\pi t/T_k)\right].}
#'   For trend functions linear in their parameters (linear, logarithmic,
#'   hyperbolic, square root, inverse quadratic, quadratic without a linear
#'   term, quadratic, cubic) trend and cycles are estimated jointly in one
#'   least-squares regression. For the other forms of [kk_ts_trend()] the
#'   trend is fitted first, the cycles are estimated on the detrended series
#'   and the trend is added back; a message says so.
#'
#'   Model quality is given by the multiple correlation \eqn{R} and the
#'   prognostic index \eqn{PrIdx = \sqrt{\sum(y - \bar y)^2 / \sum(y - \hat
#'   y)^2} - 1} of Komitov. A model that fits well need not forecast well, so
#'   the forecast is checked with a rolling origin against the naive forecast
#'   "repeat the last known value" (and the seasonal naive forecast when a
#'   period is a whole number of steps); the forecast is worth reporting only
#'   when it is more accurate. The check is optimistic, because the periods
#'   were chosen from the same data. For trends linear in their parameters
#'   the prediction interval combines the residual variance and the
#'   coefficient uncertainty, the latter inflated by \eqn{(1 + r)/(1 - r)}
#'   when the residuals are autocorrelated. For the other trend forms the
#'   trend is estimated in a separate first stage, so the interval comes
#'   from a moving-block bootstrap of the residuals in which *both* stages
#'   are refitted (`n_boot` replicates), and it includes the uncertainty of
#'   the trend. Nonlinear trends are extrapolated cautiously; keep their
#'   horizon short. Trends fitted on a log or reciprocal scale describe the
#'   median (geometric mean) rather than the mean of the original values.
#'
#' @inheritParams kk_periodogram
#' @param periods Numeric vector of cycle periods (from [kk_periodogram()],
#'   column `model_period`, or known: 12 months, 7 days, 24 h).
#' @param trend Trend function: any form of [kk_ts_trend()], `"quadratic"`,
#'   `"cubic"` or `"none"` (default `"linear"`).
#' @param horizon Number of future steps to forecast (default 0). One step is
#'   the median sampling interval.
#' @param h Horizon used in the rolling-origin check (default
#'   `max(1, horizon)`).
#' @param min_train Share of the series in the first training window of the
#'   check (default 0.6).
#' @param robust Newey-West standard errors for the coefficients (default
#'   `TRUE`).
#' @param conf.level Confidence level (default 0.95).
#' @param n_boot Bootstrap replicates for the prediction interval of a trend
#'   that is not linear in its parameters (default 499).
#' @param seed Random seed for the bootstrap (default 1).
#'
#' @return Tibble of coefficients (`term`, `estimate`, `std.error`,
#'   `conf.low`, `conf.high`, `p.value`). Attributes: `cycles` (period,
#'   amplitude and the time of the maximum after the start), `fit` (R,
#'   R-squared, PrIdx, RMSE, residual autocorrelation), `fitted`, `forecast`
#'   (with `interval`, the method of the prediction interval), `validation`
#'   (successful origins, MAE of the model and of the naive forecasts,
#'   percentage reduction of the error), `decisions` and `model`.
#'
#' @references Komitov B (1998) *Shestmerna statistika (6D STAT), version
#'   5.0. User manual.* Stara Zagora.
#'   Frisch R, Waugh FV (1933) Partial time regressions as compared with
#'   individual trends. *Econometrica* 1:387-401. \doi{10.2307/1907330}
#'   Hyndman RJ, Athanasopoulos G (2021) *Forecasting: Principles and
#'   Practice*, 3rd ed., section 5.10. OTexts. <https://otexts.com/fpp3/>
#'   Stefanov R (2026) *CycleLab: analysis of biomedical time series*.
#'   <https://cyclelab.raredis.org>
#'
#' @seealso [kk_periodogram()], [kk_ts_trend()], [kk_its()]
#'
#' @examples
#' pn <- kk_cycle_example("pneumonia")
#' res <- kk_harmonic_model(pn$series, rate, month, periods = c(12, 6),
#'                          horizon = 12)
#' attr(res, "cycles")
#' attr(res, "fit")
#' attr(res, "validation")
#' # compare the forecast with the 12 held-out months
#' cbind(attr(res, "forecast")[, c("t", "forecast")], actual = pn$holdout$rate)
#'
#' @export
kk_harmonic_model <- function(data, outcome, time, periods, trend = "linear",
                              horizon = 0, h = max(1, horizon),
                              min_train = 0.6, robust = TRUE,
                              conf.level = 0.95, n_boot = 499, seed = 1) {
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
  n <- length(y)
  if (missing(periods) || length(periods) == 0) {
    stop("Give at least one period (see kk_periodogram()).", call. = FALSE)
  }
  if (any(!is.finite(periods) | periods <= 0)) {
    stop("`periods` must be positive numbers.", call. = FALSE)
  }
  if (n < 2 * length(periods) + 4) stop("Too few observations.", call. = FALSE)
  ok_forms <- c(.kk_trend_joint, names(.kk_trend_defs), "verhulst")
  if (!trend %in% ok_forms) stop("Unknown trend: ", trend, call. = FALSE)
  joint <- trend %in% .kk_trend_joint
  dt <- stats::median(diff(t))
  origin <- min(t)
  decisions <- character(0)
  if (!joint) {
    decisions <- sprintf(paste0(
      "The %s trend is not linear in its parameters: it was fitted first, ",
      "the cycles were estimated on the detrended series and the trend was ",
      "added back."), trend)
    message(decisions)
  }

  cyc_cols <- function(tt) {
    do.call(cbind, lapply(seq_along(periods), function(k) {
      m <- cbind(cos(2 * pi * tt / periods[k]), sin(2 * pi * tt / periods[k]))
      colnames(m) <- paste0(c("cos_", "sin_"), signif(periods[k], 4))
      m
    }))
  }
  # fits on index set idx; returns a predictor for new times
  fit_on <- function(idx, yv = y) {
    tt <- t[idx]
    yy <- yv[idx]

    if (joint) {
      X <- cbind(.kk_trend_matrix_origin(tt, trend, origin, dt), cyc_cols(tt))
      f <- stats::lm.fit(cbind(1, X), yy)
      cf <- f$coefficients
      cf[is.na(cf)] <- 0
      function(tn) drop(cbind(1, .kk_trend_matrix_origin(tn, trend, origin, dt),
                              cyc_cols(tn)) %*% cf)
    } else {
      Xtr <- .kk_trend_X(tt, origin, dt)
      tf <- .kk_fit_trend_form(yy, Xtr, trend)
      if (is.null(tf)) stop("The ", trend, " trend cannot be fitted.", call. = FALSE)
      r <- yy - tf$fitted
      f <- stats::lm.fit(cbind(1, cyc_cols(tt)), r)
      cf <- f$coefficients
      cf[is.na(cf)] <- 0
      function(tn) tf$predict(.kk_trend_X(tn, origin, dt)) +
        drop(cbind(1, cyc_cols(tn)) %*% cf)
    }
  }

  # main fit with inference
  if (joint) {
    X <- cbind(.kk_trend_matrix_origin(t, trend, origin, dt), cyc_cols(t))
    trend_vals <- NULL
    yy <- y
  } else {
    tf <- .kk_fit_trend_form(y, .kk_trend_X(t, origin, dt), trend)
    if (is.null(tf)) stop("The ", trend, " trend cannot be fitted.", call. = FALSE)
    trend_vals <- tf$fitted
    X <- cyc_cols(t)
    yy <- y - trend_vals
  }
  df <- data.frame(.y = yy, X, check.names = FALSE)
  terms_x <- colnames(X)
  form <- stats::as.formula(paste(".y ~", paste0("`", terms_x, "`", collapse = " + ")))
  fit <- stats::lm(form, data = df)
  if (anyNA(stats::coef(fit))) {
    stop("The model is rank deficient: the periods are not distinguishable ",
         "in this record (too close to each other, or too long).", call. = FALSE)
  }
  V <- .kk_ts_vcov(fit, robust = robust)
  # a separately fitted trend uses parameters too
  df_res <- fit$df.residual - if (joint) 0 else if (trend == "verhulst") 4 else 2
  if (df_res < 1) stop("Too few observations for this model.", call. = FALSE)
  z <- stats::qt(1 - (1 - conf.level) / 2, df = df_res)
  est <- stats::coef(fit)
  se <- sqrt(pmax(0, diag(V)))
  out <- tibble::tibble(
    term = c("intercept", terms_x), estimate = unname(est),
    std.error = unname(se), conf.low = unname(est - z * se),
    conf.high = unname(est + z * se),
    p.value = unname(2 * stats::pt(-abs(est / se), df_res))
  )
  if (!joint) {
    out <- dplyr::bind_rows(
      tibble::tibble(term = c("trend_A", "trend_B"), estimate = c(tf$A, tf$B)),
      out)
  }

  cyc <- dplyr::bind_rows(lapply(seq_along(periods), function(k) {
    nm <- paste0(c("cos_", "sin_"), signif(periods[k], 4))
    ix <- match(nm, gsub("`", "", names(est)))
    ap <- .kk_amp_phase(est[ix[1]], est[ix[2]], V[ix, ix], periods[k], z)
    tibble::tibble(period = periods[k], amplitude = unname(ap$amplitude),
                   amplitude_low = unname(ap$amplitude_low),
                   amplitude_high = unname(ap$amplitude_high),
                   peak = unname((ap$peak - origin) %% periods[k]),
                   peak_t = unname(ap$peak %% periods[k]))
  }))

  fitted_all <- stats::fitted(fit) + if (joint) 0 else trend_vals
  res <- y - fitted_all
  r_res <- .kk_r1_corrected(res, length(est))
  ss_tot <- sum((y - mean(y))^2)
  ss_res <- sum(res^2)
  fit_stats <- tibble::tibble(
    n = n, trend = trend, joint = joint,
    R = sqrt(max(0, 1 - ss_res / ss_tot)), r_squared = 1 - ss_res / ss_tot,
    PrIdx = sqrt(ss_tot / ss_res) - 1, rmse = sqrt(mean(res^2)),
    r_residual = r_res
  )

  # forecast with the autocorrelation-inflated prediction interval
  fc <- NULL
  if (horizon > 0) {
    tf_new <- max(t) + dt * seq_len(horizon)
    Xn <- if (joint) cbind(.kk_trend_matrix_origin(tf_new, trend, origin, dt),
                           cyc_cols(tf_new)) else cyc_cols(tf_new)
    Xn1 <- cbind(1, Xn)
    pr <- drop(Xn1 %*% est) +
      if (joint) 0 else tf$predict(.kk_trend_X(tf_new, origin, dt))
    if (joint) {
      sigma2 <- sum(stats::residuals(fit)^2) / df_res
      infl <- if (r_res > 0) (1 + r_res) / (1 - r_res) else 1
      var_coef <- rowSums((Xn1 %*% stats::vcov(fit)) * Xn1) * infl
      sep <- sqrt(sigma2 + var_coef)
      lo <- pr - z * sep
      hi <- pr + z * sep
      how <- "analytic (residual + coefficient variance, autocorrelation-inflated)"
    } else {
      # moving-block bootstrap: refit trend and cycles on y* = fit + e*,
      # add a future error block; percentile interval
      if (!is.null(seed)) set.seed(seed)
      bl <- max(2, ceiling(n^(1 / 3)))
      draw_block <- function(len) {
        starts <- sample.int(max(1, n - bl + 1), ceiling(len / bl), replace = TRUE)
        unlist(lapply(starts, function(s0) res[s0:min(n, s0 + bl - 1)]))[seq_len(len)]
      }
      B <- replicate(n_boot, {
        ystar <- fitted_all + draw_block(n)
        pf <- tryCatch(fit_on(seq_len(n), ystar), error = function(e) NULL)
        if (is.null(pf)) rep(NA_real_, horizon) else pf(tf_new) + draw_block(horizon)
      })
      B <- matrix(B, nrow = horizon)
      a <- (1 - conf.level) / 2
      lo <- apply(B, 1, stats::quantile, a, na.rm = TRUE)
      hi <- apply(B, 1, stats::quantile, 1 - a, na.rm = TRUE)
      how <- sprintf("moving-block bootstrap, %d replicates, both stages refitted",
                     n_boot)
    }
    fc <- tibble::tibble(time = .kk_ts_untime(tf_new, info), t = tf_new,
                         forecast = pr, pred.low = lo, pred.high = hi,
                         interval = how)
  }

  # rolling-origin validation against the naive forecasts
  o_start <- max(length(est) + 3, floor(min_train * n))
  val <- NULL
  if (o_start <= n - h) {
    dts <- diff(t)
    regular <- (max(dts) - min(dts)) <= 1e-6 * max(abs(dts))
    seas <- periods[abs(periods / dt - round(periods / dt)) < 1e-6]
    # the seasonal naive forecast works on row offsets: regular sampling only
    m_seas <- if (length(seas) && regular) round(max(seas) / dt) else NA
    errs <- lapply(o_start:(n - h), function(o) {
      te <- o + seq_len(h)
      pfun <- tryCatch(fit_on(seq_len(o)), error = function(e) NULL)
      if (is.null(pfun)) return(NULL)
      pred <- pfun(t[te])
      sn <- if (!is.na(m_seas) && o > m_seas) {
        y[o - m_seas + ((seq_len(h) - 1) %% m_seas) + 1]
      } else rep(NA_real_, h)
      cbind(model = abs(pred - y[te]), naive = abs(y[o] - y[te]),
            seasonal_naive = abs(sn - y[te]))
    })
    n_ok <- sum(!vapply(errs, is.null, logical(1)))
    E <- do.call(rbind, errs)
    mae <- if (is.null(E)) c(model = NA, naive = NA, seasonal_naive = NA) else
      colMeans(E, na.rm = TRUE)
    val <- tibble::tibble(
      origins = n_ok, origins_failed = length(errs) - n_ok, h = h,
      mae_model = mae[["model"]], mae_naive = mae[["naive"]],
      mae_seasonal_naive = if (is.na(m_seas)) NA_real_ else mae[["seasonal_naive"]],
      error_reduction_pct = 100 * (1 - mae[["model"]] / mae[["naive"]]),
      better_than_naive = mae[["model"]] < mae[["naive"]]
    )
    if (isTRUE(!val$better_than_naive)) {
      decisions <- c(decisions, paste0(
        "The forecast is not more accurate than repeating the last value; ",
        "do not report it."))
    }
    decisions <- c(decisions, paste0(
      "The rolling-origin check refits the coefficients at every origin but ",
      "keeps the periods, which were chosen from the whole record; it is ",
      "therefore optimistic."))
  }

  attr(out, "cycles") <- cyc
  attr(out, "fit") <- fit_stats
  attr(out, "fitted") <- tibble::tibble(time = data[[t_name]], t = t,
                                        observed = y,
                                        fitted = unname(fitted_all),
                                        trend = if (joint) NA_real_ else trend_vals)
  attr(out, "forecast") <- fc
  attr(out, "validation") <- val
  attr(out, "decisions") <- decisions
  attr(out, "model") <- fit
  attr(out, "conf.level") <- conf.level
  out
}

# ---- planning: how many measurements --------------------------------------

#' Power to Detect a Cycle, and the Record Length Needed (KK)
#'
#' @description The planning question "how long, and how often, must I
#'   measure to detect a rhythm of this size without declaring false
#'   cycles?". Records are simulated with the given period, amplitude, noise
#'   SD, lag-1 autocorrelation and share of missing measurements, for each
#'   candidate duration; for each, the share of records without a cycle in
#'   which a cycle is declared (false cycles) and the share of records with a
#'   cycle in which it is found (power) are reported, with their Monte Carlo
#'   standard errors.
#'
#'   * `method = "periodogram"` (default): the period is searched for. Two
#'     rules are compared: the naive \eqn{R/S_R \ge 1.96} anywhere in the
#'     spectrum, and the red-noise max-T test of [kk_periodogram()] as it is
#'     applied to data — linear detrending, the lag-1 autocorrelation
#'     *estimated from each record* (signal included), standardisation by a
#'     pilot set of AR(1) surrogates and an independent reference set, the
#'     test on the trial-period grid. To keep planning fast the surrogate
#'     sets are shared by records whose estimated autocorrelation rounds to
#'     the same value (steps of 0.05), and `n_ref` surrogates are used. With
#'     autocorrelated noise the naive rule finds a "cycle" in almost every
#'     record; the red-noise test keeps false cycles near `alpha`. A
#'     detection counts as power only within 10% of the true period.
#'   * `method = "cosinor"`: the period is known and tested with the
#'     zero-amplitude F test of [kk_cosinor()], both with Newey-West (HAC)
#'     standard errors, the default of [kk_cosinor()], and with classical
#'     OLS errors. With white noise and no gaps the OLS power is exact
#'     (noncentral F); everything else is simulated. With autocorrelated
#'     noise the OLS test has too many false positives — the reason to use
#'     HAC errors.
#'
#'   Missing measurements are redrawn for every block of records (ten
#'   different patterns per duration), so the result averages over sampling
#'   patterns. The recommended duration (`required`) is the shortest one
#'   whose power reaches `target` and whose false-cycle rate does not exceed
#'   `alpha` by more than two Monte Carlo standard errors.
#'
#' @param period Period of the cycle.
#' @param amplitude Expected amplitude (half the peak-to-trough difference).
#' @param sd Standard deviation of the noise.
#' @param interval Sampling interval, in the units of `period`.
#' @param duration Candidate record lengths (same units). Default: 2 to 14
#'   cycles.
#' @param ar1 Lag-1 autocorrelation of the noise (default 0).
#' @param missing Share of measurements missing at random (default 0).
#' @param method `"periodogram"` (default) or `"cosinor"`.
#' @param alpha Significance level (default 0.05).
#' @param target Target power (default 0.8).
#' @param n_sim Simulated records per duration (default 300).
#' @param n_ref Surrogates in each of the pilot and reference sets of the
#'   red-noise test (default 200).
#' @param seed Random seed (default 1).
#'
#' @return Tibble with `duration`, `n_obs`, `n_cycles` and, for the
#'   periodogram, `false_rsr196`, `power_rsr196`, `false_noise_test`,
#'   `power` (noise test), `power_se`, `false_se`; for the cosinor `power`
#'   and `false_hac` (HAC test), `power_ols`, `false_ols` and their standard
#'   errors. Attribute `required`: the shortest duration meeting the
#'   criteria above.
#'
#' @references Stefanov R (2026) *CycleLab: analysis of biomedical time
#'   series*, "Planning a study". <https://cyclelab.raredis.org>
#'
#' @seealso [kk_periodogram()], [kk_cosinor()], [kk_nof1_schedule()]
#'
#' @examples
#' # Temperature every 2 h; rhythm amplitude 0.3 C, noise SD 0.3 C, r = 0.3
#' pw <- kk_cycle_power(period = 24, amplitude = 0.3, sd = 0.3, interval = 2,
#'                      duration = 24 * c(2, 3, 5, 7), ar1 = 0.3,
#'                      n_sim = 100, n_ref = 100)
#' pw
#' attr(pw, "required")
#'
#' @export
kk_cycle_power <- function(period, amplitude, sd, interval, duration = NULL,
                           ar1 = 0, missing = 0,
                           method = c("periodogram", "cosinor"),
                           alpha = 0.05, target = 0.8, n_sim = 300,
                           n_ref = 200, seed = 1) {
  method <- match.arg(method)
  if (!is.null(seed)) set.seed(seed)
  if (abs(ar1) >= 1) stop("`ar1` must be between -1 and 1.", call. = FALSE)
  if (missing < 0 || missing >= 1) stop("`missing` must be in [0, 1).", call. = FALSE)
  if (is.null(duration)) duration <- period * 2:14
  duration <- sort(duration)
  w <- 2 * pi / period
  se_p <- function(p, m) sqrt(p * (1 - p) / m)
  ar_noise <- function(nn, m, phi, s) {
    E <- matrix(stats::rnorm(nn * m, 0, s * sqrt(1 - phi^2)), nn, m)
    Y <- E
    Y[1, ] <- E[1, ] / sqrt(1 - phi^2)
    if (nn > 1 && phi != 0) for (i in 2:nn) Y[i, ] <- phi * Y[i - 1, ] + E[i, ]
    Y
  }
  rows <- lapply(duration, function(D) {
    t_full <- seq(0, D - interval, by = interval)
    n_full <- length(t_full)
    n_keep <- round(n_full * (1 - missing))
    base <- tibble::tibble(duration = D, n_obs = n_keep, n_cycles = D / period)
    if (n_keep < 8) return(base)
    # records in blocks, one missingness pattern per block
    n_pat <- if (missing > 0) min(10, n_sim) else 1
    blk <- rep_len(seq_len(n_pat), n_sim)
    res <- lapply(seq_len(n_pat), function(b) {
      m <- sum(blk == b)
      keep <- if (missing > 0) sort(sample.int(n_full, n_keep)) else seq_len(n_full)
      t <- t_full[keep]
      n <- length(t)
      ph <- stats::runif(m, 0, 2 * pi)
      S <- amplitude * cos(outer(w * t, ph, "-"))
      N0 <- ar_noise(n_full, m, ar1, sd)[keep, , drop = FALSE]
      N1 <- S + ar_noise(n_full, m, ar1, sd)[keep, , drop = FALSE]
      if (method == "cosinor") {
        X <- cbind(1, cos(w * t), sin(w * t))
        ols <- function(Y) {
          B <- solve(crossprod(X), crossprod(X, Y))
          rss1 <- colSums((Y - X %*% B)^2)
          rss0 <- colSums(sweep(Y, 2, colMeans(Y))^2)
          Fst <- ((rss0 - rss1) / 2) / (rss1 / (n - 3))
          stats::pf(Fst, 2, n - 3, lower.tail = FALSE) < alpha
        }
        hac <- function(Y) apply(Y, 2, function(v) {
          isTRUE(suppressWarnings(
            .kk_cosinor_fit(v, t, period, TRUE, 1 - alpha))$row$p_value < alpha)
        })
        return(cbind(p_ols = ols(N1), f_ols = ols(N0),
                     p_hac = hac(N1), f_hac = hac(N0)))
      }
      fmin <- 2 / D
      fmax <- 1 / (2 * interval)
      if (fmax <= fmin) return(NULL)
      freqs <- seq(fmin, fmax, length.out = max(50, min(1000, ceiling(3 * (fmax - fmin) * D))))
      freqs <- freqs[freqs < 0.999 / (2 * interval)]
      st <- .kk_gls_setup(t, freqs)
      rsr <- function(P) sqrt(P) / ((1 - P) / sqrt(n))
      # as in kk_periodogram(): linear detrending of data and surrogates
      N0 <- .kk_project_out(N0, cbind(t))
      N1 <- .kk_project_out(N1, cbind(t))
      R0 <- rsr(.kk_gls_power(st, N0))
      R1 <- rsr(.kk_gls_power(st, N1))
      phi_hat <- function(Y) apply(Y, 2, function(v) {
        r <- stats::acf(v, lag.max = 1, plot = FALSE)$acf[2]
        max(min(r, 0.98), -0.98)
      })
      maxz_test <- function(R, phis) {
        bins <- pmin(pmax(round(phis / 0.05) * 0.05, -0.95), 0.95)
        det <- logical(ncol(R))
        jmax <- integer(ncol(R))
        for (bb in unique(bins)) {
          cols <- which(bins == bb)
          pil <- rsr(.kk_gls_power(st, .kk_project_out(ar_noise(n, n_ref, bb, 1), cbind(t))))
          mu <- rowMeans(pil, na.rm = TRUE)
          sdv <- apply(pil, 1, stats::sd, na.rm = TRUE)
          sdv[!is.finite(sdv) | sdv <= 0] <- NA
          ref <- rsr(.kk_gls_power(st, .kk_project_out(ar_noise(n, n_ref, bb, 1), cbind(t))))
          mz <- apply((ref - mu) / sdv, 2, max, na.rm = TRUE)
          Z <- (R[, cols, drop = FALSE] - mu) / sdv
          zo <- apply(Z, 2, max, na.rm = TRUE)
          jmax[cols] <- apply(Z, 2, which.max)
          pv <- vapply(zo, function(z0) (1 + sum(mz >= z0)) / (n_ref + 1), numeric(1))
          det[cols] <- pv < alpha
        }
        list(det = det, near = abs(1 / freqs[jmax] - period) / period < 0.10)
      }
      t0 <- maxz_test(R0, phi_hat(N0))
      t1 <- maxz_test(R1, phi_hat(N1))
      j1 <- apply(R1, 2, which.max)
      cbind(f196 = apply(R0, 2, max, na.rm = TRUE) >= 1.96,
            p196 = apply(R1, 2, max, na.rm = TRUE) >= 1.96 &
              abs(1 / freqs[j1] - period) / period < 0.10,
            fnt = t0$det, pnt = t1$det & t1$near)
    })
    M <- do.call(rbind, res)
    if (is.null(M)) return(base)
    m <- nrow(M)
    if (method == "cosinor") {
      if (ar1 == 0 && missing == 0) {
        t <- t_full
        n <- length(t)
        X <- cbind(1, cos(w * t), sin(w * t))
        H <- X %*% solve(crossprod(X), t(X))
        exact <- mean(vapply(seq(0, 2 * pi, length.out = 13)[-13], function(p0) {
          sig <- amplitude * cos(w * t - p0)
          lam <- drop(t(sig) %*% (H - matrix(1 / n, n, n)) %*% sig) / sd^2
          stats::pf(stats::qf(1 - alpha, 2, n - 3), 2, n - 3, ncp = lam,
                    lower.tail = FALSE)
        }, numeric(1)))
        base$power_ols <- exact
      } else {
        base$power_ols <- mean(M[, "p_ols"])
      }
      base$false_ols <- mean(M[, "f_ols"])
      base$power <- mean(M[, "p_hac"])
      base$false_hac <- mean(M[, "f_hac"])
      base$power_se <- se_p(base$power, m)
      base$false_se <- se_p(base$false_hac, m)
      return(base)
    }
    base$false_rsr196 <- mean(M[, "f196"])
    base$power_rsr196 <- mean(M[, "p196"])
    base$false_noise_test <- mean(M[, "fnt"])
    base$power <- mean(M[, "pnt"])
    base$power_se <- se_p(base$power, m)
    base$false_se <- se_p(base$false_noise_test, m)
    base
  })
  out <- dplyr::bind_rows(rows)
  fr <- if (method == "cosinor") out$false_hac else out$false_noise_test
  ok <- which(!is.na(out$power) & out$power >= target &
                !is.na(fr) & fr <= alpha + 2 * pmax(out$false_se, 1e-9))
  attr(out, "required") <- if (length(ok)) out$duration[min(ok)] else NA_real_
  out
}
