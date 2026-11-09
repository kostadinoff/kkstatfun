# ============================================================
# BIOLOGICAL RHYTHMS: COSINOR (SINGLE, POPULATION, COMPARISON)
# ============================================================

# ---- shared internals for the time-series tier --------------------------

# Convert a time column to a numeric scale. Date -> days, POSIXct -> hours,
# numeric left alone. Returns the numeric values with the unit and the
# origin needed to map back.
#
# Time policy for POSIXct (one rule for every function of the tier): elapsed
# hours are measured from the first measurement, and clock time is the
# clock time of the first measurement plus the elapsed hours, i.e. the
# UTC offset in force at the first measurement is kept for the whole record.
# A daylight-saving change therefore does not create duplicated or missing
# clock hours, two distinct measurements never share a time, and clock
# hours, calendar days, clock-matched controls and peak labels all agree.
.kk_ts_time <- function(tval) {
  if (inherits(tval, "POSIXt")) {
    tval <- as.POSIXct(tval)
    origin <- min(tval, na.rm = TRUE)
    el <- as.numeric(difftime(tval, origin, units = "hours"))
    c0 <- .kk_clock_hours(origin)
    list(t = el, unit = "hours", origin = origin, class = "POSIXct",
         clock0 = c0, local = c0 + el, clock = (c0 + el) %% 24,
         tz = attr(tval, "tzone"))
  } else if (inherits(tval, "Date")) {
    origin <- min(tval, na.rm = TRUE)
    list(t = as.numeric(tval - origin), unit = "days", origin = origin,
         class = "Date", clock = NULL)
  } else if (is.numeric(tval)) {
    list(t = as.numeric(tval), unit = "time units", origin = 0,
         class = "numeric", clock = NULL)
  } else {
    stop("`time` must be numeric, Date or POSIXct.", call. = FALSE)
  }
}

.kk_clock_hours <- function(x) {
  lt <- as.POSIXlt(x)
  lt$hour + lt$min / 60 + lt$sec / 3600
}

# Map numeric times back onto the original class.
.kk_ts_untime <- function(t, info) {
  switch(info$class,
    POSIXct = info$origin + t * 3600,
    Date = info$origin + t,
    t
  )
}

# Calendar day (Date) of each POSIXct time under the fixed-offset policy.
.kk_ts_day <- function(info) {
  d0 <- as.Date(format(info$origin, "%Y-%m-%d", tz = .kk_tz_or(info$tz, "")))
  d0 + floor(info$local / 24)
}

# Resolve and check a set of required columns; drop incomplete rows.
# `numeric` names the columns that must be numeric measurements: factors and
# text are refused (a factor would silently become its integer codes) and
# rows with infinite values are dropped with the missing ones.
.kk_ts_prepare <- function(data, cols, numeric = NULL) {
  validate_data_frame(data)
  miss <- setdiff(cols, names(data))
  if (length(miss) > 0) {
    stop("Column(s) not found in data: ", paste(miss, collapse = ", "),
         call. = FALSE)
  }
  for (cl in numeric) {
    v <- data[[cl]]
    if (!(is.numeric(v) || is.logical(v))) {
      stop("Column `", cl, "` must be numeric.", call. = FALSE)
    }
  }
  ok <- stats::complete.cases(data[, cols, drop = FALSE])
  for (cl in numeric) ok <- ok & is.finite(as.numeric(data[[cl]]))
  data[ok, , drop = FALSE]
}

# Stop unless the times are strictly increasing and equally spaced. Methods
# that work on row offsets (lags, FFT) would otherwise treat a gap as one
# step.
.kk_ts_check_regular <- function(t, what = "This method") {
  d <- diff(t)
  if (any(d <= 0)) {
    stop("Times must be strictly increasing (repeated time points found).",
         call. = FALSE)
  }
  if (length(d) && (max(d) - min(d)) > 1e-6 * max(abs(d))) {
    stop(what, " needs equally spaced measurements without gaps ",
         "(missing values remove rows and create gaps); fill or ",
         "interpolate the gaps first.", call. = FALSE)
  }
  invisible(TRUE)
}

# Robust covariance for an lm fit: Newey-West for serially ordered data,
# cluster-robust when a cluster is given, HC1 otherwise. The method actually
# used is returned in attribute "method"; a fallback is announced with a
# warning, because the result is then less robust than requested.
.kk_ts_vcov <- function(fit, robust = TRUE, cluster = NULL, nw_lag = NULL) {
  tag <- function(V, m) {
    attr(V, "method") <- m
    V
  }
  fallback <- function(V, wanted, got, e) {
    warning(sprintf("%s covariance failed (%s); %s standard errors are used.",
                    wanted, conditionMessage(e), got), call. = FALSE)
    tag(V, got)
  }
  if (!robust) return(tag(stats::vcov(fit), "OLS"))
  if (!is.null(cluster)) {
    return(tryCatch(tag(sandwich::vcovCL(fit, cluster = cluster, type = "HC1"),
                        "cluster-robust (HC1)"),
                    error = function(e) fallback(stats::vcov(fit),
                                                 "Cluster-robust", "OLS", e)))
  }
  n <- stats::nobs(fit)
  lag_l <- if (is.null(nw_lag)) floor(4 * (n / 100)^(2 / 9)) else nw_lag
  if (lag_l > 0) {
    tryCatch(tag(sandwich::NeweyWest(fit, lag = lag_l, prewhite = FALSE,
                                     adjust = TRUE),
                 sprintf("Newey-West (lag %d)", lag_l)),
             error = function(e) fallback(sandwich::vcovHC(fit, type = "HC1"),
                                          "Newey-West", "HC1", e))
  } else {
    tryCatch(tag(sandwich::vcovHC(fit, type = "HC1"), "HC1"),
             error = function(e) fallback(stats::vcov(fit), "HC1", "OLS", e))
  }
}

# Amplitude and peak time from the cosine/sine coefficients with
# delta-method standard errors. theta is the phase angle of the peak
# (radians, 0..2pi); the peak occurs at theta / (2 pi) * period.
# When the amplitude is exactly zero the phase is undefined: SEs are NA.
.kk_amp_phase <- function(b, g, V, period, z) {
  b <- unname(b)
  g <- unname(g)
  A <- sqrt(b^2 + g^2)
  theta <- atan2(g, b) %% (2 * pi)
  if (!is.finite(A) || A <= 0) {
    return(list(amplitude = 0, amplitude_se = NA_real_, amplitude_low = 0,
                amplitude_high = NA_real_, theta = NA_real_,
                theta_se = NA_real_, peak = NA_real_, peak_low = NA_real_,
                peak_high = NA_real_, peak_se = NA_real_))
  }
  gA <- c(b / A, g / A)
  gT <- c(-g / A^2, b / A^2)
  seA <- sqrt(max(0, drop(t(gA) %*% V %*% gA)))
  seT <- sqrt(max(0, drop(t(gT) %*% V %*% gT)))
  list(
    amplitude = A, amplitude_se = seA,
    amplitude_low = max(0, A - z * seA), amplitude_high = A + z * seA,
    theta = theta, theta_se = seT,
    peak = theta / (2 * pi) * period,
    peak_low = (theta - z * seT) / (2 * pi) * period,
    peak_high = (theta + z * seT) / (2 * pi) * period,
    peak_se = seT / (2 * pi) * period
  )
}

.kk_wrap <- function(x) (x + pi) %% (2 * pi) - pi

# Time on the scale that anchors the phase. POSIXct: hours since midnight
# of the first day under the fixed-offset policy, so a peak at 17 means
# 17:00 and two measurements never share a time across a DST change.
# Date: days since the first date. Numeric: as given. Pass the `info` of the
# whole data set when the data are later split (subjects, groups), so that
# every part uses the same phase origin.
.kk_phase_time <- function(tval, info = .kk_ts_time(tval)) {
  if (info$class == "POSIXct") {
    as.numeric(difftime(as.POSIXct(tval), info$origin, units = "hours")) +
      info$clock0
  } else if (info$class == "Date") {
    as.numeric(tval - info$origin)
  } else {
    as.numeric(tval)
  }
}

# Largest gap in clock coverage of one day (hours), wrapping round midnight.
.kk_day_gap <- function(h) {
  h <- sort(h %% 24)
  if (length(h) < 2) return(24)
  max(diff(c(h, h[1] + 24)))
}

.kk_tz_or <- function(a, b) if (is.null(a) || identical(a, "")) b else a

# ---- single cosinor ---------------------------------------------------------

#' Cosinor Analysis of a Rhythm (KK)
#'
#' @description Fits the single-component cosinor model
#'   \deqn{y(t) = M + \beta \cos(2\pi t/\tau) + \gamma \sin(2\pi t/\tau)}
#'   by least squares and reports the three rhythm parameters: the MESOR `M`
#'   (rhythm-adjusted mean), the amplitude \eqn{A = \sqrt{\beta^2 + \gamma^2}}
#'   (half the peak-to-trough difference) and the acrophase (time of the
#'   peak). The zero-amplitude F test asks whether a rhythm of period
#'   \eqn{\tau} is present at all, and the percentage rhythm is the share of
#'   variance the fitted cosine explains.
#'
#'   With `POSIXct` time and the default 24-hour period the peak is reported
#'   in clock hours. Consecutive measurements are rarely independent, so the
#'   standard errors are Newey-West (HAC) by default; `robust = FALSE` gives
#'   the classical OLS intervals of Nelson et al. (1979). The acrophase
#'   interval is only meaningful when the amplitude is clearly non-zero: when
#'   the zero-amplitude test does not reject, the joint confidence region of
#'   \eqn{(\beta, \gamma)} contains the origin, the region for the peak time
#'   covers the whole cycle, `acrophase_reliable` is `FALSE` and the
#'   acrophase limits are returned as `NA` (a delta-method interval would be
#'   misleadingly narrow).
#'
#'   **Phase conventions.** `acrophase` is the time of the peak after the
#'   phase origin (midnight for clock time) and `acrophase_deg` the same as a
#'   positive angle, 0-360 degrees. The traditional cosinor acrophase of
#'   Halberg is the negative angle, `acrophase_traditional_deg` =
#'   -`acrophase_deg` (0 to -360 degrees).
#'
#'   **Time.** For `POSIXct` time the clock time is taken with the UTC offset
#'   in force at the first measurement, kept for the whole record, so a
#'   daylight-saving change neither duplicates nor removes hours.
#'
#' @param data Data frame with one row per measurement.
#' @param outcome Measured variable (bare name or string).
#' @param time Time column (bare name or string): `POSIXct` (clock time),
#'   `Date` or numeric (in the units of `period`).
#' @param period Period of the rhythm, in hours for `POSIXct`, days for `Date`
#'   and time units for numeric time (default 24).
#' @param by_day If `TRUE`, also fit a cosinor to every calendar day (or every
#'   consecutive `period`-long window for other time) with at least five
#'   measurements that cover the cycle (see `max_gap`); returned as the
#'   `per_day` attribute.
#' @param max_gap Coverage rule for the per-day fits: the largest allowed gap
#'   in the coverage of the cycle, wrapping round its end (default half the
#'   period, 12 h for a daily rhythm). Days whose measurements leave a longer
#'   gap are left out, because a cosinor fitted to part of a cycle is poorly
#'   identified.
#' @param robust Use HAC (Newey-West) standard errors (default `TRUE`).
#' @param conf.level Confidence level (default 0.95).
#'
#' @return One-row tibble: `n`, `period`, `mesor`, `amplitude`, `acrophase`
#'   (peak time, clock hours for `POSIXct`), `acrophase_deg` (peak as a phase
#'   angle, 0-360 degrees), their standard errors and confidence limits,
#'   `acrophase_traditional_deg`, `percent_rhythm`, the zero-amplitude
#'   `f_statistic` and `p_value`, `acrophase_reliable`, `se_method` (the
#'   covariance actually used) and `conf.level`. Attributes: `fitted` (time,
#'   observed, fitted, ready to plot), `per_day` (when `by_day = TRUE`),
#'   `model` (the `lm` fit) and `vcov` (the covariance matrix used).
#'
#' @references Nelson W, Tong YL, Lee JK, Halberg F (1979) Methods for
#'   cosinor-rhythmometry. *Chronobiologia* 6:305-323.
#'   Cornelissen G (2014) Cosinor-based rhythmometry. *Theoretical Biology and
#'   Medical Modelling* 11:16. \doi{10.1186/1742-4682-11-16}
#'
#' @seealso [kk_cosinor_population()], [kk_cosinor_compare()],
#'   [kk_periodogram()], [kk_plot_cosinor()]
#'
#' @examples
#' set.seed(1)
#' t <- as.POSIXct("2026-01-05 07:00", tz = "UTC") + sort(runif(120, 0, 14)) * 86400
#' hr <- as.numeric(format(t, "%H")) + as.numeric(format(t, "%M")) / 60
#' temp <- 36.6 + 0.35 * cos(2 * pi * (hr - 17) / 24) + rnorm(120, 0, 0.12)
#' d <- data.frame(time = t, temp = temp)
#' kk_cosinor(d, temp, time)
#'
#' @export
kk_cosinor <- function(data, outcome, time, period = 24, by_day = FALSE,
                       robust = TRUE, conf.level = 0.95, max_gap = period / 2) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  y_name <- .kk_colname(rlang::enquo(outcome))
  t_name <- .kk_colname(rlang::enquo(time))
  data <- .kk_ts_prepare(data, c(y_name, t_name), numeric = y_name)
  data <- data[order(data[[t_name]]), , drop = FALSE]
  if (nrow(data) < 5) stop("At least 5 measurements are needed.", call. = FALSE)

  info <- .kk_ts_time(data[[t_name]])
  # Clock time anchors the phase for POSIXct, so the peak reads as an hour.
  tt <- .kk_phase_time(data[[t_name]], info)
  y <- as.numeric(data[[y_name]])
  res <- .kk_cosinor_fit(y, tt, period, robust, conf.level)
  out <- res$row

  fitted <- tibble::tibble(time = data[[t_name]], t = tt, observed = y,
                           fitted = stats::fitted(res$fit))
  attr(out, "fitted") <- fitted
  attr(out, "model") <- res$fit
  attr(out, "vcov") <- res$V
  attr(out, "time_class") <- info$class

  if (by_day) {
    day_id <- .kk_cycle_id(tt, info, period)
    pieces <- split(seq_along(y), day_id)
    pd <- lapply(names(pieces), function(k) {
      idx <- pieces[[k]]
      if (length(idx) < 5) return(NULL)
      if (.kk_day_gap(tt[idx] / period * 24) > max_gap / period * 24) return(NULL)
      r <- tryCatch(.kk_cosinor_fit(y[idx], tt[idx], period, FALSE,
                                    conf.level)$row,
                    error = function(e) NULL)
      if (is.null(r)) return(NULL)
      tibble::add_column(r, day = k, .before = 1)
    })
    attr(out, "per_day") <- dplyr::bind_rows(pd)
  }
  out
}

# Identifier of the cycle: the calendar day for a 24-h period with clock
# time, else consecutive period-long windows from the phase origin.
.kk_cycle_id <- function(tt, info, period) {
  if (info$class == "POSIXct" && isTRUE(all.equal(period, 24))) {
    as.character(.kk_ts_day(info))
  } else {
    as.character(floor(tt / period))
  }
}

.kk_cosinor_fit <- function(y, tt, period, robust, conf.level) {
  w <- 2 * pi / period
  cs <- cos(w * tt)
  sn <- sin(w * tt)
  fit <- stats::lm(y ~ cs + sn)
  n <- length(y)
  if (n - 3 < 1) stop("Too few measurements for a cosinor.", call. = FALSE)
  V <- .kk_ts_vcov(fit, robust = robust)
  z <- stats::qt(1 - (1 - conf.level) / 2, df = n - 3)
  cf <- stats::coef(fit)
  if (anyNA(cf)) {
    stop("The cosinor design is rank deficient (too few distinct times ",
         "within the cycle).", call. = FALSE)
  }
  ap <- .kk_amp_phase(cf[["cs"]], cf[["sn"]], V[2:3, 2:3], period, z)
  # Zero-amplitude Wald test on (beta, gamma), F with 2 and n-3 df
  bg <- cf[c("cs", "sn")]
  Fst <- tryCatch(drop(t(bg) %*% solve(V[2:3, 2:3]) %*% bg) / 2,
                  error = function(e) NA_real_)
  p <- stats::pf(Fst, 2, n - 3, lower.tail = FALSE)
  reliable <- isTRUE(p < (1 - conf.level))
  se_m <- sqrt(V[1, 1])
  r2 <- summary(fit)$r.squared
  row <- tibble::tibble(
    n = n, period = period,
    mesor = cf[[1]], mesor_se = se_m,
    mesor_low = cf[[1]] - z * se_m, mesor_high = cf[[1]] + z * se_m,
    amplitude = ap$amplitude, amplitude_se = ap$amplitude_se,
    amplitude_low = ap$amplitude_low, amplitude_high = ap$amplitude_high,
    acrophase = ap$peak %% period, acrophase_se = ap$peak_se,
    acrophase_low = if (reliable) ap$peak_low %% period else NA_real_,
    acrophase_high = if (reliable) ap$peak_high %% period else NA_real_,
    acrophase_deg = ap$theta * 180 / pi,
    acrophase_traditional_deg = -ap$theta * 180 / pi,
    percent_rhythm = 100 * r2,
    f_statistic = Fst, p_value = p,
    acrophase_reliable = reliable,
    se_method = if (is.null(attr(V, "method"))) "OLS" else attr(V, "method"),
    conf.level = conf.level
  )

  list(row = row, fit = fit, V = V)
}

# ---- population-mean cosinor -----------------------------------------------

#' Population-Mean Cosinor (KK)
#'
#' @description The population-mean cosinor of Halberg and Bingham: a cosinor
#'   is fitted to each subject, and the subject-level coefficients
#'   \eqn{(M_i, \beta_i, \gamma_i)} are averaged. Inference treats the
#'   subjects, not the measurements, as the independent units, which is what
#'   generalises to the population the subjects were drawn from. Rhythm
#'   detection is Hotelling's \eqn{T^2} on the mean \eqn{(\beta, \gamma)};
#'   the amplitude and acrophase intervals use the delta method on the
#'   between-subject covariance.
#'
#'   Subjects can be in long format (`subject` column) or, as in CycleLab, one
#'   column per subject: pass `outcome = NULL` and the subject columns in
#'   `subjects`.
#'
#' @inheritParams kk_cosinor
#' @param subject Subject identifier column for long data.
#' @param subjects For wide data: a tidyselect specification of the subject
#'   columns (each column is one subject's record). Used when `outcome` is
#'   missing.
#'
#' @return One-row tibble with `k` (subjects), the population `mesor`,
#'   `amplitude` and `acrophase` with confidence limits, the Hotelling
#'   `f_statistic` and `p_value` for a non-zero rhythm, and `conf.level`.
#'   Attribute `subjects`: the per-subject cosinor parameters.
#'
#' @references Bingham C, Arbogast B, Cornelissen Guillaume G, Lee JK,
#'   Halberg F (1982) Inferential statistical methods for estimating and
#'   comparing cosinor parameters. *Chronobiologia* 9:397-439.
#'
#' @seealso [kk_cosinor()], [kk_cosinor_compare()]
#'
#' @examples
#' set.seed(2)
#' d <- do.call(rbind, lapply(1:8, function(i) {
#'   h <- seq(0, 72, by = 2)
#'   data.frame(id = i, hour = h,
#'              y = 70 + rnorm(1, 0, 3) + (8 + rnorm(1)) *
#'                cos(2 * pi * (h - 15 - rnorm(1)) / 24) + rnorm(length(h), 0, 3))
#' }))
#' kk_cosinor_population(d, y, hour, subject = id)
#'
#' @export
kk_cosinor_population <- function(data, outcome = NULL, time, subject = NULL,
                                  subjects = NULL, period = 24,
                                  conf.level = 0.95) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  t_name <- .kk_colname(rlang::enquo(time))
  out_q <- rlang::enquo(outcome)
  if (rlang::quo_is_null(out_q)) {
    cols <- names(tidyselect::eval_select(rlang::enquo(subjects), data))
    if (length(cols) < 3) {
      stop("Give at least 3 subject columns in `subjects`.", call. = FALSE)
    }
    long <- tidyr::pivot_longer(data[, c(t_name, cols)], dplyr::all_of(cols),
                                names_to = ".subject", values_to = ".y")
    y_name <- ".y"
    s_name <- ".subject"
    data <- long
  } else {
    y_name <- .kk_colname(out_q)
    s_name <- .kk_colname(rlang::enquo(subject))
  }
  data <- .kk_ts_prepare(data, c(y_name, t_name, s_name), numeric = y_name)
  per <- .kk_cosinor_subjects(data, y_name, t_name, s_name, period)
  k <- nrow(per)
  if (k < 3) stop("At least 3 subjects with usable records are needed.",
                  call. = FALSE)

  X <- as.matrix(per[, c("beta", "gamma")])
  mb <- colMeans(X)
  S <- stats::cov(X)
  tq <- stats::qt(1 - (1 - conf.level) / 2, df = k - 1)
  ap <- .kk_amp_phase(mb[1], mb[2], S / k, period, tq)
  Tsq <- tryCatch(k * drop(t(mb) %*% solve(S) %*% mb),
                  error = function(e) NA_real_)
  Fst <- (k - 2) / (2 * (k - 1)) * Tsq
  p <- stats::pf(Fst, 2, k - 2, lower.tail = FALSE)
  reliable <- isTRUE(p < (1 - conf.level))
  m <- mean(per$mesor)
  se_m <- stats::sd(per$mesor) / sqrt(k)

  out <- tibble::tibble(
    k = k, period = period,
    mesor = m, mesor_low = m - tq * se_m, mesor_high = m + tq * se_m,
    amplitude = ap$amplitude, amplitude_low = ap$amplitude_low,
    amplitude_high = ap$amplitude_high,
    acrophase = ap$peak %% period,
    acrophase_low = if (reliable) ap$peak_low %% period else NA_real_,
    acrophase_high = if (reliable) ap$peak_high %% period else NA_real_,
    acrophase_deg = ap$theta * 180 / pi,
    acrophase_traditional_deg = -ap$theta * 180 / pi,
    f_statistic = Fst, p_value = p,
    acrophase_reliable = reliable,
    conf.level = conf.level
  )
  attr(out, "subjects") <- per
  out
}

# One phase origin for all subjects: the phase time is computed on the whole
# data set before it is split, so that the subjects' coefficients are on the
# same phase scale and can be averaged.
.kk_cosinor_subjects <- function(data, y_name, t_name, s_name, period) {
  info <- .kk_ts_time(data[[t_name]])
  tt_all <- .kk_phase_time(data[[t_name]], info)
  ids <- unique(data[[s_name]])
  rows <- lapply(ids, function(id) {
    sel <- data[[s_name]] == id
    d <- data[sel, , drop = FALSE]
    if (nrow(d) < 5) return(NULL)
    tt <- tt_all[sel]
    w <- 2 * pi / period
    f <- stats::lm(d[[y_name]] ~ cos(w * tt) + sin(w * tt))
    cf <- unname(stats::coef(f))
    if (anyNA(cf)) return(NULL)

    tibble::tibble(subject = id, n = nrow(d), mesor = cf[1], beta = cf[2],
                   gamma = cf[3], amplitude = sqrt(cf[2]^2 + cf[3]^2),
                   acrophase = (atan2(cf[3], cf[2]) %% (2 * pi)) /
                     (2 * pi) * period)
  })
  dplyr::bind_rows(rows)
}

# ---- comparison of groups or stages ----------------------------------------

#' Compare Cosinor Parameters Between Groups or Stages (KK)
#'
#' @description Tests whether two or more groups — or stages of one record,
#'   such as before and after a change to night shifts — differ in MESOR,
#'   amplitude or acrophase. Two analyses are available, and they answer
#'   different questions:
#'
#'   * `method = "units"` — the coefficient-level comparison of Bingham et
#'     al. (1982), as in CycleLab. A cosinor is fitted to every independent
#'     unit — each subject when `subject` is given, otherwise each cycle
#'     (each day for clock time and a 24-h period) of each stage — and the
#'     unit coefficients \eqn{(M_i, \beta_i, \gamma_i)} are compared: the
#'     MESORs by one-way ANOVA, the rhythms \eqn{(\beta, \gamma)} jointly by
#'     Wilks' lambda (MANOVA), and the population amplitudes and acrophases
#'     of the groups by delta-method Wald tests on the between-unit
#'     covariances. Every unit has equal weight, and the inference
#'     generalises to the population of subjects or days. Units must cover
#'     the cycle (see `max_gap`).
#'   * `method = "pooled"` — one model for all measurements with a separate
#'     MESOR, cosine and sine coefficient per group,
#'     \deqn{y = M_g + \beta_g \cos(2\pi t/\tau) + \gamma_g \sin(2\pi t/\tau),}
#'     and Wald tests with Newey-West (or, with `subject`, cluster-robust)
#'     standard errors. The estimand is the measurement-weighted curve of
#'     each group: a subject or day with more measurements weighs more. Use
#'     it when there are too few units for the coefficient-level comparison.
#'
#'   The default `"auto"` uses `"units"` when every group has at least three
#'   usable units, otherwise `"pooled"`, and says which in the `method`
#'   column. Acrophase differences are wrapped to \eqn{(-\tau/2, \tau/2]}, so
#'   23:00 versus 01:00 is a 2-hour shift, not 22. Comparing acrophases is
#'   only meaningful when every group has a detectable rhythm; otherwise the
#'   acrophase test is `NA`.
#'
#' @inheritParams kk_cosinor
#' @param group Group or stage column (bare name or string).
#' @param subject Optional subject column: the units for `method = "units"`,
#'   the clusters for `method = "pooled"`.
#' @param method `"auto"` (default), `"units"` or `"pooled"`; see Description.
#' @param max_gap Coverage rule for a unit in `method = "units"` (default half
#'   the period), as in [kk_cosinor()].
#'
#' @return Tibble with one row per test (`mesor`, `rhythm`, `amplitude`,
#'   `acrophase`): `statistic`, `df`, `p_value`, `test_type` (the test used)
#'   and `method`. Attribute `groups`: the per-group MESOR, amplitude and
#'   acrophase with confidence limits (and the number of units); attribute
#'   `differences`: each group against the first, with the wrapped acrophase
#'   shift; attribute `units` (for `"units"`): the per-unit coefficients.
#'
#' @references Bingham C, Arbogast B, Cornelissen Guillaume G, Lee JK,
#'   Halberg F (1982) Inferential statistical methods for estimating and
#'   comparing cosinor parameters. *Chronobiologia* 9:397-439.
#'   Stefanov R (2026) *CycleLab: analysis of biomedical time series*.
#'   <https://cyclelab.raredis.org>
#'
#' @seealso [kk_cosinor()], [kk_cosinor_population()]
#'
#' @examples
#' set.seed(3)
#' h <- rep(seq(7, 23, by = 2.5), 28) + rep(24 * 0:27, each = 7)
#' stage <- ifelse(h < 14 * 24, "day shifts", "evening shifts")
#' peak <- ifelse(stage == "day shifts", 17, 20)
#' y <- 36.6 + 0.3 * cos(2 * pi * (h - peak) / 24) + rnorm(length(h), 0, 0.1)
#' res <- kk_cosinor_compare(data.frame(h, y, stage), y, h, stage)
#' res
#' attr(res, "differences")
#'
#' @export
kk_cosinor_compare <- function(data, outcome, time, group, subject = NULL,
                               period = 24, method = c("auto", "units", "pooled"),
                               conf.level = 0.95, max_gap = period / 2) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  method <- match.arg(method)
  y_name <- .kk_colname(rlang::enquo(outcome))
  t_name <- .kk_colname(rlang::enquo(time))
  g_name <- .kk_colname(rlang::enquo(group))
  s_q <- rlang::enquo(subject)
  s_name <- if (rlang::quo_is_null(s_q)) NULL else .kk_colname(s_q)
  data <- .kk_ts_prepare(data, c(y_name, t_name, g_name, s_name),
                         numeric = y_name)
  data <- data[order(data[[t_name]]), , drop = FALSE]

  info <- .kk_ts_time(data[[t_name]])
  tt <- .kk_phase_time(data[[t_name]], info)
  g <- factor(data[[g_name]])
  lv <- levels(g)
  G <- length(lv)
  if (G < 2) stop("`group` must have at least two levels.", call. = FALSE)
  y <- as.numeric(data[[y_name]])

  # unit-level coefficients (subjects, or cycles within each group)
  unit <- if (!is.null(s_name)) as.character(data[[s_name]]) else
    .kk_cycle_id(tt, info, period)
  uid <- paste(g, unit, sep = "\r")
  w <- 2 * pi / period
  units <- dplyr::bind_rows(lapply(split(seq_along(y), uid), function(idx) {
    if (length(idx) < 5) return(NULL)
    if (.kk_day_gap(tt[idx] / period * 24) > max_gap / period * 24) return(NULL)
    f <- stats::lm(y[idx] ~ cos(w * tt[idx]) + sin(w * tt[idx]))
    cf <- unname(stats::coef(f))
    if (anyNA(cf)) return(NULL)
    tibble::tibble(group = as.character(g[idx[1]]), unit = unit[idx[1]],
                   n = length(idx), mesor = cf[1], beta = cf[2], gamma = cf[3])
  }))
  k_g <- if (nrow(units)) table(factor(units$group, levels = lv)) else
    stats::setNames(rep(0, G), lv)
  if (method == "auto") method <- if (all(k_g >= 3)) "units" else "pooled"
  if (method == "units" && any(k_g < 3)) {
    stop("method = \"units\" needs at least 3 usable units (subjects or ",
         "cycles covering the cycle) in every group.", call. = FALSE)
  }
  z <- if (method == "units") NULL else stats::qnorm(1 - (1 - conf.level) / 2)

  if (method == "units") {
    res <- .kk_compare_units(units, lv, period, conf.level)
  } else {
    res <- .kk_compare_pooled(y, tt, g, lv, data, s_name, period, z)
  }
  out <- res$tests
  out$method <- method
  attr(out, "groups") <- res$groups
  attr(out, "differences") <- res$differences
  if (method == "units") attr(out, "units") <- units
  if (!is.null(res$model)) attr(out, "model") <- res$model
  out
}

# Delta-method amplitude and acrophase comparisons from named coefficients
# b_<level>, g_<level> and their covariance. `detected` says, per group,
# whether a rhythm was detected; acrophases are compared only if all were.
.kk_compare_ampphase <- function(cf, V, lv, period, z, detected) {
  G <- length(lv)
  grad <- function(j, what) {
    gr <- stats::setNames(rep(0, length(cf)), names(cf))
    b <- cf[[paste0("b_", lv[j])]]
    gg <- cf[[paste0("g_", lv[j])]]
    A2 <- b^2 + gg^2
    if (what == "A") {
      gr[paste0("b_", lv[j])] <- b / sqrt(A2)
      gr[paste0("g_", lv[j])] <- gg / sqrt(A2)
    } else {
      gr[paste0("b_", lv[j])] <- -gg / A2
      gr[paste0("g_", lv[j])] <- b / A2
    }
    gr
  }
  A_est <- vapply(seq_len(G), function(j) {
    sqrt(cf[[paste0("b_", lv[j])]]^2 + cf[[paste0("g_", lv[j])]]^2)
  }, numeric(1))
  T_est <- vapply(seq_len(G), function(j) {
    atan2(cf[[paste0("g_", lv[j])]], cf[[paste0("b_", lv[j])]]) %% (2 * pi)
  }, numeric(1))
  LA <- t(vapply(2:G, function(j) grad(1, "A") - grad(j, "A"), numeric(length(cf))))
  LT <- t(vapply(2:G, function(j) grad(1, "T") - grad(j, "T"), numeric(length(cf))))
  dA <- A_est[1] - A_est[-1]
  dT <- .kk_wrap(T_est[1] - T_est[-1])
  wald <- function(L, est) {
    Vl <- L %*% V %*% t(L)
    st <- tryCatch(drop(t(est) %*% solve(Vl) %*% est), error = function(e) NA_real_)
    c(st, nrow(L))
  }
  t_amp <- wald(LA, dA)
  t_phase <- if (all(detected)) wald(LT, dT) else c(NA_real_, G - 1)
  se_dA <- sqrt(pmax(0, diag(LA %*% V %*% t(LA))))
  se_dT <- sqrt(pmax(0, diag(LT %*% V %*% t(LT))))
  shift <- -dT / (2 * pi) * period
  se_shift <- se_dT / (2 * pi) * period
  ok_ph <- all(detected)
  list(t_amp = t_amp, t_phase = t_phase,
       differences = tibble::tibble(
         group = lv[-1], reference = lv[1],
         amplitude_diff = -dA,
         amplitude_diff_low = -dA - z * se_dA, amplitude_diff_high = -dA + z * se_dA,
         acrophase_shift = shift,
         acrophase_shift_low = if (ok_ph) shift - z * se_shift else NA_real_,
         acrophase_shift_high = if (ok_ph) shift + z * se_shift else NA_real_))
}

.kk_compare_units <- function(units, lv, period, conf.level) {
  G <- length(lv)
  ug <- factor(units$group, levels = lv)
  k <- nrow(units)
  # MESOR: one-way ANOVA on the unit MESORs
  av <- stats::anova(stats::lm(units$mesor ~ ug))
  # rhythm: Wilks' lambda on (beta, gamma)
  mv <- tryCatch(summary(stats::manova(cbind(units$beta, units$gamma) ~ ug),
                         test = "Wilks")$stats,
                 error = function(e) NULL)
  # per-group population estimates; independent groups -> block-diagonal V
  cf <- numeric(0)
  V <- matrix(0, 2 * G, 2 * G)
  nm <- c(paste0("b_", lv), paste0("g_", lv))
  dimnames(V) <- list(nm, nm)
  grp <- list()
  detected <- logical(G)
  for (j in seq_len(G)) {
    X <- as.matrix(units[ug == lv[j], c("beta", "gamma")])
    kj <- nrow(X)
    mb <- colMeans(X)
    S <- stats::cov(X) / kj
    cf[paste0("b_", lv[j])] <- mb[1]
    cf[paste0("g_", lv[j])] <- mb[2]
    ix <- c(paste0("b_", lv[j]), paste0("g_", lv[j]))
    V[ix, ix] <- S
    tq <- stats::qt(1 - (1 - conf.level) / 2, df = kj - 1)
    Tsq <- tryCatch(drop(t(mb) %*% solve(S) %*% mb), error = function(e) NA_real_)
    pz <- if (kj > 2) stats::pf((kj - 2) / (2 * (kj - 1)) * Tsq, 2, kj - 2,
                                lower.tail = FALSE) else NA_real_
    detected[j] <- isTRUE(pz < 1 - conf.level)
    ap <- .kk_amp_phase(mb[1], mb[2], S, period, tq)
    ms <- units$mesor[ug == lv[j]]
    sm <- stats::sd(ms) / sqrt(kj)
    grp[[j]] <- tibble::tibble(
      group = lv[j], units = kj, mesor = mean(ms),
      mesor_low = mean(ms) - tq * sm, mesor_high = mean(ms) + tq * sm,
      amplitude = ap$amplitude, amplitude_low = ap$amplitude_low,
      amplitude_high = ap$amplitude_high,
      acrophase = ap$peak %% period,
      acrophase_low = if (detected[j]) ap$peak_low %% period else NA_real_,
      acrophase_high = if (detected[j]) ap$peak_high %% period else NA_real_,
      rhythm_p = pz)
  }
  groups_tbl <- dplyr::bind_rows(grp)
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  ca <- .kk_compare_ampphase(cf, V, lv, period, z, detected)
  tests <- tibble::tibble(
    test = c("mesor", "rhythm", "amplitude", "acrophase"),
    statistic = c(av$`F value`[1], if (is.null(mv)) NA_real_ else mv[1, "approx F"],
                  ca$t_amp[1], ca$t_phase[1]),
    df = c(av$Df[1], if (is.null(mv)) NA_real_ else mv[1, "num Df"],
           ca$t_amp[2], ca$t_phase[2]),
    p_value = c(av$`Pr(>F)`[1], if (is.null(mv)) NA_real_ else mv[1, "Pr(>F)"],
                stats::pchisq(ca$t_amp[1], ca$t_amp[2], lower.tail = FALSE),
                stats::pchisq(ca$t_phase[1], ca$t_phase[2], lower.tail = FALSE)),
    test_type = c("one-way ANOVA of unit MESORs", "Wilks' lambda (MANOVA)",
                  "Wald, delta method", "Wald, delta method")
  )
  diffs <- ca$differences
  diffs <- tibble::add_column(diffs, mesor_diff = groups_tbl$mesor[-1] -
                                groups_tbl$mesor[1], .after = "reference")
  list(tests = tests, groups = groups_tbl, differences = diffs)
}

.kk_compare_pooled <- function(y, tt, g, lv, data, s_name, period, z) {
  G <- length(lv)
  w <- 2 * pi / period
  X <- stats::model.matrix(~ 0 + g)
  Xc <- X * cos(w * tt)
  Xs <- X * sin(w * tt)
  colnames(X) <- paste0("M_", lv)
  colnames(Xc) <- paste0("b_", lv)
  colnames(Xs) <- paste0("g_", lv)
  XX <- cbind(X, Xc, Xs)
  fit <- stats::lm(y ~ 0 + XX)
  names(fit$coefficients) <- colnames(XX)
  cl <- if (is.null(s_name)) NULL else data[[s_name]]
  V <- .kk_ts_vcov(fit, robust = TRUE, cluster = cl)
  dimnames(V) <- list(colnames(XX), colnames(XX))
  cf <- stats::coef(fit)

  detected <- logical(G)
  per <- lapply(seq_len(G), function(j) {
    ib <- paste0("b_", lv[j])
    ig <- paste0("g_", lv[j])
    Vj <- V[c(ib, ig), c(ib, ig)]
    bg <- cf[c(ib, ig)]
    Fz <- tryCatch(drop(t(bg) %*% solve(Vj) %*% bg), error = function(e) NA_real_)
    pz <- stats::pchisq(Fz, 2, lower.tail = FALSE)
    detected[j] <<- isTRUE(pz < 2 * stats::pnorm(-z))
    ap <- .kk_amp_phase(cf[[ib]], cf[[ig]], Vj, period, z)
    m <- cf[[paste0("M_", lv[j])]]
    sm <- sqrt(V[paste0("M_", lv[j]), paste0("M_", lv[j])])
    tibble::tibble(
      group = lv[j], n = sum(g == lv[j]),
      mesor = m, mesor_low = m - z * sm, mesor_high = m + z * sm,
      amplitude = ap$amplitude, amplitude_low = ap$amplitude_low,
      amplitude_high = ap$amplitude_high,
      acrophase = ap$peak %% period,
      acrophase_low = ap$peak_low %% period,
      acrophase_high = ap$peak_high %% period,
      rhythm_p = pz)
  })
  groups_tbl <- dplyr::bind_rows(per)
  groups_tbl$acrophase_low[!detected] <- NA_real_
  groups_tbl$acrophase_high[!detected] <- NA_real_

  wald <- function(L) {
    est <- drop(L %*% cf)
    st <- tryCatch(drop(t(est) %*% solve(L %*% V %*% t(L)) %*% est),
                   error = function(e) NA_real_)
    c(st, nrow(L))
  }
  contrast <- function(prefix) {
    L <- matrix(0, G - 1, length(cf), dimnames = list(NULL, names(cf)))
    for (j in 2:G) {
      L[j - 1, paste0(prefix, lv[1])] <- 1
      L[j - 1, paste0(prefix, lv[j])] <- -1
    }
    L
  }
  t_mesor <- wald(contrast("M_"))
  t_rhythm <- wald(rbind(contrast("b_"), contrast("g_")))
  ca <- .kk_compare_ampphase(cf, V, lv, period, z, detected)
  tests <- rbind(t_mesor, t_rhythm, ca$t_amp, ca$t_phase)
  se_lab <- if (is.null(attr(V, "method"))) "OLS" else attr(V, "method")
  out <- tibble::tibble(
    test = c("mesor", "rhythm", "amplitude", "acrophase"),
    statistic = tests[, 1], df = tests[, 2],
    p_value = stats::pchisq(tests[, 1], tests[, 2], lower.tail = FALSE),
    test_type = paste0("Wald chi-square, ", se_lab)
  )
  diffs <- tibble::add_column(ca$differences, mesor_diff = groups_tbl$mesor[-1] -
                                groups_tbl$mesor[1], .after = "reference")
  list(tests = out, groups = groups_tbl, differences = diffs, model = fit)
}
