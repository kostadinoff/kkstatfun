# ============================================================
# SEVERAL SERIES: CROSS-CORRELATION, GRANGER, PHASE, WAVELETS
# ============================================================

#' Compare Two to Four Time Series (KK)
#'
#' @description For every pair of series measured at the same times, asks
#'   whether they move together and which one leads:
#'
#'   * **Cross-correlation after prewhitening.** Two autocorrelated series
#'     correlate at many lags by chance. Both series are filtered with the
#'     autoregressive model fitted to the first (Box-Jenkins prewhitening), so
#'     the remaining cross-correlation reflects a genuine lagged relation. The
#'     lag with the largest absolute correlation is reported with a
#'     Bonferroni-adjusted p value over all lags examined. A positive lag
#'     means the first series leads.
#'   * **Granger causality in both directions.** Does the past of one series
#'     improve the prediction of the other beyond its own past? F test of the
#'     lagged terms. Both nested models contain the same deterministic terms
#'     — a linear trend and, when `period` is given, the common rhythm (cosine
#'     and sine of `period`) — because a shared rhythm or trend alone can make
#'     one series appear to predict the other. The lag order is chosen by AIC,
#'     all orders compared on one common sample; the F test after this
#'     selection is approximate. "Granger causality" is predictive
#'     precedence, not causation.
#'   * **Phase shift** (when `period` is given): the difference between the
#'     cosinor peak times of the two series, wrapped to half a period, with a
#'     delta-method interval from the *joint* Newey-West covariance of both
#'     cosinors, so that the correlation between series measured together
#'     is taken into account.
#'   * **Common cycles**: each series is searched with [kk_periodogram()];
#'     periods confirmed in more than one series (within the frequency
#'     resolution) are listed with every series' amplitude and peak, and the
#'     pairwise phase shifts at those periods (attribute `common_shifts`).
#'   * **Coherence** from the smoothed cross-periodogram (modified Daniell
#'     smoother), with the approximate *pointwise* significance threshold of
#'     squared coherence; and the lagged regression \eqn{y = A x + B} at
#'     successive lags with \eqn{R/S_R} and the critical correlation.
#'
#' @param data Data frame, one row per time point, equally spaced.
#' @param ... Two to four series columns (tidyselect).
#' @param time Time column.
#' @param max_lag Largest lag examined (default `min(n/4, 10*log10(n))`).
#' @param period Optional common period for the phase shift (hours for
#'   `POSIXct`).
#' @param n_sim Surrogates for the periodogram of each series used to find
#'   the common cycles (default 200; 0 skips them).
#' @param conf.level Confidence level (default 0.95).
#' @param seed Random seed (default 1).
#'
#' @return Tibble with one row per pair: `x`, `y`, `n`, `ccf_lag`, `ccf`,
#'   `ccf_p` (Bonferroni), `granger_x_to_y_F`, `granger_x_to_y_p`,
#'   `granger_y_to_x_F`, `granger_y_to_x_p`, `granger_lags`, and with `period`
#'   `phase_shift` (peak of `y` minus peak of `x`), `phase_shift_low`,
#'   `phase_shift_high`. Attributes: `ccf` (the full prewhitened
#'   cross-correlation), `coherence` (smoothed-periodogram coherence and
#'   phase by period, with the pointwise significance threshold),
#'   `lag_regression` (`y = A x + B` at each lag, with `r`, `R_SR`,
#'   `r_critical`), `common_cycles` (periods detected by [kk_periodogram()]
#'   in more than one series, with each series' amplitude and peak time) and
#'   `common_shifts` (pairwise phase shifts at the common periods).
#'
#' @references Box GEP, Jenkins GM, Reinsel GC (2015) *Time Series Analysis:
#'   Forecasting and Control*, 5th ed., chapter 12. Wiley.
#'   Granger CWJ (1969) Investigating causal relations by econometric models
#'   and cross-spectral methods. *Econometrica* 37:424-438.
#'   \doi{10.2307/1912791}
#'   Bloomfield P (2000) *Fourier Analysis of Time Series: An Introduction*,
#'   2nd ed. Wiley. \doi{10.1002/0471722235}
#'   Stefanov R (2026) *CycleLab: analysis of biomedical time series*.
#'   <https://cyclelab.raredis.org>
#'
#' @seealso [kk_wavelet_coherence()], [kk_cosinor()]
#'
#' @examples
#' set.seed(7)
#' h <- seq(0, 72, by = 1/3)
#' d <- data.frame(h = h,
#'   temp = 36.7 + 0.3 * cos(2 * pi * (h - 17) / 24) + rnorm(length(h), 0, 0.08),
#'   cortisol = 10 + 6 * cos(2 * pi * (h - 8) / 24) + rnorm(length(h), 0, 1.5),
#'   pulse = 70 + 6 * cos(2 * pi * (h - 15) / 24) + rnorm(length(h), 0, 3))
#' kk_ts_compare(d, temp, cortisol, pulse, time = h, period = 24)
#'
#' @export
kk_ts_compare <- function(data, ..., time, max_lag = NULL, period = NULL,
                          n_sim = 200, conf.level = 0.95, seed = 1) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  cols <- names(tidyselect::eval_select(rlang::expr(c(...)), data))
  if (length(cols) < 2 || length(cols) > 4) {
    stop("Give between 2 and 4 series.", call. = FALSE)
  }
  t_name <- .kk_colname(rlang::enquo(time))
  data <- .kk_ts_prepare(data, c(cols, t_name), numeric = cols)
  data <- data[order(data[[t_name]]), , drop = FALSE]
  info <- .kk_ts_time(data[[t_name]])
  .kk_ts_check_regular(info$t, "kk_ts_compare()")
  n <- nrow(data)
  if (n < 20) stop("At least 20 time points are needed.", call. = FALSE)
  if (is.null(max_lag)) max_lag <- max(1, floor(min(n / 4, 10 * log10(n))))
  if (max_lag < 1 || max_lag >= n / 2) {
    stop("`max_lag` must be between 1 and n/2.", call. = FALSE)
  }
  tt <- .kk_phase_time(data[[t_name]], info)
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  # deterministic terms shared by both Granger models
  det <- cbind(trend = info$t - min(info$t))
  if (!is.null(period)) {
    det <- cbind(det, cos_p = cos(2 * pi * tt / period),
                 sin_p = sin(2 * pi * tt / period))
  }

  pairs <- utils::combn(cols, 2, simplify = FALSE)
  ccfs <- list()
  rows <- lapply(pairs, function(pr) {
    x <- as.numeric(data[[pr[1]]])
    y <- as.numeric(data[[pr[2]]])
    # prewhiten with the AR model of x
    arx <- stats::ar(x, order.max = min(10, floor(n / 5)), aic = TRUE)
    if (arx$order > 0) {
      xf <- stats::filter(x - mean(x), c(1, -arx$ar), sides = 1)
      yf <- stats::filter(y - mean(y), c(1, -arx$ar), sides = 1)
    } else {
      xf <- x - mean(x)
      yf <- y - mean(y)
    }
    ok <- !is.na(xf) & !is.na(yf)
    cc <- stats::ccf(as.numeric(yf[ok]), as.numeric(xf[ok]), lag.max = max_lag,
                     plot = FALSE)
    # ccf(y, x) at lag k estimates cor(y[t+k], x[t]): k > 0 means x leads
    lags <- drop(cc$lag)
    r <- drop(cc$acf)
    j <- which.max(abs(r))
    neff <- sum(ok)
    p <- min(1, length(r) * 2 * stats::pnorm(-abs(r[j]) * sqrt(neff)))
    ccfs[[paste(pr, collapse = " ~ ")]] <<- tibble::tibble(
      x = pr[1], y = pr[2], lag = lags, ccf = r, bound = z / sqrt(neff))

    gr <- .kk_granger(x, y, max_lag, det)
    row <- tibble::tibble(
      x = pr[1], y = pr[2], n = n, ccf_lag = lags[j], ccf = r[j], ccf_p = p,
      granger_x_to_y_F = gr$xy[1], granger_x_to_y_p = gr$xy[2],
      granger_y_to_x_F = gr$yx[1], granger_y_to_x_p = gr$yx[2],
      granger_lags = gr$p
    )
    if (!is.null(period)) {
      js <- .kk_joint_phase(x, y, tt, period, z)
      row$phase_shift <- js$shift
      row$phase_shift_low <- js$low
      row$phase_shift_high <- js$high
    }
    row
  })
  out <- dplyr::bind_rows(rows)
  attr(out, "ccf") <- dplyr::bind_rows(ccfs)

  # smoothed-periodogram (modified Daniell) coherence and phase for every pair
  spans <- max(3, 2 * floor(sqrt(n) / 4) + 1)
  attr(out, "coherence") <- dplyr::bind_rows(lapply(pairs, function(pr) {
    sp <- stats::spec.pgram(cbind(data[[pr[1]]], data[[pr[2]]]),
                            spans = c(spans, spans), taper = 0.1,
                            detrend = TRUE, plot = FALSE)
    df_s <- sp$df
    # significance of squared coherence (Koopmans): 1 - alpha^(2/(df-2))
    thr <- 1 - (1 - conf.level)^(2 / max(1, df_s - 2))
    dtt <- stats::median(diff(info$t))
    tibble::tibble(x = pr[1], y = pr[2], period = dtt / sp$freq,
                   coherence = drop(sp$coh), phase = drop(sp$phase),
                   threshold = thr)
  }))

  # lagged linear regression y = A x + B (phase-correlation analysis)
  attr(out, "lag_regression") <- dplyr::bind_rows(lapply(pairs, function(pr) {
    x <- as.numeric(data[[pr[1]]])
    y <- as.numeric(data[[pr[2]]])
    dplyr::bind_rows(lapply(-max_lag:max_lag, function(l) {
      if (l >= 0) {
        xx <- x[seq_len(n - l)]; yy <- y[(1 + l):n]
      } else {
        xx <- x[(1 - l):n]; yy <- y[seq_len(n + l)]
      }
      f <- summary(stats::lm(yy ~ xx))
      nl <- length(xx)
      r <- stats::cor(xx, yy)
      tq <- stats::qt(1 - (1 - conf.level) / 2, nl - 2)
      tibble::tibble(x = pr[1], y = pr[2], lag = l, n = nl,
                     A = f$coefficients[2, 1], B = f$coefficients[1, 1],
                     r = r, r_squared = f$r.squared,
                     R_SR = abs(r) / ((1 - r^2) / sqrt(nl)),
                     r_critical = tq / sqrt(tq^2 + nl - 2),
                     p.value = f$coefficients[2, 4])
    }))
  }))

  # common cycles: periods found in more than one series
  if (n_sim > 0) {
    span <- diff(range(info$t))
    found <- dplyr::bind_rows(lapply(cols, function(cl) {
      pg <- suppressWarnings(kk_periodogram(data, !!cl, !!t_name,
                                            n_sim = n_sim, seed = seed))
      pg <- pg[pg$in_model, , drop = FALSE]
      if (!nrow(pg)) return(NULL)
      tibble::tibble(series = cl, period = pg$model_period)
    }))
    common <- NULL
    shifts <- NULL
    if (!is.null(found) && nrow(found) >= 2) {
      fr <- 1 / found$period
      grp <- stats::cutree(stats::hclust(stats::dist(fr), "single"), h = 1 / span)
      found$cluster <- grp
      keep <- names(which(tapply(found$series, grp, function(s) length(unique(s))) > 1))
      common <- dplyr::bind_rows(lapply(keep, function(g) {
        P <- stats::median(found$period[found$cluster == as.integer(g)])
        per_s <- dplyr::bind_rows(lapply(cols, function(cl) {
          f <- .kk_cosinor_fit(as.numeric(data[[cl]]), tt, P, TRUE, conf.level)$row
          tibble::tibble(period = P, series = cl,
                         detected = cl %in% found$series[found$cluster == as.integer(g)],
                         amplitude = f$amplitude, peak = f$acrophase,
                         peak_se = f$acrophase_se)
        }))
        per_s
      }))
      if (!is.null(common) && nrow(common)) {
        shifts <- dplyr::bind_rows(lapply(unique(common$period), function(P) {
          dplyr::bind_rows(lapply(pairs, function(pr) {
            js <- .kk_joint_phase(as.numeric(data[[pr[1]]]),
                                  as.numeric(data[[pr[2]]]), tt, P, z)
            tibble::tibble(period = P, x = pr[1], y = pr[2],
                           phase_shift = js$shift, phase_shift_low = js$low,
                           phase_shift_high = js$high)
          }))
        }))
      }
    }
    attr(out, "common_cycles") <- common
    attr(out, "common_shifts") <- shifts
  }
  out
}

# Phase shift (peak of y minus peak of x) at a common period, with the joint
# Newey-West covariance of the two cosinor fits (same design, correlated
# errors), so that series measured together are not treated as independent.
.kk_joint_phase <- function(x, y, tt, period, z) {
  n <- length(x)
  w <- 2 * pi / period
  X <- cbind(1, cos(w * tt), sin(w * tt))
  Qi <- solve(crossprod(X) / n)
  bx <- drop(Qi %*% crossprod(X, x) / n)
  by <- drop(Qi %*% crossprod(X, y) / n)
  ex <- x - drop(X %*% bx)
  ey <- y - drop(X %*% by)
  Sc <- cbind(X * ex, X * ey)
  L <- floor(4 * (n / 100)^(2 / 9))
  S <- crossprod(Sc) / n
  if (L > 0) for (l in seq_len(L)) {
    G <- crossprod(Sc[(l + 1):n, , drop = FALSE], Sc[seq_len(n - l), , drop = FALSE]) / n
    S <- S + (1 - l / (L + 1)) * (G + t(G))
  }
  A <- kronecker(diag(2), Qi)
  V <- A %*% S %*% A / n
  ax2 <- bx[2]^2 + bx[3]^2
  ay2 <- by[2]^2 + by[3]^2
  thx <- atan2(bx[3], bx[2])
  thy <- atan2(by[3], by[2])
  d <- .kk_wrap(thy - thx)
  g <- c(0, bx[3] / ax2, -bx[2] / ax2, 0, -by[3] / ay2, by[2] / ay2)
  se <- sqrt(max(0, drop(t(g) %*% V %*% g)))
  shift <- d / (2 * pi) * period
  ses <- se / (2 * pi) * period
  list(shift = shift, low = shift - z * ses, high = shift + z * ses)
}

# Granger F tests in both directions. `det` holds deterministic terms (trend,
# common rhythm) included in both nested models. All lag orders are compared
# by AIC on the same sample (from max_lag + 1), with enough residual df.
.kk_granger <- function(x, y, max_lag, det = NULL) {
  n <- length(x)
  lagm <- function(v, p) {
    m <- sapply(seq_len(p), function(k) c(rep(NA, k), v[seq_len(n - k)]))
    matrix(m, nrow = n)
  }
  nd <- if (is.null(det)) 0 else ncol(det)
  rows <- (max_lag + 1):n
  p_max <- max(1, min(max_lag, floor((length(rows) - nd - 1 - 10) / 2)))
  build <- function(target, other, p) {
    d <- data.frame(yy = target, own = lagm(target, p), oth = lagm(other, p))
    if (nd) d <- cbind(d, as.data.frame(det))
    d[rows, , drop = FALSE]
  }
  pick <- function(target, other) {
    aics <- vapply(seq_len(p_max), function(p) {
      stats::AIC(stats::lm(yy ~ ., data = build(target, other, p)))
    }, numeric(1))
    which.min(aics)
  }
  p <- max(pick(y, x), pick(x, y))
  test <- function(target, other) {
    d <- build(target, other, p)
    full <- stats::lm(yy ~ ., data = d)
    red_terms <- setdiff(names(d), c("yy", grep("^oth", names(d), value = TRUE)))
    red <- stats::lm(stats::reformulate(red_terms, "yy"), data = d)
    a <- stats::anova(red, full)
    c(a$F[2], a$`Pr(>F)`[2])
  }
  list(xy = test(y, x), yx = test(x, y), p = p)
}

# ---- continuous wavelet transform (Morlet) --------------------------------

.kk_cwt <- function(x, dt, dj, s0, J, w0 = 6) {
  n <- length(x)
  x <- x - mean(x)
  npad <- 2^ceiling(log2(n)) * 2
  xp <- c(x, rep(0, npad - n))
  k <- seq_len(npad / 2) * (2 * pi) / (npad * dt)
  k <- c(0, k, -rev(k[seq_len((npad - 1) / 2)]))
  k <- k[seq_len(npad)]
  f <- stats::fft(xp)
  scales <- s0 * 2^((0:J) * dj)
  W <- matrix(0 + 0i, length(scales), n)
  for (j in seq_along(scales)) {
    s <- scales[j]
    daughter <- pi^(-0.25) * sqrt(2 * pi * s / dt) *
      exp(-((s * k - w0)^2) / 2) * (k > 0)
    W[j, ] <- stats::fft(f * daughter, inverse = TRUE)[seq_len(n)] / npad
  }
  fourier <- 4 * pi / (w0 + sqrt(2 + w0^2))
  coi <- fourier / sqrt(2) * dt *
    pmin(seq_len(n) - 1, n - seq_len(n))
  list(W = W, scales = scales, period = fourier * scales, coi = coi)
}

.kk_wt_grid <- function(n, dt, dj, s0, max_period) {
  if (is.null(s0)) s0 <- 2 * dt
  fourier <- 4 * pi / (6 + sqrt(2 + 36))
  smax <- if (is.null(max_period)) n * dt / 3 else max_period / fourier
  J <- floor(log2(smax / s0) / dj)
  list(s0 = s0, J = J)
}

#' Wavelet Power Spectrum (KK)
#'
#' @description Continuous Morlet wavelet transform: shows which periods are
#'   present *when*. Where the periodogram assumes a cycle that is stable
#'   through the record, the wavelet spectrum shows cycles that appear,
#'   vanish or drift — a rhythm disturbed by night shifts, a seasonal pattern
#'   that strengthens. Significance is against AR(1) red noise with the
#'   record's lag-1 autocorrelation (Torrence & Compo 1998), and values inside
#'   the cone of influence, where edge effects distort the transform, are
#'   flagged.
#'
#' @inheritParams kk_periodogram
#' @param dj Spacing between scales in octaves (default 1/12).
#' @param max_period Largest period examined (default a third of the record).
#' @param conf.level Confidence level of the significance contour (default
#'   0.95).
#'
#' @return Long tibble with `time`, `t`, `period`, `power` (normalised by the
#'   variance), `significant` and `in_coi`. Attribute `global`: the
#'   time-averaged spectrum outside the cone of influence, with its
#'   significance threshold.
#'
#' @references Torrence C, Compo GP (1998) A practical guide to wavelet
#'   analysis. *Bulletin of the American Meteorological Society* 79:61-78.
#'   \doi{10.1175/1520-0477(1998)079<0061:APGTWA>2.0.CO;2}
#'
#' @seealso [kk_wavelet_coherence()], [kk_periodogram()],
#'   [kk_plot_wavelet()]
#'
#' @examples
#' set.seed(8)
#' h <- seq(0, 24 * 28, by = 1)
#' peak <- ifelse(h < 24 * 14, 17, 21)
#' d <- data.frame(h = h, y = cos(2 * pi * (h - peak) / 24) + rnorm(length(h), 0, 0.7))
#' w <- kk_wavelet(d, y, h)
#' head(attr(w, "global"))
#'
#' @export
kk_wavelet <- function(data, outcome, time, dj = 1 / 12, max_period = NULL,
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
  .kk_ts_check_regular(t, "kk_wavelet()")
  dt <- stats::median(diff(t))
  y <- as.numeric(data[[y_name]])
  n <- length(y)
  if (n < 16) stop("At least 16 equally spaced points are needed.", call. = FALSE)
  if (stats::sd(y) == 0) stop("The series is constant.", call. = FALSE)
  g <- .kk_wt_grid(n, dt, dj, NULL, max_period)
  cw <- .kk_cwt(y, dt, dj, g$s0, g$J)
  v <- stats::var(y)
  P <- Mod(cw$W)^2 / v
  phi <- stats::acf(y, lag.max = 1, plot = FALSE)$acf[2]
  fr <- dt / cw$period
  red <- (1 - phi^2) / (1 + phi^2 - 2 * phi * cos(2 * pi * fr))
  thr <- red * stats::qchisq(conf.level, 2) / 2
  out <- tibble::tibble(
    time = rep(data[[t_name]], each = length(cw$period)),
    t = rep(t, each = length(cw$period)),
    period = rep(cw$period, times = n),
    power = as.vector(P),
    significant = as.vector(P > thr),
    in_coi = as.vector(outer(cw$period, cw$coi, ">"))
  )
  outside <- !outer(cw$period, cw$coi, ">")
  Pm <- P
  Pm[!outside] <- NA
  navg <- rowSums(outside)
  gws <- rowMeans(Pm, na.rm = TRUE)
  dof <- pmax(1, sqrt(1 + (navg * dt / (2.32 * cw$scales))^2))
  attr(out, "global") <- tibble::tibble(
    period = cw$period, power = gws,
    threshold = red * stats::qchisq(conf.level, 2 * dof) / (2 * dof),
    n_outside_coi = navg)
  out
}

# smoothing for coherence: Gaussian in time (sd = scale), boxcar in scale
.kk_wt_smooth <- function(M, scales, dt, dj) {
  n <- ncol(M)
  out <- M
  for (j in seq_len(nrow(M))) {
    sd_pts <- scales[j] / dt
    half <- min(n - 1, ceiling(3 * sd_pts))
    kern <- stats::dnorm(-half:half, 0, sd_pts)
    kern <- kern / sum(kern)
    row <- M[j, ]
    conv <- function(v) {
      vv <- c(rep(0, half), v, rep(0, half))
      as.numeric(stats::filter(vv, kern, sides = 2))[half + seq_len(n)]
    }
    wts <- conv(rep(1, n))
    out[j, ] <- if (is.complex(row)) {
      complex(real = conv(Re(row)), imaginary = conv(Im(row))) / wts
    } else conv(row) / wts
  }
  width <- max(1, round(0.6 / dj))
  if (width > 1) {
    out2 <- out
    for (j in seq_len(nrow(out))) {
      idx <- max(1, j - width %/% 2):min(nrow(out), j + width %/% 2)
      out2[j, ] <- colMeans(out[idx, , drop = FALSE])
    }
    out <- out2
  }
  out
}

.kk_wcoh <- function(x, y, dt, dj, g) {
  wx <- .kk_cwt(x, dt, dj, g$s0, g$J)
  wy <- .kk_cwt(y, dt, dj, g$s0, g$J)
  sinv <- 1 / wx$scales
  Wxy <- wx$W * Conj(wy$W)
  Sxy <- .kk_wt_smooth(Wxy * sinv, wx$scales, dt, dj)
  Sx <- .kk_wt_smooth(Mod(wx$W)^2 * sinv, wx$scales, dt, dj)
  Sy <- .kk_wt_smooth(Mod(wy$W)^2 * sinv, wx$scales, dt, dj)
  list(R2 = Mod(Sxy)^2 / (Sx * Sy), phase = Arg(Sxy), period = wx$period,
       coi = wx$coi,
       cross = Mod(Wxy) / (stats::sd(x) * stats::sd(y)))
}

#' Wavelet Coherence of Two Series (KK)
#'
#' @description Where and at which periods two series oscillate together,
#'   and with what phase. Wavelet coherence is a localised correlation
#'   coefficient (0-1) in the time-period plane (Grinsted et al. 2004); the
#'   phase angle says which series leads (positive: `x` leads `y`). The
#'   significance level is estimated by Monte Carlo from pairs of independent
#'   AR(1) series with the records' own lag-1 autocorrelations: at each
#'   period the coherence values outside the cone of influence are pooled
#'   over all simulated pairs, and their `conf.level` quantile is the
#'   threshold (Grinsted et al. 2004). It is a *pointwise* level — about 5%
#'   of the map exceeds it by chance — not a map-wide test. The cross-wavelet
#'   power \eqn{|W_{xy}|}, normalised by the two SDs, is returned too: large
#'   common power, whether or not the series are coherent.
#'
#' @param data Data frame, one row per time point, equally spaced.
#' @param x,y The two series (bare names or strings).
#' @param time Time column.
#' @param dj Scale spacing in octaves (default 1/12).
#' @param max_period Largest period examined (default a third of the record).
#' @param n_sim Monte Carlo pairs for the significance level (default 100;
#'   0 skips the test).
#' @param conf.level Confidence level (default 0.95).
#' @param seed Optional random seed.
#'
#' @return Long tibble with `time`, `t`, `period`, `coherence`, `phase`
#'   (radians), `lead` (phase converted to time: how far `x` leads `y`),
#'   `cross_power`, `significant` (pointwise) and `in_coi`. Attribute
#'   `summary`: mean coherence, the threshold and circular-mean lead per
#'   period outside the cone of influence (`NA` threshold where too few
#'   values lie outside it).
#'
#' @references Grinsted A, Moore JC, Jevrejeva S (2004) Application of the
#'   cross wavelet transform and wavelet coherence to geophysical time series.
#'   *Nonlinear Processes in Geophysics* 11:561-566.
#'   \doi{10.5194/npg-11-561-2004}
#'
#' @seealso [kk_wavelet()], [kk_ts_compare()]
#'
#' @examples
#' set.seed(9)
#' h <- seq(0, 24 * 10, by = 1)
#' d <- data.frame(h = h,
#'   a = cos(2 * pi * h / 24) + rnorm(length(h), 0, 0.5),
#'   b = cos(2 * pi * (h - 3) / 24) + rnorm(length(h), 0, 0.5))
#' wc <- kk_wavelet_coherence(d, a, b, h, n_sim = 20, seed = 1)
#' attr(wc, "summary")[which.max(attr(wc, "summary")$coherence), ]
#'
#' @export
kk_wavelet_coherence <- function(data, x, y, time, dj = 1 / 12,
                                 max_period = NULL, n_sim = 100,
                                 conf.level = 0.95, seed = NULL) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  x_name <- .kk_colname(rlang::enquo(x))
  y_name <- .kk_colname(rlang::enquo(y))
  t_name <- .kk_colname(rlang::enquo(time))
  data <- .kk_ts_prepare(data, c(x_name, y_name, t_name),
                         numeric = c(x_name, y_name))
  data <- data[order(data[[t_name]]), , drop = FALSE]
  if (!is.null(seed)) set.seed(seed)
  info <- .kk_ts_time(data[[t_name]])
  t <- info$t
  .kk_ts_check_regular(t, "kk_wavelet_coherence()")
  dt <- stats::median(diff(t))
  xv <- as.numeric(data[[x_name]])
  yv <- as.numeric(data[[y_name]])
  n <- length(xv)
  if (n < 16) stop("At least 16 equally spaced points are needed.", call. = FALSE)
  g <- .kk_wt_grid(n, dt, dj, NULL, max_period)
  wc <- .kk_wcoh(xv, yv, dt, dj, g)
  thr <- rep(NA_real_, length(wc$period))
  incoi <- outer(wc$period, wc$coi, ">")
  if (n_sim > 0) {
    sx <- .kk_ar1_sims(xv, n_sim)
    sy <- .kk_ar1_sims(yv, n_sim)
    # pool, per period, the simulated values outside the cone of influence
    pool <- vector("list", length(wc$period))
    for (i in seq_len(n_sim)) {
      r <- .kk_wcoh(sx[, i], sy[, i], dt, dj, g)$R2
      for (j in seq_along(pool)) pool[[j]] <- c(pool[[j]], r[j, !incoi[j, ]])
    }
    thr <- vapply(pool, function(v) {
      v <- v[is.finite(v)]
      if (length(v) < 20) NA_real_ else stats::quantile(v, conf.level, names = FALSE)
    }, numeric(1))
  }
  out <- tibble::tibble(
    time = rep(data[[t_name]], each = length(wc$period)),
    t = rep(t, each = length(wc$period)),
    period = rep(wc$period, times = n),
    coherence = as.vector(wc$R2),
    phase = as.vector(wc$phase),
    lead = as.vector(wc$phase / (2 * pi) * wc$period),
    cross_power = as.vector(wc$cross),
    significant = as.vector(wc$R2 > thr),

    in_coi = as.vector(incoi)
  )
  R <- wc$R2
  R[incoi] <- NA
  Ph <- wc$phase
  Ph[incoi] <- NA
  mph <- atan2(rowMeans(sin(Ph), na.rm = TRUE), rowMeans(cos(Ph), na.rm = TRUE))
  attr(out, "summary") <- tibble::tibble(
    period = wc$period, coherence = rowMeans(R, na.rm = TRUE),
    threshold = thr, mean_phase = mph, mean_lead = mph / (2 * pi) * wc$period)
  out
}
