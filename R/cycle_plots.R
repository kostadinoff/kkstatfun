# ============================================================
# PLOTS FOR THE CYCLES AND RHYTHMS TIER
# ============================================================
# House style: kkplot() axes on the Tufte theme that set_plot_font()
# installs. When the session still has ggplot2's grey default (the user has
# not called set_plot_font()), the same Tufte theme is added here, so the
# figures look like kkplots out of the box. Titles state the finding,
# subtitles say how to read the chart, context is grey, the focus is in the
# accent colour, and lines are labelled directly instead of by a legend.

.kk_or_num <- function(a, b) if (is.null(a)) b else a

# Direct labels in the font of the active theme (text geoms do not inherit
# it, and would otherwise fall back to the device's default serif face).
.kk_txt_family <- function() {
  f <- ggplot2::theme_get()$text$family
  if (is.null(f)) "" else f
}

.kk_cyc_cols <- function() {
  p <- kk_pal(3)
  list(focus = p[2], accent = p[1], third = p[3], grey = "grey55",
       light = "grey85")
}

.kk_default_theme_active <- function() {
  cur <- ggplot2::theme_get()$panel.background
  def <- ggplot2::theme_grey()$panel.background
  !is.null(cur) && !is.null(def) && identical(cur$fill, def$fill)
}

# White page always: theme_tufte() leaves the background blank, which turns
# transparent in PNGs and reads as black in dark viewers.
.kk_white_bg <- function() {
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    legend.background = ggplot2::element_rect(fill = "white", colour = NA),
    strip.background = ggplot2::element_rect(fill = "white", colour = NA)
  )
}

.kk_cyc_theme <- function() {
  if (!.kk_default_theme_active()) return(.kk_white_bg())
  size <- 12
  ggthemes::theme_tufte(base_size = size, base_family = "") +
    ggplot2::theme(
      axis.ticks = ggplot2::element_line(linewidth = size / 60, colour = "black"),
      axis.ticks.length = ggplot2::unit(size * 3 / 16, "mm"),
      axis.line = ggplot2::element_line(linewidth = size / 40),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0,
                                         margin = ggplot2::margin(b = 2)),
      plot.subtitle = ggplot2::element_text(size = size * 0.92, colour = "grey30",
                                            margin = ggplot2::margin(b = 6)),
      plot.caption = ggplot2::element_text(size = size * 0.7, colour = "grey40",
                                           hjust = 0),
      plot.title.position = "plot", plot.caption.position = "plot",
      panel.grid = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      legend.key = ggplot2::element_blank()
    ) + .kk_white_bg()
}

.kk_wrap_sub <- function(x, width = 95) paste(strwrap(x, width), collapse = "\n")

#' Plot a T-R Periodogram (KK)
#'
#' @description The T-R periodogram from [kk_periodogram()]: \eqn{R/S_R}
#'   against the period (focus line), the family-wise red-noise threshold
#'   (accent) and the naive 1.96 line (dashed grey), each labelled at its
#'   end. Peaks above the dashed line are not necessarily cycles; only the
#'   peaks accepted by the red-noise test (labelled points) are. The title
#'   names the cycles found.
#'
#' @param res Result of [kk_periodogram()].
#' @param log_period Logarithmic period axis (default `TRUE`).
#' @param title Optional title; by default it states the finding.
#'
#' @return A ggplot object.
#' @seealso [kk_periodogram()]
#' @examples
#' pn <- kk_cycle_example("pneumonia")
#' kk_plot_periodogram(kk_periodogram(pn$series, rate, month, n_sim = 100))
#' @export
kk_plot_periodogram <- function(res, log_period = TRUE, title = NULL) {
  sp <- attr(res, "spectrum")
  if (is.null(sp)) stop("`res` must come from kk_periodogram().", call. = FALSE)
  cl <- .kk_cyc_cols()
  acc <- res[res$in_model, , drop = FALSE]
  unit <- attr(res, "time_unit")
  if (is.null(title)) {
    title <- if (nrow(acc)) {
      sprintf("Cycles found at %s %s",
              paste(signif(sort(acc$model_period, decreasing = TRUE), 3),
                    collapse = " and "), unit)
    } else "No cycle stands out from autocorrelated noise"
  }
  sp <- sp[order(sp$period), ]
  xr <- max(sp$period)
  p <- kkplot(sp, ggplot2::aes(x = .data$period, y = .data$R_SR)) +
    ggplot2::geom_hline(yintercept = 1.96, linetype = "dashed", colour = cl$grey,
                        linewidth = 0.35) +
    ggplot2::geom_line(ggplot2::aes(y = .data$threshold), colour = cl$accent,
                       linewidth = 0.5) +
    ggplot2::geom_line(colour = cl$focus, linewidth = 0.55) +
    ggplot2::annotate("text", family = .kk_txt_family(), x = xr, y = utils::tail(sp$threshold, 1),
                      label = " red-noise\n threshold", hjust = 0, size = 3.2,
                      colour = cl$accent, lineheight = 0.9) +
    ggplot2::annotate("text", family = .kk_txt_family(), x = xr, y = 1.96, label = " 1.96", hjust = 0,
                      vjust = -0.5, size = 3.2, colour = cl$grey) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = paste0("Period (", unit, ")"),
                  y = expression(R / S[R]), title = title,
                  subtitle = .kk_wrap_sub(paste(
                    "A peak is a cycle only above the red-noise threshold; the",
                    "1.96 line holds only for one period chosen in advance.",
                    "Dots: accepted cycles, at their value after the earlier",
                    "ones were removed."))) +
    .kk_cyc_theme() +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 60, 8, 8))
  if (nrow(acc)) {
    p <- p + ggplot2::geom_point(data = acc, ggplot2::aes(x = .data$period,
                                                          y = .data$R_SR),
                                 colour = cl$accent, size = 2.4,
                                 inherit.aes = FALSE) +
      ggplot2::geom_text(family = .kk_txt_family(), data = acc,
                         ggplot2::aes(x = .data$period, y = .data$R_SR,
                                      label = signif(.data$model_period, 3)),
                         vjust = -0.9, size = 3.4, fontface = "bold",
                         colour = cl$accent, inherit.aes = FALSE)
  }
  if (log_period) p <- p + ggplot2::scale_x_log10()
  p
}

#' Plot a Cosinor Fit (KK)
#'
#' @description Measurements folded onto one cycle (clock time for
#'   `POSIXct`, grey) with the fitted cosine and its 95% confidence band,
#'   the MESOR and the peak labelled directly. With `type = "ellipse"`: the
#'   cosinor of every day (grey points) and the population cosinor as an
#'   arrow on a clock dial — its length is the amplitude, its direction the
#'   time of the peak — with the 95% confidence ellipse. The rhythm is
#'   significant when the ellipse does not cover the centre.
#'
#' @param res Result of [kk_cosinor()] (with `by_day = TRUE` for the
#'   ellipse) or [kk_cosinor_population()].
#' @param type `"fit"` (default) or `"ellipse"`.
#' @param conf.level Confidence level (default 0.95).
#' @param title Optional title; by default it states the finding.
#'
#' @return A ggplot object.
#' @seealso [kk_cosinor()], [kk_cosinor_population()]
#' @examples
#' tp <- kk_cycle_example("temperature")
#' cz <- kk_cosinor(tp, temp, time, by_day = TRUE)
#' kk_plot_cosinor(cz)
#' kk_plot_cosinor(cz, type = "ellipse")
#' @export
kk_plot_cosinor <- function(res, type = c("fit", "ellipse"), conf.level = 0.95,
                            title = NULL) {
  type <- match.arg(type)
  cl <- .kk_cyc_cols()
  P <- res$period[1]
  clock <- isTRUE(all.equal(P, 24)) &&
    !identical(attr(res, "time_class"), "numeric") &&
    !identical(attr(res, "time_class"), "Date")
  pk_lab <- function(h) if (clock) .kk_hhmm(h) else format(signif(h, 3))
  if (type == "fit") {
    f <- attr(res, "fitted")
    fit <- attr(res, "model")
    if (is.null(f)) stop("`res` must come from kk_cosinor().", call. = FALSE)
    f$phase_t <- f$t %% P
    w <- 2 * pi / P
    grid <- data.frame(x = seq(0, P, length.out = 241))
    X <- cbind(1, cos(w * grid$x), sin(w * grid$x))
    # the same covariance and critical value as the analysis itself
    V <- attr(res, "vcov")
    if (is.null(V)) V <- stats::vcov(fit)
    grid$y <- drop(X %*% stats::coef(fit))
    se <- sqrt(rowSums((X %*% V) * X))
    z <- stats::qt(1 - (1 - conf.level) / 2, df = max(1, res$n - 3))
    grid$lo <- grid$y - z * se
    grid$hi <- grid$y + z * se
    if (is.null(title)) {
      title <- if (res$p_value < 1 - conf.level) {
        sprintf("Peak at %s, %s above the rhythm-adjusted mean",
                pk_lab(res$acrophase), format(signif(res$amplitude, 2)))
      } else "No significant rhythm of this period detected"
    }
    peak_y <- res$mesor + res$amplitude
    brks <- seq(0, P, length.out = 5)
    return(kkplot(f, ggplot2::aes(x = .data$phase_t, y = .data$observed)) +
      ggplot2::geom_point(colour = cl$grey, alpha = 0.5, size = 1.2) +
      ggplot2::geom_ribbon(data = grid, ggplot2::aes(x = .data$x, ymin = .data$lo,
                                                     ymax = .data$hi),
                           inherit.aes = FALSE, fill = cl$focus, alpha = 0.2) +
      ggplot2::geom_line(data = grid, ggplot2::aes(x = .data$x, y = .data$y),
                         colour = cl$focus, linewidth = 1) +
      ggplot2::geom_hline(yintercept = res$mesor, colour = cl$grey,
                          linetype = "dotted") +
      ggplot2::annotate("text", family = .kk_txt_family(), x = P, y = res$mesor, label = " MESOR",
                        hjust = 0, size = 3.2, colour = cl$grey) +
      ggplot2::annotate("segment", x = res$acrophase, xend = res$acrophase,
                        y = res$mesor, yend = peak_y, colour = cl$accent,
                        linewidth = 0.6,
                        arrow = ggplot2::arrow(length = ggplot2::unit(0.18, "cm"))) +
      ggplot2::annotate("label", family = .kk_txt_family(), x = res$acrophase, y = peak_y,
                        label = paste0("peak ", pk_lab(res$acrophase)),
                        vjust = -0.4, size = 3.3, fontface = "bold",
                        colour = cl$accent, fill = "white") +
      ggplot2::scale_x_continuous(breaks = brks,
                                  labels = if (clock) sprintf("%02d:00", brks) else
                                    signif(brks, 3)) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::labs(x = if (clock) "Clock time" else "Time within the cycle",
                    y = NULL, title = title,
                    subtitle = .kk_wrap_sub(sprintf(paste(
                      "Grey: measurements folded onto one cycle; line: cosinor",
                      "with its %d%% confidence band (zero-amplitude test p %s)."),
                      round(100 * conf.level),
                      format.pval(res$p_value, digits = 2, eps = 0.001)))) +
      .kk_cyc_theme() +
      ggplot2::theme(plot.margin = ggplot2::margin(10, 50, 8, 8)))
  }
  sub <- attr(res, "subjects")
  if (is.null(sub)) {
    pdays <- attr(res, "per_day")
    if (is.null(pdays)) stop("Use kk_cosinor(by_day = TRUE) or kk_cosinor_population().",
                             call. = FALSE)
    th <- pdays$acrophase / P * 2 * pi
    sub <- data.frame(beta = pdays$amplitude * cos(th), gamma = pdays$amplitude * sin(th))
  }
  X <- as.matrix(sub[, c("beta", "gamma")])
  k <- nrow(X)
  m <- colMeans(X)
  S <- stats::cov(X) / k
  r <- sqrt(2 * (k - 1) / (k - 2) * stats::qf(conf.level, 2, k - 2))
  e <- eigen(S)
  a <- seq(0, 2 * pi, length.out = 200)
  ell <- t(m + e$vectors %*% (sqrt(pmax(e$values, 0)) * r * rbind(cos(a), sin(a))))
  # clock dial: 00:00 at the top, time clockwise; x = gamma, y = beta
  E <- data.frame(x = ell[, 2], y = ell[, 1])
  pts <- data.frame(x = X[, 2], y = X[, 1])
  lim <- max(sqrt(c(E$x^2 + E$y^2, pts$x^2 + pts$y^2))) * 1.25
  ticks <- data.frame(h = seq(0, P - P / 8, by = P / 8))
  ticks$ang <- ticks$h / P * 2 * pi
  ticks$lab <- if (clock) sprintf("%02d:00", ticks$h) else as.character(signif(ticks$h, 3))
  ring_r <- c(0.5, 1) * lim / 1.25
  rings <- do.call(rbind, lapply(ring_r, function(rr) {
    data.frame(r = rr, x = rr * sin(a), y = rr * cos(a))
  }))
  covers <- drop(t(m) %*% solve(S) %*% m) <= r^2
  A <- sqrt(sum(m^2))
  pk <- (atan2(m[2], m[1]) %% (2 * pi)) / (2 * pi) * P
  if (is.null(title)) {
    title <- if (covers) "The confidence ellipse covers the centre: no significant rhythm detected" else
      sprintf("Peak at %s, amplitude %s", pk_lab(pk), format(signif(A, 2)))
  }
  ggplot2::ggplot(pts, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_path(data = rings, ggplot2::aes(group = .data$r), colour = cl$light,
                       linewidth = 0.3) +
    ggplot2::annotate("text", family = .kk_txt_family(), x = 0, y = ring_r, label = signif(ring_r, 2),
                      size = 2.8, colour = cl$grey, vjust = -0.3) +
    ggplot2::geom_point(colour = cl$grey, size = 1.4, alpha = 0.7) +
    ggplot2::geom_polygon(data = E, fill = cl$focus, alpha = 0.15, colour = cl$focus,
                          linewidth = 0.6) +
    ggplot2::annotate("segment", x = 0, y = 0, xend = m[2], yend = m[1],
                      colour = cl$accent, linewidth = 1.1,
                      arrow = ggplot2::arrow(length = ggplot2::unit(0.25, "cm"))) +
    ggplot2::geom_text(family = .kk_txt_family(), data = ticks, ggplot2::aes(x = lim * sin(.data$ang),
                                                  y = lim * cos(.data$ang),
                                                  label = .data$lab),
                       inherit.aes = FALSE, size = 3.1, colour = "grey35") +
    ggplot2::coord_equal(xlim = c(-lim, lim), ylim = c(-lim, lim), clip = "off") +
    ggplot2::labs(x = NULL, y = NULL, title = title,
                  subtitle = .kk_wrap_sub(paste(
                    "Arrow: population cosinor (length = amplitude, direction =",
                    "time of the peak); grey: each day; shaded:",
                    sprintf("%d%% confidence", round(100 * conf.level)),
                    "ellipse; rings: amplitude scale."), 70)) +
    .kk_cyc_theme() +
    ggplot2::theme(axis.line = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())
}

#' Plot the Response After Events (KK)
#'
#' @description Superposed-epoch curve from [kk_epoch_response()], one panel
#'   per category: the mean change from baseline around the events (accent
#'   line with its 95% band; 0 = the event), the corridor of the
#'   clock-matched controls (grey band) and the global band (dashed). Dots
#'   lie outside the global band and are significant over the whole window.
#'
#' @param res Result of [kk_epoch_response()].
#' @param title Optional title; by default it states the mean response.
#' @return A ggplot object.
#' @seealso [kk_epoch_response()]
#' @examples
#' pd <- kk_cycle_example("prediabetes")
#' res <- kk_epoch_response(pd$series, glucose, time, pd$events, time,
#'                          event_type = type, window = c(-0.5, 3),
#'                          n_control = 50)
#' kk_plot_epoch(res)
#' @export
kk_plot_epoch <- function(res, title = NULL) {
  cl <- .kk_cyc_cols()
  sm <- attr(res, "summary")
  if (is.null(title) && !is.null(sm) && nrow(sm)) {
    eff <- ifelse(is.na(sm$difference), sm$mean, sm$difference)
    title <- paste(sprintf("%s %s%s (%s%s)", sm$event_type,
                           ifelse(eff >= 0, "+", "\u2212"),
                           format(signif(abs(eff), 2)),
                           ifelse(sm$main_measure == "rise", "rise to peak",
                                  "mean change"),
                           ifelse(is.na(sm$difference), "", " vs controls")),
                   collapse = ", ")
    title <- paste("After the events:", title)
  }
  has_ctrl <- "control_low" %in% names(res)
  p <- kkplot(res, ggplot2::aes(x = .data$lag, y = .data$mean_change))
  if (has_ctrl) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$control_low,
                                               ymax = .data$control_high),
                                  fill = cl$light, alpha = 0.8) +
      ggplot2::geom_line(ggplot2::aes(y = .data$global_low), linetype = "dashed",
                         colour = cl$grey, linewidth = 0.35) +
      ggplot2::geom_line(ggplot2::aes(y = .data$global_high), linetype = "dashed",
                         colour = cl$grey, linewidth = 0.35)
  }
  p <- p + ggplot2::geom_vline(xintercept = 0, colour = cl$grey, linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = 0, colour = cl$grey, linetype = "dotted") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$conf.low, ymax = .data$conf.high),
                         fill = cl$accent, alpha = 0.18) +
    ggplot2::geom_line(colour = cl$accent, linewidth = 0.9) +
    ggplot2::facet_wrap(~ event_type, scales = "free_y") +
    ggplot2::labs(x = "Time from the event", y = "Change from baseline",
                  title = title,
                  subtitle = .kk_wrap_sub(if (has_ctrl) paste(
                    "Red: mean change with 95% band; grey band: controls at the",
                    "same clock times on other days; dashed: global band. Dots",
                    "lie outside the global band.") else
                      "Red: mean change with 95% band."))
  if ("outside_global" %in% names(res)) {
    p <- p + ggplot2::geom_point(data = res[res$outside_global, , drop = FALSE],
                                 colour = cl$accent, size = 1.7)
  }
  p + .kk_cyc_theme()
}

#' Plot a Wavelet Spectrum or Coherence (KK)
#'
#' @description Time-period map from [kk_wavelet()] (power) or
#'   [kk_wavelet_coherence()] (coherence): colour shows the strength, black
#'   contours enclose the regions significant against red noise, and the
#'   cone of influence (edge effects) is washed out.
#'
#' @param res Result of [kk_wavelet()] or [kk_wavelet_coherence()].
#' @param title Optional title; by default it names the dominant period.
#' @return A ggplot object.
#' @seealso [kk_wavelet()], [kk_wavelet_coherence()]
#' @examples
#' ch <- kk_cycle_example("chronobiology")
#' kk_plot_wavelet(kk_wavelet(ch, pulse, time))
#' @export
kk_plot_wavelet <- function(res, title = NULL) {
  val <- if ("coherence" %in% names(res)) "coherence" else "power"
  d <- res
  d$value <- d[[val]]
  d$sig <- as.numeric(d$significant & !d$in_coi)
  if (is.null(title)) {
    g <- if (val == "power") attr(res, "global") else attr(res, "summary")
    vv <- if (val == "power") g$power else g$coherence
    title <- sprintf("The strongest %s has a period of about %s",
                     if (val == "power") "oscillation" else "shared oscillation",
                     format(signif(g$period[which.max(vv)], 2)))
  }
  kkplot(d, ggplot2::aes(x = .data$t, y = .data$period)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$value)) +
    ggplot2::geom_contour(ggplot2::aes(z = .data$sig), breaks = 0.5,
                          colour = "black", linewidth = 0.4) +
    ggplot2::geom_tile(data = d[d$in_coi, , drop = FALSE], fill = "white",
                       alpha = 0.5) +
    ggplot2::scale_y_log10(expand = ggplot2::expansion(0)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(0)) +
    ggplot2::scale_fill_gradient(name = val, low = "white", high = kk_pal(2)[2]) +
    ggplot2::labs(x = "Time from the start", y = "Period", title = title,
                  subtitle = .kk_wrap_sub(paste(
                    "Darker = stronger. Black contours: significant against red",
                    "noise; washed-out edges: cone of influence, where edge",
                    "effects distort the transform."))) +
    .kk_cyc_theme()
}

#' Plot the Trend-Plus-Cycles Model and Forecast (KK)
#'
#' @description Measurements (grey points), the fitted model (focus line),
#'   the trend when it was fitted separately (accent), and the forecast with
#'   its prediction interval, each labelled directly.
#'
#' @param res Result of [kk_harmonic_model()].
#' @param title Optional title; by default it reports the fit and whether
#'   the forecast beats the naive one.
#' @return A ggplot object.
#' @seealso [kk_harmonic_model()]
#' @examples
#' pn <- kk_cycle_example("pneumonia")
#' kk_plot_model(kk_harmonic_model(pn$series, rate, month, periods = c(12, 6),
#'                                 horizon = 12))
#' @export
kk_plot_model <- function(res, title = NULL) {
  cl <- .kk_cyc_cols()
  f <- attr(res, "fitted")
  fc <- attr(res, "forecast")
  fs <- attr(res, "fit")
  v <- attr(res, "validation")
  if (is.null(title)) {
    title <- sprintf("Trend + cycles explain %d%% of the variation",
                     round(100 * fs$r_squared))
    if (!is.null(v) && !is.na(v$better_than_naive)) {
      title <- paste0(title, if (v$better_than_naive)
        sprintf("; forecast error %d%% below naive",
                round(v$error_reduction_pct)) else
          "; the forecast does not beat naive")
    }
  }
  last <- utils::tail(f, 1)
  p <- kkplot(f, ggplot2::aes(x = .data$t, y = .data$observed)) +
    ggplot2::geom_point(colour = cl$grey, size = 1.1, alpha = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = .data$fitted), colour = cl$focus,
                       linewidth = 0.7)
  if (!all(is.na(f$trend))) {
    p <- p + ggplot2::geom_line(ggplot2::aes(y = .data$trend), colour = cl$accent,
                                linewidth = 0.6) +
      ggplot2::annotate("text", family = .kk_txt_family(), x = last$t, y = last$trend, label = " trend",
                        hjust = 0, size = 3.2, colour = cl$accent)
  }
  if (!is.null(fc)) {
    p <- p + ggplot2::geom_vline(xintercept = last$t, colour = cl$light) +
      ggplot2::geom_ribbon(data = fc, ggplot2::aes(x = .data$t,
                                                   ymin = .data$pred.low,
                                                   ymax = .data$pred.high),
                           inherit.aes = FALSE, fill = cl$focus, alpha = 0.15) +
      ggplot2::geom_line(data = fc, ggplot2::aes(x = .data$t, y = .data$forecast),
                         inherit.aes = FALSE, colour = cl$focus, linetype = "dashed") +
      ggplot2::annotate("text", family = .kk_txt_family(), x = max(fc$t), y = max(fc$pred.high),
                        label = sprintf("forecast, %d%% PI",
                                        round(100 * .kk_or_num(attr(res, "conf.level"), 0.95))),
                        hjust = 1, vjust = -0.4,
                        size = 3.2, colour = cl$focus)
  }
  p + ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = "Time", y = NULL, title = title,
                  subtitle = sprintf(
                    "Grey: measurements; blue: trend + cycles (R = %.2f, PrIdx = %.2f).",
                    fs$R, fs$PrIdx)) +
    .kk_cyc_theme() +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 40, 8, 8))
}

#' Plot the Parts of the Day (KK)
#'
#' @description Each grey line is one day across the four parts of the day;
#'   the accent line joins the means. Parallel grey lines mean a consistent
#'   difference; lines crossing at random mean it is not consistent.
#'
#' @param res Result of [kk_day_parts()].
#' @param title Optional title; by default it names the highest part.
#' @return A ggplot object.
#' @seealso [kk_day_parts()]
#' @examples
#' g <- kk_cycle_example("prediabetes")$series
#' kk_plot_day_parts(kk_day_parts(g, glucose, time))
#' @export
kk_plot_day_parts <- function(res, title = NULL) {
  cl <- .kk_cyc_cols()
  d <- attr(res, "daily")
  mn <- attr(res, "means")
  mn$part <- factor(mn$part, levels = levels(d$part))
  if (is.null(title)) {
    hi <- as.character(mn$part[which.max(mn$mean)])
    title <- sprintf("Highest in the %s (Friedman %s)", hi,
                     format.pval(res$p.value[2], digits = 2, eps = 0.001))
  }
  kkplot(d, ggplot2::aes(x = .data$part, y = .data$value, group = .data$day)) +
    ggplot2::geom_line(colour = cl$light, linewidth = 0.4) +
    ggplot2::geom_point(colour = cl$light, size = 0.9) +
    ggplot2::geom_line(data = mn, ggplot2::aes(x = .data$part, y = .data$mean,
                                               group = 1), inherit.aes = FALSE,
                       colour = cl$accent, linewidth = 1.1) +
    ggplot2::geom_point(data = mn, ggplot2::aes(x = .data$part, y = .data$mean),
                        inherit.aes = FALSE, colour = cl$accent, size = 2.6) +
    ggplot2::labs(x = NULL, y = NULL, title = title,
                  subtitle = .kk_wrap_sub(paste(
                    "Grey: each day; red: mean of the days. Parallel grey lines",
                    "mean a consistent difference."))) +
    .kk_cyc_theme()
}
