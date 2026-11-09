# ============================================================
# EVENTS IN A TIME SERIES: SUPERPOSED EPOCHS, N-OF-1 TRIALS
# ============================================================

#' Response After Events: Superposed Epochs and Per-Event Measures (KK)
#'
#' @description Estimates how a continuously measured variable responds
#'   after events — meals, walks, doses, procedures — in two complementary
#'   ways.
#'
#'   **Superposed epochs** (Chree 1913). The record is cut into segments
#'   around every event, each expressed as the change from its own baseline,
#'   binned by lag and averaged. Controls are pseudo-events at the *same
#'   clock times on randomly chosen other days* (for clock time), so a
#'   circadian rhythm is not mistaken for an event effect; for other time
#'   scales the pseudo-events are placed at random. A control is used only
#'   if no real event falls inside its window. The controls give a
#'   pointwise 95% corridor and a global band — the family-wise limit for
#'   the whole window — and lags outside the global band are significant
#'   after accounting for all lags examined. The band is calibrated on a
#'   split of the control sets: one half gives the centre and scale at each
#'   lag, the other half the distribution of the largest standardised
#'   deviation, so the band is not tuned to the same simulations it is
#'   judged by. A control set contains one control for every event that has
#'   a clean control, so it may be slightly smaller than the event set.
#'
#'   **Response per event.** For every event: baseline (mean over the
#'   `baseline` window, by default the 30 min before the event for clock
#'   time), rise to the peak, time to the peak, incremental area above
#'   baseline (iAUC, from the event onwards, with baseline crossings
#'   interpolated), mean change and change at the end of the window. The
#'   main measure is the rise to the peak for meals and the signed mean
#'   change for everything else, because drugs or exercise may lower as well
#'   as raise the variable.
#'
#'   **The test is against the controls, not against zero.** The rise to the
#'   peak is a maximum, so it is positive even after a pseudo-event in pure
#'   noise; the iAUC likewise. A t test of the mean rise against zero would
#'   therefore declare a response that is not there. The same measures are
#'   computed for the clock-matched control windows, and the event mean is
#'   compared with the control mean by the Welch t test (`difference`,
#'   `p.value`); this also removes the part of the mean change that is due
#'   to the daily rhythm. The p values are Holm-adjusted across event
#'   categories (`p.adjusted`). Without controls (`n_control = 0`, or no
#'   clean control window) only the signed mean change can be tested, against
#'   zero; for the rise and the iAUC the p value is then `NA`.
#'
#'   Categories are analysed separately. Numeric event
#'   variables (carbohydrates, dose, duration) are covariates in a linear
#'   regression (Holm-adjusted when there are several); the part of the day
#'   and co-occurring events of another type within an hour are factors,
#'   tested parametrically (Welch t / Welch ANOVA) and non-parametrically
#'   (Mann-Whitney / Kruskal-Wallis). With few events or a skewed
#'   distribution, trust the non-parametric test.
#'
#'   The unit of inference is the event, not the measurement. Events whose
#'   window contains another event — including another event at the same
#'   time — are excluded from the tests by default and listed in the
#'   `excluded` attribute. Exact duplicates (same time and category) are
#'   counted once, with a warning.
#'   When events happen at almost the same time every day, their effect
#'   cannot be separated from the daily rhythm; a warning recommends varying
#'   the times or including days without the event.
#'
#' @param data Data frame with the series, one row per measurement.
#' @param outcome Measured variable (bare name or string).
#' @param time Time column: numeric, `Date` (days) or `POSIXct` (hours).
#' @param events Data frame of events.
#' @param event_time Column of `events` with the event times (same class as
#'   `time`).
#' @param event_type Optional column of `events` with the category.
#' @param covariates Optional numeric columns of `events` (tidyselect), e.g.
#'   carbohydrates, dose.
#' @param window Window around each event, `c(before, after)` in hours for
#'   `POSIXct`, days for `Date` (default `c(-1, 3)`).
#' @param baseline Baseline window (default the 30 min before the event for
#'   `POSIXct`, else `c(window[1], 0)`).
#' @param measure Main per-event measure: `"auto"` (rise for categories
#'   whose name contains "meal", "food", "breakfast", "lunch" or "dinner"; mean change
#'   otherwise), `"rise"`, `"mean"`, `"iauc"`.
#' @param bin_width Width of the lag bins (default: the median step).
#' @param exclude_overlap Exclude events with another event inside their
#'   window (default `TRUE`).
#' @param n_control Number of control sets for the corridor (default 200;
#'   0 skips it).
#' @param conf.level Confidence level (default 0.95).
#' @param seed Random seed (default 1).
#'
#' @return Tibble with one row per category and lag: `event_type`, `lag`,
#'   `n_events`, `mean_change`, `std.error`, `conf.low`, `conf.high`,
#'   `control_low`, `control_high`, `global_low`, `global_high`,
#'   `outside_global`. Attributes: `summary` (per category: events, main
#'   measure, its event mean with interval, `control_mean`, `n_controls`,
#'   the event-minus-control `difference` with `diff.low`, `diff.high`,
#'   `p.value`, `p.adjusted` (Holm across categories), `test`, peak lags),
#'   `per_event` (all measures per event), `controls` (the same measures for
#'   the control windows), `excluded` (events left out, with the reason),
#'   `covariate_effect`, `factor_tests`, `warnings`.
#'
#' @references Chree C (1913) Some phenomena of sunspots and of terrestrial
#'   magnetism at Kew Observatory. *Philosophical Transactions of the Royal
#'   Society A* 212:75-116. \doi{10.1098/rsta.1913.0003}
#'   Brouns F et al. (2005) Glycaemic index methodology. *Nutrition Research
#'   Reviews* 18:145-171. \doi{10.1079/NRR2005100}
#'   Stefanov R (2026) *CycleLab: analysis of biomedical time series*.
#'   <https://cyclelab.raredis.org>
#'
#' @seealso [kk_distributed_lag()], [kk_its()], [kk_nof1()],
#'   [kk_plot_epoch()]
#'
#' @examples
#' pd <- kk_cycle_example("prediabetes")
#' res <- kk_epoch_response(pd$series, glucose, time, pd$events, time,
#'                          event_type = type, covariates = carbs,
#'                          window = c(-0.5, 3), n_control = 100)
#' attr(res, "summary")
#' attr(res, "covariate_effect")
#'
#' @export
kk_epoch_response <- function(data, outcome, time, events, event_time,
                              event_type = NULL, covariates = NULL,
                              window = c(-1, 3), baseline = NULL,
                              measure = c("auto", "rise", "mean", "iauc"),
                              bin_width = NULL, exclude_overlap = TRUE,
                              n_control = 200, conf.level = 0.95, seed = 1) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  measure <- match.arg(measure)
  y_name <- .kk_colname(rlang::enquo(outcome))
  t_name <- .kk_colname(rlang::enquo(time))
  et_name <- .kk_colname(rlang::enquo(event_time))
  ty_q <- rlang::enquo(event_type)
  ty_name <- if (rlang::quo_is_null(ty_q)) NULL else .kk_colname(ty_q)
  cv_q <- rlang::enquo(covariates)
  cv_names <- if (rlang::quo_is_null(cv_q)) character(0) else
    names(tidyselect::eval_select(cv_q, events))
  data <- .kk_ts_prepare(data, c(y_name, t_name), numeric = y_name)
  events <- .kk_ts_prepare(events, c(et_name, ty_name))
  if (!is.null(seed)) set.seed(seed)
  if (length(window) != 2 || window[1] >= 0 || window[2] <= 0) {
    stop("`window` must start before and end after the event.", call. = FALSE)
  }
  if (!inherits(data[[t_name]], class(events[[et_name]])[1]) &&
      !(is.numeric(data[[t_name]]) && is.numeric(events[[et_name]]))) {
    stop("`event_time` must have the same class as `time`.", call. = FALSE)
  }
  dup <- duplicated(data.frame(events[[et_name]],
                               if (is.null(ty_name)) 1 else events[[ty_name]]))
  if (any(dup)) {
    warning(sprintf("%d duplicated event(s) (same time and category) were counted once.",
                    sum(dup)), call. = FALSE)
    events <- events[!dup, , drop = FALSE]
  }
  data <- data[order(data[[t_name]]), , drop = FALSE]
  info <- .kk_ts_time(data[[t_name]])
  t <- info$t
  y <- as.numeric(data[[y_name]])
  if (is.null(baseline)) {
    baseline <- if (info$class == "POSIXct") c(-0.5, 0) else c(window[1], 0)
  }
  et_raw <- events[[et_name]]
  et <- if (info$class == "POSIXct") {
    as.numeric(difftime(et_raw, info$origin, units = "hours"))
  } else if (info$class == "Date") {
    as.numeric(et_raw - info$origin)
  } else as.numeric(et_raw)
  type <- if (is.null(ty_name)) rep("all events", length(et)) else
    as.character(events[[ty_name]])
  if (is.null(bin_width)) bin_width <- stats::median(diff(t))
  if (!is.finite(bin_width) || bin_width <= 0) {
    stop("`bin_width` must be positive.", call. = FALSE)
  }
  warn <- character(0)

  # another event (by index, so simultaneous events count) inside the window
  overlap <- vapply(seq_along(et), function(i) {
    o <- et[-i]
    any(o >= et[i] + window[1] & o <= et[i] + window[2])
  }, logical(1))
  inside <- et + window[1] >= min(t) & et + window[2] <= max(t)
  use <- inside & (!exclude_overlap | !overlap)
  excluded <- tibble::tibble(
    event_type = type[!use], event_time = et_raw[!use],
    reason = ifelse(!inside[!use], "window outside the record",
                    "another event inside the window"))

  lag_bins <- seq(window[1], window[2], by = bin_width)
  epoch <- function(te) {
    rel <- t - te
    inw <- rel >= window[1] - 1e-9 & rel <= window[2] + 1e-9
    inb <- rel >= baseline[1] - 1e-9 & rel <= baseline[2] + 1e-9
    if (sum(inw) < 3 || !any(inb)) return(NULL)
    b <- mean(y[inb])
    bin <- lag_bins[pmin(length(lag_bins),
                         floor((rel[inw] - window[1]) / bin_width + 0.5) + 1)]
    v <- tapply(y[inw] - b, bin, mean)
    out <- stats::setNames(rep(NA_real_, length(lag_bins)), lag_bins)
    out[names(v)] <- v
    attr(out, "baseline") <- b
    out
  }
  measures <- function(v) {
    post <- lag_bins > 0
    vp <- v[post]
    lp <- lag_bins[post]
    okp <- !is.na(vp)
    nm <- c("baseline", "rise", "time_to_peak", "iauc", "mean_change",
            "end_change")
    if (sum(okp) < 2) return(stats::setNames(rep(NA_real_, 6), nm))
    vp <- unname(vp)
    j <- which.max(vp)
    # iAUC from the event onwards (lag >= 0), crossings interpolated
    from0 <- lag_bins >= 0 & !is.na(v)
    stats::setNames(c(attr(v, "baseline"), vp[j], lp[j],
                      .kk_pos_area(lag_bins[from0], unname(v[from0])),
                      mean(vp[okp]), utils::tail(vp[okp], 1)), nm)
  }
  is_meal <- function(ty) grepl("meal|food|breakfast|lunch|dinner|snack", tolower(ty))

  # clock-matched control events
  day_len <- if (info$class == "POSIXct") 24 else NA
  control_times <- function(te) {
    if (is.na(day_len)) {
      return(vapply(te, function(x) {
        for (i in 1:50) {
          cc <- stats::runif(1, min(t) - window[1], max(t) - window[2])
          if (!any(et > cc + window[1] & et < cc + window[2])) return(cc)
        }
        NA_real_
      }, numeric(1)))
    }
    pools <- control_pools(te)
    vapply(pools, function(cand) {
      if (!length(cand)) return(NA_real_)
      cand[sample.int(length(cand), 1)]
    }, numeric(1))
  }
  # candidate control windows of each event: the same clock time on other
  # days, inside the record, with no real event of any type in the window
  control_pools <- function(te) {
    days <- floor(min(t) / 24):floor(max(t) / 24)
    lapply(te, function(x) {
      cand <- (x %% 24) + 24 * days
      cand <- cand[cand + window[1] >= min(t) & cand + window[2] <= max(t) &
                     abs(cand - x) > window[2] - window[1]]
      clean <- vapply(cand, function(cc) !any(et >= cc + window[1] &
                                                et <= cc + window[2]), logical(1))
      cand[clean]
    })
  }

  if (info$class == "POSIXct" && sum(use) >= 3) {
    for (ty in unique(type[use])) {
      ch <- et[use & type == ty] %% 24
      if (length(ch) >= 3) {
        circ_sd <- sqrt(-2 * log(abs(mean(exp(1i * 2 * pi * ch / 24))))) * 24 / (2 * pi)
        if (is.finite(circ_sd) && circ_sd < 1) {
          warn <- c(warn, sprintf(paste0(
            "'%s' events happen at almost the same time every day (SD %.1f h): ",
            "their effect cannot be separated from the daily rhythm. Vary the ",
            "times or include days without the event."), ty, circ_sd))
        }
      }
    }
    for (w in warn) warning(w, call. = FALSE)
  }

  zt <- function(k) stats::qt(1 - (1 - conf.level) / 2, df = max(1, k - 1))
  rows <- list()
  summ <- list()
  per_event <- list()
  ctrl_all <- list()
  for (ty in unique(type[use])) {
    idx <- which(use & type == ty)
    ep <- lapply(et[idx], epoch)
    ok <- !vapply(ep, is.null, logical(1))
    ep <- ep[ok]
    idx <- idx[ok]
    if (length(ep) < 2) next
    M <- do.call(rbind, ep)
    k_b <- colSums(!is.na(M))
    mn <- colMeans(M, na.rm = TRUE)
    se <- apply(M, 2, stats::sd, na.rm = TRUE) / sqrt(k_b)
    zz <- vapply(k_b, zt, numeric(1))
    tb <- tibble::tibble(event_type = ty, lag = lag_bins, n_events = k_b,
                         mean_change = mn, std.error = se,
                         conf.low = mn - zz * se, conf.high = mn + zz * se)
    main <- if (measure == "auto") (if (is_meal(ty)) "rise" else "mean_change") else
      switch(measure, rise = "rise", mean = "mean_change", iauc = "iauc")
    ctrl_meas <- NULL
    if (n_control > 0) {
      share_clean <- mean(!is.na(control_times(et[idx])))
      if (share_clean < 0.5) {
        w <- sprintf(paste0(
          "Only %.0f%% of '%s' events have a clean control (same clock time, ",
          "no event in the window, on another day): the events recur at the ",
          "same times, so the corridor is not informative. Rely on the ",
          "per-event response or vary the event times."), 100 * share_clean, ty)
        warn <- c(warn, w)
        warning(w, call. = FALSE)
      }
      # An event is compared with the mean of a finite pool of m windows:
      # Var(event - pool mean) = s^2 (1 + 1/m), while a control drawn from
      # the pool varies by s^2 (1 - 1/m). The control spread is inflated
      # accordingly (factor 1 for random controls, whose pool is unlimited).
      infl <- 1
      if (!is.na(day_len)) {
        m_j <- lengths(control_pools(et[idx]))
        m_j <- m_j[m_j >= 1]
        if (length(m_j)) infl <- sqrt(sum(1 + 1 / m_j) / max(1e-9, sum(1 - 1 / m_j)))
        if (!is.finite(infl)) infl <- NA_real_
      }
      ctl_seen <- new.env()
      C <- t(replicate(n_control, {
        ct <- control_times(et[idx])
        ct <- ct[!is.na(ct)]
        ce <- lapply(ct, epoch)
        keep_c <- !vapply(ce, is.null, logical(1))
        for (q in which(keep_c)) {
          key <- format(ct[q], digits = 12)
          if (is.null(ctl_seen[[key]])) ctl_seen[[key]] <- c(ct[q], measures(ce[[q]]))
        }
        ce <- ce[keep_c]
        if (length(ce) < 2) rep(NA_real_, length(lag_bins)) else
          colMeans(do.call(rbind, ce), na.rm = TRUE)
      }))
      if (length(ls(ctl_seen))) {
        ctrl_meas <- do.call(rbind, mget(ls(ctl_seen), envir = ctl_seen))
        colnames(ctrl_meas)[1] <- "t"
        ctrl_all[[ty]] <- tibble::tibble(event_type = ty,
                                         control_time = .kk_ts_untime(ctrl_meas[, "t"], info),
                                         !!!as.data.frame(ctrl_meas[, -1, drop = FALSE]))
      }
      # split calibration: centre/scale from one half, maxima from the other
      half <- seq_len(nrow(C)) %% 2 == 1
      if (sum(!half) < 10) half <- rep(TRUE, nrow(C))
      cm <- colMeans(C[half, , drop = FALSE], na.rm = TRUE)
      cs <- apply(C[half, , drop = FALSE], 2, stats::sd, na.rm = TRUE)
      cs[!is.finite(cs) | cs == 0] <- NA
      Cb <- if (all(half)) C else C[!half, , drop = FALSE]
      maxdev <- apply(abs(sweep(sweep(Cb, 2, cm), 2, cs, "/")), 1, function(z) {
        if (all(is.na(z))) NA_real_ else max(z, na.rm = TRUE)
      })
      # finite-sample quantile: the ceiling((m + 1) * conf.level)-th order
      # statistic keeps the exceedance probability at most 1 - conf.level
      md <- sort(maxdev[is.finite(maxdev)])
      g <- if (length(md)) md[min(length(md), ceiling((length(md) + 1) * conf.level))] else NA_real_
      qlo <- apply(C, 2, stats::quantile, (1 - conf.level) / 2, na.rm = TRUE)
      qhi <- apply(C, 2, stats::quantile, 1 - (1 - conf.level) / 2, na.rm = TRUE)
      cm_all <- colMeans(C, na.rm = TRUE)
      tb$control_low <- cm_all - infl * (cm_all - qlo)
      tb$control_high <- cm_all + infl * (qhi - cm_all)
      tb$global_low <- cm - g * infl * cs
      tb$global_high <- cm + g * infl * cs
      tb$outside_global <- !is.na(cs) & (mn < tb$global_low | mn > tb$global_high)
    }
    rows[[ty]] <- tb[tb$n_events >= 2, , drop = FALSE]

    pm <- do.call(rbind, lapply(ep, measures))
    pe <- tibble::tibble(event_type = ty, event_time = et_raw[idx],
                         !!!as.data.frame(pm), main_measure = main,
                         main_value = pm[, main])
    if (info$class == "POSIXct") {
      hh <- et[idx] %% 24 + .kk_clock_hours(info$origin)
      hh <- hh %% 24
      pe$day_part <- as.character(cut(hh, c(-Inf, 5, 11, 16, 22, Inf),
                                      labels = c("night", "morning", "midday",
                                                 "evening", "night2"),
                                      right = FALSE))
      pe$day_part[pe$day_part == "night2"] <- "night"
    }
    others <- et[type != ty]
    pe$co_event <- vapply(et[idx], function(x) any(abs(others - x) <= 1 *
      (if (info$class == "POSIXct") 1 else bin_width)), logical(1))
    for (cv in cv_names) pe[[cv]] <- as.numeric(events[[cv]][idx])
    per_event[[ty]] <- pe

    v <- pe$main_value
    kk <- sum(!is.na(v))
    cv <- if (is.null(ctrl_meas)) numeric(0) else ctrl_meas[, main]
    cv <- cv[!is.na(cv)]
    if (kk >= 2 && length(cv) >= 2) {
      tt_c <- stats::t.test(v, cv, conf.level = conf.level)
      dif <- mean(v, na.rm = TRUE) - mean(cv)
      dlo <- tt_c$conf.int[1]
      dhi <- tt_c$conf.int[2]
      pv <- tt_c$p.value
      test <- "Welch t, events vs clock-matched controls"
    } else if (kk >= 2 && main == "mean_change") {
      tt_0 <- stats::t.test(v, conf.level = conf.level)
      dif <- mean(v, na.rm = TRUE)
      dlo <- tt_0$conf.int[1]
      dhi <- tt_0$conf.int[2]
      pv <- tt_0$p.value
      test <- "one-sample t vs 0 (no controls)"
    } else {
      dif <- dlo <- dhi <- pv <- NA_real_
      test <- if (kk >= 2) "not tested: a maximum or area needs controls" else
        "not tested: fewer than 2 events"
    }
    summ[[ty]] <- tibble::tibble(
      event_type = ty, n_events = kk, n_overlapping_excluded =
        sum(inside & overlap & type == ty & exclude_overlap),
      main_measure = main, mean = mean(v, na.rm = TRUE),
      conf.low = mean(v, na.rm = TRUE) - zt(kk) * stats::sd(v, na.rm = TRUE) / sqrt(kk),
      conf.high = mean(v, na.rm = TRUE) + zt(kk) * stats::sd(v, na.rm = TRUE) / sqrt(kk),
      control_mean = if (length(cv)) mean(cv) else NA_real_,
      n_controls = length(cv),
      difference = dif, diff.low = dlo, diff.high = dhi,
      p.value = pv, test = test,
      median_time_to_peak = stats::median(pe$time_to_peak, na.rm = TRUE),
      peak_lag_mean_curve = {
        pt <- tb[tb$lag > 0, ]
        pt$lag[which.max(abs(pt$mean_change))]
      }
    )
  }
  out <- dplyr::bind_rows(rows)
  pe_all <- dplyr::bind_rows(per_event)

  # covariates: multiple regression of the main measure, Holm across covariates
  ce <- NULL
  if (length(cv_names) && nrow(pe_all)) {
    ce <- dplyr::bind_rows(lapply(split(pe_all, pe_all$event_type), function(d) {
      d <- d[stats::complete.cases(d[, c("main_value", cv_names)]), ]
      if (nrow(d) < length(cv_names) + 3) return(NULL)
      f <- stats::lm(stats::reformulate(cv_names, "main_value"), data = d)
      ct <- summary(f)$coefficients[-1, , drop = FALSE]
      ci <- stats::confint(f, level = conf.level)[-1, , drop = FALSE]
      tibble::tibble(event_type = d$event_type[1], covariate = rownames(ct),
                     n_events = nrow(d), slope = ct[, 1], std.error = ct[, 2],
                     conf.low = ci[, 1], conf.high = ci[, 2],
                     p.value = ct[, 4],
                     p.adjusted = stats::p.adjust(ct[, 4], "holm"))
    }))
  }
  # factors: day part and co-occurring events
  ft <- NULL
  if (nrow(pe_all)) {
    ft <- dplyr::bind_rows(lapply(split(pe_all, pe_all$event_type), function(d) {
      res <- list()
      for (fc in intersect(c("day_part", "co_event"), names(d))) {
        g <- factor(d[[fc]])
        okg <- !is.na(d$main_value) & !is.na(g)
        g <- droplevels(g[okg])
        v <- d$main_value[okg]
        if (nlevels(g) < 2 || any(table(g) < 2)) next
        if (nlevels(g) == 2) {
          pp <- stats::t.test(v ~ g)$p.value
          pn <- suppressWarnings(stats::wilcox.test(v ~ g, exact = FALSE))$p.value
          nm <- c("Welch t test", "Mann-Whitney U")
        } else {
          pp <- stats::oneway.test(v ~ g)$p.value
          pn <- stats::kruskal.test(v ~ g)$p.value
          nm <- c("Welch ANOVA", "Kruskal-Wallis")
        }
        res[[fc]] <- tibble::tibble(event_type = d$event_type[1], factor = fc,
                                    levels = nlevels(g),
                                    parametric = nm[1], p_parametric = pp,
                                    nonparametric = nm[2], p_nonparametric = pn)
      }
      dplyr::bind_rows(res)
    }))
  }
  sm_all <- dplyr::bind_rows(summ)
  if (nrow(sm_all)) {
    sm_all$p.adjusted <- stats::p.adjust(sm_all$p.value, "holm")
    sm_all <- dplyr::relocate(sm_all, "p.adjusted", .after = "p.value")
  }
  attr(out, "summary") <- sm_all
  attr(out, "per_event") <- pe_all
  attr(out, "controls") <- dplyr::bind_rows(ctrl_all)
  attr(out, "excluded") <- excluded
  attr(out, "covariate_effect") <- ce
  attr(out, "factor_tests") <- ft
  attr(out, "warnings") <- warn
  out
}

# Area of the positive part of a piecewise-linear curve through (x, y),
# with zero crossings interpolated.
.kk_pos_area <- function(x, y) {
  if (length(x) < 2) return(NA_real_)
  a <- 0
  for (i in seq_len(length(x) - 1)) {
    x1 <- x[i]; x2 <- x[i + 1]; y1 <- y[i]; y2 <- y[i + 1]
    if (y1 >= 0 && y2 >= 0) {
      a <- a + (x2 - x1) * (y1 + y2) / 2
    } else if (y1 > 0 || y2 > 0) {
      xc <- x1 + (x2 - x1) * y1 / (y1 - y2)
      a <- a + if (y1 > 0) (xc - x1) * y1 / 2 else (x2 - xc) * y2 / 2
    }
  }
  a
}

#' Distributed-Lag Effect of Events or Exposures (KK)

#'
#' @description At which lag after an event (a dose, a meal, a pollution
#'   peak) does the effect appear, and how long does it last? The outcome is
#'   regressed on the exposure at lags 0 to `max_lag` simultaneously,
#'   \eqn{y_t = \alpha + \sum_{l=0}^{L} \beta_l x_{t-l} + \varepsilon_t},
#'   optionally with a daily rhythm (`period`) and a linear trend. Each
#'   \eqn{\beta_l} is the effect at lag \eqn{l}; the sum is the cumulative
#'   effect. Standard errors are Newey-West, and the per-lag p values are
#'   adjusted by Benjamini-Hochberg. The exposure is a column of the series
#'   (an event indicator, a dose, a concentration).
#'
#' @param data Data frame with the series, equally spaced without gaps
#'   (rows with a missing value create gaps; fill them first).
#' @param outcome Outcome column.
#' @param exposure Exposure column (0/1 event marker, dose, level).
#' @param time Time column.
#' @param max_lag Largest lag, in steps (default 12).
#' @param period Optional period of a rhythm adjusted for (e.g. 24 h; in the
#'   units of `time`).
#' @param trend Adjust for a linear trend (default `TRUE`).
#' @param conf.level Confidence level (default 0.95).
#'
#' @return Tibble with one row per lag (`lag`, `lag_time`, `estimate`,
#'   `std.error`, `conf.low`, `conf.high`, `p.value`, `p.adjusted`) and a
#'   final `cumulative` row.
#'
#' @references Schwartz J (2000) The distributed lag between air pollution
#'   and daily deaths. *Epidemiology* 11:320-326.
#'   \doi{10.1097/00001648-200005000-00016}
#'   Benjamini Y, Hochberg Y (1995) Controlling the false discovery rate.
#'   *Journal of the Royal Statistical Society B* 57:289-300.
#'
#' @seealso [kk_epoch_response()], [kk_its()]
#'
#' @examples
#' n1 <- kk_cycle_example("nof1")
#' s <- n1$series
#' s$dose <- as.integer(s$time %in% n1$events$time)
#' kk_distributed_lag(s, glucose, dose, time, max_lag = 36, period = 24)
#'
#' @export
kk_distributed_lag <- function(data, outcome, exposure, time, max_lag = 12,
                               period = NULL, trend = TRUE,
                               conf.level = 0.95) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  y_name <- .kk_colname(rlang::enquo(outcome))
  x_name <- .kk_colname(rlang::enquo(exposure))
  t_name <- .kk_colname(rlang::enquo(time))
  data <- .kk_ts_prepare(data, c(y_name, x_name, t_name),
                         numeric = c(y_name, x_name))
  data <- data[order(data[[t_name]]), , drop = FALSE]
  info <- .kk_ts_time(data[[t_name]])
  t <- info$t
  # lags are row offsets: a gap would silently become one step
  .kk_ts_check_regular(t, "kk_distributed_lag()")
  tt <- .kk_phase_time(data[[t_name]], info)
  y <- as.numeric(data[[y_name]])
  x <- as.numeric(data[[x_name]])
  n <- length(y)
  if (max_lag < 0 || max_lag != round(max_lag) || n - max_lag < max_lag + 10) {
    stop("`max_lag` must be a whole number well below the series length.",
         call. = FALSE)
  }
  step <- stats::median(diff(t))
  L <- sapply(0:max_lag, function(l) c(rep(NA, l), x[seq_len(n - l)]))
  colnames(L) <- paste0("lag", 0:max_lag)
  X <- L
  if (trend) X <- cbind(X, trend_t = t - min(t))
  if (!is.null(period)) {
    X <- cbind(X, cos_p = cos(2 * pi * tt / period), sin_p = sin(2 * pi * tt / period))
  }
  d <- stats::na.omit(data.frame(.y = y, X))
  fit <- stats::lm(.y ~ ., data = d)
  V <- .kk_ts_vcov(fit, robust = TRUE)
  cf <- stats::coef(fit)
  lg <- paste0("lag", 0:max_lag)
  est <- cf[lg]
  se <- sqrt(diag(V)[lg])
  z <- stats::qt(1 - (1 - conf.level) / 2, fit$df.residual)
  p <- 2 * stats::pt(-abs(est / se), fit$df.residual)
  w <- stats::setNames(rep(0, length(cf)), names(cf))
  w[lg] <- 1
  ce <- sum(est)
  cse <- sqrt(drop(t(w) %*% V %*% w))
  dplyr::bind_rows(
    tibble::tibble(lag = as.character(0:max_lag), lag_time = (0:max_lag) * step,
                   estimate = unname(est), std.error = unname(se),
                   conf.low = unname(est - z * se), conf.high = unname(est + z * se),
                   p.value = unname(p), p.adjusted = stats::p.adjust(unname(p), "BH")),
    tibble::tibble(lag = "cumulative", lag_time = NA_real_, estimate = ce,
                   std.error = cse, conf.low = ce - z * cse,
                   conf.high = ce + z * cse,
                   p.value = 2 * stats::pt(-abs(ce / cse), fit$df.residual),
                   p.adjusted = NA_real_)
  )
}

#' Randomised N-of-1 Schedule (KK)
#'
#' @description Generates the treatment schedule for an N-of-1 trial: the
#'   record is divided into blocks (pairs of days, weeks, ...) and within
#'   every block each treatment is given once, in an independently randomised
#'   order. Randomising within blocks protects the comparison against trends
#'   and day-of-week effects, and it is what the randomisation test in
#'   [kk_nof1()] relies on.
#'
#' @param n_blocks Number of blocks.
#' @param treatments Treatment labels (default `c("A", "B")`).
#' @param start Optional start (`Date` or `POSIXct`); adds a `start` column.
#' @param period_length Length of one treatment period in days (default 1).
#' @param seed Optional random seed; record it in the protocol.
#'
#' @return Tibble with `block`, `period` (1, 2, ...), `position` within the
#'   block, `treatment` and, with `start`, the period's start date.
#'
#' @references Kravitz RL, Duan N, eds. (2014) *Design and Implementation of
#'   N-of-1 Trials: A User's Guide*. AHRQ Publication No. 13(14)-EHC122-EF.
#'
#' @seealso [kk_nof1()], [kk_cycle_power()]
#'
#' @examples
#' kk_nof1_schedule(10, c("no drug", "drug"), start = as.Date("2026-12-01"),
#'                  seed = 42)
#'
#' @export
kk_nof1_schedule <- function(n_blocks, treatments = c("A", "B"), start = NULL,
                             period_length = 1, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  m <- length(treatments)
  if (m < 2) stop("Give at least two treatments.", call. = FALSE)
  tr <- unlist(lapply(seq_len(n_blocks), function(b) sample(treatments)))
  out <- tibble::tibble(block = rep(seq_len(n_blocks), each = m),
                        period = seq_len(n_blocks * m),
                        position = rep(seq_len(m), n_blocks),
                        treatment = tr)
  if (!is.null(start)) {
    out$start <- if (inherits(start, "POSIXt")) {
      start + (out$period - 1) * period_length * 86400
    } else start + (out$period - 1) * period_length
  }
  out
}

#' Analyse an N-of-1 Trial (KK)
#'
#' @description Estimates the treatment effect in a block-randomised N-of-1
#'   trial from one outcome value per treatment period (aggregate first, e.g.
#'   the daily mean glucose). The effect is the mean within-block difference
#'   between `active` and the reference treatment; it is tested with a paired
#'   t test and with the exact randomisation test that follows the design
#'   (every block's two labels swapped in all \eqn{2^k} ways, or 10 000
#'   random swaps when \eqn{k > 15}).
#'
#' @param data Data frame, one row per treatment period.
#' @param outcome Outcome per period.
#' @param treatment Treatment column.
#' @param block Block column.
#' @param active Level of `treatment` regarded as active (default: the
#'   second level).
#' @param conf.level Confidence level (default 0.95).
#'
#' @return One-row tibble: `n_blocks`, `effect` (active minus reference),
#'   `std.error`, `conf.low`, `conf.high`, `p_ttest`, `p_randomisation`,
#'   `reference`, `active`. Attribute `blocks`: per-block differences.
#'
#' @seealso [kk_nof1_schedule()], [kk_epoch_response()]
#'
#' @examples
#' s <- kk_nof1_schedule(10, c("no drug", "drug"), seed = 1)
#' s$glucose <- 8 - 0.9 * (s$treatment == "drug") + rnorm(20, 0, 0.6)
#' kk_nof1(s, glucose, treatment, block, active = "drug")
#'
#' @export
kk_nof1 <- function(data, outcome, treatment, block, active = NULL,
                    conf.level = 0.95) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  y_name <- .kk_colname(rlang::enquo(outcome))
  tr_name <- .kk_colname(rlang::enquo(treatment))
  b_name <- .kk_colname(rlang::enquo(block))
  data <- .kk_ts_prepare(data, c(y_name, tr_name, b_name))
  tr <- factor(data[[tr_name]])
  if (nlevels(tr) != 2) stop("`treatment` must have exactly two levels.",
                             call. = FALSE)
  if (is.null(active)) active <- levels(tr)[2]
  ref <- setdiff(levels(tr), active)
  agg <- stats::aggregate(data[[y_name]],
                          list(block = data[[b_name]], tr = as.character(tr)),
                          mean)
  w <- stats::reshape(agg, idvar = "block", timevar = "tr", direction = "wide")
  a <- w[[paste0("x.", active)]]
  r <- w[[paste0("x.", ref)]]
  ok <- !is.na(a) & !is.na(r)
  d <- a[ok] - r[ok]
  k <- length(d)
  if (k < 2) stop("At least two complete blocks are needed.", call. = FALSE)
  m <- mean(d)
  se <- stats::sd(d) / sqrt(k)
  tq <- stats::qt(1 - (1 - conf.level) / 2, k - 1)
  p_t <- 2 * stats::pt(-abs(m / se), k - 1)
  if (k <= 15) {
    signs <- as.matrix(expand.grid(rep(list(c(-1, 1)), k)))
  } else {
    signs <- matrix(sample(c(-1, 1), 10000 * k, replace = TRUE), ncol = k)
  }
  null <- drop(signs %*% d) / k
  p_r <- mean(abs(null) >= abs(m) - 1e-12)
  out <- tibble::tibble(n_blocks = k, effect = m, std.error = se,
                        conf.low = m - tq * se, conf.high = m + tq * se,
                        p_ttest = p_t, p_randomisation = p_r,
                        reference = ref, active = active)
  attr(out, "blocks") <- tibble::tibble(block = w$block[ok], active = a[ok],
                                        reference = r[ok], difference = d)
  out
}
