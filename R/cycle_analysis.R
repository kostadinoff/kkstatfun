# ============================================================
# FULL ANALYSIS OF A BIOMEDICAL TIME SERIES, WITH REPORT TEXT
# ============================================================

#' Full Analysis of a Biomedical Time Series (KK)
#'
#' @description Runs every analysis that suits the data, in a fixed order,
#'   and writes the conclusion and report text — the "full analysis" of
#'   CycleLab (Stefanov 2026):
#'
#'   1. distribution, step, gaps and outliers ([kk_ts_describe()]);
#'   2. trend with the autocorrelation-corrected test and the comparison of
#'      trend functions ([kk_ts_trend()]); when the trend is not significant
#'      on its own but cycles are found, it is re-tested together with them;
#'   3. cycles with the T-R periodogram and the red-noise test
#'      ([kk_periodogram()]);
#'   4. with clock time: the daily rhythm by cosinor, per day and as the
#'      population cosinor of the days ([kk_cosinor()]), and the parts of the
#'      day ([kk_day_parts()]);
#'   5. text columns with a few repeated levels (stage, treatment) as
#'      factors, compared on day means (clock time) so that neighbouring
#'      measurements are not counted as independent; with clock time the
#'      daily rhythm is also compared between the stages
#'      ([kk_cosinor_compare()]);
#'   6. with an event log: the response after events by category
#'      ([kk_epoch_response()]);
#'   7. the trend-plus-cycles model and, with `horizon`, the validated
#'      forecast ([kk_harmonic_model()]).
#'
#'   Every automatic decision is recorded with its reason. The conclusion
#'   lists separately what was **established** (significant after the stated
#'   corrections) and what was **not established**; "not established" does
#'   not mean "absent" — the data may be insufficient — and an association
#'   with a cycle, trend or event does not prove causation.
#'
#' @param data Data frame with the series. A grouped data frame is analysed
#'   group by group: the result is then a named list with one
#'   `kk_cycle_analysis` per group.
#' @param outcome Measured variable.
#' @param time Time column: numeric, `Date` or `POSIXct`.
#' @param events Optional event log (data frame).
#' @param event_time,event_type,covariates Columns of `events` (see
#'   [kk_epoch_response()]).
#' @param window Window around events (default `c(-0.5, 3)` h for clock
#'   time).
#' @param trend Trend function (default `"linear"`).
#' @param include_trend How the trend enters the cycle search and the model:
#'   `"test"` (default) — only when it is significant, alone or together with
#'   the cycles; `"always"`; `"never"`. When the chosen trend cannot be
#'   tested (e.g. a Verhulst fit that does not converge, or too few effective
#'   observations), `"test"` leaves it out and records the decision.
#' @param calendar Calendar periods passed to [kk_periodogram()].
#' @param horizon Forecast horizon in steps (default 0).
#' @param n_sim Surrogates for the red-noise test (default 500).
#' @param unit Measurement unit for the text (e.g. `"mmol/l"`).
#' @param label Name of the variable for the text (default the column name).
#' @param seed Random seed (default 1).
#'
#' @details **Factors.** Text columns with two to six levels are compared on
#'   independent units, not on measurements: each unit is a contiguous
#'   segment of the record at one level (for clock time, cut also at
#'   midnight, so a unit is one day at one level), and its mean enters a
#'   Welch t test / Welch ANOVA and a Mann-Whitney / Kruskal-Wallis test.
#'   Neighbouring segments may still be correlated; when the level changes
#'   within days, segments of the same day share that day, and a decision
#'   says so.
#'
#'   **What is not automated.** Interrupted time series around an
#'   intervention ([kk_its()]), the joint type-II factor analysis and the
#'   aligned-rank analysis of CycleLab, and the cross-wavelet map are not part
#'   of the automatic run; see the vignette for the reduced scope.
#'
#' @return A list of class `kk_cycle_analysis` with the results of each step
#'   (`describe`, `trend`, `cycles`, `cosinor`, `cosinor_days`, `day_parts`,
#'   `factors`, `stages`, `events`, `model`), `established`, `not_established`,
#'   `decisions`, `methods_text`, `results_text` and `limitations`.
#'
#' @references Stefanov R (2026) *CycleLab: analysis of biomedical time
#'   series*, version 0.14.4. Plovdiv: Institute for Rare Diseases.
#'   <https://cyclelab.raredis.org>
#'
#' @seealso [kk_periodogram()], [kk_cosinor()], [kk_harmonic_model()]
#'
#' @examples
#' pn <- kk_cycle_example("pneumonia")
#' fa <- kk_cycle_analysis(pn$series, rate, month, calendar = c(12, 6),
#'                         horizon = 12, n_sim = 200,
#'                         label = "pneumonia hospitalisations",
#'                         unit = "per 100 000")
#' fa
#' cat(fa$methods_text)
#'
#' @export
kk_cycle_analysis <- function(data, outcome, time, events = NULL,
                              event_time = NULL, event_type = NULL,
                              covariates = NULL, window = NULL,
                              trend = "linear",
                              include_trend = c("test", "always", "never"),
                              calendar = NULL, horizon = 0,
                              n_sim = 500, unit = NULL, label = NULL,
                              seed = 1) {
  if (dplyr::is_grouped_df(data)) {
    cl <- match.call()
    env <- parent.frame()
    keys <- dplyr::group_keys(data)
    parts <- dplyr::group_split(data)
    out <- lapply(parts, function(d) {
      cl$data <- dplyr::ungroup(d)
      eval(cl, env)
    })
    names(out) <- do.call(paste, c(lapply(keys, as.character), sep = " / "))
    return(out)
  }
  include_trend <- match.arg(include_trend)
  y_name <- .kk_colname(rlang::enquo(outcome))
  t_name <- .kk_colname(rlang::enquo(time))
  data <- .kk_ts_prepare(data, c(y_name, t_name), numeric = y_name)
  data <- data[order(data[[t_name]]), , drop = FALSE]
  info <- .kk_ts_time(data[[t_name]])
  if (is.null(label)) label <- y_name
  u <- if (is.null(unit)) "" else paste0(" ", unit)
  L <- .kk_txt()
  res <- list()
  ran <- list()
  est <- character(0)
  nest <- character(0)
  dec <- character(0)
  fmt <- function(x, d = 2) formatC(x, digits = d, format = "f")
  fp <- function(p) if (p < 0.001) "p < 0.001" else
    paste0("p = ", formatC(p, digits = 3, format = "f"))

  # 1. description
  res$describe <- kk_ts_describe(data, !!y_name, !!t_name)

  # 2. trend
  res$trend <- tryCatch(suppressMessages(suppressWarnings(
    kk_ts_trend(data, !!y_name, !!t_name, form = trend))),
    error = function(e) NULL)
  tr <- res$trend
  tr_tested <- !is.null(tr) && !is.na(tr$p.value)
  tr_sig <- switch(include_trend,
    always = !is.null(tr), never = FALSE,
    test = tr_tested && tr$p.value < 0.05)
  if (include_trend == "test" && !tr_tested) {
    dec <- c(dec, sprintf(L$trend_untested, trend))
  }
  ran$trend_test <- tr_tested
  trend_by <- if (include_trend == "test" && tr_sig) "alone" else include_trend

  # 3. cycles
  run_pg <- function(detr) suppressWarnings(kk_periodogram(
    data, !!y_name, !!t_name, detrend = detr, n_sim = n_sim,
    calendar = calendar, seed = seed))
  res$cycles <- run_pg(if (tr_sig) trend else "none")
  cyc <- res$cycles[res$cycles$in_model, , drop = FALSE]
  dec <- c(dec, attr(res$cycles, "decisions"))
  if (include_trend == "test" && !tr_sig && nrow(cyc) &&
      trend %in% .kk_trend_joint && trend != "none") {
    m0 <- suppressWarnings(kk_harmonic_model(data, !!y_name, !!t_name,
                                             periods = cyc$model_period,
                                             trend = trend))
    tt <- m0[grepl("^trend", m0$term), ][1, ]
    if (nrow(tt) && isTRUE(tt$p.value < 0.05)) {
      tr_sig <- TRUE
      trend_by <- "with_cycles"
      res$trend_with_cycles <- tt
      dec <- c(dec, L$trend_with_cycles)
      res$cycles <- run_pg(trend)
      cyc <- res$cycles[res$cycles$in_model, , drop = FALSE]
    }
  }
  if (tr_sig && trend_by %in% c("alone", "with_cycles")) {
    if (trend_by == "alone") {
      est <- c(est, sprintf(L$est_trend, tr$formula, fmt(tr$A, 3), fp(tr$p.value)))
    } else {
      est <- c(est, sprintf(L$est_trend_cyc, tr$formula,
                            fmt(res$trend_with_cycles$estimate, 3),
                            fp(res$trend_with_cycles$p.value)))
    }
  } else if (include_trend == "always") {
    dec <- c(dec, sprintf(L$trend_always, trend))
  } else if (tr_tested) {
    nest <- c(nest, L$nest_trend)
  }
  if (nrow(cyc)) {
    for (i in seq_len(nrow(cyc))) {
      est <- c(est, sprintf(L$est_cycle, fmt(cyc$model_period[i], 3),
                            fmt(cyc$amplitude[i]), u, fp(cyc$p_value[i])))
    }
  } else nest <- c(nest, L$nest_cycle)

  # 4. daily rhythm
  clock <- info$class == "POSIXct"
  if (clock) {
    cz <- tryCatch(kk_cosinor(data, !!y_name, !!t_name, period = 24,
                              by_day = TRUE, max_gap = 12),
                   error = function(e) NULL)
    res$cosinor <- cz
    if (!is.null(cz)) {
      ran$cosinor <- cz$se_method
      pdays <- attr(cz, "per_day")
      if (!is.null(pdays) && nrow(pdays) >= 3) {
        dd <- data
        dd$.day <- as.character(.kk_ts_day(info))
        dd <- dd[dd$.day %in% pdays$day, , drop = FALSE]
        res$cosinor_days <- tryCatch(
          kk_cosinor_population(dd, !!y_name, !!t_name, subject = ".day"),
          error = function(e) NULL)
        ran$cosinor_days <- !is.null(res$cosinor_days)
      }
      if (isTRUE(cz$p_value < 0.05)) {
        est <- c(est, sprintf(L$est_rhythm, fmt(cz$mesor), u, fmt(cz$amplitude), u,
                              .kk_hhmm(cz$acrophase), fp(cz$p_value)))
      } else nest <- c(nest, L$nest_rhythm)
    }
    res$day_parts <- tryCatch(kk_day_parts(data, !!y_name, !!t_name),
                              error = function(e) NULL)
    if (!is.null(res$day_parts)) {
      pdp <- res$day_parts$p.value[2]
      if (isTRUE(pdp < 0.05)) est <- c(est, sprintf(L$est_parts, fp(pdp))) else
        nest <- c(nest, L$nest_parts)
    }
  }

  # 5. factors from text columns, compared on independent segments
  fac_cols <- names(data)[vapply(data, function(x) {
    (is.character(x) || is.factor(x)) && length(unique(x)) >= 2 &&
      length(unique(x)) <= 6
  }, logical(1))]
  if (length(fac_cols)) {
    day <- if (clock) as.character(.kk_ts_day(info)) else rep("1", nrow(data))
    res$factors <- dplyr::bind_rows(lapply(fac_cols, function(fc) {
      lev <- as.character(data[[fc]])
      ok <- !is.na(lev)
      brk <- c(TRUE, lev[-1] != lev[-length(lev)] | day[-1] != day[-length(day)])
      seg <- cumsum(brk)
      ag <- stats::aggregate(list(v = data[[y_name]][ok]),
                             list(seg = seg[ok], level = lev[ok]), mean)
      if (length(unique(ag$level)) < 2 || any(table(ag$level) < 2)) return(NULL)
      g <- factor(ag$level)
      pp <- if (nlevels(g) == 2) stats::t.test(ag$v ~ g)$p.value else
        stats::oneway.test(ag$v ~ g)$p.value
      pn <- if (nlevels(g) == 2) suppressWarnings(stats::wilcox.test(ag$v ~ g, exact = FALSE))$p.value else
        stats::kruskal.test(ag$v ~ g)$p.value
      means <- tapply(ag$v, g, mean)
      mixed <- clock && any(tapply(lev[ok], day[ok], function(z) length(unique(z))) > 1)
      tibble::tibble(factor = fc,
                     unit = if (clock) "day segment" else "segment",
                     n_units = nrow(ag), mixed_days = mixed,
                     levels = paste(sprintf("%s: %s", names(means), fmt(means)), collapse = "; "),
                     p_parametric = pp, p_nonparametric = pn)
    }))
    if (!is.null(res$factors) && nrow(res$factors)) {
      for (i in seq_len(nrow(res$factors))) {
        f <- res$factors[i, ]
        if (isTRUE(f$p_parametric < 0.05)) {
          est <- c(est, sprintf(L$est_factor, f$factor, f$levels, fp(f$p_parametric)))
        } else nest <- c(nest, sprintf(L$nest_factor, f$factor))
        if (f$n_units < 6) dec <- c(dec, sprintf(L$factor_few, f$factor, f$n_units))
        if (isTRUE(f$mixed_days)) dec <- c(dec, sprintf(L$factor_mixed, f$factor))
      }
    }
    if (clock) {
      res$stages <- dplyr::bind_rows(lapply(fac_cols, function(fc) {
        cc <- tryCatch(suppressWarnings(
          kk_cosinor_compare(data, !!y_name, !!t_name, !!fc)),
          error = function(e) NULL)
        if (is.null(cc)) return(NULL)
        dff <- attr(cc, "differences")
        tibble::tibble(factor = fc, test = cc$test, p_value = cc$p_value,
                       method = cc$method,
                       shift = paste(sprintf("%s: %s h", dff$group,
                                             fmt(dff$acrophase_shift, 1)),
                                     collapse = "; "),
                       amp = paste(sprintf("%s: %s", dff$group,
                                           fmt(dff$amplitude_diff)),
                                   collapse = "; "))
      }))
      if (!is.null(res$stages) && nrow(res$stages)) {
        for (fc in unique(res$stages$factor)) {
          st <- res$stages[res$stages$factor == fc, ]
          pa <- st$p_value[st$test == "acrophase"]
          pm <- st$p_value[st$test == "amplitude"]
          if (isTRUE(pa < 0.05)) est <- c(est, sprintf(L$est_stage_phase, fc, st$shift[1], fp(pa)))
          if (isTRUE(pm < 0.05)) est <- c(est, sprintf(L$est_stage_amp, fc, st$amp[1], u, fp(pm)))
          if (!isTRUE(pa < 0.05) && !isTRUE(pm < 0.05)) nest <- c(nest, sprintf(L$nest_stage, fc))
        }
      }
    }
  }

  # 6. events: event windows against clock-matched controls, Holm across types
  if (!is.null(events)) {
    ev_q <- rlang::enquo(event_time)
    ty_q <- rlang::enquo(event_type)
    cv_q <- rlang::enquo(covariates)
    if (is.null(window)) window <- if (clock) c(-0.5, 3) else c(-1, 3)
    res$events <- tryCatch(suppressWarnings(kk_epoch_response(
      data, !!y_name, !!t_name, events, !!ev_q, event_type = !!ty_q,
      covariates = !!cv_q, window = window, n_control = 100, seed = seed)),
      error = function(e) {
        dec <<- c(dec, paste("The event analysis failed:", conditionMessage(e)))
        NULL
      })
    if (!is.null(res$events)) {
      sm <- attr(res$events, "summary")
      for (i in seq_len(nrow(sm))) {
        if (isTRUE(sm$p.adjusted[i] < 0.05)) {
          est <- c(est, sprintf(L$est_event, sm$event_type[i], sm$main_measure[i],
                                fmt(sm$difference[i]), u, sm$n_events[i],
                                fp(sm$p.adjusted[i])))
        } else if (is.na(sm$p.value[i])) {
          nest <- c(nest, sprintf(L$nest_event_untested, sm$event_type[i]))
        } else nest <- c(nest, sprintf(L$nest_event, sm$event_type[i]))
      }
      dec <- c(dec, attr(res$events, "warnings"))
    }
  }

  # 7. model and forecast
  if (nrow(cyc)) {
    res$model <- tryCatch(suppressWarnings(suppressMessages(kk_harmonic_model(
      data, !!y_name, !!t_name, periods = cyc$model_period,
      trend = if (tr_sig) trend else "none", horizon = horizon, seed = seed))),
      error = function(e) {
        dec <<- c(dec, paste("The trend-plus-cycles model failed:", conditionMessage(e)))
        NULL
      })
    if (!is.null(res$model)) dec <- c(dec, attr(res$model, "decisions"))
  }

  res$established <- est
  res$not_established <- nest
  res$decisions <- unique(dec)
  res$methods_text <- .kk_methods_text(res, L, clock, ran, n_sim, trend, tr_sig)
  res$results_text <- .kk_results_text(res, L, label, u, fmt, fp, clock)
  res$limitations <- L$limitations
  class(res) <- c("kk_cycle_analysis", "list")
  res
}

#' @export
print.kk_cycle_analysis <- function(x, ...) {
  cat("Established (significant after the stated corrections):\n")
  if (length(x$established)) cat(paste0("  * ", x$established, "\n"), sep = "") else
    cat("  (nothing)\n")
  cat("\nNot established (does not mean absent):\n")
  if (length(x$not_established)) cat(paste0("  - ", x$not_established, "\n"), sep = "") else
    cat("  (nothing)\n")
  if (length(x$decisions)) {
    cat("\nAutomatic decisions:\n")
    cat(paste0("  > ", x$decisions, "\n"), sep = "")
  }
  invisible(x)
}

.kk_hhmm <- function(h) {
  m <- round((h %% 24) * 60) %% (24 * 60)
  sprintf("%02d:%02d", m %/% 60, m %% 60)
}

# Methods text built from what actually ran.
.kk_methods_text <- function(res, L, clock, ran, n_sim, trend, tr_sig) {
  parts <- L$m_intro
  if (isTRUE(ran$trend_test)) parts <- c(parts, sprintf(L$m_trend, trend))
  if (!is.null(res$cycles)) {
    parts <- c(parts, sprintf(if (tr_sig) L$m_cycles_detr else L$m_cycles, n_sim))
  }
  if (!is.null(res$cosinor)) {
    parts <- c(parts, sprintf(L$m_cosinor, ran$cosinor))
    if (isTRUE(ran$cosinor_days)) parts <- c(parts, L$m_cosinor_days)
  }
  if (!is.null(res$day_parts)) parts <- c(parts, L$m_parts)
  if (!is.null(res$factors) && nrow(res$factors)) parts <- c(parts, L$m_factors)
  if (!is.null(res$stages) && nrow(res$stages)) parts <- c(parts, L$m_stages)
  if (!is.null(res$events)) parts <- c(parts, L$m_events)
  if (!is.null(res$model)) {
    parts <- c(parts, if (isTRUE(attr(res$model, "fit")$joint)) L$m_model else L$m_model_2stage)
    if (!is.null(attr(res$model, "validation"))) parts <- c(parts, L$m_forecast)
  }
  parts <- c(parts, L$m_soft)
  paste(parts, collapse = " ")
}

.kk_results_text <- function(res, L, label, u, fmt, fp, clock) {
  d <- res$describe
  unit_txt <- d$unit
  s <- sprintf(L$r_desc, d$n, label, fmt(d$duration, 1), unit_txt,
               if (d$near_normal) sprintf(L$r_meansd, fmt(d$mean), fmt(d$sd), u) else
                 sprintf(L$r_mediqr, fmt(d$median), fmt(d$q1), fmt(d$q3), u))
  out <- s
  if (length(res$established)) out <- c(out, paste(res$established, collapse = " "))
  if (length(res$not_established)) {
    out <- c(out, sprintf(L$r_not, paste(res$not_established, collapse = " ")))
  }
  if (!is.null(res$model)) {
    f <- attr(res$model, "fit")
    v <- attr(res$model, "validation")
    out <- c(out, sprintf(L$r_model, fmt(f$R), fmt(f$PrIdx)))
    if (!is.null(v) && !is.na(v$better_than_naive)) {
      out <- c(out, if (v$better_than_naive)
        sprintf(L$r_val_good, fmt(v$error_reduction_pct, 0)) else L$r_val_bad)
    }
  }
  paste(out, collapse = " ")
}

.kk_txt <- function() {
    list(
    trend_with_cycles = "The trend was not significant on its own but was significant together with the accepted cycles in one harmonic regression; it was included and the cycles were searched again in the detrended series.",
    est_trend = "The trend (%s) is statistically significant: slope A = %s (%s, t test with the effective number of observations).",
    nest_trend = "No significant trend was found.",
    trend_untested = "The %s trend could not be tested (fit failed or too few effective observations); it was not removed before the cycle search. Set include_trend = \"always\" to include it anyway.",
    trend_always = "The %s trend was included in the cycle search and the model by request (include_trend = \"always\"), without a test.",
    est_trend_cyc = "The trend (%s) is statistically significant together with the cycles: slope A = %s (%s, harmonic regression with Newey-West standard errors).",
    est_cycle = "A cycle with period %s and amplitude %s%s was found (%s, test against autocorrelated noise).",
    nest_cycle = "No cycles significant against autocorrelated noise were found.",
    est_rhythm = "A daily rhythm was found: MESOR %s%s, amplitude %s%s, peak at %s (%s).",
    nest_rhythm = "No daily rhythm was found.",
    est_parts = "Values differ between parts of the day (Friedman test, %s).",
    nest_parts = "No differences between parts of the day were found.",
    est_factor = "Values differ by '%s' (%s; %s).",
    nest_factor = "No differences by '%s' were found.",
    est_stage_phase = "The peak of the daily rhythm shifts between the stages of '%s' (%s relative to the first stage; %s).",
    est_stage_amp = "The rhythm amplitude differs between the stages of '%s' (%s%s; %s).",
    nest_stage = "No difference in the daily rhythm between the stages of '%s' was found.",
    factor_few = "Factor '%s' was compared on only %d independent segments; the test has little power.",
    factor_mixed = "The level of '%s' changes within days: segments of the same day share that day and are not fully independent.",
    est_event = "The variable changes after '%s' events (%s, events minus clock-matched controls: %s%s, %d events, Holm-adjusted %s).",
    nest_event = "No response after '%s' events was found (compared with clock-matched controls).",
    nest_event_untested = "The response after '%s' events could not be tested (no clean control windows, or too few events).",
    m_intro = "The series was described by the mean and standard deviation, or the median and quartiles, according to the Shapiro-Wilk test; values more than three standard deviations from the mean were checked as possible entry errors.",
    m_trend = "The trend was described by a %s function fitted by least squares, and the slope was tested with a t test using the effective number of observations N(1 - r)/(1 + r), r being the bias-corrected lag-1 autocorrelation of the residuals; the trend shape was compared with fifteen two-parameter functions by R-squared.",
    m_cycles = "Cyclicity was examined with the T-R periodogram of Komitov; peaks were tested against first-order autoregressive surrogates (%d in a pilot set for standardisation and as many in an independent reference set) with a max-T test over all trial periods, and cycles were extracted sequentially, each step conditional on the cycles already accepted.",
    m_cycles_detr = "Cyclicity was examined with the T-R periodogram of Komitov on the detrended series (F1(t) = F(t) - Y(t) + mean F); peaks were tested against first-order autoregressive surrogates (%d in a pilot set for standardisation and as many in an independent reference set) with a max-T test over all trial periods, and cycles were extracted sequentially, each step conditional on the cycles already accepted.",
    m_cosinor = "The daily rhythm was estimated by cosinor analysis with a 24-h period (MESOR, amplitude and acrophase with confidence intervals; %s standard errors); clock time kept the UTC offset of the first measurement.",
    m_cosinor_days = "A cosinor was also fitted to every day whose measurements covered the day without a gap longer than 12 h, and the days were combined as a population-mean cosinor.",
    m_parts = "Mean values in the morning (05-11 h), midday (11-16 h), evening (16-22 h) and night (22-05 h) were compared by ANOVA with day as a blocking factor and by the Friedman test, and pairwise by paired t and Wilcoxon tests with Holm correction.",
    m_factors = "Factor levels were compared on the means of contiguous segments at one level (for clock time, within one day) with the Welch t test or ANOVA and the Mann-Whitney or Kruskal-Wallis test.",
    m_stages = "The daily rhythm was compared between factor levels with cosinor comparisons (coefficient-level comparison of days when every level had at least three days covering the cycle, otherwise a pooled model with Newey-West standard errors).",
    m_events = "The response after events was estimated by superposed epoch analysis with controls at the same clock times on randomly chosen days without events; for each event and control window the rise to peak, time to peak, incremental area above baseline and mean change were computed, the event mean was compared with the control mean by the Welch t test with Holm correction across event types, and covariates entered a linear regression with Holm correction.",
    m_model = "Accepted cycles and the trend were combined in a trigonometric model with jointly estimated coefficients; model quality was assessed by the multiple correlation R and the prognostic index PrIdx.",
    m_model_2stage = "Accepted cycles were fitted to the series after removing the separately fitted trend and the trend was added back; model quality was assessed by the multiple correlation R and the prognostic index PrIdx.",
    m_forecast = "Forecast accuracy was checked with a rolling origin against repeating the last known value.",
    m_soft = "The analysis was performed with the kkstatfun R package following the algorithm of CycleLab (Stefanov 2026).",
    r_desc = "The analysis included %d measurements of %s over %s %s; %s.",
    r_meansd = "mean %s (SD %s)%s",
    r_mediqr = "median %s (IQR %s-%s)%s",
    r_not = "%s",
    r_model = "The trigonometric model describes the series with R = %s (PrIdx = %s).",
    r_val_good = "The forecast is more accurate than repeating the last value (mean absolute error %s%% lower).",
    r_val_bad = "The forecast is not more accurate than repeating the last value and should not be used.",
    limitations = "The periods that can be detected are limited by the sampling step and the record length; an oscillation longer than half the record may reflect a trend or a single event. 'Not established' does not mean absent, and an association with a cycle, trend or event does not prove causation."
  )
}
