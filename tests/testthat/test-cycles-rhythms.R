# Cycles and rhythms tier (v1.4.0). Every test checks a simulated series with
# known truth (see ?kk_cycle_example), not just that the call runs.

test_that("cosinor recovers MESOR, amplitude and the 17:00 peak", {
  tp <- kk_cycle_example("temperature")
  r <- kk_cosinor(tp, temp, time)
  expect_equal(r$mesor, 36.6, tolerance = 0.01)
  expect_equal(r$amplitude, 0.30, tolerance = 0.15)
  expect_lt(abs(r$acrophase - 17), 0.5)
  expect_lt(r$p_value, 1e-6)
  expect_true(r$acrophase_low < 17 && r$acrophase_high > 17)
  pd <- attr(kk_cosinor(tp, temp, time, by_day = TRUE), "per_day")
  expect_equal(nrow(pd), 28)
})

test_that("cosinor on pure noise rarely finds a rhythm", {
  set.seed(11)
  p <- replicate(200, {
    d <- data.frame(h = seq(0, 72, by = 2), y = rnorm(37))
    kk_cosinor(d, y, h, robust = FALSE)$p_value
  })
  expect_lt(mean(p < 0.05), 0.10)
})

test_that("population cosinor and group comparison find the night-work shift", {
  su <- kk_cycle_example("subjects")
  pc <- kk_cosinor_population(su[su$group == "day work", ], hr, time, subject = id)
  expect_equal(pc$k, 6)
  expect_lt(abs(pc$acrophase - 15), 1.5)
  cc <- kk_cosinor_compare(su, hr, time, group, subject = id)
  expect_lt(cc$p_value[cc$test == "acrophase"], 0.001)
  sh <- attr(cc, "differences")$acrophase_shift
  expect_lt(abs(sh - 6), 1.5)
  expect_gt(cc$p_value[cc$test == "mesor"], 0.01)
})

test_that("stage comparison wraps the acrophase across midnight", {
  set.seed(3)
  h <- seq(0, 24 * 20, by = 1)
  stage <- ifelse(h < 240, "a", "b")
  pk <- ifelse(stage == "a", 23, 1)
  y <- 10 + 2 * cos(2 * pi * (h - pk) / 24) + rnorm(length(h), 0, 0.5)
  cc <- kk_cosinor_compare(data.frame(h, y, stage), y, h, stage)
  expect_lt(abs(attr(cc, "differences")$acrophase_shift - 2), 0.5)
})

test_that("T-R periodogram finds 12 and 6 months and snaps to calendar values", {
  pn <- kk_cycle_example("pneumonia")
  p <- kk_periodogram(pn$series, rate, month, calendar = c(12, 6), n_sim = 200)
  acc <- p[p$in_model, ]
  expect_equal(sort(acc$model_period), c(6, 12))
  expect_true(any(grepl("calendar period 12", attr(p, "decisions"))))
  expect_true(all(c("R", "R_SR", "threshold") %in% names(attr(p, "spectrum"))))
  expect_s3_class(attr(p, "fourier"), "tbl_df")
  expect_true(all(attr(p, "phase")$phase >= 0 & attr(p, "phase")$phase < 1))
})

test_that("the red-noise test keeps false cycles near alpha", {
  set.seed(5)
  hits <- replicate(40, {
    d <- data.frame(t = 1:120, y = as.numeric(arima.sim(list(ar = 0.6), 120)))
    p <- kk_periodogram(d, y, t, detrend = "none", n_sim = 99, seed = NULL,
                        max_cycles = 1)
    isTRUE(p$in_model[1])
  })
  expect_lt(mean(hits), 0.15)
})

test_that("a changing rhythm does not loop on the same calendar period", {
  nu <- kk_cycle_example("nurse")
  p <- kk_periodogram(nu, temp, time, n_sim = 100)
  expect_lte(sum(p$in_model & abs(p$model_period - 24) < 1e-9), 1)
})

test_that("trend forms: effective N and the nonlinear shape", {
  er <- kk_cycle_example("emergency")
  expect_message(tr <- kk_ts_trend(er, visits, month), "explains")
  expect_lt(tr$n_eff, tr$n)
  f <- attr(tr, "forms")
  expect_equal(nrow(f), 15)
  expect_true(f$form[1] %in% c("michaelis_menten", "hyperbolic_saturation",
                               "log", "power"))
  set.seed(8)
  d <- data.frame(t = 1:100, y = as.numeric(arima.sim(list(ar = 0.8), 100)))
  expect_s3_class(kk_ts_trend(d, y, t), "tbl_df")
})

test_that("harmonic model forecasts the held-out year better than naive", {
  pn <- kk_cycle_example("pneumonia")
  h <- kk_harmonic_model(pn$series, rate, month, periods = c(12, 6), horizon = 12)
  cy <- attr(h, "cycles")
  expect_equal(cy$amplitude[cy$period == 12], 9, tolerance = 0.15)
  v <- attr(h, "validation")
  expect_true(v$better_than_naive)
  fc <- attr(h, "forecast")
  cover <- mean(pn$holdout$rate >= fc$pred.low & pn$holdout$rate <= fc$pred.high)
  expect_gte(cover, 0.75)
  expect_gt(attr(h, "fit")$PrIdx, 0)
  # nonlinear trend: two-stage fit
  er <- kk_cycle_example("emergency")
  expect_message(h2 <- kk_harmonic_model(er, visits, month, periods = 12,
                                         trend = "michaelis_menten"),
                 "not linear")
  expect_false(attr(h2, "fit")$joint)
})

test_that("describe flags outliers and chooses the summary measure", {
  bp <- kk_cycle_example("hypertension")
  bp$sbp[50] <- 250
  ds <- kk_ts_describe(bp, sbp, date)
  expect_gte(ds$n_outliers, 1)
  expect_equal(attr(ds, "moving_average")$window[1], 7)
})

test_that("parts of the day use complete days and Holm-adjusted pairs", {
  pd <- kk_cycle_example("prediabetes")$series
  dp <- kk_day_parts(pd, glucose, time)
  expect_equal(nrow(dp), 2)
  expect_equal(nrow(attr(dp, "pairwise")), 6)
  expect_true(all(attr(dp, "pairwise")$p_ttest_holm >= attr(dp, "pairwise")$p_ttest))
})

test_that("epoch response recovers the meal rise and the carbohydrate slope", {
  pd <- kk_cycle_example("prediabetes")
  er <- suppressWarnings(kk_epoch_response(
    pd$series, glucose, time, pd$events, time, event_type = type,
    covariates = carbs, window = c(-0.5, 3), n_control = 50))
  sm <- attr(er, "summary")
  expect_equal(sm$main_measure[sm$event_type == "meal"], "rise")
  expect_gt(sm$mean[sm$event_type == "meal"], 1)
  expect_lt(sm$mean[sm$event_type == "walk"], 0)
  ce <- attr(er, "covariate_effect")
  expect_equal(ce$slope, 0.035, tolerance = 0.2)
  expect_true(all(c("rise", "time_to_peak", "iauc", "mean_change",
                    "end_change") %in% names(attr(er, "per_event"))))
})

test_that("distributed lag finds the 2-hour drug peak", {
  n1 <- kk_cycle_example("nof1")
  s <- n1$series
  s$dose <- as.integer(s$time %in% n1$events$time)
  dl <- kk_distributed_lag(s, glucose, dose, time, max_lag = 32, period = 24)
  lagrows <- dl[dl$lag != "cumulative", ]
  expect_equal(lagrows$lag_time[which.min(lagrows$estimate)], 2, tolerance = 0.5)
  expect_lt(dl$estimate[dl$lag == "cumulative"], 0)
})

test_that("N-of-1 schedule is balanced and the analysis detects the drug", {
  s <- kk_nof1_schedule(8, c("A", "B"), seed = 1)
  expect_equal(as.vector(table(s$treatment)), c(8, 8))
  expect_true(all(tapply(s$treatment, s$block, function(x) length(unique(x))) == 2))
  n1 <- kk_cycle_example("nof1")
  dm <- aggregate(glucose ~ day, n1$series, mean)
  dm <- merge(dm, data.frame(day = n1$schedule$period, block = n1$schedule$block,
                             treatment = n1$schedule$treatment))
  r <- kk_nof1(dm, glucose, treatment, block, active = "drug")
  expect_lt(r$effect, 0)
  expect_lt(r$p_randomisation, 0.05)
})

test_that("series comparison recovers the cortisol-pulse phase shift", {
  ch <- kk_cycle_example("chronobiology")
  tc <- kk_ts_compare(ch, cortisol, pulse, time = time, period = 24, n_sim = 50)
  expect_lt(abs(tc$phase_shift - 7), 1)
  expect_s3_class(attr(tc, "coherence"), "tbl_df")
  expect_s3_class(attr(tc, "lag_regression"), "tbl_df")
  expect_true(24 %in% attr(tc, "common_cycles")$period)
  expect_error(kk_ts_compare(ch[-5, ], cortisol, pulse, time = time),
               "equally spaced")
})

test_that("wavelet spectrum peaks near 24 h", {
  ch <- kk_cycle_example("chronobiology")
  g <- attr(kk_wavelet(ch, pulse, time), "global")
  expect_lt(abs(g$period[which.max(g$power)] - 24), 4)
  wc <- kk_wavelet_coherence(ch, temp, pulse, time, n_sim = 5)
  expect_true(all(wc$coherence >= 0 & wc$coherence <= 1 + 1e-8, na.rm = TRUE))
})

test_that("cycle power: exact cosinor power rises with duration; naive rule fails", {
  pc <- kk_cycle_power(24, 0.3, 0.3, 2, duration = 24 * c(1, 2, 4),
                       method = "cosinor")
  expect_true(all(diff(pc$power) > 0))
  pp <- kk_cycle_power(24, 0.3, 0.3, 2, duration = 24 * c(2, 5), ar1 = 0.5,
                       n_sim = 80)
  expect_true(all(pp$false_rsr196 > 0.5))
  expect_true(all(pp$false_noise_test < 0.2))
})

test_that("full analysis writes the conclusion and report text", {
  pn <- kk_cycle_example("pneumonia")
  fa <- kk_cycle_analysis(pn$series, rate, month, calendar = c(12, 6),
                          n_sim = 100)
  expect_s3_class(fa, "kk_cycle_analysis")
  expect_true(any(grepl("period 12", fa$established)))
  expect_match(fa$methods_text, "Komitov")
  nu <- kk_cycle_example("nurse")
  fb <- kk_cycle_analysis(nu, temp, time, n_sim = 100)
  expect_true(any(grepl("daily rhythm", fb$established)))
  expect_true(any(grepl("shifts", fb$established)))
  expect_output(print(fb), "Established")
})

test_that("plots build", {
  pn <- kk_cycle_example("pneumonia")
  expect_s3_class(kk_plot_periodogram(kk_periodogram(pn$series, rate, month,
                                                     n_sim = 30)), "ggplot")
  tp <- kk_cosinor(kk_cycle_example("temperature"), temp, time, by_day = TRUE)
  expect_s3_class(kk_plot_cosinor(tp), "ggplot")
  expect_s3_class(kk_plot_cosinor(tp, type = "ellipse"), "ggplot")
  expect_s3_class(kk_plot_model(kk_harmonic_model(pn$series, rate, month,
                                                  periods = 12, horizon = 6)),
                  "ggplot")
})

test_that("group_by dispatch works", {
  su <- kk_cycle_example("subjects")
  r <- dplyr::group_by(su, group) |> kk_cosinor(hr, time)
  expect_equal(nrow(r), 2)
  expect_true("group" %in% names(r))
})

# ---- regression tests for the statistical review (v1.4.0) ------------------

test_that("iAUC integrates from the event and interpolates crossings", {
  expect_equal(.kk_pos_area(c(0, 0.5, 1), c(0, 2, 0)), 1)
  expect_equal(.kk_pos_area(c(0, 1), c(1, -1)), 0.25)
  expect_equal(.kk_pos_area(c(0, 1), c(-1, -2)), 0)
})

test_that("population cosinor uses one phase origin for all subjects", {
  d <- do.call(rbind, lapply(0:2, function(i) {
    dd <- as.Date("2026-01-05") + i + 0:41
    data.frame(id = i, date = dd,
               y = cos(2 * pi * (as.numeric(dd - as.Date("2026-01-05")) - 2) / 7))
  }))
  pc <- kk_cosinor_population(d, y, date, subject = id, period = 7)
  expect_equal(pc$amplitude, 1, tolerance = 1e-6)
  expect_equal(pc$acrophase, 2, tolerance = 1e-6)
})

test_that("clock time keeps one offset across a DST change", {
  tm <- seq(as.POSIXct("2026-10-23 00:00", tz = "Europe/Sofia"), by = "hour",
            length.out = 24 * 6)
  set.seed(1)
  y <- cos(2 * pi * (seq_along(tm) - 1 - 15) / 24) + rnorm(length(tm), 0, 0.1)
  cz <- kk_cosinor(data.frame(tm, y), y, tm)
  expect_false(anyDuplicated(attr(cz, "fitted")$t) > 0)
  expect_lt(abs(cz$acrophase - 15), 0.5)
})

test_that("unidentified phase gets no interval; traditional acrophase given", {
  set.seed(2)
  d <- data.frame(h = seq(0, 96, by = 2))
  d$y <- rnorm(nrow(d))
  cz <- kk_cosinor(d, y, h)
  if (!cz$acrophase_reliable) expect_true(is.na(cz$acrophase_low))
  expect_equal(cz$acrophase_traditional_deg, -cz$acrophase_deg)
  expect_true(nzchar(cz$se_method))
})

test_that("cosinor comparison: coefficient-level method on days", {
  nu <- kk_cycle_example("nurse")
  cc <- kk_cosinor_compare(nu, temp, time, stage)
  expect_equal(unique(cc$method), "units")
  expect_lt(cc$p_value[cc$test == "acrophase"], 0.01)
  cp <- kk_cosinor_compare(nu, temp, time, stage, method = "pooled")
  expect_equal(unique(cp$method), "pooled")
})

test_that("epoch rise is tested against controls, not against zero", {
  set.seed(11)
  tm <- as.POSIXct("2026-03-01", tz = "UTC") + seq(0, 14 * 86400 - 900, by = 900)
  h <- as.numeric(difftime(tm, tm[1], units = "hours"))
  y <- 6 + 0.8 * cos(2 * pi * (h - 8) / 24) + rnorm(length(h), 0, 0.3)
  ev <- data.frame(time = tm[1] + sort(sample(seq(6, 14 * 24 - 6, by = 0.25), 20)) * 3600,
                   type = "meal")
  out <- suppressWarnings(kk_epoch_response(data.frame(tm, y), y, tm, ev, time,
                                            event_type = type, window = c(-0.5, 3),
                                            n_control = 100))
  sm <- attr(out, "summary")
  # a naive t test of the rise against 0 would be overwhelmingly significant
  expect_lt(t.test(attr(out, "per_event")$rise)$p.value, 0.01)
  expect_true(grepl("controls", sm$test))
  expect_gt(sm$p.value, 0.001)
  expect_true("p.adjusted" %in% names(sm))
  expect_gt(nrow(attr(out, "controls")), 20)
})

test_that("simultaneous events count as overlapping", {
  pd <- kk_cycle_example("prediabetes")
  ev <- pd$events[1:6, ]
  ev2 <- rbind(ev, transform(ev[1, ], type = "walk"))
  out <- suppressWarnings(kk_epoch_response(pd$series, glucose, time, ev2, time,
                                            event_type = type, window = c(-0.5, 3),
                                            n_control = 0))
  ex <- attr(out, "excluded")
  expect_true(any(ex$reason == "another event inside the window"))
})

test_that("row-offset methods refuse gaps", {
  s <- kk_cycle_example("nof1")$series
  s$dose <- 0
  s <- s[-c(10, 11), ]
  expect_error(kk_distributed_lag(s, glucose, dose, time, max_lag = 4), "equally spaced")
  expect_error(kk_wavelet(s, glucose, time), "equally spaced")
})

test_that("every trend form is tested; constancy null depends on the form", {
  set.seed(3)
  de <- data.frame(t = 1:60, y = 10 * exp(0.02 * (1:60)) * exp(rnorm(60, 0, 0.05)))
  tr <- kk_ts_trend(de, y, t, form = "exponential_base")
  expect_lt(tr$p.value, 1e-6)
  expect_match(tr$test, "A = 1")
  expect_false(is.na(kk_ts_trend(de, y, t, form = "michaelis_menten")$p.value))
})

test_that("periodogram: test on the grid, refine after; p_fixed reported", {
  pn <- kk_cycle_example("pneumonia")
  pg <- kk_periodogram(pn$series, rate, month, calendar = c(12, 6), n_sim = 200)
  expect_true(all(c("p_fixed", "p_value") %in% names(pg)))
  expect_true("p_fixed" %in% names(attr(pg, "spectrum")))
  set.seed(5)
  dn <- data.frame(t = 1:120, y = cos(2 * pi * (1:120 - 3) / 12) + rnorm(120, 0, 0.2))
  pg2 <- kk_periodogram(dn, y, t, n_sim = 200, calendar = FALSE, max_cycles = 1)
  expect_lt(abs(pg2$peak[1] - 2), 0.5)   # peak measured from the first time
})

test_that("nonlinear-trend forecast interval is bootstrapped", {
  pn <- kk_cycle_example("pneumonia")
  hm <- suppressMessages(kk_harmonic_model(pn$series, rate, month,
                                           periods = c(12, 6), trend = "power",
                                           horizon = 6, n_boot = 99))
  fc <- attr(hm, "forecast")
  expect_match(fc$interval[1], "bootstrap")
  expect_true(all(fc$pred.low < fc$forecast & fc$forecast < fc$pred.high))
})

test_that("grouped full analysis returns one result per group", {
  pn <- kk_cycle_example("pneumonia")$series
  d <- rbind(transform(pn, g = "a"), transform(pn, g = "b"))
  fa <- kk_cycle_analysis(dplyr::group_by(d, g), rate, month, calendar = c(12, 6),
                          n_sim = 50)
  expect_named(fa, c("a", "b"))
  expect_s3_class(fa$a, "kk_cycle_analysis")
})
