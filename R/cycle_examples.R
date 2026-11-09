# ============================================================
# SIMULATED EXAMPLE SERIES FOR THE CYCLES AND RHYTHMS TIER
# ============================================================

#' Simulated Biomedical Time Series with Known Properties (KK)
#'
#' @description Example records for the cycles-and-rhythms functions. Each
#'   one is simulated with known trend, cycles, rhythm or event effects, so
#'   the analysis can be checked against the truth. The scenarios follow the
#'   worked examples of CycleLab (Stefanov 2026), where they are explained
#'   step by step; the data here are generated independently.
#'
#'   * `"hypertension"`: morning systolic pressure of a 42-year-old woman
#'     with grade 1 hypertension for 182 days after a lifestyle change.
#'     True properties: decline from 148 to about 133 mmHg (exponential
#'     approach), +7 mmHg on workdays (7-day cycle), AR(1) noise 0.3, SD 4.
#'   * `"pneumonia"`: monthly hospitalisations for pneumonia per 100 000 for
#'     8 years, plus 12 held-out months. True: annual cycle (amplitude 9,
#'     peak in January) and a semi-annual cycle (amplitude 3, peaks in April
#'     and October), trend +0.04 per month. Returns `series` and `holdout`.
#'   * `"emergency"`: mean daily visits to a new emergency room by month for
#'     8 years after its opening in January. True: saturating growth
#'     (Michaelis-Menten, 70 m/(12 + m): from 5 to about 62 visits) and a summer peak (July,
#'     amplitude 6).
#'   * `"temperature"`: axillary temperature of a healthy volunteer 7 times
#'     a day (07-23 h) for 28 days. True: MESOR 36.6 C, amplitude 0.30 C,
#'     peak 17:00.
#'   * `"nurse"`: the same design for a nurse who moves from day to evening
#'     shifts (ending at 23 h) on day 15. True: peak 17:00 before and 20:00
#'     after, amplitude 0.30 then 0.22 C. Has a `stage` column.
#'   * `"prediabetes"`: continuous glucose every 15 min for 8 days and a log
#'     of meals (with carbohydrates) and walks. True: rise of about
#'     0.035 mmol/l per gram of carbohydrate peaking 45 min after a meal, a
#'     fall of 0.6 mmol/l after walks, small circadian rhythm. Returns
#'     `series` and `events`.
#'   * `"nof1"`: a patient with type 2 diabetes wears a glucose sensor for 20
#'     days; on one day of each pair, chosen at random, a short-acting drug is
#'     taken at 08:00. True: drug lowers glucose by up to 1.8 mmol/l, peak
#'     effect 2 h after the dose, lasting about 6 h. Returns `series`,
#'     `schedule` and `events`.
#'   * `"chronobiology"`: every 20 min for 3 days, temperature, pulse and
#'     free tissue cortisol of a healthy volunteer. True peaks: cortisol
#'     08:00, pulse 15:00, temperature 17:00.
#'   * `"subjects"`: heart rate every 2 h for 3 days in 12 people, 6 day
#'     workers and 6 night workers. True: peak 15:00 vs 21:00, amplitude
#'     8 vs 5 bpm. Long format with `id` and `group`.
#'
#' @param name Name of the example (see Description).
#' @param seed Random seed (default 2026); change it to see sampling
#'   variation.
#'
#' @return A tibble, or a named list of tibbles for the examples with events
#'   or held-out data.
#'
#' @references Stefanov R (2026) *CycleLab: analysis of biomedical time
#'   series*, examples. Plovdiv: Institute for Rare Diseases.
#'   <https://cyclelab.raredis.org>
#'
#' @seealso [kk_periodogram()], [kk_cosinor()], [kk_epoch_response()]
#'
#' @examples
#' str(kk_cycle_example("temperature"))
#' names(kk_cycle_example("prediabetes"))
#'
#' @export
kk_cycle_example <- function(name = c("hypertension", "pneumonia",
                                      "emergency", "temperature", "nurse",
                                      "prediabetes", "nof1",
                                      "chronobiology", "subjects"),
                             seed = 2026) {
  name <- match.arg(name)
  set.seed(seed)
  ar1 <- function(n, phi, s) {
    e <- stats::rnorm(n, 0, s * sqrt(1 - phi^2))
    x <- numeric(n)
    x[1] <- stats::rnorm(1, 0, s)
    for (i in seq_len(n)[-1]) x[i] <- phi * x[i - 1] + e[i]
    x
  }
  start <- as.POSIXct("2026-01-05 00:00", tz = "UTC")
  clock <- function(tm) .kk_clock_hours(tm)
  switch(name,
    hypertension = {
      date <- as.Date("2026-01-05") + 0:181
      wd <- as.integer(format(date, "%u")) <= 5
      sbp <- 133 + 15 * exp(-(0:181) / 45) + 7 * wd - 5 + ar1(182, 0.3, 4)
      tibble::tibble(date = date, sbp = round(sbp),
                     workday = ifelse(wd, "workday", "weekend"))
    },
    pneumonia = {
      m <- 1:108
      rate <- 30 + 0.04 * m + 9 * cos(2 * pi * (m - 1) / 12) +
        3 * cos(2 * pi * (m - 4) / 6) + ar1(108, 0.3, 2.5)
      d <- tibble::tibble(month = m,
                          date = seq(as.Date("2018-01-01"), by = "month",
                                     length.out = 108),
                          rate = round(rate, 1))
      list(series = d[1:96, ], holdout = d[97:108, ])
    },
    emergency = {
      m <- 1:96
      visits <- 70 * m / (12 + m) + 6 * cos(2 * pi * (m - 7) / 12) +
        ar1(96, 0.3, 2)
      tibble::tibble(month = m, visits = round(pmax(visits, 1), 1))
    },
    temperature = {
      tm <- start + 3600 * c(outer(c(7, 9.5, 12, 14.5, 17, 19.5, 23),
                                   24 * 0:27, "+"))
      tm <- tm + stats::runif(length(tm), -0.25, 0.25) * 3600
      temp <- 36.6 + 0.30 * cos(2 * pi * (clock(tm) - 17) / 24) +
        stats::rnorm(length(tm), 0, 0.12)
      tibble::tibble(time = tm, temp = round(temp, 2))
    },
    nurse = {
      tm <- start + 3600 * c(outer(c(7, 9.5, 12, 14.5, 17, 19.5, 23),
                                   24 * 0:27, "+"))
      day <- as.numeric(as.Date(tm) - as.Date(start)) + 1
      stage <- ifelse(day < 15, "day shifts", "evening shifts")
      pk <- ifelse(day < 15, 17, 20)
      amp <- ifelse(day < 15, 0.30, 0.22)
      temp <- 36.6 + amp * cos(2 * pi * (clock(tm) - pk) / 24) +
        stats::rnorm(length(tm), 0, 0.12)
      tibble::tibble(time = tm, temp = round(temp, 2), stage = stage)
    },
    prediabetes = {
      tm <- start + seq(0, 8 * 24 * 3600 - 1, by = 900)
      h <- as.numeric(difftime(tm, start, units = "hours"))
      meal_h <- c(outer(c(8, 13.5, 19.75), 24 * 0:7, "+")) +
        stats::runif(24, -1.5, 1.5)
      carbs <- round(stats::runif(24, 30, 90))
      walk_h <- c(outer(c(16.5, 22.5), 24 * c(0, 1, 3, 4, 6, 7), "+"))
      g <- 5.6 + 0.2 * cos(2 * pi * (clock(tm) - 8) / 24) + ar1(length(h), 0.9, 0.15)
      for (i in seq_along(meal_h)) {
        d <- h - meal_h[i]
        g <- g + ifelse(d > 0, 0.035 * carbs[i] * (d / 0.75) * exp(1 - d / 0.75), 0)
      }
      for (i in seq_along(walk_h)) {
        d <- h - walk_h[i]
        g <- g - ifelse(d > 0, 0.6 * (d / 0.5) * exp(1 - d / 0.5), 0)
      }
      ev <- rbind(
        data.frame(time = start + meal_h * 3600, type = "meal", carbs = carbs),
        data.frame(time = start + walk_h * 3600, type = "walk", carbs = NA))
      list(series = tibble::tibble(time = tm, glucose = round(g, 2)),
           events = tibble::as_tibble(ev[order(ev$time), ]))
    },
    nof1 = {
      sched <- kk_nof1_schedule(10, c("no drug", "drug"),
                                start = as.Date("2026-01-05"), seed = seed)
      tm <- start + seq(0, 20 * 24 * 3600 - 1, by = 900)
      h <- as.numeric(difftime(tm, start, units = "hours"))
      day <- floor(h / 24) + 1
      drug_day <- sched$period[sched$treatment == "drug"]
      g <- 8.2 + 0.4 * cos(2 * pi * (clock(tm) - 9) / 24) + ar1(length(h), 0.9, 0.3)
      dose_h <- (drug_day - 1) * 24 + 8
      for (dh in dose_h) {
        d <- h - dh
        g <- g - ifelse(d > 0, 1.8 * (d / 2) * exp(1 - d / 2), 0)
      }
      list(series = tibble::tibble(time = tm, glucose = round(g, 2), day = day),
           schedule = sched,
           events = tibble::tibble(time = start + dose_h * 3600, type = "drug"))
    },
    chronobiology = {
      tm <- start + seq(0, 3 * 24 * 3600 - 1, by = 1200)
      ch <- clock(tm)
      n <- length(tm)
      tibble::tibble(
        time = tm,
        temp = round(36.7 + 0.3 * cos(2 * pi * (ch - 17) / 24) + ar1(n, 0.6, 0.08), 2),
        pulse = round(70 + 7 * cos(2 * pi * (ch - 15) / 24) + ar1(n, 0.5, 3)),
        cortisol = round(pmax(0.5, 9 + 6 * cos(2 * pi * (ch - 8) / 24) +
                                ar1(n, 0.6, 1.5)), 1))
    },
    subjects = {
      do.call(rbind, lapply(1:12, function(i) {
        grp <- if (i <= 6) "day work" else "night work"
        pk <- if (i <= 6) 15 else 21
        a <- if (i <= 6) 8 else 5
        tm <- start + 3600 * seq(0, 70, by = 2)
        tibble::tibble(id = i, group = grp, time = tm,
                       hr = round(68 + stats::rnorm(1, 0, 3) +
                                    (a + stats::rnorm(1, 0, 1)) *
                                    cos(2 * pi * (clock(tm) - pk -
                                                    stats::rnorm(1, 0, 1)) / 24) +
                                    stats::rnorm(length(tm), 0, 3)))
      }))
    }
  )
}
