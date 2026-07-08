# ============================================================
# INCIDENCE (FORCE OF INFECTION) FROM SEROPREVALENCE SURVEYS
# ============================================================

#' Incidence / Force of Infection from Seroprevalence Surveys (KK)
#'
#' @description Estimates the incidence rate (force of infection, FOI) of an
#'   infection from repeated cross-sectional seroprevalence surveys, using the
#'   catalytic (irreversible-seroconversion) model in which the seronegative
#'   ("susceptible") fraction is `S = 1 - seroprevalence` and
#'   `S(a) = exp(-\int_0^a lambda)`.
#'
#'   Two methods are provided:
#'   \describe{
#'     \item{`"cohort"` (default)}{Follows each **birth cohort** (`cohort = year -
#'       age`) across two or more surveys and estimates the FOI it experienced
#'       between consecutive surveys as `lambda = -[log S2 - log S1] / (t2 - t1)`.
#'       This is a genuine longitudinal incidence for that cohort over the ages
#'       and calendar years between surveys, so it captures age and cohort effects
#'       directly. Requires >= 2 surveys, spaced so that cohorts realign across
#'       age groups.}
#'     \item{`"catalytic"`}{Within each survey, estimates the age-specific FOI from
#'       the rise in seroprevalence between consecutive age groups,
#'       `lambda(a) = -[log S(a2) - log S(a1)] / (a2 - a1)` (a cross-sectional
#'       estimate that assumes stationarity).}
#'   }
#'
#' @param data Data frame. Either aggregated (one row per age x survey with
#'   `positive` and `total` counts) or individual-level (one row per person with a
#'   0/1 `positive`, aggregated internally when `total` is omitted).
#' @param age Age or age-group midpoint column (bare name or string); numeric.
#' @param positive Seropositive count (aggregated) or 0/1 indicator (individual).
#' @param year Calendar year of the survey (bare name or string); numeric.
#'   Optional: with a single cross-sectional survey it may be omitted, in which
#'   case only the `"catalytic"` method is available.
#' @param total Sample size per age x survey (aggregated input). If omitted,
#'   `positive` is treated as an individual 0/1 indicator and counts are formed by
#'   aggregation.
#' @param method `"cohort"` (default, between-survey) or `"catalytic"`
#'   (within-survey age gradient). A single survey requires `"catalytic"`.
#' @param age_breaks Optional numeric vector of cut points used to group ages into
#'   bands before estimation (e.g. `seq(0, 70, 10)`), so patient-level data with
#'   exact ages can be binned in one call. Each band is represented by its
#'   sample-size-weighted mean age.
#' @param conf.level Confidence level (default 0.95).
#'
#' @return Tibble of FOI (annual incidence rate among susceptibles) estimates with
#'   confidence intervals. For `"cohort"`: one row per cohort per between-survey
#'   interval (with ages, years and seroprevalences at each end). For
#'   `"catalytic"`: one row per survey per age interval.
#'
#' @examples
#' # Two surveys 10 years apart; true constant FOI = 0.05 / year
#' set.seed(1)
#' make_survey <- function(year, foi = 0.05, ages = seq(5, 65, 10), n = 500) {
#'   p <- 1 - exp(-foi * ages)
#'   data.frame(age = ages, year = year,
#'              positive = rbinom(length(ages), n, p), total = n)
#' }
#' sero <- rbind(make_survey(2000), make_survey(2010))
#' kk_sero_incidence(sero, age, positive, year, total = total)
#'
#' # Single survey, patient-level data: bin ages and use the catalytic method
#' set.seed(1)
#' n <- 2000
#' ages <- runif(n, 0, 70)
#' pat <- data.frame(age = ages, seropos = rbinom(n, 1, 1 - exp(-0.05 * ages)))
#' kk_sero_incidence(pat, age, seropos, method = "catalytic",
#'                   age_breaks = seq(0, 70, 10))
#'
#' @export
kk_sero_incidence <- function(data, age, positive, year = NULL, total = NULL,
                              method = c("cohort", "catalytic"),
                              age_breaks = NULL,
                              conf.level = 0.95) {
  validate_data_frame(data)
  method <- match.arg(method)

  age_name <- .kk_colname(rlang::enquo(age))
  pos_name <- .kk_colname(rlang::enquo(positive))
  year_quo <- rlang::enquo(year)
  has_year <- !rlang::quo_is_null(year_quo)
  year_name <- if (has_year) .kk_colname(year_quo) else NULL
  total_quo <- rlang::enquo(total)
  has_total <- !rlang::quo_is_null(total_quo)
  total_name <- if (has_total) .kk_colname(total_quo) else NULL

  missing_cols <- setdiff(c(age_name, pos_name, year_name, total_name), names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }
  if (!is.numeric(data[[age_name]])) stop("`age` must be numeric.")
  if (has_year && !is.numeric(data[[year_name]])) stop("`year` must be numeric.")

  # A single cross-sectional survey (no `year`) only supports the catalytic method
  year_vec <- if (has_year) data[[year_name]] else rep(0, nrow(data))
  if (!has_year && method == "cohort") {
    stop("The cohort method needs a `year` (>= 2 surveys). ",
         "For a single survey use method = 'catalytic'.")
  }

  # Build the per-record working frame (pos / n), then optionally bin ages so
  # patient-level data can be grouped in one call.
  if (has_total) {
    work <- data.frame(age = data[[age_name]], year = year_vec,
      pos = data[[total_name]] * 0 + data[[pos_name]], n = data[[total_name]])
  } else {
    yv <- data[[pos_name]]
    if (!all(stats::na.omit(yv) %in% c(0, 1))) {
      stop("Without `total`, `positive` must be an individual 0/1 indicator.")
    }
    work <- data.frame(age = data[[age_name]], year = year_vec,
      pos = as.numeric(yv), n = 1)
  }

  if (!is.null(age_breaks)) {
    bin <- cut(work$age, breaks = age_breaks, include.lowest = TRUE, right = FALSE)
    if (anyNA(bin)) {
      warning(sum(is.na(bin)), " record(s) fell outside `age_breaks` and were dropped.")
    }
    work <- work[!is.na(bin), , drop = FALSE]
    bin <- bin[!is.na(bin)]
    grp <- interaction(bin, work$year, drop = TRUE)
    agg <- data.frame(
      age = as.numeric(tapply(work$age * work$n, grp, sum) / tapply(work$n, grp, sum)),
      year = as.numeric(tapply(work$year, grp, function(x) x[1])),
      pos = as.numeric(tapply(work$pos, grp, sum)),
      n = as.numeric(tapply(work$n, grp, sum))
    )
  } else {
    agg <- stats::aggregate(cbind(pos = pos, n = n) ~ age + year, data = work, FUN = sum)
  }
  agg$p <- agg$pos / agg$n
  z <- stats::qnorm(1 - (1 - conf.level) / 2)

  # Delta-method SE of log(S) = log(1 - p): Var ~ p / ((1 - p) n)
  se_log_surv <- function(p, n) {
    ifelse(p >= 1 | p <= 0 | n <= 0, NA_real_, sqrt(p / ((1 - p) * n)))
  }
  log_surv <- function(p) ifelse(p >= 1, NA_real_, log(1 - p))

  foi_between <- function(p1, n1, p2, n2, dt) {
    ls1 <- log_surv(p1); ls2 <- log_surv(p2)
    lambda <- -(ls2 - ls1) / dt
    se <- sqrt(se_log_surv(p1, n1)^2 + se_log_surv(p2, n2)^2) / dt
    list(lambda = lambda, low = lambda - z * se, high = lambda + z * se, se = se)
  }

  if (method == "cohort") {
    agg$cohort <- agg$year - agg$age
    rows <- list()
    for (co in sort(unique(agg$cohort))) {
      sub <- agg[agg$cohort == co, ]
      sub <- sub[order(sub$year), ]
      if (nrow(sub) < 2) next
      for (i in seq_len(nrow(sub) - 1)) {
        a <- sub[i, ]; b <- sub[i + 1, ]
        dt <- b$year - a$year
        if (dt <= 0) next
        f <- foi_between(a$p, a$n, b$p, b$n, dt)
        rows[[length(rows) + 1]] <- tibble::tibble(
          cohort = co,
          age_start = a$age, age_end = b$age,
          year_start = a$year, year_end = b$year,
          seroprev_start = a$p, seroprev_end = b$p,
          n_start = a$n, n_end = b$n,
          foi = f$lambda, foi_low = f$low, foi_high = f$high,
          conf.level = conf.level
        )
      }
    }
    if (length(rows) == 0) {
      stop("No birth cohort was observed in >= 2 surveys; ",
           "check that surveys are spaced so cohorts realign across age groups.")
    }
    return(dplyr::bind_rows(rows))
  }

  # method == "catalytic": within-survey age gradient
  rows <- list()
  for (yr in sort(unique(agg$year))) {
    sub <- agg[agg$year == yr, ]
    sub <- sub[order(sub$age), ]
    if (nrow(sub) < 2) next
    for (i in seq_len(nrow(sub) - 1)) {
      a <- sub[i, ]; b <- sub[i + 1, ]
      da <- b$age - a$age
      if (da <= 0) next
      f <- foi_between(a$p, a$n, b$p, b$n, da)
      rows[[length(rows) + 1]] <- tibble::tibble(
        year = yr,
        age_start = a$age, age_end = b$age,
        seroprev_start = a$p, seroprev_end = b$p,
        n_start = a$n, n_end = b$n,
        foi = f$lambda, foi_low = f$low, foi_high = f$high,
        conf.level = conf.level
      )
    }
  }
  if (length(rows) == 0) stop("Need >= 2 age groups per survey for the catalytic method.")
  dplyr::bind_rows(rows)
}
