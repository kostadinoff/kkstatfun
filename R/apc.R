# ============================================================
# AGE-PERIOD-COHORT ANALYSIS
# ============================================================

#' Age-Period-Cohort Analysis (KK)
#'
#' @description Analyses trends in rates by age, calendar period and birth cohort.
#'   Because cohort = period - age, the three time scales are linearly dependent
#'   and their individual linear trends are **not separately identifiable** (the
#'   identifiability problem). This function therefore reports only estimable
#'   quantities: the age-specific rates, the overall **net drift** (the common
#'   annual rate of change, from an age-drift Poisson model), and a sequence of
#'   nested Poisson models whose deviances isolate the *non-linear* (curvature)
#'   contributions of period and cohort, which *are* identifiable.
#'
#' @param data Long data frame with one row per age-by-period cell.
#' @param age Age (group) column (bare name or string).
#' @param period Calendar period column (bare name or string).
#' @param count Event-count column (bare name or string).
#' @param pop Population / person-time column (bare name or string); enters as an
#'   offset.
#'
#' @return A list with: `rates` (age x period observed rates), `net_drift` (annual
#'   percent change with CI, estimable), and `models` (a deviance table for the
#'   Age, Age-drift, Age-Cohort, Age-Period and Age-Period-Cohort Poisson models,
#'   with likelihood-ratio tests for the period and cohort curvatures).
#'
#' @examples
#' \dontrun{
#' d <- expand.grid(age = seq(30, 70, 10), period = seq(1980, 2010, 10))
#' d$pop <- 100000
#' d$count <- rpois(nrow(d),
#'   d$pop * exp(-9 + 0.05 * (d$age - 50) - 0.02 * (d$period - 1995)))
#' fit <- kk_apc(d, age, period, count, pop)
#' fit$net_drift
#' fit$models
#' }
#'
#' @export
kk_apc <- function(data, age, period, count, pop) {
  validate_data_frame(data)
  age_name <- .kk_colname(rlang::enquo(age))
  per_name <- .kk_colname(rlang::enquo(period))
  cnt_name <- .kk_colname(rlang::enquo(count))
  pop_name <- .kk_colname(rlang::enquo(pop))
  need <- c(age_name, per_name, cnt_name, pop_name)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }

  d <- data.frame(
    age = data[[age_name]],
    period = data[[per_name]],
    count = data[[cnt_name]],
    pop = data[[pop_name]]
  )
  if (!is.numeric(d$period)) stop("`period` must be numeric to define drift and cohort.")
  d$cohort <- d$period - as.numeric(as.character(d$age))
  d$age_f <- factor(d$age)
  d$period_f <- factor(d$period)
  d$cohort_f <- factor(d$cohort)
  # Centre period for a stable, interpretable drift coefficient
  d$period_c <- d$period - mean(d$period)

  pois <- function(fml) stats::glm(fml, family = stats::poisson(), data = d,
                                   offset = log(pop))

  m_age <- pois(count ~ age_f)
  m_drift <- pois(count ~ age_f + period_c)
  m_ac <- pois(count ~ age_f + cohort_f)
  m_ap <- pois(count ~ age_f + period_f)
  m_apc <- pois(count ~ age_f + period_f + cohort_f)

  # Net drift: annual multiplicative change from the age-drift model
  b <- stats::coef(m_drift)[["period_c"]]
  se <- sqrt(stats::vcov(m_drift)["period_c", "period_c"])
  z <- stats::qnorm(0.975)
  net_drift <- tibble::tibble(
    annual_pct_change = (exp(b) - 1) * 100,
    lower = (exp(b - z * se) - 1) * 100,
    upper = (exp(b + z * se) - 1) * 100
  )

  dev_row <- function(nm, m) {
    tibble::tibble(model = nm, resid_dev = stats::deviance(m),
      resid_df = stats::df.residual(m), AIC = stats::AIC(m))
  }
  models <- dplyr::bind_rows(
    dev_row("Age", m_age),
    dev_row("Age-drift", m_drift),
    dev_row("Age-Cohort", m_ac),
    dev_row("Age-Period", m_ap),
    dev_row("Age-Period-Cohort", m_apc)
  )
  # Curvature LRTs: does period (cohort) add non-linear structure beyond drift?
  lrt <- function(m0, m1) {
    stats::pchisq(stats::deviance(m0) - stats::deviance(m1),
      df = stats::df.residual(m0) - stats::df.residual(m1), lower.tail = FALSE)
  }
  models$lrt_p_vs_drift <- c(
    NA,
    NA,
    lrt(m_drift, m_ac),  # cohort curvature
    lrt(m_drift, m_ap),  # period curvature
    lrt(m_ap, m_apc)     # cohort curvature beyond age-period
  )

  rates <- d %>%
    dplyr::transmute(
      age = .data$age, period = .data$period, cohort = .data$cohort,
      count = .data$count, pop = .data$pop,
      rate = .data$count / .data$pop
    ) %>%
    dplyr::arrange(.data$period, .data$age)

  list(rates = tibble::as_tibble(rates), net_drift = net_drift, models = models)
}
