# ============================================================
# INTERRUPTED TIME SERIES (SEGMENTED REGRESSION)
# ============================================================

#' Interrupted Time Series (KK)
#'
#' @description Segmented regression for a single series observed before and
#'   after a known intervention. Two effects are separated: the immediate
#'   *level change* at the moment of the intervention, and the *trend change* -
#'   the shift in slope that accumulates afterwards. A programme can look
#'   successful on one and useless on the other, so both belong in the report.
#'
#'   The counterfactual is the pre-intervention trend extrapolated forward.
#'   That is the design's weak point: it assumes nothing else changed at the
#'   same time and that the pre-period trend would have continued. A
#'   contemporaneous control series ([kk_synth()]) or a comparison group
#'   ([kk_did()]) tests that assumption; segmented regression alone cannot.
#'
#'   Serial correlation is the standard failure mode of ITS - ignoring it
#'   makes the intervals far too narrow - so standard errors are
#'   heteroscedasticity- and autocorrelation-consistent (Newey-West) by
#'   default, and the residual autocorrelation diagnostics are returned.
#'
#' @param data Data frame with one row per period, in any order.
#' @param outcome Outcome column (bare name or string): a count, rate or
#'   continuous measure.
#' @param time Time column (bare name or string); numeric or `Date`. Periods
#'   are assumed equally spaced.
#' @param intervention_time The first period *after* the intervention, on the
#'   scale of `time`.
#' @param offset Optional population/person-time column; entered as
#'   `log(offset)` for count families, turning the model into one for rates.
#' @param transition Number of periods immediately after the intervention to
#'   drop as a wash-out (default 0).
#' @param family `"gaussian"` (default), `"poisson"` or `"quasipoisson"`.
#' @param harmonic Number of Fourier pairs for seasonality (default 0). Needs
#'   `period`.
#' @param period Seasonal period in observations (e.g. 12 for monthly data).
#' @param nw_lag Newey-West lag truncation. `NULL` (default) uses
#'   `floor(4 * (n/100)^(2/9))`; `0` gives ordinary heteroscedasticity-robust
#'   errors.
#' @param conf.level Confidence level (default 0.95).
#'
#' @return Tibble with one row per model term - `intercept`, `baseline_trend`,
#'   `level_change`, `trend_change` and any harmonics - with `estimate`,
#'   `std.error`, confidence interval and `p.value`; count families add the
#'   exponentiated `ratio`. Attributes: `counterfactual` (per-period observed,
#'   fitted and counterfactual values with an interval, ready to plot),
#'   `impact` (the absolute and relative effect in the final period and the
#'   cumulative difference over the whole post period) and `diagnostics`
#'   (Durbin-Watson and Ljung-Box tests for residual autocorrelation).
#'
#' @references Bernal JL, Cummins S, Gasparrini A (2017) Interrupted time
#'   series regression for the evaluation of public health interventions.
#'   *International Journal of Epidemiology* 46:348-355.
#'
#' @seealso [kk_did()], [kk_synth()], [kk_time_series()]
#'
#' @examples
#' set.seed(9)
#' n <- 60
#' d <- data.frame(month = 1:n)
#' d$cases <- 100 - 0.5 * d$month - 15 * (d$month >= 37) -
#'   0.8 * pmax(0, d$month - 36) + rnorm(n, 0, 4)
#'
#' res <- kk_its(d, cases, month, intervention_time = 37)
#' res
#' attr(res, "impact")
#'
#' @export
kk_its <- function(data, outcome, time, intervention_time, offset = NULL,
                   transition = 0,
                   family = c("gaussian", "poisson", "quasipoisson"),
                   harmonic = 0, period = NULL, nw_lag = NULL,
                   conf.level = 0.95) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  validate_data_frame(data)
  family <- match.arg(family)

  y_name <- .kk_colname(rlang::enquo(outcome))
  t_name <- .kk_colname(rlang::enquo(time))
  off_quo <- rlang::enquo(offset)
  off_name <- if (rlang::quo_is_null(off_quo)) NULL else .kk_colname(off_quo)

  need <- c(y_name, t_name, off_name)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  data <- data[stats::complete.cases(data[, need, drop = FALSE]), , drop = FALSE]
  data <- data[order(data[[t_name]]), , drop = FALSE]

  tval <- data[[t_name]]
  y <- as.numeric(data[[y_name]])
  n <- length(y)
  if (n < 8) stop("Too few periods for a segmented regression.", call. = FALSE)

  post <- as.integer(tval >= intervention_time)
  if (all(post == 0) || all(post == 1)) {
    stop("`intervention_time` lies outside the observed time range.",
         call. = FALSE)
  }
  t_idx <- seq_len(n)
  first_post <- min(t_idx[post == 1])
  trend_after <- pmax(0, t_idx - first_post + 1)

  keep <- rep(TRUE, n)
  if (transition > 0) {
    keep <- !(post == 1 & trend_after <= transition)
  }

  df <- data.frame(.y = y, .t = t_idx, .level = post, .trend = trend_after)
  harm_names <- character(0)
  if (harmonic > 0) {
    if (is.null(period)) {
      stop("`period` is required when `harmonic` > 0.", call. = FALSE)
    }
    for (k in seq_len(harmonic)) {
      sn <- paste0("sin", k)
      cs <- paste0("cos", k)
      df[[sn]] <- sin(2 * pi * k * t_idx / period)
      df[[cs]] <- cos(2 * pi * k * t_idx / period)
      harm_names <- c(harm_names, sn, cs)
    }
  }
  if (!is.null(off_name)) df$.off <- log(as.numeric(data[[off_name]]))

  rhs <- c(".t", ".level", ".trend", harm_names)
  form <- stats::as.formula(paste(".y ~", paste(rhs, collapse = " + ")))
  fit_df <- df[keep, , drop = FALSE]
  fit <- if (family == "gaussian") {
    stats::lm(form, data = fit_df)
  } else if (!is.null(off_name)) {
    stats::glm(form, data = fit_df, family = family, offset = fit_df$.off)
  } else {
    stats::glm(form, data = fit_df, family = family)
  }

  lag_l <- if (is.null(nw_lag)) floor(4 * (sum(keep) / 100)^(2 / 9)) else nw_lag
  V <- if (lag_l > 0) {
    tryCatch(
      sandwich::NeweyWest(fit, lag = lag_l, prewhite = FALSE, adjust = TRUE),
      error = function(e) sandwich::vcovHC(fit, type = "HC1")
    )
  } else {
    tryCatch(sandwich::vcovHC(fit, type = "HC1"),
             error = function(e) stats::vcov(fit))
  }

  cf <- stats::coef(fit)
  se <- sqrt(diag(V))[names(cf)]
  dfree <- stats::df.residual(fit)
  tcrit <- stats::qt(1 - (1 - conf.level) / 2, df = dfree)
  pretty <- c("(Intercept)" = "intercept", ".t" = "baseline_trend",
              ".level" = "level_change", ".trend" = "trend_change")
  term_lbl <- ifelse(names(cf) %in% names(pretty), pretty[names(cf)], names(cf))

  out <- tibble::tibble(
    term = unname(term_lbl),
    estimate = unname(cf),
    std.error = unname(se),
    conf.low = unname(cf) - tcrit * unname(se),
    conf.high = unname(cf) + tcrit * unname(se),
    statistic = unname(cf / se),
    p.value = 2 * stats::pt(-abs(unname(cf / se)), df = dfree)
  )
  if (family != "gaussian") {
    out$ratio <- exp(out$estimate)
    out$ratio.low <- exp(out$conf.low)
    out$ratio.high <- exp(out$conf.high)
  }
  out$conf.level <- conf.level

  # Counterfactual: the pre-intervention model carried forward, with the level
  # and trend-change columns zeroed out.
  X <- stats::model.matrix(form, data = df)
  # An aliased coefficient is absent from vcov(); keep only terms both objects
  # agree on so the quadratic forms below stay conformable.
  keep_terms <- intersect(colnames(X), rownames(V))
  X <- X[, keep_terms, drop = FALSE]
  Vk <- V[keep_terms, keep_terms, drop = FALSE]
  Xcf <- X
  if (".level" %in% colnames(Xcf)) Xcf[, ".level"] <- 0
  if (".trend" %in% colnames(Xcf)) Xcf[, ".trend"] <- 0
  bb <- cf[keep_terms]
  bb[is.na(bb)] <- 0
  eta_fit <- drop(X %*% bb)
  eta_cf <- drop(Xcf %*% bb)
  se_cf <- sqrt(pmax(0, rowSums((Xcf %*% Vk) * Xcf)))
  if (!is.null(off_name)) {
    eta_fit <- eta_fit + df$.off
    eta_cf <- eta_cf + df$.off
  }
  inv <- if (family == "gaussian") identity else exp
  cfl <- inv(eta_cf - tcrit * se_cf)
  cfh <- inv(eta_cf + tcrit * se_cf)

  counterfactual <- tibble::tibble(
    time = tval,
    observed = y,
    fitted = inv(eta_fit),
    counterfactual = inv(eta_cf),
    cf.low = cfl,
    cf.high = cfh,
    period = ifelse(post == 1, "post", "pre"),
    in_model = keep
  )

  # Cumulative impact over the post period: observed minus the extrapolated
  # counterfactual, with the counterfactual's own uncertainty carried through.
  post_rows <- which(post == 1 & keep)
  if (length(post_rows) > 0) {
    if (family == "gaussian") {
      cvec <- colSums(Xcf[post_rows, , drop = FALSE])
      cum_cf <- sum(eta_cf[post_rows])
      cum_se <- sqrt(drop(t(cvec) %*% Vk %*% cvec))
    } else {
      cum_cf <- sum(inv(eta_cf[post_rows]))
      # Delta method on the sum of exponentials.
      grad <- colSums(Xcf[post_rows, , drop = FALSE] * inv(eta_cf[post_rows]))
      cum_se <- sqrt(drop(t(grad) %*% Vk %*% grad))
    }
    cum_obs <- sum(y[post_rows])
    last <- utils::tail(post_rows, 1)
    impact <- tibble::tibble(
      final_observed = inv(eta_fit[last]),
      final_counterfactual = inv(eta_cf[last]),
      final_absolute = inv(eta_fit[last]) - inv(eta_cf[last]),
      final_relative = (inv(eta_fit[last]) - inv(eta_cf[last])) /
        inv(eta_cf[last]),
      cumulative_observed = cum_obs,
      cumulative_counterfactual = cum_cf,
      cumulative_difference = cum_obs - cum_cf,
      cumulative.low = cum_obs - cum_cf - tcrit * cum_se,
      cumulative.high = cum_obs - cum_cf + tcrit * cum_se,
      n_post = length(post_rows)
    )
  } else {
    impact <- tibble::tibble()
  }

  res <- stats::residuals(fit, type = if (family == "gaussian") "response" else "pearson")
  dw <- sum(diff(res)^2) / sum(res^2)
  lb <- tryCatch(
    stats::Box.test(res, lag = min(10, max(1, floor(length(res) / 5))),
                    type = "Ljung-Box"),
    error = function(e) NULL
  )
  diagnostics <- tibble::tibble(
    test = c("Durbin-Watson", "Ljung-Box"),
    statistic = c(dw, if (is.null(lb)) NA_real_ else unname(lb$statistic)),
    df = c(NA_real_, if (is.null(lb)) NA_real_ else unname(lb$parameter)),
    p.value = c(NA_real_, if (is.null(lb)) NA_real_ else lb$p.value)
  )

  attr(out, "counterfactual") <- counterfactual
  attr(out, "impact") <- impact
  attr(out, "diagnostics") <- diagnostics
  attr(out, "nw_lag") <- lag_l
  .kk_attach_models(out, multivariable = fit, data = fit_df)
}
