# ============================================================
# DIFFERENCE-IN-DIFFERENCES
# Canonical 2x2, two-way fixed effects, event study, and
# staggered adoption (group-time ATT)
# ============================================================

#' Coerce a Two-Level Variable to 0/1
#'
#' @param x The vector to coerce.
#' @param label Name used in error messages.
#'
#' @return An integer vector of 0s and 1s.
#' @noRd
.kk_as01 <- function(x, label) {
  if (is.logical(x)) {
    return(as.integer(x))
  }
  if (is.numeric(x) && all(x %in% c(0, 1))) {
    return(as.integer(x))
  }
  lv <- sort(unique(stats::na.omit(x)))
  if (length(lv) != 2) {
    stop("`", label, "` must have exactly two levels; found ", length(lv),
         ".", call. = FALSE)
  }
  as.integer(x == lv[2])
}

#' Robust or Cluster-Robust Variance for a Fitted Model
#'
#' @param fit Fitted `lm`/`glm`.
#' @param cluster Cluster vector, or `NULL` for heteroscedasticity-robust.
#' @param type Sandwich type.
#'
#' @return List with the variance matrix `V`, the degrees of freedom `df` to
#'   use for t-based inference, and the number of clusters `n_clusters`.
#' @noRd
.kk_robust_vcov <- function(fit, cluster = NULL, type = "HC1") {
  if (is.null(cluster)) {
    v <- tryCatch(sandwich::vcovHC(fit, type = type),
                  error = function(e) stats::vcov(fit))
    return(list(V = v, df = stats::df.residual(fit), n_clusters = NA_integer_))
  }
  g <- length(unique(cluster))
  v <- tryCatch(sandwich::vcovCL(fit, cluster = cluster, type = type),
                error = function(e) stats::vcov(fit))
  list(V = v, df = max(g - 1, 1), n_clusters = g)
}

#' Difference-in-Differences (KK)
#'
#' @description Estimates the average treatment effect on the treated from a
#'   before/after comparison of a treated and a control group. In the canonical
#'   2x2 form the estimand is the interaction of `treated` and `post`: the
#'   change in the treated group net of the change in the control group. With
#'   `unit` and `time` supplied the same coefficient is estimated from a
#'   two-way fixed-effects regression, which allows repeated observations and
#'   more than two periods.
#'
#'   Identification rests on parallel trends: absent the intervention, the two
#'   groups' outcomes would have moved together. That assumption is not
#'   testable from a 2x2 design - use [kk_event_study()] to inspect pre-period
#'   leads before trusting the estimate.
#'
#'   With staggered adoption (units treated at different times) the two-way
#'   fixed-effects coefficient is a variance-weighted average of 2x2
#'   comparisons that can include already-treated units as controls, and is
#'   biased when effects are heterogeneous over time. Use
#'   [kk_did_staggered()] in that case.
#'
#' @param data Data frame.
#' @param outcome Outcome column (bare name or string).
#' @param treated Treatment-group indicator (bare name or string): 1/`TRUE`
#'   for units ever exposed, 0 for the comparison group. Constant within unit.
#' @param post Period indicator (bare name or string): 1/`TRUE` for
#'   observations after the intervention.
#' @param covariates Optional character vector of covariate column names,
#'   entered linearly.
#' @param unit Optional panel identifier (bare name or string). When supplied,
#'   unit fixed effects replace the `treated` main effect and, unless `cluster`
#'   is given, standard errors are clustered on it.
#' @param time Optional period identifier (bare name or string). When supplied,
#'   period fixed effects replace the `post` main effect.
#' @param cluster Optional clustering column for the standard errors
#'   (defaults to `unit` when that is supplied, otherwise
#'   heteroscedasticity-robust HC1).
#' @param family Outcome family: `"gaussian"` (default, effect on the outcome
#'   scale), `"binomial"` or `"poisson"` (effect on the link scale, with the
#'   exponentiated ratio-of-ratios also reported).
#' @param offset Optional offset column for `family = "poisson"` (person-time
#'   or population); entered as `log(offset)`.
#' @param conf.level Confidence level (default 0.95).
#'
#' @return One-row tibble with the DiD `estimate`, `std.error`, confidence
#'   interval, `statistic`, `p.value`, the number of clusters, and
#'   `unadjusted` - the raw 2x2 difference-in-differences of the cell means.
#'   For non-Gaussian families the exponentiated `ratio` and its interval are
#'   appended. The 2x2 cell means are attached as the attribute `cells`, and
#'   the fitted model is retrievable with [kk_model()].
#'
#' @references Angrist JD, Pischke JS (2009) *Mostly Harmless Econometrics*,
#'   ch. 5. Princeton University Press.
#'
#' @seealso [kk_event_study()], [kk_did_staggered()], [kk_its()], [kk_synth()]
#'
#' @examples
#' set.seed(1)
#' n <- 400
#' d <- data.frame(
#'   unit = rep(seq_len(n), each = 2),
#'   post = rep(c(0, 1), times = n),
#'   treated = rep(rbinom(n, 1, 0.5), each = 2)
#' )
#' d$y <- 10 + 2 * d$post + 1.5 * d$treated -
#'   3 * d$treated * d$post + rnorm(nrow(d))
#'
#' kk_did(d, y, treated, post)
#' kk_did(d, y, treated, post, unit = unit, time = post)
#'
#' @export
kk_did <- function(data, outcome, treated, post, covariates = NULL,
                   unit = NULL, time = NULL, cluster = NULL,
                   family = c("gaussian", "binomial", "poisson"),
                   offset = NULL, conf.level = 0.95) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  validate_data_frame(data)
  family <- match.arg(family)

  y_name <- .kk_colname(rlang::enquo(outcome))
  tr_name <- .kk_colname(rlang::enquo(treated))
  po_name <- .kk_colname(rlang::enquo(post))
  unit_quo <- rlang::enquo(unit)
  time_quo <- rlang::enquo(time)
  cl_quo <- rlang::enquo(cluster)
  off_quo <- rlang::enquo(offset)
  unit_name <- if (rlang::quo_is_null(unit_quo)) NULL else .kk_colname(unit_quo)
  time_name <- if (rlang::quo_is_null(time_quo)) NULL else .kk_colname(time_quo)
  cl_name <- if (rlang::quo_is_null(cl_quo)) NULL else .kk_colname(cl_quo)
  off_name <- if (rlang::quo_is_null(off_quo)) NULL else .kk_colname(off_quo)

  need <- c(y_name, tr_name, po_name, covariates, unit_name, time_name,
            cl_name, off_name)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  data <- data[stats::complete.cases(data[, need, drop = FALSE]), , drop = FALSE]
  if (nrow(data) < 4) stop("Too few complete observations for a DiD.", call. = FALSE)

  tr <- .kk_as01(data[[tr_name]], tr_name)
  po <- .kk_as01(data[[po_name]], po_name)
  if (length(unique(tr)) < 2 || length(unique(po)) < 2) {
    stop("Both `treated` and `post` must vary in the data.", call. = FALSE)
  }

  df <- data.frame(.y = data[[y_name]], .treated = tr, .post = po,
                   .did = tr * po)
  if (!is.null(covariates)) df[covariates] <- data[covariates]
  if (!is.null(unit_name)) df$.unit <- data[[unit_name]]
  if (!is.null(time_name)) df$.time <- data[[time_name]]
  if (!is.null(off_name)) df$.off <- log(data[[off_name]])

  rhs <- ".did"
  rhs <- c(rhs, if (is.null(unit_name)) ".treated" else "factor(.unit)")
  rhs <- c(rhs, if (is.null(time_name)) ".post" else "factor(.time)")
  rhs <- c(rhs, covariates)
  form <- stats::as.formula(paste(".y ~", paste(rhs, collapse = " + ")))

  if (!is.null(unit_name) && length(unique(df$.unit)) > 500) {
    message("Fitting ", length(unique(df$.unit)),
            " unit fixed effects by least squares; this may be slow.")
  }

  fit <- if (family == "gaussian") {
    stats::lm(form, data = df)
  } else if (!is.null(off_name)) {
    stats::glm(form, data = df, family = family, offset = df$.off)
  } else {
    stats::glm(form, data = df, family = family)
  }

  cl_vec <- if (!is.null(cl_name)) {
    data[[cl_name]]
  } else if (!is.null(unit_name)) {
    df$.unit
  } else {
    NULL
  }
  rv <- .kk_robust_vcov(fit, cl_vec)

  if (!".did" %in% names(stats::coef(fit)) || is.na(stats::coef(fit)[".did"])) {
    stop("The DiD interaction could not be identified; check that treated ",
         "units are observed both before and after.", call. = FALSE)
  }
  est <- unname(stats::coef(fit)[".did"])
  se <- unname(sqrt(rv$V[".did", ".did"]))
  tcrit <- stats::qt(1 - (1 - conf.level) / 2, df = rv$df)
  stat <- est / se
  pval <- 2 * stats::pt(-abs(stat), df = rv$df)

  cell <- stats::aggregate(list(mean = df$.y),
                           by = list(treated = df$.treated, post = df$.post),
                           FUN = mean)
  cell$n <- stats::aggregate(list(n = df$.y),
                             by = list(treated = df$.treated, post = df$.post),
                             FUN = length)$n
  pick <- function(tt, pp) {
    v <- cell$mean[cell$treated == tt & cell$post == pp]
    if (length(v) == 1) v else NA_real_
  }
  raw <- (pick(1, 1) - pick(1, 0)) - (pick(0, 1) - pick(0, 0))

  out <- tibble::tibble(
    term = "did",
    estimate = est,
    std.error = se,
    conf.low = est - tcrit * se,
    conf.high = est + tcrit * se,
    statistic = stat,
    p.value = pval,
    unadjusted = raw,
    n = nrow(df),
    n_treated = sum(tr == 1),
    n_control = sum(tr == 0),
    n_clusters = rv$n_clusters,
    family = family,
    conf.level = conf.level
  )
  if (family != "gaussian") {
    out <- dplyr::mutate(
      out,
      ratio = exp(est),
      ratio.low = exp(est - tcrit * se),
      ratio.high = exp(est + tcrit * se)
    )
  }
  attr(out, "cells") <- tibble::as_tibble(cell)
  .kk_attach_models(out, multivariable = fit, data = df)
}

#' Event-Study Difference-in-Differences (KK)
#'
#' @description Estimates one treatment effect per period relative to
#'   adoption, from a two-way fixed-effects regression of the outcome on a full
#'   set of lead and lag indicators. The leads are the parallel-trends check:
#'   if the groups were already diverging before the intervention, the
#'   pre-period coefficients will differ from zero and the DiD estimate is not
#'   credible. The lags trace out how the effect evolves after adoption.
#'
#'   One period - by default the last pre-treatment period, `reference = -1` -
#'   is omitted as the baseline, so every coefficient is read relative to it.
#'   Units that are never treated (`treat_time` of `NA` or `Inf`) contribute to
#'   every period as pure controls.
#'
#'   Under staggered adoption these coefficients still borrow comparisons from
#'   already-treated units; [kk_did_staggered()] avoids that.
#'
#' @param data Data frame in long panel form, one row per unit-period.
#' @param outcome Outcome column (bare name or string).
#' @param unit Panel identifier column (bare name or string).
#' @param time Period column (bare name or string); numeric.
#' @param treat_time Column giving each unit's treatment adoption period
#'   (bare name or string); `NA` or `Inf` marks never-treated units.
#' @param covariates Optional character vector of covariate column names.
#' @param reference Relative period used as the baseline (default `-1`).
#' @param leads Maximum number of pre-periods to estimate; earlier periods are
#'   binned into the endpoint. `NULL` (default) estimates all of them.
#' @param lags Maximum number of post-periods to estimate; later periods are
#'   binned into the endpoint. `NULL` (default) estimates all of them.
#' @param cluster Optional clustering column (defaults to `unit`).
#' @param conf.level Confidence level (default 0.95).
#'
#' @return Tibble with one row per relative period: `rel_time`, `period`
#'   (`"pre"`, `"ref"` or `"post"`), `estimate`, `std.error`, confidence
#'   interval, `statistic`, `p.value` and the number of treated observations
#'   contributing. The joint Wald test of all leads is attached as the
#'   attribute `pretrend`.
#'
#' @references Sun L, Abraham S (2021) Estimating dynamic treatment effects in
#'   event studies with heterogeneous treatment effects. *Journal of
#'   Econometrics* 225:175-199.
#'
#' @seealso [kk_did()], [kk_did_staggered()]
#'
#' @examples
#' set.seed(2)
#' units <- 60
#' d <- expand.grid(unit = seq_len(units), time = 1:8)
#' adopt <- ifelse(seq_len(units) <= 30, 5, Inf)
#' d$treat_time <- adopt[d$unit]
#' d$y <- 5 + 0.3 * d$time + rnorm(nrow(d), 0, 0.5) +
#'   2 * (d$time >= d$treat_time)
#'
#' kk_event_study(d, y, unit, time, treat_time)
#'
#' @export
kk_event_study <- function(data, outcome, unit, time, treat_time,
                           covariates = NULL, reference = -1,
                           leads = NULL, lags = NULL, cluster = NULL,
                           conf.level = 0.95) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  validate_data_frame(data)

  y_name <- .kk_colname(rlang::enquo(outcome))
  u_name <- .kk_colname(rlang::enquo(unit))
  t_name <- .kk_colname(rlang::enquo(time))
  g_name <- .kk_colname(rlang::enquo(treat_time))
  cl_quo <- rlang::enquo(cluster)
  cl_name <- if (rlang::quo_is_null(cl_quo)) NULL else .kk_colname(cl_quo)

  need <- c(y_name, u_name, t_name, covariates, cl_name)
  missing_cols <- setdiff(c(need, g_name), names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  data <- data[stats::complete.cases(data[, need, drop = FALSE]), , drop = FALSE]

  tt <- data[[t_name]]
  gg <- data[[g_name]]
  if (!is.numeric(tt)) stop("`time` must be numeric.", call. = FALSE)
  gg <- suppressWarnings(as.numeric(gg))
  gg[is.na(gg)] <- Inf

  rel <- tt - gg
  rel[is.infinite(gg)] <- NA_real_
  if (all(is.na(rel))) stop("No treated units found.", call. = FALSE)
  if (!is.null(leads)) rel[!is.na(rel) & rel < -abs(leads)] <- -abs(leads)
  if (!is.null(lags)) rel[!is.na(rel) & rel > abs(lags)] <- abs(lags)

  keep_rel <- sort(unique(rel[!is.na(rel)]))
  if (!reference %in% keep_rel) {
    stop("Reference period ", reference, " does not occur in the data. ",
         "Available relative periods: ", paste(keep_rel, collapse = ", "),
         call. = FALSE)
  }
  est_rel <- setdiff(keep_rel, reference)

  df <- data.frame(.y = data[[y_name]], .unit = data[[u_name]], .time = tt)
  if (!is.null(covariates)) df[covariates] <- data[covariates]
  lbl <- function(k) paste0("d_", ifelse(k < 0, "m", "p"), abs(k))
  for (k in est_rel) df[[lbl(k)]] <- as.integer(!is.na(rel) & rel == k)

  rhs <- c(lbl(est_rel), "factor(.unit)", "factor(.time)", covariates)
  form <- stats::as.formula(paste(".y ~", paste(rhs, collapse = " + ")))
  fit <- stats::lm(form, data = df)

  cl_vec <- if (!is.null(cl_name)) data[[cl_name]] else df$.unit
  rv <- .kk_robust_vcov(fit, cl_vec)
  cf <- stats::coef(fit)
  tcrit <- stats::qt(1 - (1 - conf.level) / 2, df = rv$df)

  terms_k <- lbl(est_rel)
  ok <- terms_k %in% names(cf) & !is.na(cf[terms_k])
  est <- ifelse(ok, cf[terms_k], NA_real_)
  se <- vapply(seq_along(terms_k), function(i) {
    if (!ok[i]) return(NA_real_)
    sqrt(rv$V[terms_k[i], terms_k[i]])
  }, numeric(1))
  n_k <- vapply(est_rel, function(k) sum(!is.na(rel) & rel == k), integer(1))

  res <- tibble::tibble(
    rel_time = est_rel,
    period = ifelse(est_rel < 0, "pre", "post"),
    estimate = unname(est),
    std.error = se,
    conf.low = unname(est) - tcrit * se,
    conf.high = unname(est) + tcrit * se,
    statistic = unname(est) / se,
    p.value = 2 * stats::pt(-abs(unname(est) / se), df = rv$df),
    n_treated = n_k
  )
  ref_row <- tibble::tibble(
    rel_time = reference, period = "ref", estimate = 0, std.error = 0,
    conf.low = 0, conf.high = 0, statistic = NA_real_, p.value = NA_real_,
    n_treated = sum(!is.na(rel) & rel == reference)
  )
  res <- dplyr::arrange(dplyr::bind_rows(res, ref_row), .data$rel_time)
  res$conf.level <- conf.level

  # Joint Wald test that every lead is zero: the parallel-trends check.
  lead_terms <- terms_k[est_rel < 0 & ok]
  pretrend <- if (length(lead_terms) > 0) {
    b <- cf[lead_terms]
    v <- rv$V[lead_terms, lead_terms, drop = FALSE]
    w <- tryCatch(
      as.numeric(t(b) %*% solve(v) %*% b),
      error = function(e) NA_real_
    )
    tibble::tibble(
      test = "Joint Wald test of pre-treatment leads",
      statistic = w, df = length(lead_terms),
      p.value = stats::pchisq(w, df = length(lead_terms), lower.tail = FALSE),
      n_leads = length(lead_terms)
    )
  } else {
    tibble::tibble(test = "Joint Wald test of pre-treatment leads",
                   statistic = NA_real_, df = 0L, p.value = NA_real_,
                   n_leads = 0L)
  }
  attr(res, "pretrend") <- pretrend
  .kk_attach_models(res, multivariable = fit, data = df)
}

#' Group-Time Average Treatment Effects with Staggered Adoption (KK)
#'
#' @description Difference-in-differences when units adopt the treatment at
#'   different times. Instead of one two-way fixed-effects coefficient - which
#'   under staggered adoption uses already-treated units as controls and can
#'   carry the wrong sign when effects change over time - this estimates a
#'   separate 2x2 comparison ATT(g, t) for every adoption cohort `g` and every
#'   period `t`, always against units not yet treated, and then aggregates.
#'
#'   Four aggregations are returned: `overall` (one number, the average effect
#'   across all post-treatment cell), `dynamic` (an event study by time since
#'   adoption), `group` (one effect per adoption cohort) and `calendar` (one
#'   effect per period). Standard errors come from a unit-level nonparametric
#'   bootstrap, which respects the within-unit dependence of a panel;
#'   simultaneous bands use the bootstrapped maximum t-statistic, so the whole
#'   set of intervals covers at `conf.level` jointly rather than one at a time.
#'
#' @param data Data frame in long panel form, one row per unit-period.
#' @param outcome Outcome column (bare name or string).
#' @param unit Panel identifier column (bare name or string).
#' @param time Period column (bare name or string); numeric.
#' @param treat_time Adoption-period column (bare name or string); `NA` or
#'   `Inf` marks never-treated units.
#' @param covariates Optional character vector of time-invariant covariate
#'   names. When supplied, each ATT(g, t) is estimated by outcome regression:
#'   the change in outcome is modelled among the comparison units and the
#'   fitted values are subtracted from the treated units' changes.
#' @param control Comparison group: `"notyettreated"` (default, uses both
#'   never-treated and not-yet-treated units) or `"nevertreated"`.
#' @param base_period `"universal"` (default) compares every period with the
#'   period before adoption; `"varying"` compares each pre-period with the one
#'   immediately before it, which is more sensitive to short-run pre-trends.
#' @param n_boot Bootstrap replicates (default 200). Raise to 999+ for
#'   published estimates.
#' @param conf.level Confidence level (default 0.95).
#'
#' @return A named list of tibbles:
#'   \describe{
#'     \item{att_gt}{One row per cohort-period cell: `group`, `time`,
#'       `rel_time`, `estimate`, `std.error`, pointwise and simultaneous
#'       confidence bounds, `p.value` and cell sizes.}
#'     \item{dynamic}{Event study averaged by time since adoption.}
#'     \item{group}{One post-treatment average per adoption cohort.}
#'     \item{calendar}{One average per calendar period.}
#'     \item{overall}{One-row summary: the cohort-size-weighted average of all
#'       post-treatment ATT(g, t).}
#'   }
#'
#' @references Callaway B, Sant'Anna PHC (2021) Difference-in-differences with
#'   multiple time periods. *Journal of Econometrics* 225:200-230.
#'
#' @seealso [kk_did()], [kk_event_study()]
#'
#' @examples
#' set.seed(3)
#' units <- 90
#' d <- expand.grid(unit = seq_len(units), time = 1:6)
#' adopt <- rep(c(3, 5, Inf), each = units / 3)
#' d$treat_time <- adopt[d$unit]
#' d$y <- 4 + 0.2 * d$time + rnorm(nrow(d), 0, 0.4) +
#'   1.5 * pmax(0, d$time - d$treat_time + 1)
#'
#' res <- kk_did_staggered(d, y, unit, time, treat_time, n_boot = 50)
#' res$overall
#' res$dynamic
#'
#' @export
kk_did_staggered <- function(data, outcome, unit, time, treat_time,
                             covariates = NULL,
                             control = c("notyettreated", "nevertreated"),
                             base_period = c("universal", "varying"),
                             n_boot = 200, conf.level = 0.95) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  validate_data_frame(data)
  control <- match.arg(control)
  base_period <- match.arg(base_period)

  y_name <- .kk_colname(rlang::enquo(outcome))
  u_name <- .kk_colname(rlang::enquo(unit))
  t_name <- .kk_colname(rlang::enquo(time))
  g_name <- .kk_colname(rlang::enquo(treat_time))

  need <- c(y_name, u_name, t_name, covariates)
  missing_cols <- setdiff(c(need, g_name), names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  data <- data[stats::complete.cases(data[, need, drop = FALSE]), , drop = FALSE]
  if (!is.numeric(data[[t_name]])) stop("`time` must be numeric.", call. = FALSE)

  uid <- as.character(data[[u_name]])
  units_all <- unique(uid)
  times <- sort(unique(data[[t_name]]))
  n_u <- length(units_all)
  n_t <- length(times)

  # Wide outcome matrix: units x periods. Everything downstream is matrix
  # arithmetic on this, which keeps the bootstrap affordable.
  ymat <- matrix(NA_real_, n_u, n_t, dimnames = list(units_all, as.character(times)))
  ymat[cbind(match(uid, units_all), match(data[[t_name]], times))] <- data[[y_name]]

  gvec <- suppressWarnings(as.numeric(data[[g_name]]))
  gvec[is.na(gvec)] <- Inf
  gu <- gvec[match(units_all, uid)]
  names(gu) <- units_all

  xmat <- NULL
  if (!is.null(covariates)) {
    xmat <- as.matrix(data[match(units_all, uid), covariates, drop = FALSE])
    xmat <- apply(xmat, 2, as.numeric)
    if (!is.matrix(xmat)) xmat <- matrix(xmat, nrow = n_u)
    colnames(xmat) <- covariates
  }

  cohorts <- sort(unique(gu[is.finite(gu)]))
  cohorts <- cohorts[cohorts > min(times)]
  if (length(cohorts) == 0) {
    stop("No adoption cohort has a pre-treatment period.", call. = FALSE)
  }
  if (control == "nevertreated" && !any(is.infinite(gu))) {
    stop("control = \"nevertreated\" but there are no never-treated units.",
         call. = FALSE)
  }

  # Enumerate the (g, t) cells once; the bootstrap reuses this grid.
  cells <- list()
  for (g in cohorts) {
    for (ti in seq_len(n_t)) {
      t_val <- times[ti]
      base_val <- if (base_period == "universal" || t_val >= g) {
        max(times[times < g])
      } else {
        if (ti == 1L) next
        times[ti - 1L]
      }
      if (t_val == base_val) next
      cells[[length(cells) + 1L]] <- list(
        g = g, t = t_val, ti = ti, bi = match(base_val, times)
      )
    }
  }

  att_one <- function(cell, rows) {
    g <- cell$g
    delta <- ymat[rows, cell$ti] - ymat[rows, cell$bi]
    gr <- gu[rows]
    ref_t <- max(cell$t, times[cell$bi])
    is_treat <- gr == g
    is_ctrl <- if (control == "nevertreated") {
      is.infinite(gr)
    } else {
      gr > ref_t
    }
    ok <- !is.na(delta)
    tr_i <- which(is_treat & ok)
    ct_i <- which(is_ctrl & ok)
    if (length(tr_i) < 2 || length(ct_i) < 2) {
      return(c(NA_real_, length(tr_i), length(ct_i)))
    }
    if (is.null(xmat)) {
      est <- mean(delta[tr_i]) - mean(delta[ct_i])
    } else {
      xc <- xmat[rows, , drop = FALSE]
      fitd <- tryCatch(
        stats::lm.fit(cbind(1, xc[ct_i, , drop = FALSE]), delta[ct_i]),
        error = function(e) NULL
      )
      if (is.null(fitd) || anyNA(fitd$coefficients)) {
        est <- mean(delta[tr_i]) - mean(delta[ct_i])
      } else {
        pred <- drop(cbind(1, xc[tr_i, , drop = FALSE]) %*% fitd$coefficients)
        est <- mean(delta[tr_i] - pred)
      }
    }
    c(est, length(tr_i), length(ct_i))
  }

  compute_all <- function(rows) {
    vapply(cells, function(cl) att_one(cl, rows)[1], numeric(1))
  }

  # Cohort sizes weight every aggregation, so a large cohort counts more.
  cohort_n <- vapply(cohorts, function(g) sum(gu == g), integer(1))
  names(cohort_n) <- as.character(cohorts)

  g_vec <- vapply(cells, function(z) z$g, numeric(1))
  t_vec <- vapply(cells, function(z) z$t, numeric(1))
  e_vec <- t_vec - g_vec
  post <- t_vec >= g_vec

  aggregate_all <- function(att, w_n) {
    wts <- w_n[as.character(g_vec)]
    ok <- post & !is.na(att)
    overall <- if (any(ok)) stats::weighted.mean(att[ok], wts[ok]) else NA_real_
    dyn_e <- sort(unique(e_vec))
    dyn <- vapply(dyn_e, function(e) {
      k <- e_vec == e & !is.na(att)
      if (!any(k)) return(NA_real_)
      stats::weighted.mean(att[k], wts[k])
    }, numeric(1))
    grp <- vapply(cohorts, function(g) {
      k <- g_vec == g & post & !is.na(att)
      if (!any(k)) return(NA_real_)
      mean(att[k])
    }, numeric(1))
    cal <- vapply(times, function(tv) {
      k <- t_vec == tv & post & !is.na(att)
      if (!any(k)) return(NA_real_)
      stats::weighted.mean(att[k], wts[k])
    }, numeric(1))
    list(att = att, overall = overall, dynamic = dyn, group = grp,
         calendar = cal, dyn_e = dyn_e)
  }

  point <- aggregate_all(compute_all(seq_len(n_u)), cohort_n)
  sizes <- vapply(cells, function(cl) att_one(cl, seq_len(n_u))[2:3], numeric(2))

  # Unit-level bootstrap: resampling whole units keeps the serial correlation
  # within a unit intact, which resampling rows would destroy.
  n_boot <- max(as.integer(n_boot), 2L)
  boot <- lapply(seq_len(n_boot), function(b) {
    rows <- sample.int(n_u, n_u, replace = TRUE)
    w_n <- vapply(cohorts, function(g) sum(gu[rows] == g), integer(1))
    names(w_n) <- as.character(cohorts)
    aggregate_all(compute_all(rows), w_n)
  })

  robust_se <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 5) return(NA_real_)
    q <- stats::quantile(x, c(0.25, 0.75), names = FALSE)
    (q[2] - q[1]) / (stats::qnorm(0.75) - stats::qnorm(0.25))
  }
  z <- stats::qnorm(1 - (1 - conf.level) / 2)

  se_att <- apply(vapply(boot, function(b) b$att, numeric(length(cells))), 1, robust_se)
  # Simultaneous band: the quantile of the bootstrapped max |t| across cells.
  tmax <- vapply(boot, function(b) {
    tt <- abs((b$att - point$att) / se_att)
    if (all(!is.finite(tt))) return(NA_real_)
    max(tt[is.finite(tt)])
  }, numeric(1))
  crit <- stats::quantile(tmax[is.finite(tmax)], conf.level, names = FALSE)
  if (!is.finite(crit) || crit < z) crit <- z

  att_gt <- tibble::tibble(
    group = g_vec, time = t_vec, rel_time = e_vec,
    period = ifelse(post, "post", "pre"),
    estimate = point$att, std.error = se_att,
    conf.low = point$att - z * se_att,
    conf.high = point$att + z * se_att,
    band.low = point$att - crit * se_att,
    band.high = point$att + crit * se_att,
    p.value = 2 * stats::pnorm(-abs(point$att / se_att)),
    n_treated = as.integer(sizes[1, ]),
    n_control = as.integer(sizes[2, ])
  )
  att_gt <- dplyr::arrange(att_gt, .data$group, .data$time)

  tidy_agg <- function(label_name, labels, est, boot_get) {
    bm <- vapply(boot, boot_get, numeric(length(est)))
    if (is.null(dim(bm))) bm <- matrix(bm, nrow = length(est))
    se <- apply(bm, 1, robust_se)
    out <- tibble::tibble(
      !!label_name := labels, estimate = est, std.error = se,
      conf.low = est - z * se, conf.high = est + z * se,
      p.value = 2 * stats::pnorm(-abs(est / se))
    )
    out$conf.level <- conf.level
    out
  }

  dynamic <- tidy_agg("rel_time", point$dyn_e, point$dynamic,
                      function(b) b$dynamic)
  dynamic$period <- ifelse(dynamic$rel_time < 0, "pre", "post")
  group_agg <- tidy_agg("group", cohorts, point$group, function(b) b$group)
  group_agg$n_units <- as.integer(cohort_n)
  calendar <- tidy_agg("time", times, point$calendar, function(b) b$calendar)

  se_o <- robust_se(vapply(boot, function(b) b$overall, numeric(1)))
  overall <- tibble::tibble(
    estimate = point$overall, std.error = se_o,
    conf.low = point$overall - z * se_o,
    conf.high = point$overall + z * se_o,
    p.value = 2 * stats::pnorm(-abs(point$overall / se_o)),
    n_units = n_u, n_periods = n_t, n_cohorts = length(cohorts),
    n_never_treated = sum(is.infinite(gu)),
    control = control, n_boot = n_boot, conf.level = conf.level
  )

  list(att_gt = att_gt, dynamic = dynamic, group = group_agg,
       calendar = calendar, overall = overall)
}
