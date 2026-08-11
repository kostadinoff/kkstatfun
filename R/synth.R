# ============================================================
# SYNTHETIC CONTROL
# ============================================================

#' Euclidean Projection onto the Probability Simplex
#'
#' @param v Numeric vector.
#'
#' @return The closest vector with non-negative entries summing to one.
#' @noRd
.kk_simplex_project <- function(v) {
  n <- length(v)
  u <- sort(v, decreasing = TRUE)
  css <- cumsum(u)
  rho <- which(u - (css - 1) / seq_len(n) > 0)
  if (length(rho) == 0) {
    return(rep(1 / n, n))
  }
  rho <- max(rho)
  theta <- (css[rho] - 1) / rho
  pmax(v - theta, 0)
}

#' Donor Weights for a Synthetic Control
#'
#' @description Minimises `||a - B w||^2` subject to `w >= 0` and `sum(w) = 1`
#'   by projected gradient descent. The simplex constraint is what keeps the
#'   synthetic unit inside the convex hull of the donors, so it cannot
#'   extrapolate.
#'
#' @param B Predictor matrix, donors in columns.
#' @param a Predictor vector for the treated unit.
#' @param max_iter Maximum iterations.
#' @param tol Convergence tolerance on the weight vector.
#'
#' @return Weight vector, one per donor.
#' @noRd
.kk_synth_weights <- function(B, a, max_iter = 5000, tol = 1e-10) {
  k <- ncol(B)
  if (k == 1) return(1)
  w <- rep(1 / k, k)
  btb <- crossprod(B)
  bta <- drop(crossprod(B, a))
  lip <- max(abs(eigen(btb, symmetric = TRUE, only.values = TRUE)$values))
  if (!is.finite(lip) || lip <= 0) lip <- 1
  step <- 1 / lip
  for (i in seq_len(max_iter)) {
    grad <- drop(btb %*% w) - bta
    w_new <- .kk_simplex_project(w - step * grad)
    if (max(abs(w_new - w)) < tol) {
      w <- w_new
      break
    }
    w <- w_new
  }
  w
}

#' Synthetic Control (KK)
#'
#' @description Builds a comparison unit for a single treated unit as a
#'   weighted average of untreated donor units, choosing the weights so the
#'   synthetic unit tracks the treated one as closely as possible *before* the
#'   intervention. The treatment effect is the gap between the two afterwards.
#'
#'   The method suits the case a difference-in-differences cannot handle: one
#'   treated region, hospital or country, several plausible comparators, and no
#'   reason to think any single one of them follows a parallel trend. Weights
#'   are non-negative and sum to one, so the synthetic unit is an interpolation
#'   of the donors rather than an extrapolation, and the weights themselves are
#'   reported - a synthetic control resting on one donor at 90% is a two-unit
#'   comparison wearing a disguise.
#'
#'   Inference is by placebo permutation: the same estimator is run pretending
#'   each donor in turn was the treated unit, and the treated unit's
#'   post/pre root-mean-square prediction error ratio is ranked against that
#'   distribution. With few donors the smallest attainable p-value is large -
#'   with 19 donors it is 0.05 - so the design needs a decent donor pool.
#'
#' @param data Data frame in long panel form, one row per unit-period.
#' @param outcome Outcome column (bare name or string).
#' @param unit Unit identifier column (bare name or string).
#' @param time Period column (bare name or string); numeric.
#' @param treated_unit The value of `unit` identifying the treated unit.
#' @param intervention_time First treated period, on the scale of `time`.
#' @param predictors Optional character vector of further covariate columns.
#'   Each is averaged over the pre-intervention period and added to the
#'   matching problem alongside the pre-period outcomes.
#' @param donors Optional character vector restricting the donor pool.
#' @param placebo Whether to run the placebo permutation inference
#'   (default `TRUE`).
#' @param placebo_trim Drop placebo units whose pre-period fit is worse than
#'   this multiple of the treated unit's pre-period RMSPE (default `Inf`, keep
#'   all). A value of 2 to 5 is a common robustness check: a donor the method
#'   cannot fit before the intervention tells you nothing about it afterwards.
#'
#' @return One-row tibble with the average post-intervention gap `att`, the
#'   same as a percentage of the synthetic unit's level (`att_pct`), the
#'   pre- and post-intervention RMSPE and their ratio, the placebo `p.value`
#'   and pool sizes. Attributes: `weights` (donor weights, descending), `path`
#'   (per-period observed, synthetic and gap series for plotting) and `placebo`
#'   (one row per placebo unit).
#'
#' @references Abadie A, Diamond A, Hainmueller J (2010) Synthetic control
#'   methods for comparative case studies. *JASA* 105:493-505. Abadie A (2021)
#'   Using synthetic controls. *Journal of Economic Literature* 59:391-425.
#'
#' @seealso [kk_did()], [kk_its()], [kk_did_staggered()]
#'
#' @examples
#' set.seed(10)
#' units <- paste0("region", 1:12)
#' d <- expand.grid(unit = units, time = 1:20, stringsAsFactors = FALSE)
#' base <- setNames(runif(12, 8, 12), units)
#' d$y <- base[d$unit] + 0.15 * d$time + rnorm(nrow(d), 0, 0.3)
#' d$y[d$unit == "region1" & d$time >= 15] <-
#'   d$y[d$unit == "region1" & d$time >= 15] - 2
#'
#' res <- kk_synth(d, y, unit, time, "region1", intervention_time = 15)
#' res
#' head(attr(res, "weights"))
#'
#' @export
kk_synth <- function(data, outcome, unit, time, treated_unit,
                     intervention_time, predictors = NULL, donors = NULL,
                     placebo = TRUE, placebo_trim = Inf) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  validate_data_frame(data)

  y_name <- .kk_colname(rlang::enquo(outcome))
  u_name <- .kk_colname(rlang::enquo(unit))
  t_name <- .kk_colname(rlang::enquo(time))

  need <- c(y_name, u_name, t_name, predictors)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  data <- data[stats::complete.cases(data[, need, drop = FALSE]), , drop = FALSE]

  uid <- as.character(data[[u_name]])
  treated_unit <- as.character(treated_unit)
  if (!treated_unit %in% uid) {
    stop("`treated_unit` \"", treated_unit, "\" not found in ", u_name, ".",
         call. = FALSE)
  }
  times <- sort(unique(data[[t_name]]))
  pre <- times[times < intervention_time]
  post <- times[times >= intervention_time]
  if (length(pre) < 3) {
    stop("At least three pre-intervention periods are needed; found ",
         length(pre), ".", call. = FALSE)
  }
  if (length(post) < 1) {
    stop("`intervention_time` leaves no post-intervention periods.",
         call. = FALSE)
  }

  units_all <- unique(uid)
  ymat <- matrix(NA_real_, length(units_all), length(times),
                 dimnames = list(units_all, as.character(times)))
  ymat[cbind(match(uid, units_all), match(data[[t_name]], times))] <-
    as.numeric(data[[y_name]])

  # A donor with a hole in its series cannot be matched on; say so rather than
  # silently filling it in.
  complete_units <- units_all[stats::complete.cases(ymat)]
  pool <- setdiff(complete_units, treated_unit)
  if (!is.null(donors)) pool <- intersect(pool, as.character(donors))
  if (length(pool) < 2) {
    stop("Fewer than two donor units with a complete outcome series.",
         call. = FALSE)
  }
  if (!treated_unit %in% complete_units) {
    stop("The treated unit's outcome series has gaps.", call. = FALSE)
  }

  pre_idx <- match(pre, times)
  post_idx <- match(post, times)

  # Matching variables: every pre-period outcome, plus the pre-period mean of
  # each extra predictor. Rows are standardised so a large-scale predictor
  # cannot dominate the fit.
  pred_mat <- NULL
  if (!is.null(predictors)) {
    is_pre <- data[[t_name]] < intervention_time
    agg <- stats::aggregate(data[is_pre, predictors, drop = FALSE],
                            by = list(unit = uid[is_pre]), FUN = mean)
    rownames(agg) <- agg$unit
    pred_mat <- t(as.matrix(agg[c(treated_unit, pool), predictors, drop = FALSE]))
  }

  build_x <- function(target, donor_ids) {
    m <- rbind(
      t(ymat[c(target, donor_ids), pre_idx, drop = FALSE]),
      if (is.null(pred_mat)) NULL else pred_mat[, c(target, donor_ids), drop = FALSE]
    )
    sds <- apply(m, 1, stats::sd)
    sds[!is.finite(sds) | sds < 1e-12] <- 1
    m / sds
  }

  fit_unit <- function(target, donor_ids) {
    xm <- build_x(target, donor_ids)
    w <- .kk_synth_weights(xm[, -1, drop = FALSE], xm[, 1])
    names(w) <- donor_ids
    synth <- drop(w %*% ymat[donor_ids, , drop = FALSE])
    gap <- ymat[target, ] - synth
    pre_rmspe <- sqrt(mean(gap[pre_idx]^2))
    post_rmspe <- sqrt(mean(gap[post_idx]^2))
    list(w = w, synth = synth, gap = gap, pre = pre_rmspe, post = post_rmspe)
  }

  main <- fit_unit(treated_unit, pool)
  att <- mean(main$gap[post_idx])
  synth_post <- mean(main$synth[post_idx])

  path <- tibble::tibble(
    time = times,
    observed = as.numeric(ymat[treated_unit, ]),
    synthetic = as.numeric(main$synth),
    gap = as.numeric(main$gap),
    period = ifelse(times >= intervention_time, "post", "pre")
  )
  wt <- tibble::tibble(unit = names(main$w), weight = as.numeric(main$w))
  wt <- dplyr::arrange(wt, dplyr::desc(.data$weight))

  placebo_tbl <- NULL
  p_val <- NA_real_
  if (isTRUE(placebo)) {
    plc <- lapply(pool, function(u) {
      others <- setdiff(pool, u)
      if (length(others) < 2) return(NULL)
      f <- fit_unit(u, others)
      tibble::tibble(unit = u, att = mean(f$gap[post_idx]),
                     pre_rmspe = f$pre, post_rmspe = f$post,
                     rmspe_ratio = f$post / f$pre)
    })
    placebo_tbl <- dplyr::bind_rows(plc)
    if (nrow(placebo_tbl) > 0) {
      trimmed <- placebo_tbl
      if (is.finite(placebo_trim)) {
        trimmed <- placebo_tbl[placebo_tbl$pre_rmspe <=
                                 placebo_trim * main$pre, , drop = FALSE]
      }
      ratio <- main$post / main$pre
      # Rank of the treated unit among all units, itself included: the exact
      # permutation p-value for the sharp null of no effect anywhere.
      p_val <- (1 + sum(trimmed$rmspe_ratio >= ratio)) / (1 + nrow(trimmed))
      placebo_tbl$included <- placebo_tbl$unit %in% trimmed$unit
      placebo_tbl <- dplyr::bind_rows(
        tibble::tibble(unit = treated_unit, att = att, pre_rmspe = main$pre,
                       post_rmspe = main$post,
                       rmspe_ratio = main$post / main$pre, included = TRUE),
        placebo_tbl
      )
    }
  }

  out <- tibble::tibble(
    treated_unit = treated_unit,
    att = att,
    att_pct = if (isTRUE(synth_post != 0)) att / synth_post else NA_real_,
    pre_rmspe = main$pre,
    post_rmspe = main$post,
    rmspe_ratio = main$post / main$pre,
    p.value = p_val,
    intervention_time = intervention_time,
    n_pre = length(pre),
    n_post = length(post),
    n_donors = length(pool),
    n_donors_used = sum(main$w > 0.001)
  )
  attr(out, "weights") <- wt
  attr(out, "path") <- path
  attr(out, "placebo") <- placebo_tbl
  out
}
