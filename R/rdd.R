# ============================================================
# REGRESSION DISCONTINUITY AND REGRESSION KINK DESIGNS
# ============================================================

#' Boundary Kernel Weights
#'
#' @param x Centred running variable.
#' @param h Bandwidth.
#' @param kernel Kernel name.
#'
#' @return Numeric weight vector, zero outside the bandwidth.
#' @noRd
.kk_rd_weights <- function(x, h, kernel) {
  u <- x / h
  inside <- abs(u) <= 1
  w <- switch(
    kernel,
    triangular = pmax(0, 1 - abs(u)),
    uniform = as.numeric(inside),
    epanechnikov = 0.75 * pmax(0, 1 - u^2)
  )
  w * inside
}

#' Imbens-Kalyanaraman Optimal Bandwidth
#'
#' @description The MSE-optimal bandwidth for a local linear regression
#'   discontinuity estimator, following the three-step algorithm of Imbens &
#'   Kalyanaraman (2012).
#'
#' @param x Running variable, already centred on the cutoff.
#' @param y Outcome.
#' @param kernel `"triangular"` or `"uniform"`.
#'
#' @return A single bandwidth.
#' @noRd
.kk_ik_bandwidth <- function(x, y, kernel = "triangular") {
  n <- length(x)
  ck <- if (kernel == "uniform") 5.40 else 3.4375

  # Step 1: a pilot window gives the density and the residual variance at the
  # cutoff.
  h1 <- 1.84 * stats::sd(x) * n^(-1 / 5)
  ip <- x >= 0 & x <= h1
  im <- x < 0 & x >= -h1
  np <- sum(ip)
  nm <- sum(im)
  if (np < 3 || nm < 3) {
    return(1.84 * stats::sd(x) * n^(-1 / 5))
  }
  f_hat <- (np + nm) / (2 * n * h1)
  sigma2 <- (sum((y[ip] - mean(y[ip]))^2) + sum((y[im] - mean(y[im]))^2)) /
    (np + nm)

  # Step 2: a global cubic gives the third derivative, which sets the pilot
  # bandwidths for the curvature estimates.
  above <- as.integer(x >= 0)
  cub <- tryCatch(
    stats::lm(y ~ above + x + I(x^2) + I(x^3)),
    error = function(e) NULL
  )
  m3 <- if (is.null(cub)) 0 else 6 * unname(stats::coef(cub)[["I(x^3)"]])
  if (!is.finite(m3)) m3 <- 0
  denom3 <- f_hat * max(m3^2, 0.01)
  n_pos <- sum(x >= 0)
  n_neg <- sum(x < 0)
  h2p <- 3.56 * (sigma2 / denom3)^(1 / 7) * n_pos^(-1 / 7)
  h2m <- 3.56 * (sigma2 / denom3)^(1 / 7) * n_neg^(-1 / 7)

  curv <- function(keep) {
    if (sum(keep) < 4) return(list(m2 = 0, n = sum(keep)))
    q <- tryCatch(
      stats::lm(y[keep] ~ x[keep] + I(x[keep]^2)),
      error = function(e) NULL
    )
    m2 <- if (is.null(q)) 0 else 2 * unname(stats::coef(q)[3])
    if (!is.finite(m2)) m2 <- 0
    list(m2 = m2, n = sum(keep))
  }
  cp <- curv(x >= 0 & x <= h2p)
  cm <- curv(x < 0 & x >= -h2m)

  # Step 3: regularisation keeps the bandwidth finite when the two curvatures
  # happen to coincide.
  rp <- if (cp$n > 0) 2160 * sigma2 / (cp$n * h2p^4) else 0
  rm <- if (cm$n > 0) 2160 * sigma2 / (cm$n * h2m^4) else 0
  h <- ck * (2 * sigma2 / (f_hat * ((cp$m2 - cm$m2)^2 + rp + rm)))^(1 / 5) *
    n^(-1 / 5)
  if (!is.finite(h) || h <= 0) h <- 1.84 * stats::sd(x) * n^(-1 / 5)
  h
}

#' Bin a Running Variable for Plotting
#'
#' @noRd
.kk_rd_bins <- function(x, y, h, bins) {
  keep <- abs(x) <= h
  if (!any(keep)) return(tibble::tibble())
  xs <- x[keep]
  ys <- y[keep]
  side <- ifelse(xs >= 0, "right", "left")
  out <- lapply(c("left", "right"), function(s) {
    k <- side == s
    if (sum(k) < 2) return(NULL)
    brk <- seq(min(xs[k]), max(xs[k]), length.out = bins + 1)
    idx <- cut(xs[k], breaks = brk, include.lowest = TRUE, labels = FALSE)
    tibble::tibble(
      side = s,
      x = as.numeric(tapply(xs[k], idx, mean)),
      y = as.numeric(tapply(ys[k], idx, mean)),
      n = as.integer(tapply(ys[k], idx, length))
    )
  })
  dplyr::bind_rows(out)
}

#' Regression Discontinuity (KK)
#'
#' @description Estimates the causal effect of a treatment assigned by a
#'   threshold rule: units just above the cutoff on a running variable get the
#'   treatment, units just below do not, and near the cutoff the two are
#'   otherwise comparable. The estimate is the jump in the outcome at the
#'   cutoff from a local polynomial regression fitted separately on each side
#'   with kernel weights.
#'
#'   In a **sharp** design treatment is a deterministic function of the running
#'   variable. Leave `treatment` `NULL` for that case. In a **fuzzy** design
#'   crossing the cutoff only changes the *probability* of treatment - pass the
#'   actual treatment column and the estimand becomes the local average
#'   treatment effect on compliers, obtained by two-stage least squares with
#'   the cutoff indicator as the instrument.
#'
#'   The estimate is local: it identifies the effect *at* the cutoff, for units
#'   near it, and says nothing about units far away. Two things should be
#'   checked before believing it - that units cannot precisely manipulate their
#'   position around the cutoff ([kk_rd_density()]) and that the estimate does
#'   not swing with the bandwidth (returned in `bw_sensitivity`).
#'
#' @param data Data frame.
#' @param outcome Outcome column (bare name or string).
#' @param running Running (forcing, assignment) variable column.
#' @param cutoff Threshold value of the running variable (default 0).
#' @param treatment Optional actual-treatment column for a fuzzy design.
#' @param bandwidth Bandwidth on the scale of the running variable. `NULL`
#'   (default) selects the MSE-optimal bandwidth of Imbens & Kalyanaraman
#'   (2012).
#' @param kernel Weighting kernel: `"triangular"` (default), `"uniform"` or
#'   `"epanechnikov"`.
#' @param degree Polynomial order fitted on each side (default 1, local
#'   linear). Orders above 2 are not recommended.
#' @param covariates Optional character vector of covariate names, entered
#'   linearly. Covariates should not be affected by the treatment.
#' @param vcov_type Heteroscedasticity-robust variance type (default `"HC1"`).
#' @param bins Number of bins per side used for the plotting data (default 20).
#' @param conf.level Confidence level (default 0.95).
#'
#' @return One-row tibble with the RD `estimate`, `std.error`, confidence
#'   interval, `p.value`, the `bandwidth` used and effective sample sizes on
#'   each side. Fuzzy designs add `first_stage` (the jump in treatment
#'   probability) and `reduced_form` (the jump in the outcome). Attributes:
#'   `binned` (binned means for plotting), `bw_sensitivity` (the estimate at
#'   0.5x to 2x the bandwidth), and the fitted model via [kk_model()].
#'
#' @references Imbens GW, Kalyanaraman K (2012) Optimal bandwidth choice for
#'   the regression discontinuity estimator. *Review of Economic Studies*
#'   79:933-959. Lee DS, Lemieux T (2010) Regression discontinuity designs in
#'   economics. *Journal of Economic Literature* 48:281-355.
#'
#' @seealso [kk_rd_density()], [kk_rkd()], [kk_iv()]
#'
#' @examples
#' set.seed(4)
#' n <- 1000
#' x <- runif(n, -1, 1)
#' y <- 1 + 0.8 * x + 0.5 * (x >= 0) + rnorm(n, 0, 0.3)
#' d <- data.frame(x = x, y = y)
#'
#' kk_rdd(d, y, x, cutoff = 0)
#'
#' @export
kk_rdd <- function(data, outcome, running, cutoff = 0, treatment = NULL,
                   bandwidth = NULL,
                   kernel = c("triangular", "uniform", "epanechnikov"),
                   degree = 1, covariates = NULL, vcov_type = "HC1",
                   bins = 20, conf.level = 0.95) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  validate_data_frame(data)
  kernel <- match.arg(kernel)
  degree <- as.integer(degree)
  if (degree < 1) stop("`degree` must be at least 1.", call. = FALSE)

  y_name <- .kk_colname(rlang::enquo(outcome))
  x_name <- .kk_colname(rlang::enquo(running))
  d_quo <- rlang::enquo(treatment)
  d_name <- if (rlang::quo_is_null(d_quo)) NULL else .kk_colname(d_quo)

  need <- c(y_name, x_name, d_name, covariates)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  data <- data[stats::complete.cases(data[, need, drop = FALSE]), , drop = FALSE]

  y <- as.numeric(data[[y_name]])
  x <- as.numeric(data[[x_name]]) - cutoff
  if (!any(x >= 0) || !any(x < 0)) {
    stop("The running variable has no observations on one side of the cutoff.",
         call. = FALSE)
  }
  above <- as.integer(x >= 0)
  d <- if (is.null(d_name)) NULL else .kk_as01(data[[d_name]], d_name)

  bw_src <- if (is.null(bandwidth)) "Imbens-Kalyanaraman" else "user"
  h <- if (is.null(bandwidth)) .kk_ik_bandwidth(x, y, kernel) else bandwidth
  if (!is.finite(h) || h <= 0) stop("Invalid bandwidth.", call. = FALSE)

  cov_mat <- if (!is.null(covariates)) {
    as.matrix(data[, covariates, drop = FALSE])
  } else {
    NULL
  }

  # Local polynomial fit, fully interacted so both sides get their own slopes.
  poly_mat <- function(xx) {
    m <- vapply(seq_len(degree), function(p) xx^p, numeric(length(xx)))
    matrix(m, nrow = length(xx),
           dimnames = list(NULL, paste0("x", seq_len(degree))))
  }

  fit_at <- function(hh) {
    w <- .kk_rd_weights(x, hh, kernel)
    keep <- w > 0
    if (sum(keep & above == 1) < degree + 2 ||
        sum(keep & above == 0) < degree + 2) {
      return(NULL)
    }
    P <- poly_mat(x[keep])
    A <- above[keep]
    base <- cbind(above = A, P, A * P)
    colnames(base) <- c("above", colnames(P), paste0("above_", colnames(P)))
    if (!is.null(cov_mat)) base <- cbind(base, cov_mat[keep, , drop = FALSE])
    df <- data.frame(.y = y[keep], base, check.names = TRUE)
    if (is.null(d)) {
      fit <- stats::lm(.y ~ ., data = df, weights = w[keep])
      list(fit = fit, keep = keep, w = w, df = df)
    } else {
      # Fuzzy: instrument the observed treatment with the cutoff indicator.
      X <- cbind(1, treat = d[keep], base[, -1, drop = FALSE])
      Z <- cbind(1, above = A, base[, -1, drop = FALSE])
      fit <- .kk_2sls_fit(y[keep], X, Z, weights = w[keep],
                          vcov_type = vcov_type)
      list(fit = fit, keep = keep, w = w, df = df)
    }
  }

  main <- fit_at(h)
  if (is.null(main)) {
    stop("Too few observations inside the bandwidth (", signif(h, 3),
         ") to fit a degree-", degree, " polynomial on both sides.",
         call. = FALSE)
  }

  extract <- function(res) {
    if (is.null(res)) return(c(NA_real_, NA_real_, NA_real_))
    if (is.null(d)) {
      cf <- stats::coef(res$fit)
      if (!"above" %in% names(cf) || is.na(cf[["above"]])) {
        return(c(NA_real_, NA_real_, NA_real_))
      }
      V <- tryCatch(sandwich::vcovHC(res$fit, type = vcov_type),
                    error = function(e) stats::vcov(res$fit))
      c(unname(cf[["above"]]), sqrt(V["above", "above"]),
        stats::df.residual(res$fit))
    } else {
      cf <- res$fit$coefficients
      c(unname(cf[["treat"]]), sqrt(res$fit$vcov["treat", "treat"]),
        res$fit$df.residual)
    }
  }

  ez <- extract(main)
  est <- ez[1]
  se <- ez[2]
  dfree <- ez[3]
  tcrit <- stats::qt(1 - (1 - conf.level) / 2, df = max(dfree, 1))

  n_left <- sum(main$w > 0 & above == 0)
  n_right <- sum(main$w > 0 & above == 1)

  out <- tibble::tibble(
    design = if (is.null(d)) "sharp" else "fuzzy",
    estimate = est,
    std.error = se,
    conf.low = est - tcrit * se,
    conf.high = est + tcrit * se,
    statistic = est / se,
    p.value = 2 * stats::pt(-abs(est / se), df = max(dfree, 1)),
    cutoff = cutoff,
    bandwidth = h,
    bandwidth_source = bw_src,
    kernel = kernel,
    degree = degree,
    n_left = n_left,
    n_right = n_right,
    conf.level = conf.level
  )

  if (!is.null(d)) {
    # First stage and reduced form, reported so the reader can see whether the
    # cutoff actually moved treatment take-up at all.
    wk <- main$w > 0
    P <- poly_mat(x[wk])
    A <- above[wk]
    base <- cbind(above = A, P, A * P)
    colnames(base) <- c("above", colnames(P), paste0("above_", colnames(P)))
    if (!is.null(cov_mat)) base <- cbind(base, cov_mat[wk, , drop = FALSE])
    fs <- stats::lm(d[wk] ~ ., data = data.frame(base, check.names = TRUE),
                    weights = main$w[wk])
    rf <- stats::lm(y[wk] ~ ., data = data.frame(base, check.names = TRUE),
                    weights = main$w[wk])
    out$first_stage <- unname(stats::coef(fs)[["above"]])
    out$reduced_form <- unname(stats::coef(rf)[["above"]])
    if (abs(out$first_stage) < 0.05) {
      warning("The treatment probability jumps by only ",
              signif(out$first_stage, 2),
              " at the cutoff; the fuzzy RD estimate is weakly identified.",
              call. = FALSE)
    }
  }

  mult <- c(0.5, 0.75, 1, 1.25, 1.5, 2)
  sens <- lapply(mult, function(m) {
    e <- extract(fit_at(h * m))
    tibble::tibble(multiplier = m, bandwidth = h * m, estimate = e[1],
                   std.error = e[2],
                   conf.low = e[1] - stats::qnorm(1 - (1 - conf.level) / 2) * e[2],
                   conf.high = e[1] + stats::qnorm(1 - (1 - conf.level) / 2) * e[2])
  })
  attr(out, "bw_sensitivity") <- dplyr::bind_rows(sens)
  attr(out, "binned") <- .kk_rd_bins(x, y, h, bins)
  .kk_attach_models(out, multivariable = main$fit, data = main$df)
}

#' Regression Kink Design (KK)
#'
#' @description Estimates a causal effect from a change in *slope* rather than
#'   a jump in level. Where a policy formula bends at a threshold - a benefit
#'   that is flat up to an income limit and then tapers, a fee that starts
#'   rising at a size cut-off - the kink in the assignment rule induces a kink
#'   in the outcome, and the ratio of the two slope changes identifies the
#'   effect at the threshold.
#'
#'   With `treatment` supplied the estimate is a fuzzy kink: the change in the
#'   outcome slope divided by the change in the treatment slope. Otherwise the
#'   returned estimate is the change in the outcome slope itself.
#'
#'   Kink designs are more demanding than discontinuity designs: slopes are
#'   estimated less precisely than levels, so wider bandwidths and larger
#'   samples are needed, and the estimate is more sensitive to the polynomial
#'   order. Report the bandwidth sensitivity table.
#'
#' @param data Data frame.
#' @param outcome Outcome column (bare name or string).
#' @param running Running variable column.
#' @param cutoff Kink point on the running variable (default 0).
#' @param treatment Optional policy/treatment variable for a fuzzy kink.
#' @param bandwidth Bandwidth. `NULL` (default) uses a rule-of-thumb
#'   `1.84 * sd(x) * n^(-1/7)`, the kink-appropriate rate; supplying your own
#'   bandwidth is strongly preferred.
#' @param kernel Weighting kernel: `"triangular"` (default), `"uniform"` or
#'   `"epanechnikov"`.
#' @param degree Local polynomial order (default 1).
#' @param covariates Optional character vector of covariate names.
#' @param vcov_type Robust variance type (default `"HC1"`).
#' @param conf.level Confidence level (default 0.95).
#'
#' @return One-row tibble with the kink `estimate` (slope change, or the ratio
#'   of slope changes when fuzzy), `std.error`, confidence interval, `p.value`,
#'   bandwidth and effective sample sizes. The bandwidth sensitivity table is
#'   attached as the attribute `bw_sensitivity`.
#'
#' @references Card D, Lee DS, Pei Z, Weber A (2015) Inference on causal
#'   effects in a generalized regression kink design. *Econometrica*
#'   83:2453-2483.
#'
#' @seealso [kk_rdd()]
#'
#' @examples
#' set.seed(5)
#' n <- 1500
#' x <- runif(n, -1, 1)
#' y <- 1 + 0.5 * x + 1.5 * pmax(0, x) + rnorm(n, 0, 0.25)
#' d <- data.frame(x = x, y = y)
#'
#' kk_rkd(d, y, x, bandwidth = 0.6)
#'
#' @export
kk_rkd <- function(data, outcome, running, cutoff = 0, treatment = NULL,
                   bandwidth = NULL,
                   kernel = c("triangular", "uniform", "epanechnikov"),
                   degree = 1, covariates = NULL, vcov_type = "HC1",
                   conf.level = 0.95) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  validate_data_frame(data)
  kernel <- match.arg(kernel)
  degree <- as.integer(degree)

  y_name <- .kk_colname(rlang::enquo(outcome))
  x_name <- .kk_colname(rlang::enquo(running))
  d_quo <- rlang::enquo(treatment)
  d_name <- if (rlang::quo_is_null(d_quo)) NULL else .kk_colname(d_quo)

  need <- c(y_name, x_name, d_name, covariates)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  data <- data[stats::complete.cases(data[, need, drop = FALSE]), , drop = FALSE]

  y <- as.numeric(data[[y_name]])
  x <- as.numeric(data[[x_name]]) - cutoff
  above <- as.integer(x >= 0)
  dvar <- if (is.null(d_name)) NULL else as.numeric(data[[d_name]])
  cov_mat <- if (!is.null(covariates)) {
    as.matrix(data[, covariates, drop = FALSE])
  } else {
    NULL
  }

  bw_src <- if (is.null(bandwidth)) "rule of thumb" else "user"
  h <- if (is.null(bandwidth)) {
    1.84 * stats::sd(x) * length(x)^(-1 / 7)
  } else {
    bandwidth
  }

  design_at <- function(hh) {
    w <- .kk_rd_weights(x, hh, kernel)
    keep <- w > 0
    if (sum(keep & above == 1) < degree + 2 ||
        sum(keep & above == 0) < degree + 2) {
      return(NULL)
    }
    P <- vapply(seq_len(degree), function(p) x[keep]^p, numeric(sum(keep)))
    P <- matrix(P, nrow = sum(keep),
                dimnames = list(NULL, paste0("x", seq_len(degree))))
    A <- above[keep]
    base <- cbind(above = A, P, A * P)
    colnames(base) <- c("above", colnames(P), paste0("kink_", colnames(P)))
    if (!is.null(cov_mat)) base <- cbind(base, cov_mat[keep, , drop = FALSE])
    list(base = base, keep = keep, w = w[keep])
  }

  slope_change <- function(hh, resp) {
    dz <- design_at(hh)
    if (is.null(dz)) return(list(est = NA_real_, se = NA_real_, df = NA_real_,
                                 fit = NULL, dz = NULL))
    df <- data.frame(.y = resp[dz$keep], dz$base, check.names = TRUE)
    fit <- stats::lm(.y ~ ., data = df, weights = dz$w)
    V <- tryCatch(sandwich::vcovHC(fit, type = vcov_type),
                  error = function(e) stats::vcov(fit))
    cf <- stats::coef(fit)
    if (!"kink_x1" %in% names(cf) || is.na(cf[["kink_x1"]])) {
      return(list(est = NA_real_, se = NA_real_, df = NA_real_, fit = fit,
                  dz = dz))
    }
    list(est = unname(cf[["kink_x1"]]), se = sqrt(V["kink_x1", "kink_x1"]),
         df = stats::df.residual(fit), fit = fit, dz = dz)
  }

  yk <- slope_change(h, y)
  if (is.null(yk$dz)) {
    stop("Too few observations inside the bandwidth (", signif(h, 3), ").",
         call. = FALSE)
  }

  if (is.null(dvar)) {
    est <- yk$est
    se <- yk$se
    fs <- NA_real_
  } else {
    dk <- slope_change(h, dvar)
    if (!is.finite(dk$est) || abs(dk$est) < 1e-8) {
      stop("The treatment slope does not change at the kink; the fuzzy kink ",
           "estimate is not identified.", call. = FALSE)
    }
    est <- yk$est / dk$est
    # Delta method for the ratio, treating numerator and denominator as
    # independent: the two fits share the same design, so this is conservative
    # only when their errors are negatively correlated.
    se <- sqrt((yk$se / dk$est)^2 + (yk$est^2 * dk$se^2) / dk$est^4)
    fs <- dk$est
  }
  dfree <- max(yk$df, 1)
  tcrit <- stats::qt(1 - (1 - conf.level) / 2, df = dfree)

  out <- tibble::tibble(
    design = if (is.null(dvar)) "sharp kink" else "fuzzy kink",
    estimate = est,
    std.error = se,
    conf.low = est - tcrit * se,
    conf.high = est + tcrit * se,
    statistic = est / se,
    p.value = 2 * stats::pt(-abs(est / se), df = dfree),
    cutoff = cutoff,
    bandwidth = h,
    bandwidth_source = bw_src,
    kernel = kernel,
    degree = degree,
    first_stage_slope = fs,
    n_left = sum(yk$dz$w > 0 & above[yk$dz$keep] == 0),
    n_right = sum(yk$dz$w > 0 & above[yk$dz$keep] == 1),
    conf.level = conf.level
  )

  mult <- c(0.5, 0.75, 1, 1.25, 1.5, 2)
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  sens <- lapply(mult, function(m) {
    k <- slope_change(h * m, y)
    e <- if (is.null(dvar)) {
      c(k$est, k$se)
    } else {
      dk <- slope_change(h * m, dvar)
      if (!is.finite(dk$est) || abs(dk$est) < 1e-8) {
        c(NA_real_, NA_real_)
      } else {
        c(k$est / dk$est,
          sqrt((k$se / dk$est)^2 + (k$est^2 * dk$se^2) / dk$est^4))
      }
    }
    tibble::tibble(multiplier = m, bandwidth = h * m, estimate = e[1],
                   std.error = e[2], conf.low = e[1] - z * e[2],
                   conf.high = e[1] + z * e[2])
  })
  attr(out, "bw_sensitivity") <- dplyr::bind_rows(sens)
  .kk_attach_models(out, multivariable = yk$fit, data = NULL)
}

#' Manipulation Test for a Regression Discontinuity (KK)
#'
#' @description Tests whether the density of the running variable jumps at the
#'   cutoff. A discontinuity design assumes units cannot precisely control
#'   which side of the threshold they land on; if they can - a clinician
#'   nudging a score to secure treatment, a firm reporting just under a size
#'   limit - the units on either side are no longer comparable and the design
#'   fails. A pile-up on one side shows as a jump in the density.
#'
#'   This is the McCrary (2008) test: bin the running variable, then fit a
#'   local linear regression of the binned density on each side of the cutoff
#'   and test the log difference of the two intercepts.
#'
#'   Severe manipulation empties the bins just below the cutoff, which can push
#'   the extrapolated intercept on that side below zero. Rather than take the
#'   log of a negative number the function then falls back to the
#'   kernel-weighted mean density on that side and warns; the fallback is
#'   conservative, understating the jump it is reporting.
#'
#' @param data Data frame.
#' @param running Running variable column (bare name or string).
#' @param cutoff Threshold value (default 0).
#' @param bin_width Histogram bin width. `NULL` (default) uses
#'   `2 * sd(x) * n^(-1/2)`.
#' @param bandwidth Smoothing bandwidth for the local linear density fit.
#'   `NULL` (default) uses `2 * sd(x) * n^(-1/5)`.
#' @param conf.level Confidence level (default 0.95).
#'
#' @return One-row tibble with the log density discontinuity `estimate`, its
#'   `std.error`, confidence interval and `p.value`, plus the estimated
#'   densities on each side. A non-significant result is what the design needs.
#'   The binned density is attached as the attribute `density`.
#'
#' @references McCrary J (2008) Manipulation of the running variable in the
#'   regression discontinuity design: a density test. *Journal of Econometrics*
#'   142:698-714.
#'
#' @seealso [kk_rdd()]
#'
#' @examples
#' set.seed(6)
#' x <- rnorm(2000)
#' kk_rd_density(data.frame(x = x), x, cutoff = 0)
#'
#' @export
kk_rd_density <- function(data, running, cutoff = 0, bin_width = NULL,
                          bandwidth = NULL, conf.level = 0.95) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  validate_data_frame(data)
  x_name <- .kk_colname(rlang::enquo(running))
  if (!x_name %in% names(data)) {
    stop("Column not found in data: ", x_name, call. = FALSE)
  }
  x <- as.numeric(data[[x_name]])
  x <- x[!is.na(x)] - cutoff
  n <- length(x)
  if (n < 50) stop("Too few observations for a density test.", call. = FALSE)

  b <- if (is.null(bin_width)) 2 * stats::sd(x) * n^(-1 / 2) else bin_width
  h <- if (is.null(bandwidth)) 2 * stats::sd(x) * n^(-1 / 5) else bandwidth

  # Bins are aligned so that no bin straddles the cutoff: the whole point is to
  # compare the two sides without smoothing across the boundary.
  brk_r <- seq(0, max(x) + b, by = b)
  brk_l <- rev(seq(0, min(x) - b, by = -b))
  brks <- unique(c(brk_l, brk_r))
  idx <- .bincode(x, brks, right = TRUE, include.lowest = TRUE)
  counts <- tabulate(idx, nbins = length(brks) - 1L)
  mid <- (brks[-1] + brks[-length(brks)]) / 2
  dens <- counts / (n * b)

  side_fit <- function(pick) {
    keep <- pick & abs(mid) <= h
    if (sum(keep) < 3) return(NA_real_)
    xm <- mid[keep]
    w <- pmax(0, 1 - abs(xm) / h)
    f <- stats::lm(dens[keep] ~ xm, weights = w)
    est <- unname(stats::coef(f)[1])
    if (!is.finite(est) || est <= 0) {
      # A hole immediately beside the cutoff - which is exactly what heavy
      # manipulation looks like - can drive the extrapolated intercept
      # negative. Fall back to the kernel-weighted mean density, which cannot
      # go below zero, rather than returning a log of a negative number.
      est <- stats::weighted.mean(dens[keep], w)
      warning("The local linear density fit was not positive on one side; ",
              "using the kernel-weighted mean density there instead.",
              call. = FALSE)
    }
    est
  }
  f_right <- side_fit(mid > 0)
  f_left <- side_fit(mid < 0)
  if (!is.finite(f_right) || !is.finite(f_left) ||
      f_right <= 0 || f_left <= 0) {
    stop("The density estimate is not positive on both sides of the cutoff; ",
         "widen `bandwidth` or `bin_width`.", call. = FALSE)
  }

  theta <- log(f_right) - log(f_left)
  se <- sqrt((24 / (5 * n * h)) * (1 / f_right + 1 / f_left))
  z <- stats::qnorm(1 - (1 - conf.level) / 2)

  out <- tibble::tibble(
    test = "McCrary density discontinuity",
    estimate = theta,
    std.error = se,
    conf.low = theta - z * se,
    conf.high = theta + z * se,
    statistic = theta / se,
    p.value = 2 * stats::pnorm(-abs(theta / se)),
    density_left = f_left,
    density_right = f_right,
    cutoff = cutoff,
    bin_width = b,
    bandwidth = h,
    n = n,
    conf.level = conf.level
  )
  attr(out, "density") <- tibble::tibble(
    x = mid + cutoff, density = dens, count = counts,
    side = ifelse(mid > 0, "right", "left")
  )
  out
}
