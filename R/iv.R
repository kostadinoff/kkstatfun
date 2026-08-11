# ============================================================
# INSTRUMENTAL VARIABLES (2SLS) AND PRE-POST ANCOVA
# ============================================================

#' Two-Stage Least Squares Core
#'
#' @description Bare-matrix 2SLS with robust or cluster-robust variance. The
#'   projection is done once and reused, so the RD, kink and IV entry points
#'   all share the same estimator.
#'
#' @param y Outcome vector.
#' @param X Regressor matrix, including the intercept column and the
#'   endogenous regressors.
#' @param Z Instrument matrix, including the intercept and every exogenous
#'   regressor from `X`.
#' @param weights Optional observation weights.
#' @param cluster Optional cluster vector.
#' @param vcov_type `"HC1"` (default), `"HC0"` or `"const"`.
#'
#' @return List with `coefficients`, `vcov`, `residuals`, `df.residual`, `n`
#'   and the projected regressors `Xhat`.
#' @noRd
.kk_2sls_fit <- function(y, X, Z, weights = NULL, cluster = NULL,
                         vcov_type = "HC1") {
  X <- as.matrix(X)
  Z <- as.matrix(Z)
  if (is.null(colnames(X))) colnames(X) <- paste0("x", seq_len(ncol(X)))
  if (ncol(Z) < ncol(X)) {
    stop("The model is under-identified: ", ncol(Z), " instruments for ",
         ncol(X), " regressors.", call. = FALSE)
  }
  if (!is.null(weights)) {
    sw <- sqrt(weights)
    y <- y * sw
    X <- X * sw
    Z <- Z * sw
  }
  n <- length(y)
  k <- ncol(X)

  inv <- function(m) {
    tryCatch(solve(m), error = function(e) MASS::ginv(m))
  }
  # First stage as a projection: Xhat = Z (Z'Z)^-1 Z'X.
  zz <- crossprod(Z)
  Xhat <- Z %*% (inv(zz) %*% crossprod(Z, X))
  colnames(Xhat) <- colnames(X)
  bread <- inv(crossprod(Xhat))
  b <- drop(bread %*% crossprod(Xhat, y))
  names(b) <- colnames(X)
  u <- drop(y - X %*% b)

  if (identical(vcov_type, "const")) {
    s2 <- sum(u^2) / max(n - k, 1)
    V <- s2 * bread
  } else if (is.null(cluster)) {
    meat <- crossprod(Xhat * u)
    scale <- if (identical(vcov_type, "HC1")) n / max(n - k, 1) else 1
    V <- scale * (bread %*% meat %*% bread)
  } else {
    g <- factor(cluster)
    sums <- rowsum(Xhat * u, g)
    meat <- crossprod(sums)
    ng <- nlevels(g)
    scale <- (ng / max(ng - 1, 1)) * ((n - 1) / max(n - k, 1))
    V <- scale * (bread %*% meat %*% bread)
  }
  dimnames(V) <- list(colnames(X), colnames(X))
  list(coefficients = b, vcov = V, residuals = u,
       df.residual = max(n - k, 1), n = n, Xhat = Xhat)
}

#' Robust Wald Test of a Set of Coefficients
#'
#' @noRd
.kk_wald <- function(b, V, which) {
  which <- which[which %in% names(b)]
  if (length(which) == 0) return(list(stat = NA_real_, df = 0L, p = NA_real_))
  bb <- b[which]
  vv <- V[which, which, drop = FALSE]
  w <- tryCatch(as.numeric(t(bb) %*% solve(vv) %*% bb),
                error = function(e) NA_real_)
  list(stat = w / length(which), df = length(which),
       p = stats::pchisq(w, df = length(which), lower.tail = FALSE))
}

#' Instrumental-Variable Regression by Two-Stage Least Squares (KK)
#'
#' @description Estimates the effect of a treatment that is not randomly
#'   assigned, using an instrument: a variable that moves treatment but affects
#'   the outcome through no other path. Under one-sided non-compliance and
#'   monotonicity the estimand is the local average treatment effect - the
#'   effect among the compliers, those whose treatment status the instrument
#'   actually changes - not the population average.
#'
#'   Three assumptions carry the design, and only the first is testable:
#'   relevance (the instrument predicts treatment - see the first-stage F),
#'   exclusion (the instrument affects the outcome only through treatment) and
#'   independence. A weak instrument makes 2SLS badly biased towards the
#'   ordinary least squares estimate and its confidence interval far too
#'   narrow, which is why the weak-instrument-robust Anderson-Rubin interval is
#'   reported alongside the conventional one.
#'
#' @param data Data frame.
#' @param outcome Outcome column (bare name or string).
#' @param treatment Endogenous treatment/exposure column.
#' @param instruments Character vector of instrument column names. More than
#'   one over-identifies the model and enables the Sargan test.
#' @param covariates Optional character vector of exogenous covariates,
#'   included in both stages.
#' @param cluster Optional clustering column for the standard errors.
#' @param vcov_type `"HC1"` (default), `"HC0"` or `"const"`.
#' @param ar_ci Whether to compute the Anderson-Rubin weak-instrument-robust
#'   confidence interval (default `TRUE`).
#' @param conf.level Confidence level (default 0.95).
#'
#' @return One-row tibble with the 2SLS `estimate` for the treatment, its
#'   `std.error`, confidence interval and `p.value`, the `ols` estimate for
#'   comparison, the `first_stage_f`, and the Anderson-Rubin interval
#'   (`ar.low`, `ar.high`). Attributes: `first_stage` (the full first-stage
#'   coefficients), `diagnostics` (first-stage F, Sargan over-identification
#'   test, Wu-Hausman endogeneity test).
#'
#' @references Angrist JD, Imbens GW, Rubin DB (1996) Identification of causal
#'   effects using instrumental variables. *JASA* 91:444-455. Stock JH, Yogo M
#'   (2005) Testing for weak instruments in linear IV regression.
#'
#' @seealso [kk_rdd()], [kk_tmle()], [kk_iptw()]
#'
#' @examples
#' set.seed(7)
#' n <- 1200
#' z <- rbinom(n, 1, 0.5)            # randomised encouragement
#' u <- rnorm(n)                     # unmeasured confounder
#' d <- rbinom(n, 1, plogis(-0.5 + 1.5 * z + u))
#' y <- 1 + 2 * d + u + rnorm(n)
#' df <- data.frame(y = y, d = d, z = z)
#'
#' kk_iv(df, y, d, instruments = "z")
#'
#' @export
kk_iv <- function(data, outcome, treatment, instruments, covariates = NULL,
                  cluster = NULL, vcov_type = "HC1", ar_ci = TRUE,
                  conf.level = 0.95) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  validate_data_frame(data)
  if (!is.character(instruments) || length(instruments) < 1) {
    stop("`instruments` must be a non-empty character vector of column names.",
         call. = FALSE)
  }

  y_name <- .kk_colname(rlang::enquo(outcome))
  d_name <- .kk_colname(rlang::enquo(treatment))
  cl_quo <- rlang::enquo(cluster)
  cl_name <- if (rlang::quo_is_null(cl_quo)) NULL else .kk_colname(cl_quo)

  need <- c(y_name, d_name, instruments, covariates, cl_name)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  data <- data[stats::complete.cases(data[, need, drop = FALSE]), , drop = FALSE]

  y <- as.numeric(data[[y_name]])
  dvar <- as.numeric(data[[d_name]])
  Zex <- stats::model.matrix(
    stats::as.formula(paste("~", paste(instruments, collapse = " + "))),
    data = data
  )[, -1, drop = FALSE]
  W <- if (!is.null(covariates)) {
    stats::model.matrix(
      stats::as.formula(paste("~", paste(covariates, collapse = " + "))),
      data = data
    )[, -1, drop = FALSE]
  } else {
    NULL
  }

  X <- cbind(`(Intercept)` = 1, treatment = dvar)
  Zm <- cbind(`(Intercept)` = 1, Zex)
  if (!is.null(W)) {
    X <- cbind(X, W)
    Zm <- cbind(Zm, W)
  }
  cl_vec <- if (!is.null(cl_name)) data[[cl_name]] else NULL

  fit <- .kk_2sls_fit(y, X, Zm, cluster = cl_vec, vcov_type = vcov_type)
  est <- unname(fit$coefficients[["treatment"]])
  se <- sqrt(fit$vcov["treatment", "treatment"])
  z <- stats::qnorm(1 - (1 - conf.level) / 2)

  # First stage, with the robust F on the excluded instruments: below ~10 the
  # instrument is weak and the 2SLS interval understates the uncertainty.
  fs_df <- data.frame(.d = dvar, Zex, check.names = TRUE)
  if (!is.null(W)) fs_df <- cbind(fs_df, as.data.frame(W))
  fs <- stats::lm(.d ~ ., data = fs_df)
  fs_v <- if (is.null(cl_vec)) {
    tryCatch(sandwich::vcovHC(fs, type = "HC1"),
             error = function(e) stats::vcov(fs))
  } else {
    tryCatch(sandwich::vcovCL(fs, cluster = cl_vec, type = "HC1"),
             error = function(e) stats::vcov(fs))
  }
  inst_terms <- intersect(colnames(fs_df), names(stats::coef(fs)))
  inst_terms <- setdiff(inst_terms, ".d")
  inst_terms <- inst_terms[seq_len(ncol(Zex))]
  wald <- .kk_wald(stats::coef(fs), fs_v, inst_terms)

  ols_df <- data.frame(y = y, treatment = dvar)
  if (!is.null(W)) ols_df <- cbind(ols_df, as.data.frame(W))
  ols_fit <- stats::lm(y ~ ., data = ols_df)
  ols <- unname(stats::coef(ols_fit)[["treatment"]])

  # Sargan over-identification test: with more instruments than endogenous
  # regressors, the exclusion restriction becomes partially testable.
  sargan <- if (ncol(Zex) > 1) {
    aux <- stats::lm(fit$residuals ~ Zm[, -1, drop = FALSE])
    r2 <- summary(aux)$r.squared
    j <- length(y) * r2
    dfj <- ncol(Zex) - 1
    tibble::tibble(test = "Sargan over-identification", statistic = j,
                   df = dfj, p.value = stats::pchisq(j, dfj, lower.tail = FALSE))
  } else {
    tibble::tibble(test = "Sargan over-identification", statistic = NA_real_,
                   df = 0L, p.value = NA_real_)
  }

  # Wu-Hausman via the control function: if the first-stage residual predicts
  # the outcome, treatment is endogenous and OLS is inconsistent.
  cf_df <- data.frame(y = y, treatment = dvar, .v = stats::resid(fs))
  if (!is.null(W)) cf_df <- cbind(cf_df, as.data.frame(W))
  cf_fit <- stats::lm(y ~ ., data = cf_df)
  cf_sum <- summary(cf_fit)$coefficients
  hausman <- tibble::tibble(
    test = "Wu-Hausman endogeneity",
    statistic = if (".v" %in% rownames(cf_sum)) cf_sum[".v", "t value"] else NA_real_,
    df = stats::df.residual(cf_fit),
    p.value = if (".v" %in% rownames(cf_sum)) cf_sum[".v", "Pr(>|t|)"] else NA_real_
  )

  # Anderson-Rubin interval: valid whatever the instrument strength, because it
  # never divides by the first stage.
  ar <- c(NA_real_, NA_real_)
  if (isTRUE(ar_ci) && is.finite(se) && se > 0) {
    grid <- seq(est - 10 * se, est + 10 * se, length.out = 1001)
    accept <- vapply(grid, function(b0) {
      e <- y - dvar * b0
      ad <- data.frame(.e = e, Zex, check.names = TRUE)
      if (!is.null(W)) ad <- cbind(ad, as.data.frame(W))
      af <- stats::lm(.e ~ ., data = ad)
      av <- if (is.null(cl_vec)) {
        tryCatch(sandwich::vcovHC(af, type = "HC1"),
                 error = function(e2) stats::vcov(af))
      } else {
        tryCatch(sandwich::vcovCL(af, cluster = cl_vec, type = "HC1"),
                 error = function(e2) stats::vcov(af))
      }
      w <- .kk_wald(stats::coef(af), av, inst_terms)
      isTRUE(w$p > 1 - conf.level)
    }, logical(1))
    if (any(accept)) {
      ar <- range(grid[accept])
      if (accept[1]) ar[1] <- -Inf
      if (accept[length(accept)]) ar[2] <- Inf
    }
  }

  out <- tibble::tibble(
    term = d_name,
    estimate = est,
    std.error = se,
    conf.low = est - z * se,
    conf.high = est + z * se,
    statistic = est / se,
    p.value = 2 * stats::pnorm(-abs(est / se)),
    ols = ols,
    first_stage_f = wald$stat,
    weak_instrument = isTRUE(wald$stat < 10),
    ar.low = ar[1],
    ar.high = ar[2],
    n = length(y),
    n_instruments = ncol(Zex),
    conf.level = conf.level
  )
  if (isTRUE(out$weak_instrument)) {
    warning("First-stage F = ", signif(wald$stat, 3),
            " (< 10): the instrument is weak, so prefer the Anderson-Rubin ",
            "interval to the conventional one.", call. = FALSE)
  }
  fs_coef <- stats::coef(fs)
  attr(out, "first_stage") <- tibble::tibble(
    term = names(fs_coef),
    estimate = unname(fs_coef),
    std.error = sqrt(diag(fs_v))[names(fs_coef)]
  )
  attr(out, "diagnostics") <- dplyr::bind_rows(
    tibble::tibble(test = "First-stage F (excluded instruments)",
                   statistic = wald$stat, df = wald$df, p.value = wald$p),
    sargan, hausman
  )
  .kk_attach_models(out, multivariable = fs, data = data)
}

#' Pre-Post ANCOVA (KK)
#'
#' @description Estimates a treatment effect from a pretest-posttest design by
#'   regressing the follow-up outcome on the treatment and the baseline value
#'   of the same measure. ANCOVA is the efficient choice here: it beats both
#'   the naive comparison of follow-up means (which ignores baseline imbalance)
#'   and the comparison of change scores (which over-corrects whenever the
#'   pre-post correlation is below one), and it is robust to regression to the
#'   mean. All three estimates are returned so the difference is visible.
#'
#'   In a randomised trial baseline adjustment is a precision gain, not a bias
#'   correction. In an observational pre-post comparison ANCOVA only removes
#'   confounding by the baseline value itself - not by anything else.
#'
#' @param data Data frame, one row per participant.
#' @param outcome Follow-up outcome column (bare name or string).
#' @param treatment Treatment/group column; two levels.
#' @param baseline Baseline measurement of the same outcome.
#' @param covariates Optional character vector of further covariate names.
#' @param vcov_type Robust variance type (default `"HC3"`), or `NULL` for the
#'   model-based variance.
#' @param conf.level Confidence level (default 0.95).
#'
#' @return One-row tibble with the ANCOVA-adjusted treatment `estimate`, its
#'   `std.error`, confidence interval and `p.value`; the adjusted group means
#'   `ey1` and `ey0` (standardised over the observed baseline distribution);
#'   and the `change_score` and `posttest_only` estimates for comparison.
#'
#' @references Vickers AJ, Altman DG (2001) Analysing controlled trials with
#'   baseline and follow up measurements. *BMJ* 323:1123-1124.
#'
#' @seealso [kk_did()], [kk_reg()]
#'
#' @examples
#' set.seed(8)
#' n <- 200
#' base <- rnorm(n, 140, 15)
#' arm <- rbinom(n, 1, 0.5)
#' post <- 0.6 * base + 56 - 8 * arm + rnorm(n, 0, 8)
#' d <- data.frame(base = base, arm = arm, post = post)
#'
#' kk_ancova(d, post, arm, base)
#'
#' @export
kk_ancova <- function(data, outcome, treatment, baseline, covariates = NULL,
                      vcov_type = "HC3", conf.level = 0.95) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }
  validate_data_frame(data)

  y_name <- .kk_colname(rlang::enquo(outcome))
  t_name <- .kk_colname(rlang::enquo(treatment))
  b_name <- .kk_colname(rlang::enquo(baseline))

  need <- c(y_name, t_name, b_name, covariates)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  data <- data[stats::complete.cases(data[, need, drop = FALSE]), , drop = FALSE]

  y <- as.numeric(data[[y_name]])
  b <- as.numeric(data[[b_name]])
  tr <- .kk_as01(data[[t_name]], t_name)

  df <- data.frame(.y = y, .treat = tr, .base = b)
  if (!is.null(covariates)) df[covariates] <- data[covariates]
  rhs <- c(".treat", ".base", covariates)
  fit <- stats::lm(stats::as.formula(paste(".y ~", paste(rhs, collapse = " + "))),
                   data = df)
  V <- if (is.null(vcov_type)) {
    stats::vcov(fit)
  } else {
    tryCatch(sandwich::vcovHC(fit, type = vcov_type),
             error = function(e) stats::vcov(fit))
  }
  est <- unname(stats::coef(fit)[[".treat"]])
  se <- sqrt(V[".treat", ".treat"])
  dfree <- stats::df.residual(fit)
  tcrit <- stats::qt(1 - (1 - conf.level) / 2, df = dfree)

  # Adjusted means: predict everybody as treated, then as untreated, over the
  # observed baseline distribution.
  nd1 <- df; nd1$.treat <- 1
  nd0 <- df; nd0$.treat <- 0
  ey1 <- mean(stats::predict(fit, newdata = nd1))
  ey0 <- mean(stats::predict(fit, newdata = nd0))

  chg <- stats::lm((y - b) ~ tr)
  post_only <- stats::lm(y ~ tr)

  out <- tibble::tibble(
    estimate = est,
    std.error = se,
    conf.low = est - tcrit * se,
    conf.high = est + tcrit * se,
    statistic = est / se,
    p.value = 2 * stats::pt(-abs(est / se), df = dfree),
    ey1 = ey1,
    ey0 = ey0,
    change_score = unname(stats::coef(chg)[2]),
    posttest_only = unname(stats::coef(post_only)[2]),
    baseline_correlation = stats::cor(y, b),
    n = nrow(df),
    n_treated = sum(tr == 1),
    conf.level = conf.level
  )
  .kk_attach_models(out, multivariable = fit, data = df)
}
