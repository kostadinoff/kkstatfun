# ============================================================
# TARGETED MAXIMUM LIKELIHOOD ESTIMATION (TMLE)
# Single-time-point ATE / risk difference (double-robust)
# ============================================================

#' Targeted Maximum Likelihood Estimation of the ATE (KK)
#'
#' @description Double-robust, locally efficient estimation of the average
#'   treatment effect (ATE) - the risk difference for a binary outcome or the
#'   mean difference for a continuous outcome - for a single-time-point binary
#'   exposure. TMLE combines an outcome regression (the "Q" model) with the
#'   treatment/propensity model (the "g" model) and performs a targeting
#'   fluctuation step so that the estimator solves the efficient influence-curve
#'   equation. The result is consistent if *either* the outcome model or the
#'   propensity model is correct (double robustness), with valid
#'   influence-curve-based inference.
#'
#'   This implements the classic parametric TMLE with `glm` working models. For
#'   ensemble/Super-Learner nuisance estimation or longitudinal/survival data,
#'   use the dedicated \pkg{tmle} / \pkg{ltmle} packages; this function is the
#'   transparent, dependency-free core for the point-treatment ATE.
#'
#' @param data Data frame.
#' @param outcome Outcome column (bare name or string); binary (0/1) or
#'   continuous.
#' @param treatment Binary treatment column (bare name or string); coerced so
#'   its higher level / `1` / `TRUE` is "treated".
#' @param covariates Character vector of confounder column names.
#' @param g_bounds Truncation bounds for the estimated propensity score to
#'   protect against positivity violations (default `c(0.025, 0.975)`).
#' @param conf.level Confidence level (default 0.95).
#'
#' @return One-row tibble with the targeted `ate`, its influence-curve
#'   `std.error`, confidence interval, `p.value`, and the counterfactual mean
#'   outcomes `ey1` and `ey0`. The naive (untargeted G-computation) estimate is
#'   attached as the attribute `gcomp`.
#'
#' @examples
#' set.seed(1)
#' n <- 800
#' w1 <- rnorm(n); w2 <- rbinom(n, 1, 0.5)
#' a  <- rbinom(n, 1, plogis(-0.5 + 0.8 * w1 + 0.5 * w2))  # confounded
#' y  <- rbinom(n, 1, plogis(-1 + 0.6 * a + 1.0 * w1 + 0.5 * w2))
#' df <- data.frame(y, a, w1, w2)
#' kk_tmle(df, y, a, c("w1", "w2"))
#'
#' @export
kk_tmle <- function(data, outcome, treatment, covariates,
                    g_bounds = c(0.025, 0.975), conf.level = 0.95) {
  validate_data_frame(data)
  y_name <- .kk_colname(rlang::enquo(outcome))
  a_name <- .kk_colname(rlang::enquo(treatment))
  if (!is.character(covariates) || length(covariates) < 1) {
    stop("`covariates` must be a non-empty character vector.")
  }
  need <- c(y_name, a_name, covariates)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }
  data <- data[stats::complete.cases(data[, need, drop = FALSE]), , drop = FALSE]

  y <- data[[y_name]]
  a_raw <- data[[a_name]]
  if (is.logical(a_raw)) {
    a <- as.integer(a_raw)
  } else {
    lv <- sort(unique(a_raw))
    if (length(lv) != 2) stop("`treatment` must have exactly two levels.")
    a <- as.integer(a_raw == lv[2])
  }
  n <- length(y)

  # Scale continuous outcome to [0, 1] (Gruber & van der Laan); binary unchanged
  binary <- all(y %in% c(0, 1))
  y_min <- min(y)
  y_max <- max(y)
  span <- if (binary) 1 else (y_max - y_min)
  ystar <- if (binary) y else (y - y_min) / span

  W <- data[, covariates, drop = FALSE]

  # ---- Q model: E[Y* | A, W] via quasibinomial (bounded outcome) ----
  q_data <- data.frame(ystar = ystar, A = a, W)
  q_form <- stats::as.formula(paste("ystar ~ A +", paste(covariates, collapse = " + ")))
  q_fit <- stats::glm(q_form, data = q_data, family = stats::quasibinomial())

  pred_q <- function(aval) {
    nd <- q_data
    nd$A <- aval
    stats::predict(q_fit, newdata = nd, type = "response")
  }
  QAW <- stats::predict(q_fit, type = "response")
  Q1W <- pred_q(1)
  Q0W <- pred_q(0)
  # Numeric guard for the logit transform
  bound01 <- function(p) pmin(pmax(p, 1e-8), 1 - 1e-8)
  QAW <- bound01(QAW); Q1W <- bound01(Q1W); Q0W <- bound01(Q0W)

  # ---- g model: P(A = 1 | W) ----
  g_form <- stats::as.formula(paste("A ~", paste(covariates, collapse = " + ")))
  g_fit <- stats::glm(g_form, data = q_data, family = stats::binomial())
  g1 <- stats::predict(g_fit, type = "response")
  g1 <- pmin(pmax(g1, g_bounds[1]), g_bounds[2])
  g0 <- 1 - g1

  # ---- Clever covariate & targeting fluctuation ----
  H1 <- 1 / g1
  H0 <- -1 / g0
  HA <- ifelse(a == 1, H1, H0)

  eps <- stats::coef(stats::glm(
    ystar ~ -1 + HA, offset = stats::qlogis(QAW),
    family = stats::quasibinomial()
  ))
  if (is.na(eps)) eps <- 0

  Q1star <- stats::plogis(stats::qlogis(Q1W) + eps * H1)
  Q0star <- stats::plogis(stats::qlogis(Q0W) + eps * H0)
  QAWstar <- stats::plogis(stats::qlogis(QAW) + eps * HA)

  # ---- Targeted estimate (rescaled to the original outcome scale) ----
  psi01 <- mean(Q1star - Q0star)
  ate <- psi01 * span

  # ---- Efficient influence curve & inference ----
  eic <- (HA * (ystar - QAWstar) + (Q1star - Q0star) - psi01) * span
  se <- stats::sd(eic) / sqrt(n)
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  p_val <- 2 * stats::pnorm(-abs(ate / se))

  gcomp <- mean(Q1W - Q0W) * span

  out <- tibble::tibble(
    ate = ate,
    std.error = se,
    conf.low = ate - z * se,
    conf.high = ate + z * se,
    p.value = p_val,
    ey1 = mean(Q1star) * span + if (binary) 0 else y_min,
    ey0 = mean(Q0star) * span + if (binary) 0 else y_min,
    conf.level = conf.level
  )
  attr(out, "gcomp") <- gcomp
  out
}
