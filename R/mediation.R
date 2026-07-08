# ============================================================
# CAUSAL MEDIATION ANALYSIS
# ============================================================

#' Causal Mediation Analysis (KK)
#'
#' @description Decomposes the total effect of an exposure on an outcome into a
#'   natural direct effect (NDE, not operating through the mediator) and a natural
#'   indirect effect (NIE, operating through the mediator), using the
#'   regression-based product method (Baron-Kenny / VanderWeele). A mediator model
#'   and an outcome model are fitted; effects are formed from their coefficients
#'   and confidence intervals are obtained by nonparametric bootstrap.
#'
#'   This implementation assumes a **continuous mediator** and **no
#'   exposure-mediator interaction**. For a binary outcome the effects are
#'   reported on the odds-ratio scale under the rare-outcome approximation; for a
#'   continuous outcome they are mean differences. Valid causal interpretation
#'   further requires no unmeasured exposure-outcome, mediator-outcome, or
#'   exposure-mediator confounding (adjust for these via `confounders`).
#'
#' @param data Data frame.
#' @param exposure Exposure column (bare name or string).
#' @param mediator Continuous mediator column (bare name or string).
#' @param outcome Outcome column (bare name or string).
#' @param confounders Optional character vector of confounder columns entered in
#'   both models.
#' @param mediator_family Family for the (linear) mediator model, default
#'   `"gaussian"`.
#' @param outcome_family Family for the outcome model: `"binomial"` (default,
#'   odds-ratio scale) or `"gaussian"` (mean-difference scale).
#' @param boot_reps Number of bootstrap replications for confidence intervals
#'   (default 1000).
#' @param conf.level Confidence level (default 0.95).
#'
#' @return Tibble with the Total, Direct (NDE), Indirect (NIE) effects and the
#'   Proportion Mediated, each with a bootstrap confidence interval, plus the
#'   scale of the effect.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' n <- 500
#' x <- rbinom(n, 1, 0.5)
#' m <- 0.5 * x + rnorm(n)                       # exposure -> mediator
#' y <- rbinom(n, 1, plogis(-1 + 0.3 * x + 0.6 * m))  # both paths
#' df <- data.frame(x = x, m = m, y = y)
#' kk_causal_mediation(df, x, m, y, boot_reps = 500)
#' }
#'
#' @export
kk_causal_mediation <- function(data, exposure, mediator, outcome,
                                confounders = NULL,
                                mediator_family = "gaussian",
                                outcome_family = "binomial",
                                boot_reps = 1000, conf.level = 0.95) {
  validate_data_frame(data)
  a_name <- .kk_colname(rlang::enquo(exposure))
  m_name <- .kk_colname(rlang::enquo(mediator))
  y_name <- .kk_colname(rlang::enquo(outcome))
  need <- c(a_name, m_name, y_name, confounders)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }

  conf_str <- if (!is.null(confounders)) paste("+", paste(confounders, collapse = " + ")) else ""
  m_fml <- stats::as.formula(sprintf("%s ~ %s %s", m_name, a_name, conf_str))
  y_fml <- stats::as.formula(sprintf("%s ~ %s + %s %s", y_name, a_name, m_name, conf_str))
  binom_out <- outcome_family == "binomial"

  effects <- function(d) {
    mmod <- stats::glm(m_fml, family = mediator_family, data = d)
    ymod <- stats::glm(y_fml, family = outcome_family, data = d)
    a <- stats::coef(mmod)[[a_name]]      # exposure -> mediator
    theta1 <- stats::coef(ymod)[[a_name]] # direct
    theta2 <- stats::coef(ymod)[[m_name]] # mediator -> outcome
    nde <- theta1
    nie <- a * theta2
    if (binom_out) {
      # Odds-ratio scale; VanderWeele proportion mediated
      pm <- (exp(nde) * (exp(nie) - 1)) / (exp(nde + nie) - 1)
      c(TE = exp(nde + nie), NDE = exp(nde), NIE = exp(nie), PM = pm)
    } else {
      te <- nde + nie
      c(TE = te, NDE = nde, NIE = nie, PM = nie / te)
    }
  }

  point <- effects(data)

  boot <- vapply(seq_len(boot_reps), function(i) {
    idx <- sample.int(nrow(data), replace = TRUE)
    tryCatch(effects(data[idx, , drop = FALSE]),
      error = function(e) rep(NA_real_, 4))
  }, numeric(4))

  alpha <- 1 - conf.level
  ci <- apply(boot, 1, function(v) {
    stats::quantile(v, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE, names = FALSE)
  })

  scale <- if (binom_out) "odds ratio" else "difference"
  tibble::tibble(
    Effect = c("Total effect (TE)", "Direct effect (NDE)",
               "Indirect effect (NIE)", "Proportion mediated"),
    Estimate = as.numeric(point),
    Lower = ci[1, ],
    Upper = ci[2, ],
    Scale = c(scale, scale, scale, "proportion"),
    Conf_Level = conf.level
  )
}
