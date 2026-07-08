# ============================================================
# DECISION CURVE ANALYSIS (net benefit)
# ============================================================

#' Decision Curve Analysis (KK)
#'
#' @description Evaluates the clinical utility of a risk model or diagnostic test
#'   across a range of threshold probabilities, complementing discrimination
#'   (`kk_roc`) and calibration (`kk_calibration`). For each threshold `p_t` the
#'   net benefit weighs true positives against false positives at the odds implied
#'   by the threshold:
#'   `NB = TP/N - (FP/N) * (p_t / (1 - p_t)) - harm`.
#'   The model is compared against the two default strategies of treating everyone
#'   and treating no one; the model is worth using over the range of thresholds
#'   where its net benefit is highest.
#'
#' @param data Data frame.
#' @param truth Binary outcome column (bare name or string); coerced to 0/1.
#' @param predictor Predicted-probability column (bare name or string), on the
#'   0-1 scale.
#' @param thresholds Numeric vector of threshold probabilities to evaluate
#'   (default `seq(0.01, 0.99, by = 0.01)`).
#' @param harm Optional constant harm of the model/test (e.g. the cost of
#'   obtaining the marker), on the net-benefit scale (default 0).
#'
#' @return Long tibble with one row per threshold x strategy: the `threshold`, the
#'   `strategy` ("Model", "Treat all", "Treat none"), the `net_benefit`, and the
#'   standardized net benefit (`net_benefit / prevalence`). Ready to plot as a
#'   decision curve. The event prevalence is attached as attribute `prevalence`.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(disease = rbinom(500, 1, 0.3))
#' df$risk <- plogis(qlogis(0.3) + 0.9 * df$disease + rnorm(500))
#' kk_decision_curve(df, disease, risk)
#' }
#'
#' @export
kk_decision_curve <- function(data, truth, predictor,
                              thresholds = seq(0.01, 0.99, by = 0.01),
                              harm = 0) {
  validate_data_frame(data)

  truth_name <- .kk_colname(rlang::enquo(truth))
  pred_name <- .kk_colname(rlang::enquo(predictor))
  missing_cols <- setdiff(c(truth_name, pred_name), names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }

  y_raw <- data[[truth_name]]
  y <- if (is.factor(y_raw)) as.integer(y_raw) - 1L else as.numeric(y_raw)
  p <- data[[pred_name]]
  ok <- stats::complete.cases(y, p)
  y <- y[ok]; p <- p[ok]
  if (!all(y %in% c(0, 1))) stop("`truth` must be binary (0/1).")
  if (any(p < 0 | p > 1)) stop("`predictor` must be probabilities in [0, 1].")
  if (any(thresholds <= 0 | thresholds >= 1)) {
    stop("`thresholds` must lie strictly in (0, 1).")
  }

  N <- length(y)
  prevalence <- mean(y)

  nb_at <- function(pt) {
    odds <- pt / (1 - pt)
    treat <- p >= pt
    tp <- sum(treat & y == 1)
    fp <- sum(treat & y == 0)
    nb_model <- tp / N - (fp / N) * odds - harm
    nb_all <- prevalence - (1 - prevalence) * odds
    c(Model = nb_model, `Treat all` = nb_all, `Treat none` = 0)
  }

  mat <- vapply(thresholds, nb_at, numeric(3))  # 3 strategies x thresholds

  out <- tibble::tibble(
    threshold = rep(thresholds, each = 3),
    strategy = rep(c("Model", "Treat all", "Treat none"), times = length(thresholds)),
    net_benefit = as.vector(mat),
    std_net_benefit = as.vector(mat) / prevalence
  )
  attr(out, "prevalence") <- prevalence
  out
}
