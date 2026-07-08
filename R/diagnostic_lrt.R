# ============================================================
# LIKELIHOOD RATIOS & POST-TEST PROBABILITY (Bayes at the bedside)
# ============================================================

#' Likelihood Ratios and Post-Test Probability (KK)
#'
#' @description Translates a diagnostic test's sensitivity and specificity into
#'   the clinically actionable quantities of a Bayesian update: the positive and
#'   negative likelihood ratios and the post-test probability of disease given a
#'   positive or a negative result, starting from a specified pre-test
#'   probability. When the numbers of diseased and non-diseased subjects are
#'   supplied, confidence intervals for the likelihood ratios (Simel 1991) and
#'   the resulting post-test probabilities are returned.
#'
#' @param pre_test_prob Pre-test (prior) probability of disease, in (0, 1).
#' @param sensitivity Test sensitivity, in (0, 1).
#' @param specificity Test specificity, in (0, 1).
#' @param conf.level Confidence level (default 0.95).
#' @param n_diseased,n_healthy Optional counts of diseased and non-diseased
#'   subjects used to estimate the sensitivity/specificity; required for
#'   confidence intervals.
#'
#' @return Tibble with the pre-test probability, positive and negative likelihood
#'   ratios, and the post-test probabilities after a positive and a negative
#'   test, each with a confidence interval where estimable.
#'
#' @examples
#' # D-dimer: sens 0.95, spec 0.60; pre-test probability of PE 20%
#' kk_diagnostic_lrt(pre_test_prob = 0.20, sensitivity = 0.95, specificity = 0.60,
#'                   n_diseased = 100, n_healthy = 400)
#'
#' @export
kk_diagnostic_lrt <- function(pre_test_prob, sensitivity, specificity,
                              conf.level = 0.95,
                              n_diseased = NULL, n_healthy = NULL) {
  chk <- function(x, nm) {
    if (!is.numeric(x) || length(x) != 1 || x <= 0 || x >= 1) {
      stop(sprintf("`%s` must be a single number in (0, 1).", nm))
    }
  }
  chk(pre_test_prob, "pre_test_prob")
  chk(sensitivity, "sensitivity")
  chk(specificity, "specificity")

  lr_pos <- sensitivity / (1 - specificity)
  lr_neg <- (1 - sensitivity) / specificity
  pre_odds <- pre_test_prob / (1 - pre_test_prob)

  odds_to_prob <- function(o) o / (1 + o)
  post_prob_pos <- odds_to_prob(pre_odds * lr_pos)
  post_prob_neg <- odds_to_prob(pre_odds * lr_neg)

  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  lrp_lo <- lrp_hi <- lrn_lo <- lrn_hi <- NA_real_
  ppos_lo <- ppos_hi <- pneg_lo <- pneg_hi <- NA_real_

  if (!is.null(n_diseased) && !is.null(n_healthy)) {
    # Simel (1991) log-scale standard errors
    se_log_lrp <- sqrt((1 - sensitivity) / (sensitivity * n_diseased) +
      specificity / ((1 - specificity) * n_healthy))
    se_log_lrn <- sqrt(sensitivity / ((1 - sensitivity) * n_diseased) +
      (1 - specificity) / (specificity * n_healthy))
    lrp_lo <- exp(log(lr_pos) - z * se_log_lrp)
    lrp_hi <- exp(log(lr_pos) + z * se_log_lrp)
    lrn_lo <- exp(log(lr_neg) - z * se_log_lrn)
    lrn_hi <- exp(log(lr_neg) + z * se_log_lrn)
    # Propagate LR CIs to post-test probability (monotone in LR)
    ppos_lo <- odds_to_prob(pre_odds * lrp_lo)
    ppos_hi <- odds_to_prob(pre_odds * lrp_hi)
    pneg_lo <- odds_to_prob(pre_odds * lrn_lo)
    pneg_hi <- odds_to_prob(pre_odds * lrn_hi)
  }

  tibble::tibble(
    Metric = c(
      "Pre-test probability",
      "Positive likelihood ratio (LR+)",
      "Negative likelihood ratio (LR-)",
      "Post-test probability (test +)",
      "Post-test probability (test -)"
    ),
    Estimate = c(pre_test_prob, lr_pos, lr_neg, post_prob_pos, post_prob_neg),
    Lower = c(NA, lrp_lo, lrn_lo, ppos_lo, pneg_lo),
    Upper = c(NA, lrp_hi, lrn_hi, ppos_hi, pneg_hi),
    Conf_Level = conf.level
  )
}
