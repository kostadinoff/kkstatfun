# ============================================================
# POWER ANALYSIS
# ============================================================

#' Power Analysis for Proportions
#'
#' @description Calculate the necessary sample size or power for detecting a difference in proportions.
#'   This is a wrapper around `stats::power.prop.test`.
#'
#' @param n Number of observations (per group)
#' @param p1 Proportion in group 1
#' @param p2 Proportion in group 2
#' @param sig.level Significance level (Type I error probability)
#' @param power Power of test (1 minus Type II error probability)
#' @param alternative One- or two-sided test ("two.sided", "one.sided")
#' @param strict Use strict interpretation in two-sided case
#'
#' @return Object of class "power.htest"
#'
#' @export
power_proportions <- function(n = NULL, p1 = NULL, p2 = NULL, sig.level = 0.05,
                              power = NULL, alternative = "two.sided",
                              strict = FALSE) {
              if (sum(sapply(list(n, p1, p2, power, sig.level), is.null)) != 1) {
                            stop("Exactly one of 'n', 'p1', 'p2', 'power', and 'sig.level' must be NULL")
              }

              stats::power.prop.test(
                            n = n,
                            p1 = p1,
                            p2 = p2,
                            sig.level = sig.level,
                            power = power,
                            alternative = alternative,
                            strict = strict
              )
}
