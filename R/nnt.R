#' Number Needed to Treat/Harm (KK)
#'
#' @description Calculates Number Needed to Treat (NNT) or Number Needed to Harm (NNH)
#'   with confidence intervals. Can calculate from Risk Difference, OR, or RR.
#'   For OR and RR, a baseline risk (Control Event Rate) must be provided.
#'
#' @param estimate Point estimate (Risk Difference, OR, or RR)
#' @param type Type of estimate: "risk_diff" (default), "odds_ratio", or "risk_ratio"
#' @param baseline_risk Baseline risk (Control Event Rate) - required for OR/RR
#' @param conf.level Confidence level (default 0.95)
#' @param ci_low Lower bound of CI for the estimate (optional, for calculating CI of NNT)
#' @param ci_high Upper bound of CI for the estimate (optional)
#'
#' @return Tibble with NNT/NNH and CIs
#' @export
#' @examples
#' # From Risk Difference (RD = -0.1)
#' kk_nnt(-0.1)
#'
#' # From Odds Ratio (OR = 0.5, Baseline Risk = 0.2)
#' kk_nnt(0.5, type = "odds_ratio", baseline_risk = 0.2)
kk_nnt <- function(estimate, type = "risk_diff", baseline_risk = NULL, conf.level = 0.95, ci_low = NULL, ci_high = NULL) {
  
  if (type %in% c("odds_ratio", "risk_ratio") && is.null(baseline_risk)) {
    stop("baseline_risk is required for OR and RR")
  }
  
  # Calculate Risk Difference (RD) based on input type
  if (type == "risk_diff") {
    rd <- estimate
    rd_low <- ci_low
    rd_high <- ci_high
  } else if (type == "risk_ratio") {
    # RD = CER * (RR - 1)
    rd <- baseline_risk * (estimate - 1)
    if (!is.null(ci_low)) rd_low <- baseline_risk * (ci_low - 1)
    if (!is.null(ci_high)) rd_high <- baseline_risk * (ci_high - 1)
  } else if (type == "odds_ratio") {
    # RD = (OR * CER) / (1 - CER + OR * CER) - CER
    cer <- baseline_risk
    calc_rd_from_or <- function(or, cer) {
      (or * cer) / (1 - cer + or * cer) - cer
    }
    rd <- calc_rd_from_or(estimate, cer)
    if (!is.null(ci_low)) rd_low <- calc_rd_from_or(ci_low, cer)
    if (!is.null(ci_high)) rd_high <- calc_rd_from_or(ci_high, cer)
  } else {
    stop("Invalid type. Must be 'risk_diff', 'risk_ratio', or 'odds_ratio'")
  }
  
  # Calculate NNT/NNH
  nnt_val <- ifelse(abs(rd) > 0, 1 / abs(rd), Inf)
  label <- ifelse(rd > 0, "NNH", ifelse(rd < 0, "NNT", "NNT/NNH"))
  
  # Calculate CI if bounds provided
  nnt_low <- NA
  nnt_high <- NA
  
  if (!is.null(ci_low) && !is.null(ci_high)) {
    # If CI crosses 0 (RD crosses 0), NNT CI includes Infinity
    # We report the bounds as 1/limit. 
    # If significant: 1/upper to 1/lower (swapped because of 1/x)
    
    if (sign(rd_low) != sign(rd_high)) {
      # Not significant
      nnt_low <- -Inf
      nnt_high <- Inf
      note <- "CI crosses null (infinity)"
    } else {
      bounds <- 1 / c(rd_high, rd_low)
      nnt_low <- min(abs(bounds))
      nnt_high <- max(abs(bounds))
      note <- "Significant"
    }
  } else {
    note <- "No CI provided"
  }
  
  tibble::tibble(
    Metric = label,
    Estimate = nnt_val,
    Lower = nnt_low,
    Upper = nnt_high,
    RD_Estimate = rd,
    Note = note
  )
}
