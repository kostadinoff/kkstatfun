#' Number Needed to Treat/Harm (KK)
#'
#' @description Calculates Number Needed to Treat (NNT) or Number Needed to Harm (NNH)
#'   with confidence intervals. Can calculate from Risk Difference, OR, or RR.
#'   Also supports passing a data frame directly to calculate these metrics from raw data.
#'
#' @param estimate Point estimate (Risk Difference, OR, or RR) OR a data frame. If a data frame is
#'   provided, the exposure and outcome variables must be specified as the next arguments.
#' @param type Type of estimate: "risk_diff" (default), "odds_ratio", or "risk_ratio" OR exposure variable
#'   (as symbol or string) if `estimate` is a data frame.
#' @param baseline_risk Baseline risk (Control Event Rate) - required for OR/RR OR outcome variable
#'   (as symbol or string) if `estimate` is a data frame.
#' @param conf.level Confidence level (default 0.95)
#' @param ci_low Lower bound of CI for the estimate (optional, for calculating CI of NNT)
#' @param ci_high Upper bound of CI for the estimate (optional)
#'
#' @return Tibble with NNT/NNH, absolute risk metrics, and relative risk metrics.
#' @export
#' @examples
#' # From Risk Difference (RD = -0.1)
#' kk_nnt(-0.1)
#'
#' # From Odds Ratio (OR = 0.5, Baseline Risk = 0.2)
#' kk_nnt(0.5, type = "odds_ratio", baseline_risk = 0.2)
#'
#' # From raw data frame
#' df_treatment <- data.frame(
#'   statin = c(rep(1, 500), rep(0, 500)),
#'   event = c(rep(1, 15), rep(0, 485), rep(1, 35), rep(0, 465))
#' )
#' kk_nnt(df_treatment, statin, event)
kk_nnt <- function(estimate, type = "risk_diff", baseline_risk = NULL, conf.level = 0.95, ci_low = NULL, ci_high = NULL) {
  
  if (inherits(estimate, "data.frame")) {
    # Dataframe mode
    data <- estimate
    exposure_expr <- substitute(type)
    outcome_expr <- substitute(baseline_risk)
    
    # Resolve strings/symbols
    resolve_arg <- function(arg_expr, pf = parent.frame()) {
      if (is.character(arg_expr)) {
        return(arg_expr)
      }
      if (is.symbol(arg_expr)) {
        val <- tryCatch(eval(arg_expr, pf), error = function(e) NULL)
        if (is.character(val) && length(val) == 1) {
          return(val)
        }
        return(as.character(arg_expr))
      }
      return(deparse(arg_expr))
    }
    
    exposure_name <- resolve_arg(exposure_expr)
    outcome_name <- resolve_arg(outcome_expr)
    
    if (!all(c(exposure_name, outcome_name) %in% names(data))) {
      stop("Exposure or outcome columns not found in data")
    }
    
    exp_vec <- data[[exposure_name]]
    out_vec <- data[[outcome_name]]
    
    valid_idx <- !is.na(exp_vec) & !is.na(out_vec)
    exp_vec <- exp_vec[valid_idx]
    out_vec <- out_vec[valid_idx]
    
    if (length(unique(stats::na.omit(exp_vec))) != 2 || length(unique(stats::na.omit(out_vec))) != 2) {
      stop("Exposure and outcome must be binary variables (exactly 2 unique values)")
    }
    
    exp_fac <- factor(exp_vec)
    out_fac <- factor(out_vec)
    tbl <- table(Exposure = exp_fac, Outcome = out_fac)
    
    levels_exp <- levels(exp_fac)
    levels_out <- levels(out_fac)
    
    a <- if (levels_exp[2] %in% rownames(tbl) && levels_out[2] %in% colnames(tbl)) tbl[levels_exp[2], levels_out[2]] else 0
    b <- if (levels_exp[2] %in% rownames(tbl) && levels_out[1] %in% colnames(tbl)) tbl[levels_exp[2], levels_out[1]] else 0
    c <- if (levels_exp[1] %in% rownames(tbl) && levels_out[2] %in% colnames(tbl)) tbl[levels_exp[1], levels_out[2]] else 0
    d <- if (levels_exp[1] %in% rownames(tbl) && levels_out[1] %in% colnames(tbl)) tbl[levels_exp[1], levels_out[1]] else 0
    
    n1 <- a + b
    n0 <- c + d
    p1 <- a / n1
    p0 <- c / n0
    
    rd <- p1 - p0
    se_rd <- sqrt((p1 * (1 - p1) / n1) + (p0 * (1 - p0) / n0))
    z <- stats::qnorm(1 - (1 - conf.level) / 2)
    rd_low <- rd - z * se_rd
    rd_high <- rd + z * se_rd
    
    abs_rd <- abs(rd)
    nnt_val <- ifelse(abs_rd > 0, 1 / abs_rd, Inf)
    
    if (rd < 0) {
      nnt_label <- "NNT"
      arr_label <- "ARR"
      rrr_label <- "RRR"
      arr_val <- -rd
      arr_low <- -rd_high
      arr_high <- -rd_low
      
      rrr_val <- (p0 - p1) / p0
      se_ln_rr <- tryCatch(sqrt((1 / max(a, 0.5) - 1 / n1) + (1 / max(c, 0.5) - 1 / n0)), error = function(e) NA)
      rr_val <- p1 / p0
      rr_low <- exp(log(rr_val) - z * se_ln_rr)
      rr_high <- exp(log(rr_val) + z * se_ln_rr)
      rrr_low <- 1 - rr_high
      rrr_high <- 1 - rr_low
    } else {
      nnt_label <- "NNH"
      arr_label <- "ARI"
      rrr_label <- "RRI"
      arr_val <- rd
      arr_low <- rd_low
      arr_high <- rd_high
      
      se_ln_rr <- tryCatch(sqrt((1 / max(a, 0.5) - 1 / n1) + (1 / max(c, 0.5) - 1 / n0)), error = function(e) NA)
      rr_val <- p1 / p0
      rr_low <- exp(log(rr_val) - z * se_ln_rr)
      rr_high <- exp(log(rr_val) + z * se_ln_rr)
      rrr_val <- rr_val - 1
      rrr_low <- rr_low - 1
      rrr_high <- rr_high - 1
    }
    
    if (sign(rd_low) != sign(rd_high)) {
      nnt_low <- -Inf
      nnt_high <- Inf
      note <- "CI crosses null (infinity)"
    } else {
      bounds <- 1 / c(rd_high, rd_low)
      nnt_low <- min(abs(bounds))
      nnt_high <- max(abs(bounds))
      note <- "Significant"
    }
    
    return(tibble::tibble(
      Metric = c(nnt_label, arr_label, rrr_label),
      Estimate = c(nnt_val, arr_val, rrr_val),
      Lower = c(nnt_low, arr_low, rrr_low),
      Upper = c(nnt_high, arr_high, rrr_high),
      Conf_Level = rep(conf.level, 3),
      Note = c(note, "", "")
    ))
  }

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
