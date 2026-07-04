#' Relative Excess Risk due to Interaction (RERI)
#'
#' @description Calculates RERI (Relative Excess Risk due to Interaction) on the additive scale
#'   using a logistic regression model.
#'   Assumes the model contains two binary exposures and their interaction.
#'   Model formula should be roughly: outcome ~ exp1 * exp2 + covariates
#'
#' @param model A glm object (family = binomial) or a data frame. If a data frame is provided, the variables
#'   must be specified in `exp1`, `exp2`, and `outcome`.
#' @param exp1 Name of first exposure variable (string or symbol)
#' @param exp2 Name of second exposure variable (string or symbol)
#' @param outcome Name of outcome variable (string or symbol), required only if `model` is a data frame
#' @param conf.level Confidence level (default 0.95)
#'
#' @return Tibble with RERI, AP, and S estimates and CIs (using delta method)
#' @export
#' @examples
#' \dontrun{
#' # Using a fitted glm model
#' model <- glm(outcome ~ exp1 * exp2 + age, family = binomial, data = df)
#' kk_reri(model, "exp1", "exp2")
#'
#' # Using a data frame directly
#' kk_reri(df, exp1, exp2, outcome)
#' }
kk_reri <- function(model, exp1, exp2, outcome = NULL, conf.level = 0.95) {
  # Helper to resolve argument names (handles strings, symbols, or variable contents)
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

  if (inherits(model, "data.frame")) {
    # Data frame mode
    data <- model
    
    exp1_expr <- substitute(exp1)
    exp2_expr <- substitute(exp2)
    outcome_expr <- substitute(outcome)
    
    if (is.null(outcome_expr)) {
      stop("Argument 'outcome' must be specified when 'model' (data) is a data frame.")
    }
    
    exp1_name <- resolve_arg(exp1_expr)
    exp2_name <- resolve_arg(exp2_expr)
    outcome_name <- resolve_arg(outcome_expr)
    
    if (!all(c(exp1_name, exp2_name, outcome_name) %in% names(data))) {
      stop("One or more specified columns not found in data.")
    }
    
    formula_str <- paste0(outcome_name, " ~ ", exp1_name, " * ", exp2_name)
    fit_formula <- stats::as.formula(formula_str)
    
    model <- stats::glm(fit_formula, family = stats::binomial, data = data)
    
    exp1_str <- exp1_name
    exp2_str <- exp2_name
  } else if (inherits(model, "glm")) {
    # GLM mode
    exp1_expr <- substitute(exp1)
    exp2_expr <- substitute(exp2)
    
    exp1_str <- resolve_arg(exp1_expr)
    exp2_str <- resolve_arg(exp2_expr)
    
    # If conf.level was passed positionally as the 4th argument, it maps to 'outcome'
    outcome_expr <- substitute(outcome)
    if (!is.null(outcome_expr)) {
      outcome_val <- tryCatch(eval(outcome_expr, parent.frame()), error = function(e) NULL)
      if (is.numeric(outcome_val) && length(outcome_val) == 1) {
        conf.level <- outcome_val
      }
    }
  } else {
    stop("Model must be a glm object or a data frame")
  }

  coefs <- stats::coef(model)
  vcov_mat <- stats::vcov(model)

  # Identify coefficients
  # We need b1 (exp1), b2 (exp2), and b3 (exp1:exp2)
  b1_name <- exp1_str
  b2_name <- exp2_str
  b3_name <- paste0(exp1_str, ":", exp2_str)
  b3_alt <- paste0(exp2_str, ":", exp1_str)

  if (!b3_name %in% names(coefs) && b3_alt %in% names(coefs)) {
    b3_name <- b3_alt
  }

  if (!all(c(b1_name, b2_name, b3_name) %in% names(coefs))) {
    stop("Could not find exact coefficients for exposures and interaction. Ensure variables are numeric/binary 0/1 or check names(coef(model)).")
  }

  b1 <- coefs[[b1_name]]
  b2 <- coefs[[b2_name]]
  b3 <- coefs[[b3_name]]

  # RERI = RR_11 - RR_10 - RR_01 + 1
  # AP = RERI / RR_11
  # S = (RR_11 - 1) / ((RR_10 - 1) + (RR_01 - 1))
  
  # For logistic regression:
  # RR_11 = exp(b1 + b2 + b3)
  # RR_10 = exp(b1)
  # RR_01 = exp(b2)
  
  rr11 <- exp(b1 + b2 + b3)
  rr10 <- exp(b1)
  rr01 <- exp(b2)
  
  reri_est <- rr11 - rr10 - rr01 + 1
  ap_est <- reri_est / rr11
  
  denom_s <- rr10 + rr01 - 2
  if (abs(denom_s) < 1e-8 || is.na(denom_s)) {
    s_est <- NA
  } else {
    s_est <- (rr11 - 1) / denom_s
  }

  # Delta Method for Variance
  relevant_indices <- c(b1_name, b2_name, b3_name)
  sub_vcov <- vcov_mat[relevant_indices, relevant_indices]
  z <- stats::qnorm(1 - (1 - conf.level) / 2)

  # 1. RERI SE and CI
  g_reri <- c(rr11 - rr10, rr11 - rr01, rr11)
  var_reri <- as.numeric(matrix(g_reri, nrow = 1) %*% sub_vcov %*% matrix(g_reri, ncol = 1))
  se_reri <- sqrt(var_reri)
  reri_low <- reri_est - z * se_reri
  reri_high <- reri_est + z * se_reri

  # 2. AP SE and CI
  g_ap <- c((rr01 - 1) / rr11, (rr10 - 1) / rr11, (rr10 + rr01 - 1) / rr11)
  var_ap <- as.numeric(matrix(g_ap, nrow = 1) %*% sub_vcov %*% matrix(g_ap, ncol = 1))
  se_ap <- sqrt(var_ap)
  ap_low <- ap_est - z * se_ap
  ap_high <- ap_est + z * se_ap

  # 3. Synergy Index (S) SE and CI (computed on log scale)
  if (is.na(s_est) || s_est <= 0) {
    se_s <- NA
    s_low <- NA
    s_high <- NA
  } else {
    g_log_s <- c(
      rr11 / (rr11 - 1) - rr10 / denom_s,
      rr11 / (rr11 - 1) - rr01 / denom_s,
      rr11 / (rr11 - 1)
    )
    var_log_s <- as.numeric(matrix(g_log_s, nrow = 1) %*% sub_vcov %*% matrix(g_log_s, ncol = 1))
    se_log_s <- sqrt(var_log_s)
    se_s <- s_est * se_log_s
    
    log_s_low <- log(s_est) - z * se_log_s
    log_s_high <- log(s_est) + z * se_log_s
    s_low <- exp(log_s_low)
    s_high <- exp(log_s_high)
  }

  tibble::tibble(
    Metric = c("RERI", "AP", "S"),
    Estimate = c(reri_est, ap_est, s_est),
    Lower = c(reri_low, ap_low, s_low),
    Upper = c(reri_high, ap_high, s_high),
    SE = c(se_reri, se_ap, se_s),
    Conf_Level = rep(conf.level, 3)
  )
}
