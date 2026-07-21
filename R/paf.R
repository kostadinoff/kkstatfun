#' Population Attributable Fraction (PAF) and Attributable Risk
#'
#' Calculates crude and model-adjusted Population Attributable Fraction (PAF),
#' Attributable Fraction in the Exposed (AF_exp), and Population Attributable Risk (PAR).
#' Supports raw data input (unadjusted & adjusted via logistic regression) or a fitted \code{glm} object.
#' Uses Miettinen's and Bruzzi's model-standardization methods for adjusted PAF with confidence intervals.
#'
#' @param data A data frame containing the variables, or a fitted \code{glm} object (binomial family).
#' @param outcome Name of the binary outcome variable (0/1 or factor). Omit if \code{data} is a \code{glm} object.
#' @param exposure Name of the binary exposure variable (0/1 or factor). Omit if \code{data} is a \code{glm} object.
#' @param covariates Optional character vector of covariate names for adjusted PAF via logistic regression.
#' @param conf_level Confidence level for confidence intervals (default 0.95).
#' @param n_boot Number of bootstrap iterations for adjusted PAF confidence intervals (default 500).
#'
#' @return A tidy \code{tibble} with columns:
#'   \item{metric}{Name of the metric (e.g., "PAF (Adjusted - Bruzzi)", "PAF (Adjusted - Miettinen)", "PAF (Crude)", "AF (Exposed)", "PAR")}
#'   \item{estimate}{Point estimate (as a proportion or rate difference)}
#'   \item{conf_low}{Lower confidence limit}
#'   \item{conf_high}{Upper confidence limit}
#'   \item{method}{Calculation method used}
#'
#' @details
#' Adjusted PAF is estimated using both Bruzzi's model-based standardization method
#' (Bruzzi et al., 1985) and Miettinen's adjusted formula (Miettinen, 1974):
#' \deqn{PAF_{adj} = \frac{p_{cases|exp} \cdot (OR_{adj} - 1)}{OR_{adj}}}
#' Bootstrap resampling (BCa / percentile) is used to construct robust non-parametric confidence intervals for adjusted estimates.
#'
#' @references
#' Bruzzi P, Green SB, Byar DP, Brinton LA, Schairer C (1985). Estimating the population attributable risk for multiple risk factors using case-control data. \emph{Am J Epidemiol}, 122(5):904-914.
#' Miettinen OS (1974). Proportion of disease attributable to the exposure. \emph{Am J Epidemiol}, 99(5):325-332.
#' Rothman KJ, Greenland S, Lash TL (2008). \emph{Modern Epidemiology}, 3rd Edition. Lippincott Williams & Wilkins.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' dat <- data.frame(
#'   disease = rbinom(200, 1, 0.25),
#'   smoking = rbinom(200, 1, 0.4),
#'   age = rnorm(200, 50, 10),
#'   bmi = rnorm(200, 26, 4)
#' )
#' # Crude and adjusted PAF
#' kk_paf(dat, outcome = "disease", exposure = "smoking", covariates = c("age", "bmi"), n_boot = 100)
#'
#' # From fitted logistic model directly
#' fit <- glm(disease ~ smoking + age + bmi, data = dat, family = binomial)
#' kk_paf(fit, exposure = "smoking", n_boot = 100)
kk_paf <- function(data, outcome = NULL, exposure = NULL, covariates = NULL, conf_level = 0.95, n_boot = 200) {
  
  # Check if data is a fitted glm
  if (inherits(data, "glm")) {
    model <- data
    if (family(model)$family != "binomial") {
      stop("kk_paf requires a binomial family glm model.")
    }
    model_data <- model$model
    if (is.null(exposure)) {
      # Take first non-intercept predictor
      exposure <- names(coef(model))[2]
    }
    outcome <- names(model_data)[1]
    covariates <- setdiff(names(model_data)[-1], exposure)
    df <- model_data
  } else {
    df <- as.data.frame(data)
    outcome <- rlang::as_name(rlang::enquo(outcome))
    exposure <- rlang::as_name(rlang::enquo(exposure))
    if (!is.null(covariates)) {
      covariates <- as.character(covariates)
    }
  }

  # Clean binary outcome & exposure
  y <- as.numeric(df[[outcome]])
  if (all(y %in% c(1, 2))) y <- y - 1
  x <- as.numeric(df[[exposure]])
  if (all(x %in% c(1, 2))) x <- x - 1
  
  if (!all(y %in% c(0, 1)) || !all(x %in% c(0, 1))) {
    stop("Outcome and exposure must be binary (0/1).")
  }
  
  # Calculate crude 2x2 statistics
  a <- sum(x == 1 & y == 1, na.rm = TRUE) # exposed cases
  b <- sum(x == 1 & y == 0, na.rm = TRUE) # exposed non-cases
  c <- sum(x == 0 & y == 1, na.rm = TRUE) # unexposed cases
  d <- sum(x == 0 & y == 0, na.rm = TRUE) # unexposed non-cases
  
  p_exp_cases <- a / (a + c) # proportion of cases exposed
  p_exp_pop <- (a + b) / (a + b + c + d) # population exposure prevalence
  r1 <- a / (a + b) # risk in exposed
  r0 <- c / (c + d) # risk in unexposed
  r_pop <- (a + c) / (a + b + c + d) # overall risk
  
  # Crude PAF & AF_exp & PAR
  crude_rr <- if (r0 > 0) r1 / r0 else NA
  crude_or <- if (b * c > 0) (a * d) / (b * c) else NA
  
  paf_crude <- if (r_pop > 0) (r_pop - r0) / r_pop else NA
  af_exp_crude <- if (r1 > 0) (r1 - r0) / r1 else NA
  par_crude <- r_pop - r0
  
  # Exact 95% CIs for crude PAF (Kahn & Sempos / Greenland formula)
  # SE of ln(1 - paf) = SE(ln(RR_crude)) = sqrt(1/a - 1/(a+b) + 1/c - 1/(c+d))
  se_ln_rr <- sqrt(max(0, (1/a - 1/(a+b) + 1/c - 1/(c+d)), na.rm = TRUE))
  z_crit <- stats::qnorm((1 + conf_level) / 2)
  
  paf_crude_low <- 1 - (1 - paf_crude) * exp(z_crit * se_ln_rr)
  paf_crude_high <- 1 - (1 - paf_crude) * exp(-z_crit * se_ln_rr)
  
  results_list <- list(
    tibble::tibble(
      metric = "PAF (Crude)",
      estimate = paf_crude,
      conf_low = min(paf_crude_low, paf_crude_high),
      conf_high = max(paf_crude_low, paf_crude_high),
      method = "Crude Risk Difference / Population Risk"
    ),
    tibble::tibble(
      metric = "AF (Exposed)",
      estimate = af_exp_crude,
      conf_low = max(0, 1 - exp(log(1 - af_exp_crude + 1e-6) + z_crit * se_ln_rr)),
      conf_high = min(1, 1 - exp(log(1 - af_exp_crude + 1e-6) - z_crit * se_ln_rr)),
      method = "Attributable Fraction in Exposed (Etiologic Fraction)"
    ),
    tibble::tibble(
      metric = "PAR (Population Attributable Risk)",
      estimate = par_crude,
      conf_low = par_crude - z_crit * sqrt(max(0, r_pop*(1-r_pop)/nrow(df))),
      conf_high = par_crude + z_crit * sqrt(max(0, r_pop*(1-r_pop)/nrow(df))),
      method = "Absolute Risk Difference (Risk_pop - Risk_unexposed)"
    )
  )
  
  # Adjusted PAF calculation (if covariates or glm present)
  if (!is.null(covariates) && length(covariates) > 0) {
    formula_str <- paste(outcome, "~", exposure, "+", paste(covariates, collapse = " + "))
    fit_adj <- stats::glm(stats::as.formula(formula_str), data = df, family = stats::binomial())
    
    or_adj <- exp(stats::coef(fit_adj)[exposure])
    
    # Miettinen's Adjusted PAF
    paf_miettinen <- (p_exp_cases * (or_adj - 1)) / or_adj
    
    # Bruzzi's Model-based PAF: 1 - 1/N_cases * sum(1 / RR_i)
    cases_df <- df[df[[outcome]] == 1, ]
    # Predict with exposure set to 0 for all cases
    cases_unexp <- cases_df
    cases_unexp[[exposure]] <- 0
    p_orig <- stats::predict(fit_adj, newdata = cases_df, type = "response")
    p_unexp <- stats::predict(fit_adj, newdata = cases_unexp, type = "response")
    
    rr_cases <- p_orig / p_unexp
    paf_bruzzi <- 1 - (mean(1 / rr_cases, na.rm = TRUE))
    
    # Bootstrap CIs for adjusted estimates
    if (n_boot > 0) {
      boot_bruzzi <- numeric(n_boot)
      boot_miettinen <- numeric(n_boot)
      n_total <- nrow(df)
      
      for (b_i in seq_len(n_boot)) {
        idx <- sample.int(n_total, replace = TRUE)
        b_df <- df[idx, ]
        tryCatch({
          b_fit <- stats::glm(stats::as.formula(formula_str), data = b_df, family = stats::binomial())
          b_or <- exp(stats::coef(b_fit)[exposure])
          b_cases <- b_df[b_df[[outcome]] == 1, ]
          b_p_cases_exp <- sum(b_cases[[exposure]] == 1) / nrow(b_cases)
          
          boot_miettinen[b_i] <- (b_p_cases_exp * (b_or - 1)) / b_or
          
          b_cases_unexp <- b_cases
          b_cases_unexp[[exposure]] <- 0
          b_po <- stats::predict(b_fit, newdata = b_cases, type = "response")
          b_pu <- stats::predict(b_fit, newdata = b_cases_unexp, type = "response")
          boot_bruzzi[b_i] <- 1 - mean(b_po / b_pu, na.rm = TRUE)
        }, error = function(e) {
          boot_bruzzi[b_i] <<- NA
          boot_miettinen[b_i] <<- NA
        })
      }
      
      alpha <- 1 - conf_level
      ci_bruzzi <- stats::quantile(boot_bruzzi, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE)
      ci_miettinen <- stats::quantile(boot_miettinen, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE)
    } else {
      ci_bruzzi <- c(NA, NA)
      ci_miettinen <- c(NA, NA)
    }
    
    results_list[[length(results_list) + 1]] <- tibble::tibble(
      metric = "PAF (Adjusted - Bruzzi)",
      estimate = paf_bruzzi,
      conf_low = ci_bruzzi[1],
      conf_high = ci_bruzzi[2],
      method = paste("Bruzzi Model Standardization (Adjusted for", paste(covariates, collapse = ", "), ")")
    )
    
    results_list[[length(results_list) + 1]] <- tibble::tibble(
      metric = "PAF (Adjusted - Miettinen)",
      estimate = paf_miettinen,
      conf_low = ci_miettinen[1],
      conf_high = ci_miettinen[2],
      method = paste("Miettinen Formula (p_cases * (OR_adj - 1) / OR_adj)")
    )
  }
  
  res <- dplyr::bind_rows(results_list)
  return(res)
}
