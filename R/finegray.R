#' Fine-Gray Competing Risks Regression
#'
#' Fits Fine-Gray subdistribution hazards model for competing risks in time-to-event epidemiological data.
#' Returns tidy univariate and multivariable Subdistribution Hazard Ratios (SHR) with 95% confidence intervals and robust standard errors.
#'
#' @param data A data frame containing survival time, event status, and predictor variables.
#' @param time Name of the time-to-event variable.
#' @param status Name of the status variable (0 = censored, 1 = event of interest, 2+ = competing events).
#' @param predictors Character vector of predictor variable names.
#' @param cause Event code for the cause of interest (default 1).
#'
#' @return A tidy \code{tibble} with columns:
#'   \item{variable}{Name of predictor variable}
#'   \item{term}{Level / contrast term name}
#'   \item{subdist_hr}{Subdistribution Hazard Ratio (SHR = \eqn{\exp(\beta)})}
#'   \item{conf_low}{Lower 95% confidence limit}
#'   \item{conf_high}{Upper 95% confidence limit}
#'   \item{std_error}{Robust standard error of \eqn{\beta}}
#'   \item{z_stat}{Wald $Z$ statistic}
#'   \item{p_value}{$p$-value}
#'   \item{model_type}{"Univariate" or "Multivariable"}
#'
#' @details
#' The Fine-Gray model (Fine & Gray, 1999) models the hazard of the subdistribution for a specific cause in the presence of competing events.
#' Data transformation is performed using \code{\link[survival]{finegray}} to build a expanded weighted dataset with censor-weights,
#' followed by robust Cox proportional hazards regression (\code{\link[survival]{coxph}}).
#'
#' @references
#' Fine JP, Gray RJ (1999). A proportional hazards model for the subdistribution of a competing risk. \emph{Journal of the American Statistical Association}, 94(446):496-509.
#' Szklo M, Nieto FJ (2014). \emph{Epidemiology: Beyond the Basics}, 3rd Edition. Jones & Bartlett Learning.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' dat <- data.frame(
#'   time = runif(150, 1, 100),
#'   status = sample(c(0, 1, 2), 150, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
#'   age = rnorm(150, 60, 10),
#'   treatment = factor(sample(c("Control", "Drug"), 150, replace = TRUE))
#' )
#'
#' # Fine-Gray regression for cause = 1
#' kk_finegray(dat, time = "time", status = "status", predictors = c("age", "treatment"), cause = 1)
kk_finegray <- function(data, time, status, predictors, cause = 1) {
  
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required for kk_finegray. Please install it.")
  }
  
  df <- as.data.frame(data)
  time_col <- rlang::as_name(rlang::enquo(time))
  status_col <- rlang::as_name(rlang::enquo(status))
  predictors <- as.character(predictors)
  
  # Ensure complete cases across variables
  vars_to_check <- c(time_col, status_col, predictors)
  df <- df[stats::complete.cases(df[, vars_to_check]), ]
  
  # Format status as factor for finegray
  df[[status_col]] <- factor(df[[status_col]])
  
  # Transform dataset using survival::finegray
  formula_fg <- stats::as.formula(paste("survival::Surv(", time_col, ",", status_col, ") ~ ."))
  
  fg_data <- tryCatch({
    survival::finegray(formula_fg, data = df, etype = cause)
  }, error = function(e) {
    stop("Error expanding dataset in survival::finegray: ", e$message)
  })
  
  results_list <- list()
  
  # 1. Univariate models
  for (pred in predictors) {
    uni_formula <- stats::as.formula(paste("survival::Surv(fgstart, fgstop, fgstatus) ~", pred))
    fit_uni <- survival::coxph(uni_formula, data = fg_data, weights = fg_data$fgwt, cluster = fg_data$id)
    
    sum_uni <- summary(fit_uni)
    coef_mat <- sum_uni$coefficients
    conf_mat <- sum_uni$conf.int
    
    terms <- rownames(coef_mat)
    for (i in seq_along(terms)) {
      results_list[[length(results_list) + 1]] <- tibble::tibble(
        variable = pred,
        term = terms[i],
        subdist_hr = conf_mat[i, "exp(coef)"],
        conf_low = conf_mat[i, "lower .95"],
        conf_high = conf_mat[i, "upper .95"],
        std_error = coef_mat[i, "robust se"],
        z_stat = coef_mat[i, "z"],
        p_value = coef_mat[i, "Pr(>|z|)"],
        model_type = "Univariate"
      )
    }
  }
  
  # 2. Multivariable model
  if (length(predictors) > 1) {
    multi_formula <- stats::as.formula(paste("survival::Surv(fgstart, fgstop, fgstatus) ~", paste(predictors, collapse = " + ")))
    fit_multi <- survival::coxph(multi_formula, data = fg_data, weights = fg_data$fgwt, cluster = fg_data$id)
    
    sum_multi <- summary(fit_multi)
    coef_mat <- sum_multi$coefficients
    conf_mat <- sum_multi$conf.int
    
    terms <- rownames(coef_mat)
    for (i in seq_along(terms)) {
      # Determine variable corresponding to term
      var_match <- predictors[sapply(predictors, function(p) grepl(paste0("^", p), terms[i]))]
      var_name <- if (length(var_match) > 0) var_match[1] else terms[i]
      
      results_list[[length(results_list) + 1]] <- tibble::tibble(
        variable = var_name,
        term = terms[i],
        subdist_hr = conf_mat[i, "exp(coef)"],
        conf_low = conf_mat[i, "lower .95"],
        conf_high = conf_mat[i, "upper .95"],
        std_error = coef_mat[i, "robust se"],
        z_stat = coef_mat[i, "z"],
        p_value = coef_mat[i, "Pr(>|z|)"],
        model_type = "Multivariable"
      )
    }
  }
  
  res <- dplyr::bind_rows(results_list)
  return(res)
}
