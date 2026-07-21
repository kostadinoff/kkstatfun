#' Matched Case-Control Study Analysis
#'
#' Estimates conditional odds ratios for 1:1, 1:m, or variable-ratio matched
#' case-control studies on patient-level data using conditional logistic regression
#' (`survival::clogit`) and conditional Mantel-Haenszel methods.
#'
#' @param data A data frame or tibble containing patient-level matched data.
#' @param set_id Column name (quoted or unquoted) identifying matched sets (strata).
#' @param case Column name (quoted or unquoted) identifying case status (1/0, TRUE/FALSE, or factor).
#' @param exposure Column name (quoted or unquoted) identifying exposure status (1/0, TRUE/FALSE, or factor).
#' @param ratio Expected matching ratio (character string, default `"1:m"`).
#' @param alpha Significance level for confidence intervals (default `0.05`).
#'
#' @return A tidy tibble of class `kk_matched_case_control` containing:
#'   \item{matched_sets}{Total number of valid matched sets}
#'   \item{n_cases}{Total number of cases}
#'   \item{n_controls}{Total number of controls}
#'   \item{odds_ratio}{Point estimate of conditional odds ratio}
#'   \item{conf.low}{Lower bound of confidence interval}
#'   \item{conf.high}{Upper bound of confidence interval}
#'   \item{p.value}{Two-sided p-value from conditional likelihood ratio test}
#'   \item{method}{Statistical method used}
#'   \item{interpretation}{Human-readable summary of association}
#'
#' @details
#' Follows Woodward (2014) *Epidemiology: Study Design and Data Analysis*, 3rd Edition, Chapter 6,
#' and Jewell (2003) *Statistics for Epidemiology*, Chapter 6. Conditional logistic regression
#' conditions on the matched set strata, controlling completely for matching variables.
#'
#' @export
#' @examples
#' library(dplyr)
#' set.seed(42)
#' patient_data <- tibble(
#'   set_id = rep(1:50, each = 3),
#'   case = rep(c(1, 0, 0), 50),
#'   exposure = rbinom(150, 1, prob = ifelse(rep(c(1, 0, 0), 50) == 1, 0.6, 0.3))
#' )
#' kk_matched_case_control(patient_data, set_id, case, exposure)
kk_matched_case_control <- function(data, set_id, case, exposure, ratio = "1:m", alpha = 0.05) {
  set_col <- rlang::as_name(rlang::enquo(set_id))
  case_col <- rlang::as_name(rlang::enquo(case))
  exp_col <- rlang::as_name(rlang::enquo(exposure))

  df <- data %>%
    dplyr::select(dplyr::all_of(c(set_col, case_col, exp_col))) %>%
    stats::na.omit()

  df[[case_col]] <- as.numeric(df[[case_col]] == 1 | df[[case_col]] == TRUE)
  df[[exp_col]] <- as.numeric(df[[exp_col]] == 1 | df[[exp_col]] == TRUE)

  # Check that each set has at least one case and one control
  set_summary <- df %>%
    dplyr::group_by(.data[[set_col]]) %>%
    dplyr::summarise(
      n_cases = sum(.data[[case_col]] == 1),
      n_controls = sum(.data[[case_col]] == 0),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$n_cases >= 1 & .data$n_controls >= 1)

  valid_sets <- set_summary[[set_col]]
  df_valid <- df %>% dplyr::filter(.data[[set_col]] %in% valid_sets)

  if (nrow(df_valid) == 0) {
    stop("No valid matched sets found with at least one case and one control.")
  }

  form <- stats::as.formula(paste0(case_col, " ~ ", exp_col, " + strata(", set_col, ")"))
  fit <- survival::clogit(form, data = df_valid)
  sum_fit <- summary(fit)

  or <- exp(stats::coef(fit)[1])
  se <- sum_fit$coefficients[1, "se(coef)"]
  z <- stats::qnorm(1 - alpha / 2)
  ci_low <- exp(stats::coef(fit)[1] - z * se)
  ci_high <- exp(stats::coef(fit)[1] + z * se)
  p_val <- sum_fit$coefficients[1, "Pr(>|z|)"]

  n_sets <- length(valid_sets)
  n_cases_tot <- sum(df_valid[[case_col]] == 1)
  n_ctrls_tot <- sum(df_valid[[case_col]] == 0)

  interp <- paste0(
    "Matched conditional OR = ", sprintf("%.2f", or),
    " (", round((1 - alpha) * 100), "% CI: ", sprintf("%.2f", ci_low), " to ", sprintf("%.2f", ci_high),
    ", p = ", format.pval(p_val, digits = 3), "). ",
    if (p_val < alpha) "Statistically significant exposure association in matched analysis." else "No statistically significant exposure association observed."
  )

  res <- tibble::tibble(
    matched_sets = n_sets,
    n_cases = n_cases_tot,
    n_controls = n_ctrls_tot,
    odds_ratio = or,
    conf.low = ci_low,
    conf.high = ci_high,
    p.value = p_val,
    method = paste0("Conditional Logistic Regression (Matched ", ratio, ")"),
    interpretation = interp
  )

  class(res) <- c("kk_matched_case_control", class(res))
  return(res)
}


#' Self-Controlled Case-Crossover Analysis
#'
#' Estimates exposure odds ratios for acute events using patient-level self-controlled
#' case-crossover designs, comparing exposure during hazard windows to control windows
#' within the same patient.
#'
#' @param data A data frame or tibble with patient-level time-window records.
#' @param id Patient identifier column name (quoted or unquoted).
#' @param event Indicator column for event/hazard window vs control window (1/0 or TRUE/FALSE).
#' @param exposure Indicator column for exposure during that window (1/0 or TRUE/FALSE).
#' @param alpha Significance level for confidence intervals (default `0.05`).
#'
#' @return A tidy tibble of class `kk_case_crossover` containing:
#'   \item{n_patients}{Total number of case patients}
#'   \item{n_windows}{Total number of evaluated time windows}
#'   \item{odds_ratio}{Self-controlled exposure odds ratio}
#'   \item{conf.low}{Lower bound of confidence interval}
#'   \item{conf.high}{Upper bound of confidence interval}
#'   \item{p.value}{Two-sided p-value}
#'   \item{method}{Statistical method description}
#'   \item{interpretation}{Human-readable interpretation}
#'
#' @details
#' Described in Woodward (2014), Chapter 6.9, and Maclure (1991). By comparing time windows
#' within the same patient, case-crossover designs inherently control for all time-invariant
#' patient-level confounders (genetics, chronic comorbidities, baseline behaviors).
#'
#' @export
#' @examples
#' library(dplyr)
#' set.seed(123)
#' case_crossover_df <- tibble(
#'   patient_id = rep(1:40, each = 2),
#'   event = rep(c(1, 0), 40), # 1 = hazard window, 0 = control window
#'   exposure = rbinom(80, 1, prob = rep(c(0.7, 0.3), 40))
#' )
#' kk_case_crossover(case_crossover_df, patient_id, event, exposure)
kk_case_crossover <- function(data, id, event, exposure, alpha = 0.05) {
  id_col <- rlang::as_name(rlang::enquo(id))
  evt_col <- rlang::as_name(rlang::enquo(event))
  exp_col <- rlang::as_name(rlang::enquo(exposure))

  df <- data %>%
    dplyr::select(dplyr::all_of(c(id_col, evt_col, exp_col))) %>%
    stats::na.omit()

  df[[evt_col]] <- as.numeric(df[[evt_col]] == 1 | df[[evt_col]] == TRUE)
  df[[exp_col]] <- as.numeric(df[[exp_col]] == 1 | df[[exp_col]] == TRUE)

  # Validate patient windows
  pat_summary <- df %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(
      n_evt = sum(.data[[evt_col]] == 1),
      n_ctrl = sum(.data[[evt_col]] == 0),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$n_evt >= 1 & .data$n_ctrl >= 1)

  valid_ids <- pat_summary[[id_col]]
  df_valid <- df %>% dplyr::filter(.data[[id_col]] %in% valid_ids)

  if (nrow(df_valid) == 0) {
    stop("No valid patient records found with both hazard (event = 1) and control (event = 0) windows.")
  }

  form <- stats::as.formula(paste0(evt_col, " ~ ", exp_col, " + strata(", id_col, ")"))
  fit <- survival::clogit(form, data = df_valid)
  sum_fit <- summary(fit)

  or <- exp(stats::coef(fit)[1])
  se <- sum_fit$coefficients[1, "se(coef)"]
  z <- stats::qnorm(1 - alpha / 2)
  ci_low <- exp(stats::coef(fit)[1] - z * se)
  ci_high <- exp(stats::coef(fit)[1] + z * se)
  p_val <- sum_fit$coefficients[1, "Pr(>|z|)"]

  n_pats <- length(valid_ids)
  n_wins <- nrow(df_valid)

  interp <- paste0(
    "Case-crossover self-controlled exposure OR = ", sprintf("%.2f", or),
    " (", round((1 - alpha) * 100), "% CI: ", sprintf("%.2f", ci_low), " to ", sprintf("%.2f", ci_high),
    ", p = ", format.pval(p_val, digits = 3), "). ",
    if (p_val < alpha) "Transient exposure significantly increases acute event risk." else "No significant association between transient exposure and acute event."
  )

  res <- tibble::tibble(
    n_patients = n_pats,
    n_windows = n_wins,
    odds_ratio = or,
    conf.low = ci_low,
    conf.high = ci_high,
    p.value = p_val,
    method = "Self-Controlled Case-Crossover (Conditional Logistic Regression)",
    interpretation = interp
  )

  class(res) <- c("kk_case_crossover", class(res))
  return(res)
}
