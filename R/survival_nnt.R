# ============================================================
# TIME-VARYING SURVIVAL NNT MODELLING
# ============================================================

#' Time-Varying Number Needed to Treat (NNT) from Survival Data
#'
#' Computes time-varying Absolute Risk Reduction (ARR) and Number Needed to Treat (NNT) or Harm (NNH)
#' from Kaplan-Meier survival curves at specified time points, with Greenwood-based confidence intervals.
#'
#' @param data A data frame or tibble containing survival data.
#' @param time Column name (quoted or unquoted) for time-to-event.
#' @param status Column name (quoted or unquoted) for event status (1/0, TRUE/FALSE).
#' @param group Column name (quoted or unquoted) for binary treatment/exposure group.
#' @param times Numeric vector of evaluation times (default `NULL`, which evaluates at 25th, 50th, and 75th percentiles of event times).
#' @param conf.level Significance level for confidence intervals (default `0.95`).
#'
#' @return A tidy tibble of class `kk_survival_nnt` containing:
#'   \describe{
#'     \item{time}{Evaluation time point}
#'     \item{S_control}{Kaplan-Meier survival probability in control group}
#'     \item{S_treated}{Kaplan-Meier survival probability in treated group}
#'     \item{ARR}{Absolute Risk Reduction}
#'     \item{ARR_low}{Lower bound of ARR 95% CI}
#'     \item{ARR_high}{Upper bound of ARR 95% CI}
#'     \item{Metric}{"NNT" (if ARR > 0) or "NNH" (if ARR < 0)}
#'     \item{NNT}{Estimated Number Needed to Treat/Harm}
#'     \item{NNT_low}{Lower bound of NNT 95% CI}
#'     \item{NNT_high}{Upper bound of NNT 95% CI}
#'     \item{Note}{Significance or CI interpretation}
#'   }
#'
#' @details
#' Follows Woodward (2014) *Epidemiology: Study Design and Data Analysis*, 3rd Edition, Chapter 8.
#' Fits Kaplan-Meier curves for control and treatment groups via `survival::survfit`. Standard errors of
#' survival probabilities use Greenwood's formula, propagated to ARR and NNT.
#'
#' @export
#' @examples
#' library(survival)
#' set.seed(42)
#' surv_df <- data.frame(
#'   time = rexp(200, rate = rep(c(0.05, 0.02), each = 100)),
#'   status = rbinom(200, 1, 0.8),
#'   rx = rep(c("Control", "Treated"), each = 100)
#' )
#' kk_survival_nnt(surv_df, time, status, rx, times = c(10, 20, 30))
kk_survival_nnt <- function(data, time, status, group, times = NULL, conf.level = 0.95) {
  if (dplyr::is_grouped_df(data)) {
    return(.kk_by_group(data, match.call(), parent.frame()))
  }

  validate_data_frame(data)
  
  time_col <- rlang::as_name(rlang::enquo(time))
  status_col <- rlang::as_name(rlang::enquo(status))
  group_col <- rlang::as_name(rlang::enquo(group))
  
  cols_needed <- c(time_col, status_col, group_col)
  if (!all(cols_needed %in% names(data))) {
    stop("One or more specified columns were not found in data.")
  }
  
  df <- data %>%
    dplyr::select(dplyr::all_of(cols_needed)) %>%
    stats::na.omit()
  
  df[[status_col]] <- as.numeric(df[[status_col]] == 1 | df[[status_col]] == TRUE)
  grp_factor <- factor(df[[group_col]])
  grp_levels <- levels(grp_factor)
  
  if (length(grp_levels) != 2) {
    stop("Group variable must have exactly two distinct levels.")
  }
  
  control_lbl <- grp_levels[1]
  treat_lbl <- grp_levels[2]
  
  eval_times <- if (is.null(times)) {
    event_times <- df[[time_col]][df[[status_col]] == 1]
    if (length(event_times) == 0) event_times <- df[[time_col]]
    as.numeric(stats::quantile(event_times, probs = c(0.25, 0.50, 0.75), na.rm = TRUE))
  } else {
    as.numeric(times)
  }
  eval_times <- sort(unique(eval_times))
  
  # Fit KM curves
  surv_obj <- survival::Surv(df[[time_col]], df[[status_col]])
  km_fit <- survival::survfit(surv_obj ~ grp_factor, data = df)
  
  sum_km <- summary(km_fit, times = eval_times, extend = TRUE)
  
  # Group level assignments in summary output
  strata_clean <- gsub("^grp_factor=", "", sum_km$strata)
  
  res_list <- vector("list", length(eval_times))
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  
  for (i in seq_along(eval_times)) {
    t_val <- eval_times[i]
    
    idx_ctrl <- which(sum_km$time == t_val & strata_clean == control_lbl)
    idx_treat <- which(sum_km$time == t_val & strata_clean == treat_lbl)
    
    s_ctrl <- if (length(idx_ctrl) > 0) sum_km$surv[idx_ctrl[1]] else NA_real_
    se_ctrl <- if (length(idx_ctrl) > 0) sum_km$std.err[idx_ctrl[1]] else NA_real_
    
    s_treat <- if (length(idx_treat) > 0) sum_km$surv[idx_treat[1]] else NA_real_
    se_treat <- if (length(idx_treat) > 0) sum_km$std.err[idx_treat[1]] else NA_real_
    
    arr <- s_treat - s_ctrl
    se_arr <- sqrt(se_ctrl^2 + se_treat^2)
    
    arr_low <- arr - z * se_arr
    arr_high <- arr + z * se_arr
    
    abs_arr <- abs(arr)
    nnt_val <- if (!is.na(abs_arr) && abs_arr > 0) 1 / abs_arr else Inf
    metric_label <- if (!is.na(arr) && arr >= 0) "NNT" else "NNH"
    
    if (is.na(arr_low) || is.na(arr_high)) {
      nnt_low <- NA_real_
      nnt_high <- NA_real_
      note <- "Insufficient data"
    } else if (sign(arr_low) != sign(arr_high)) {
      nnt_low <- -Inf
      nnt_high <- Inf
      note <- "CI crosses null (infinity)"
    } else {
      bounds <- 1 / c(arr_high, arr_low)
      nnt_low <- min(abs(bounds))
      nnt_high <- max(abs(bounds))
      note <- "Significant"
    }
    
    res_list[[i]] <- tibble::tibble(
      time = t_val,
      S_control = s_ctrl,
      S_treated = s_treat,
      ARR = arr,
      ARR_low = arr_low,
      ARR_high = arr_high,
      Metric = metric_label,
      NNT = nnt_val,
      NNT_low = nnt_low,
      NNT_high = nnt_high,
      Note = note
    )
  }
  
  res <- dplyr::bind_rows(res_list)
  class(res) <- c("kk_survival_nnt", class(res))
  return(res)
}
