#' Net Reclassification Improvement (NRI) and Integrated Discrimination Improvement (IDI)
#'
#' Evaluates whether adding a new biomarker or risk factor improves patient risk predictions
#' over a baseline model using categorical/continuous NRI and IDI on patient-level data.
#'
#' @param data A data frame or tibble containing patient-level data.
#' @param outcome Column name (quoted or unquoted) for binary patient outcome (1/0 or TRUE/FALSE).
#' @param p_old Column name (quoted or unquoted) for predicted risk probabilities from baseline model.
#' @param p_new Column name (quoted or unquoted) for predicted risk probabilities from new model.
#' @param risk_thresholds Optional numeric vector of risk cutoffs for categorical NRI (e.g. `c(0.10, 0.20)`).
#' @param alpha Significance level for confidence intervals (default `0.05`).
#'
#' @return An S3 object of class `kk_reclassification` containing:
#'   \item{summary}{Tidy tibble with NRI_events, NRI_nonevents, NRI_overall, IDI, 95% CIs, and p-values}
#'   \item{reclass_events}{Reclassification cross-tabulation table for event cases}
#'   \item{reclass_nonevents}{Reclassification cross-tabulation table for non-event controls}
#'
#' @details
#' Implements methods described by Pencina et al. (2008, 2011) and Woodward (2014), Chapter 13.
#' Continuous $NRI_{>0}$ assesses any directionally correct change in predicted probability.
#'
#' @export
#' @examples
#' library(dplyr)
#' set.seed(42)
#' patient_df <- tibble(
#'   outcome = rbinom(200, 1, 0.25),
#'   p_old = runif(200, 0.05, 0.40)
#' ) %>%
#'   mutate(p_new = pmin(0.99, pmax(0.01, p_old + ifelse(outcome == 1, 0.10, -0.05))))
#'
#' kk_reclassification(patient_df, outcome, p_old, p_new, risk_thresholds = c(0.15, 0.30))
kk_reclassification <- function(data, outcome, p_old, p_new, risk_thresholds = NULL, alpha = 0.05) {
  out_col <- rlang::as_name(rlang::enquo(outcome))
  old_col <- rlang::as_name(rlang::enquo(p_old))
  new_col <- rlang::as_name(rlang::enquo(p_new))

  df <- data %>%
    dplyr::select(dplyr::all_of(c(out_col, old_col, new_col))) %>%
    stats::na.omit()

  df[[out_col]] <- as.numeric(df[[out_col]] == 1 | df[[out_col]] == TRUE)

  events <- df %>% dplyr::filter(.data[[out_col]] == 1)
  nonevents <- df %>% dplyr::filter(.data[[out_col]] == 0)

  n_ev <- nrow(events)
  n_ne <- nrow(nonevents)

  if (n_ev < 5 || n_ne < 5) {
    stop("Requires at least 5 events and 5 non-events for reliable NRI/IDI calculation.")
  }

  # Continuous NRI (>0)
  up_ev <- sum(events[[new_col]] > events[[old_col]])
  down_ev <- sum(events[[new_col]] < events[[old_col]])
  nri_ev_cont <- (up_ev - down_ev) / n_ev

  up_ne <- sum(nonevents[[new_col]] > nonevents[[old_col]])
  down_ne <- sum(nonevents[[new_col]] < nonevents[[old_col]])
  nri_ne_cont <- (down_ne - up_ne) / n_ne

  nri_overall_cont <- nri_ev_cont + nri_ne_cont

  se_ev_cont <- sqrt((up_ev + down_ev) / (n_ev^2))
  se_ne_cont <- sqrt((up_ne + down_ne) / (n_ne^2))
  se_tot_cont <- sqrt(se_ev_cont^2 + se_ne_cont^2)

  # IDI calculation
  mean_new_ev <- mean(events[[new_col]])
  mean_old_ev <- mean(events[[old_col]])
  mean_new_ne <- mean(nonevents[[new_col]])
  mean_old_ne <- mean(nonevents[[old_col]])

  idi_val <- (mean_new_ev - mean_old_ev) - (mean_new_ne - mean_old_ne)

  var_ev_old <- stats::var(events[[old_col]]) / n_ev
  var_ev_new <- stats::var(events[[new_col]]) / n_ev
  var_ne_old <- stats::var(nonevents[[old_col]]) / n_ne
  var_ne_new <- stats::var(nonevents[[new_col]]) / n_ne
  se_idi <- sqrt(var_ev_old + var_ev_new + var_ne_old + var_ne_new)

  z <- stats::qnorm(1 - alpha / 2)

  # Categorical NRI if thresholds supplied
  reclass_ev <- NULL
  reclass_ne <- NULL
  nri_cat_tot <- NA_real_

  if (!is.null(risk_thresholds)) {
    cuts <- c(-Inf, sort(risk_thresholds), Inf)
    cat_old_ev <- cut(events[[old_col]], breaks = cuts, labels = FALSE)
    cat_new_ev <- cut(events[[new_col]], breaks = cuts, labels = FALSE)

    cat_old_ne <- cut(nonevents[[old_col]], breaks = cuts, labels = FALSE)
    cat_new_ne <- cut(nonevents[[new_col]], breaks = cuts, labels = FALSE)

    reclass_ev <- table(Old = cat_old_ev, New = cat_new_ev)
    reclass_ne <- table(Old = cat_old_ne, New = cat_new_ne)

    up_ev_cat <- sum(cat_new_ev > cat_old_ev)
    down_ev_cat <- sum(cat_new_ev < cat_old_ev)
    nri_ev_cat <- (up_ev_cat - down_ev_cat) / n_ev

    up_ne_cat <- sum(cat_new_ne > cat_old_ne)
    down_ne_cat <- sum(cat_new_ne < cat_old_ne)
    nri_ne_cat <- (down_ne_cat - up_ne_cat) / n_ne

    nri_cat_tot <- nri_ev_cat + nri_ne_cat
  }

  summary_df <- tibble::tibble(
    metric = c("NRI_events_continuous", "NRI_nonevents_continuous", "NRI_overall_continuous", "IDI"),
    estimate = c(nri_ev_cont, nri_ne_cont, nri_overall_cont, idi_val),
    std.error = c(se_ev_cont, se_ne_cont, se_tot_cont, se_idi),
    conf.low = c(nri_ev_cont - z * se_ev_cont, nri_ne_cont - z * se_ne_cont, nri_overall_cont - z * se_tot_cont, idi_val - z * se_idi),
    conf.high = c(nri_ev_cont + z * se_ev_cont, nri_ne_cont + z * se_ne_cont, nri_overall_cont + z * se_tot_cont, idi_val + z * se_idi),
    p.value = 2 * (1 - stats::pnorm(abs(c(nri_ev_cont / se_ev_cont, nri_ne_cont / se_ne_cont, nri_overall_cont / se_tot_cont, idi_val / se_idi))))
  )

  if (!is.null(risk_thresholds)) {
    summary_df <- dplyr::bind_rows(
      summary_df,
      tibble::tibble(
        metric = "NRI_categorical",
        estimate = nri_cat_tot,
        std.error = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.value = NA_real_
      )
    )
  }

  res <- list(
    summary = summary_df,
    reclass_events = reclass_ev,
    reclass_nonevents = reclass_ne,
    n_events = n_ev,
    n_nonevents = n_ne
  )

  class(res) <- c("kk_reclassification", class(res))
  return(res)
}


#' Model Calibration & Goodness-of-Fit
#'
#' Assesses risk prediction model calibration on patient-level data using the Hosmer-Lemeshow
#' test, Observed-to-Expected (O/E) ratio, calibration slope, and calibration intercept.
#'
#' @param data A data frame or tibble containing patient-level data.
#' @param outcome Column name (quoted or unquoted) for binary patient outcome (1/0 or TRUE/FALSE).
#' @param pred_prob Column name (quoted or unquoted) for predicted probabilities.
#' @param g Number of risk groups/quantiles for calibration evaluation (default `10`).
#' @param method Calibration test method (`"hosmer_lemeshow"` or `"greenwood_nam"`).
#' @param alpha Significance level for confidence intervals (default `0.05`).
#'
#' @return A tidy tibble of class `kk_calibration` containing calibration statistics and slope/intercept estimates.
#'
#' @export
#' @examples
#' library(dplyr)
#' set.seed(123)
#' calib_df <- tibble(
#'   outcome = rbinom(300, 1, 0.20),
#'   pred_prob = runif(300, 0.05, 0.40)
#' )
#' kk_calibration(calib_df, outcome, pred_prob, g = 10)
kk_calibration <- function(data, outcome, pred_prob, g = 10, method = c("hosmer_lemeshow", "greenwood_nam"), alpha = 0.05) {
  method <- match.arg(method)
  out_col <- rlang::as_name(rlang::enquo(outcome))
  pred_col <- rlang::as_name(rlang::enquo(pred_prob))

  df <- data %>%
    dplyr::select(dplyr::all_of(c(out_col, pred_col))) %>%
    stats::na.omit()

  df[[out_col]] <- as.numeric(df[[out_col]] == 1 | df[[out_col]] == TRUE)
  p_vec <- pmax(1e-5, pmin(1 - 1e-5, df[[pred_col]]))

  # Risk group deciles
  groups <- cut(p_vec, breaks = stats::quantile(p_vec, probs = seq(0, 1, length.out = g + 1)), include.lowest = TRUE, labels = FALSE)

  group_df <- df %>%
    dplyr::mutate(group = groups) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarise(
      n = dplyr::n(),
      observed = sum(.data[[out_col]]),
      expected = sum(.data[[pred_col]]),
      mean_p = mean(.data[[pred_col]]),
      .groups = "drop"
    )

  # Hosmer-Lemeshow statistic
  hl_stat <- sum(((group_df$observed - group_df$expected)^2) / (group_df$n * group_df$mean_p * (1 - group_df$mean_p)))
  df_hl <- g - 2
  p_hl <- 1 - stats::pchisq(hl_stat, df = df_hl)

  # O/E Ratio
  total_obs <- sum(group_df$observed)
  total_exp <- sum(group_df$expected)
  oe_ratio <- total_obs / total_exp

  # Calibration slope & intercept
  logit_p <- stats::qlogis(p_vec)
  calib_slope_fit <- stats::glm(df[[out_col]] ~ logit_p, family = stats::binomial())
  calib_slope <- stats::coef(calib_slope_fit)[2]

  calib_intercept_fit <- stats::glm(df[[out_col]] ~ 1 + offset(logit_p), family = stats::binomial())
  calib_intercept <- stats::coef(calib_intercept_fit)[1]

  interp <- paste0(
    "Calibration Assessment (g = ", g, " risk groups):\n",
    "Hosmer-Lemeshow Chi2 = ", sprintf("%.2f", hl_stat), " (df = ", df_hl, ", p = ", format.pval(p_hl, digits = 3), "). ",
    if (p_hl >= alpha) "Good calibration (no significant departure from perfect calibration)." else "Poor calibration (significant departure observed).",
    " Total O/E ratio = ", sprintf("%.2f", oe_ratio),
    ", Calibration slope = ", sprintf("%.2f", calib_slope),
    ", Calibration intercept = ", sprintf("%.2f", calib_intercept), "."
  )

  res <- tibble::tibble(
    g_groups = g,
    hl_chi2 = hl_stat,
    df = df_hl,
    p.value = p_hl,
    oe_ratio = oe_ratio,
    calib_slope = calib_slope,
    calib_intercept = calib_intercept,
    method = paste0("Hosmer-Lemeshow Calibration Test (g = ", g, ")"),
    interpretation = interp
  )

  class(res) <- c("kk_calibration", class(res))
  return(res)
}
