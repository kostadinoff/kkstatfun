#' Cluster-Randomized Trial Analysis
#'
#' Estimates treatment effects for cluster-randomized trials on patient-level data,
#' adjusting standard errors for intra-cluster correlation (ICC) and reporting design
#' effects (DEFF) and effective sample size.
#'
#' @param data A data frame or tibble containing patient-level data.
#' @param outcome Column name (quoted or unquoted) for outcome variable (continuous or binary 1/0).
#' @param treatment Column name (quoted or unquoted) for treatment arm (1/0 or factor).
#' @param cluster Column name (quoted or unquoted) identifying cluster unit (e.g. hospital, clinic, village).
#' @param alpha Significance level for confidence intervals (default `0.05`).
#'
#' @return A tidy tibble of class `kk_cluster_trial` containing:
#'   \item{n_clusters}{Total number of clusters}
#'   \item{n_patients}{Total number of patients}
#'   \item{estimate}{Cluster-adjusted treatment effect}
#'   \item{se_unadjusted}{Standard error ignoring clustering}
#'   \item{se_cluster}{Cluster-robust standard error}
#'   \item{conf.low}{Lower 95% confidence limit}
#'   \item{conf.high}{Upper 95% confidence limit}
#'   \item{p.value}{Cluster-adjusted two-sided p-value}
#'   \item{icc}{Intra-cluster correlation coefficient}
#'   \item{deff}{Design effect multiplier}
#'   \item{effective_n}{Effective sample size accounting for clustering}
#'   \item{interpretation}{Human-readable interpretation of trial results}
#'
#' @details
#' Described in Woodward (2014) *Epidemiology: Study Design and Data Analysis*, 3rd Edition, Chapter 7.4.2.
#' Adjusts variance estimates using cluster sandwich estimators (`sandwich::vcovCL`) and computes
#' intra-cluster correlation from ANOVA variance components.
#'
#' @export
#' @examples
#' library(dplyr)
#' set.seed(42)
#' cluster_df <- tibble(
#'   cluster_id = rep(1:20, each = 15),
#'   treatment = rep(rep(c(1, 0), each = 10), each = 15),
#'   outcome = rnorm(300, mean = 50 + rep(rep(c(1, 0), each = 10), each = 15) * 5 + rep(rnorm(20, 0, 2), each = 15), sd = 4)
#' )
#' kk_cluster_trial(cluster_df, outcome, treatment, cluster_id)
kk_cluster_trial <- function(data, outcome, treatment, cluster, alpha = 0.05) {
  out_col <- rlang::as_name(rlang::enquo(outcome))
  trt_col <- rlang::as_name(rlang::enquo(treatment))
  cls_col <- rlang::as_name(rlang::enquo(cluster))

  df <- data %>%
    dplyr::select(dplyr::all_of(c(out_col, trt_col, cls_col))) %>%
    stats::na.omit()

  df[[trt_col]] <- as.numeric(df[[trt_col]] == 1 | df[[trt_col]] == TRUE)

  n_clusters <- length(unique(df[[cls_col]]))
  n_patients <- nrow(df)

  if (n_clusters < 4) {
    stop("Cluster RCT analysis requires at least 4 clusters.")
  }

  is_binary <- length(unique(df[[out_col]])) == 2 && all(df[[out_col]] %in% c(0, 1))

  form <- stats::as.formula(paste0(out_col, " ~ ", trt_col))

  if (is_binary) {
    fit <- stats::glm(form, data = df, family = stats::binomial())
  } else {
    fit <- stats::lm(form, data = df)
  }

  # Unadjusted SE
  sum_fit <- summary(fit)
  est <- stats::coef(fit)[2]
  se_unadj <- sum_fit$coefficients[2, 2]

  # Cluster-robust SE using sandwich::vcovCL
  vcov_cl <- sandwich::vcovCL(fit, cluster = df[[cls_col]])
  se_clust <- sqrt(vcov_cl[2, 2])

  z <- stats::qnorm(1 - alpha / 2)
  ci_low <- est - z * se_clust
  ci_high <- est + z * se_clust
  p_val <- 2 * (1 - stats::pnorm(abs(est / se_clust)))

  # ICC calculation via one-way ANOVA variance components
  anova_fit <- stats::lm(stats::as.formula(paste0(out_col, " ~ factor(", cls_col, ")")), data = df)
  anova_tbl <- stats::anova(anova_fit)

  ms_b <- anova_tbl[1, "Mean Sq"]
  ms_w <- anova_tbl[2, "Mean Sq"]

  m_avg <- n_patients / n_clusters
  icc_val <- max(0, (ms_b - ms_w) / (ms_b + (m_avg - 1) * ms_w))
  deff_val <- 1 + (m_avg - 1) * icc_val
  eff_n <- round(n_patients / deff_val)

  interp <- paste0(
    "Cluster RCT Analysis (", n_clusters, " clusters, N = ", n_patients, " patients):\n",
    "Treatment effect = ", sprintf("%.2f", est),
    " (Cluster-robust 95% CI: ", sprintf("%.2f", ci_low), " to ", sprintf("%.2f", ci_high),
    ", p = ", format.pval(p_val, digits = 3), "). ",
    "ICC = ", sprintf("%.3f", icc_val), ", DEFF = ", sprintf("%.2f", deff_val),
    " (Effective N = ", eff_n, ")."
  )

  res <- tibble::tibble(
    n_clusters = n_clusters,
    n_patients = n_patients,
    estimate = est,
    se_unadjusted = se_unadj,
    se_cluster = se_clust,
    conf.low = ci_low,
    conf.high = ci_high,
    p.value = p_val,
    icc = icc_val,
    deff = deff_val,
    effective_n = eff_n,
    method = paste0("Cluster-Adjusted ", if (is_binary) "Logistic" else "Linear", " Regression (Sandwich vcovCL)"),
    interpretation = interp
  )

  class(res) <- c("kk_cluster_trial", class(res))
  return(res)
}


#' 2x2 Crossover Trial Analysis (Grizzle Model)
#'
#' Performs Grizzle's two-stage statistical analysis for 2x2 crossover trials on patient-level data,
#' testing for sequence/carryover effects, period effects, and direct treatment effects.
#'
#' @param data A data frame or tibble containing patient-level trial data.
#' @param id Column name (quoted or unquoted) for patient identifier.
#' @param sequence Column name (quoted or unquoted) for treatment sequence (e.g. `"AB"` vs `"BA"`).
#' @param period1 Column name (quoted or unquoted) for outcome measurement in Period 1.
#' @param period2 Column name (quoted or unquoted) for outcome measurement in Period 2.
#' @param alpha Significance level for confidence intervals (default `0.05`).
#'
#' @return A tidy tibble of class `kk_crossover_trial` containing carryover test, period test, and treatment effect estimates.
#'
#' @details
#' Implements Grizzle's 2-stage crossover trial model as outlined in Woodward (2014), Chapter 7.5.
#' Stage 1 checks for carryover (sequence) effect using patient sum scores ($Y_1 + Y_2$).
#' Stage 2 evaluates period differences ($Y_1 - Y_2$) and derives the direct treatment effect.
#'
#' @export
#' @examples
#' library(dplyr)
#' set.seed(123)
#' crossover_df <- tibble(
#'   patient_id = 1:30,
#'   sequence = rep(c("AB", "BA"), each = 15),
#'   period1 = rnorm(30, mean = 100, sd = 10),
#'   period2 = rnorm(30, mean = ifelse(sequence == "AB", 90, 105), sd = 10)
#' )
#' kk_crossover_trial(crossover_df, patient_id, sequence, period1, period2)
kk_crossover_trial <- function(data, id, sequence, period1, period2, alpha = 0.05) {
  id_col <- rlang::as_name(rlang::enquo(id))
  seq_col <- rlang::as_name(rlang::enquo(sequence))
  p1_col <- rlang::as_name(rlang::enquo(period1))
  p2_col <- rlang::as_name(rlang::enquo(period2))

  df <- data %>%
    dplyr::select(dplyr::all_of(c(id_col, seq_col, p1_col, p2_col))) %>%
    stats::na.omit()

  seq_vals <- unique(df[[seq_col]])
  if (length(seq_vals) != 2) {
    stop("Crossover trial requires exactly 2 sequences (e.g. 'AB' and 'BA').")
  }

  seq1 <- seq_vals[1]
  seq2 <- seq_vals[2]

  df$patient_sum <- df[[p1_col]] + df[[p2_col]]
  df$patient_diff <- (df[[p1_col]] - df[[p2_col]]) / 2

  # Stage 1: Carryover / Sequence effect test on patient sums
  t_carryover <- stats::t.test(patient_sum ~ get(seq_col), data = df)
  p_carryover <- t_carryover$p.value

  # Stage 2: Period effect test on patient diffs
  t_period <- stats::t.test(patient_diff ~ get(seq_col), data = df)
  p_period <- t_period$p.value

  # Direct Treatment effect estimate
  # Difference in diffs between sequences
  grp1 <- df %>% dplyr::filter(.data[[seq_col]] == seq1)
  grp2 <- df %>% dplyr::filter(.data[[seq_col]] == seq2)

  mean_diff1 <- mean(grp1$patient_diff)
  mean_diff2 <- mean(grp2$patient_diff)

  trt_est <- mean_diff1 - mean_diff2

  n1 <- nrow(grp1)
  n2 <- nrow(grp2)
  var1 <- stats::var(grp1$patient_diff)
  var2 <- stats::var(grp2$patient_diff)

  sp2 <- ((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2)
  se_trt <- sqrt(sp2 * (1 / n1 + 1 / n2))

  df_trt <- n1 + n2 - 2
  t_crit <- stats::qt(1 - alpha / 2, df = df_trt)

  ci_low <- trt_est - t_crit * se_trt
  ci_high <- trt_est + t_crit * se_trt
  p_trt <- 2 * (1 - stats::pt(abs(trt_est / se_trt), df = df_trt))

  interp <- paste0(
    "2x2 Crossover Trial Analysis (N = ", n1 + n2, " patients):\n",
    "Carryover/Sequence effect test p = ", format.pval(p_carryover, digits = 3),
    if (p_carryover < 0.10) " (WARNING: Significant carryover effect detected; treatment effect may be confounded)." else " (No significant carryover effect).", "\n",
    "Treatment effect = ", sprintf("%.2f", trt_est),
    " (95% CI: ", sprintf("%.2f", ci_low), " to ", sprintf("%.2f", ci_high),
    ", p = ", format.pval(p_trt, digits = 3), ")."
  )

  res <- tibble::tibble(
    n_patients = n1 + n2,
    seq1_label = as.character(seq1),
    seq2_label = as.character(seq2),
    carryover_p = p_carryover,
    period_p = p_period,
    treatment_effect = trt_est,
    std.error = se_trt,
    conf.low = ci_low,
    conf.high = ci_high,
    p.value = p_trt,
    method = "Grizzle 2-Stage 2x2 Crossover Analysis",
    interpretation = interp
  )

  class(res) <- c("kk_crossover_trial", class(res))
  return(res)
}
