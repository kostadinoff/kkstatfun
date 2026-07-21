#' Epidemiological Sample Size & Power Calculations
#'
#' Computes sample size and power for epidemiological study designs including cohort
#' studies, unmatched case-control studies, 1:m matched case-control studies, and
#' cluster-randomized trials with design effect adjustments.
#'
#' @param design Study design: `"cohort"`, `"case_control_unmatched"`, `"case_control_matched"`, or `"cluster_trial"`.
#' @param p0 Baseline probability of disease in unexposed / controls (between 0 and 1).
#' @param rr_or Target Relative Risk ($RR$) for cohort/trial or Odds Ratio ($OR$) for case-control studies.
#' @param power Target statistical power (default `0.80`).
#' @param alpha Type I error rate (default `0.05`).
#' @param ratio Ratio of unexposed to exposed subjects or controls per case (default `1`).
#' @param m Matching ratio (controls per case for matched designs) or cluster size (for cluster trials). Default `1`.
#' @param icc Intra-cluster correlation coefficient ($\rho$) for cluster-randomized trials (default `0`).
#'
#' @return A tidy tibble of class `kk_sample_size_epi` containing:
#'   \item{design}{Study design specified}
#'   \item{p0}{Baseline risk in unexposed / control group}
#'   \item{p1}{Implied risk in exposed / case group}
#'   \item{target_effect}{Target RR or OR}
#'   \item{alpha}{Type I error rate}
#'   \item{power}{Target statistical power}
#'   \item{n_group1}{Required sample size for Group 1 (Exposed / Cases)}
#'   \item{n_group2}{Required sample size for Group 2 (Unexposed / Controls)}
#'   \item{n_total}{Total individual sample size}
#'   \item{n_clusters}{Required number of clusters (if cluster trial)}
#'   \item{interpretation}{Human-readable summary of sample size requirements}
#'
#' @details
#' Implements formulas from Woodward (2014) *Epidemiology: Study Design and Data Analysis*, 3rd Edition, Chapter 8,
#' and Fleiss et al. (2003). For cluster-randomized trials, individual sample size is adjusted by the design effect
#' $DEFF = 1 + (m - 1) \times \text{icc}$.
#'
#' @export
#' @examples
#' # Cohort study sample size calculation
#' kk_sample_size_epi(design = "cohort", p0 = 0.15, rr_or = 1.5, power = 0.80)
#'
#' # 1:2 Matched Case-Control Study
#' kk_sample_size_epi(design = "case_control_matched", p0 = 0.20, rr_or = 2.0, ratio = 2, m = 2)
#'
#' # Cluster RCT sample size
#' kk_sample_size_epi(design = "cluster_trial", p0 = 0.10, rr_or = 0.60, m = 25, icc = 0.03)
kk_sample_size_epi <- function(design = c("cohort", "case_control_unmatched", "case_control_matched", "cluster_trial"),
                               p0, rr_or, power = 0.80, alpha = 0.05, ratio = 1, m = 1, icc = 0) {
  design <- match.arg(design)

  if (p0 <= 0 || p0 >= 1) stop("p0 must be strictly between 0 and 1.")
  if (rr_or <= 0) stop("rr_or must be greater than 0.")
  if (power <= 0 || power >= 1) stop("power must be between 0 and 1.")

  z_alpha <- stats::qnorm(1 - alpha / 2)
  z_beta <- stats::qnorm(power)

  n_clusters <- NA_integer_

  if (design == "cohort") {
    p1 <- p0 * rr_or
    if (p1 >= 1) stop("Implied risk p1 (p0 * RR) exceeds 1. Reduce p0 or RR.")
    p_avg <- (p1 + ratio * p0) / (1 + ratio)
    q_avg <- 1 - p_avg
    q0 <- 1 - p0
    q1 <- 1 - p1

    n_exp <- ((z_alpha * sqrt((1 + 1 / ratio) * p_avg * q_avg) +
      z_beta * sqrt(p1 * q1 + p0 * q0 / ratio))^2) / ((p1 - p0)^2)

    n_exp <- ceiling(n_exp)
    n_unexp <- ceiling(n_exp * ratio)
    n_tot <- n_exp + n_unexp

    interp <- paste0(
      "For a cohort study (p0 = ", p0, ", target RR = ", rr_or, ", power = ", round(power * 100), "%):\n",
      "Requires ", n_exp, " exposed and ", n_unexp, " unexposed subjects (Total N = ", n_tot, ")."
    )
  } else if (design == "case_control_unmatched") {
    # Compute p1 exposure prevalence in cases from p0 in controls and OR
    p1 <- (p0 * rr_or) / (1 - p0 + p0 * rr_or)
    p_avg <- (p1 + ratio * p0) / (1 + ratio)
    q_avg <- 1 - p_avg
    q0 <- 1 - p0
    q1 <- 1 - p1

    n_cases <- ((z_alpha * sqrt((1 + 1 / ratio) * p_avg * q_avg) +
      z_beta * sqrt(p1 * q1 + p0 * q0 / ratio))^2) / ((p1 - p0)^2)

    n_cases <- ceiling(n_cases)
    n_controls <- ceiling(n_cases * ratio)
    n_tot <- n_cases + n_controls

    interp <- paste0(
      "For an unmatched case-control study (p0 = ", p0, ", target OR = ", rr_or, ", power = ", round(power * 100), "%):\n",
      "Requires ", n_cases, " cases and ", n_controls, " controls (Total N = ", n_tot, ")."
    )
  } else if (design == "case_control_matched") {
    # Matched case-control sample size using 1:m ratio
    p1 <- (p0 * rr_or) / (1 - p0 + p0 * rr_or)
    # Discordant probability approximation
    p_disc <- p0 * (1 - p1) + p1 * (1 - p0)
    p_disc <- max(p_disc, 0.05)

    psi <- rr_or
    p_pos <- psi / (1 + psi)
    n_pairs <- ((z_alpha / 2 + z_beta * sqrt(p_pos * (1 - p_pos)))^2) / ((p_pos - 0.5)^2 * p_disc)

    # Adjust for 1:m matching efficiency factor (m+1)/(2m)
    efficiency_factor <- (m + 1) / (2 * m)
    n_cases <- ceiling(n_pairs * efficiency_factor)
    n_controls <- ceiling(n_cases * m)
    n_tot <- n_cases + n_controls

    interp <- paste0(
      "For a 1:", m, " matched case-control study (p0 = ", p0, ", target OR = ", rr_or, ", power = ", round(power * 100), "%):\n",
      "Requires ", n_cases, " cases and ", n_controls, " matched controls (Total N = ", n_tot, ")."
    )
  } else if (design == "cluster_trial") {
    # Cluster trial with design effect DEFF
    p1 <- p0 * rr_or
    if (p1 >= 1) stop("Implied risk p1 exceeds 1.")
    p_avg <- (p1 + p0) / 2
    q_avg <- 1 - p_avg
    q0 <- 1 - p0
    q1 <- 1 - p1

    # Individual trial N per group
    n_ind_per_grp <- ((z_alpha * sqrt(2 * p_avg * q_avg) + z_beta * sqrt(p1 * q1 + p0 * q0))^2) / ((p1 - p0)^2)

    deff <- 1 + (m - 1) * icc
    n_adj_per_grp <- ceiling(n_ind_per_grp * deff)
    n_tot <- n_adj_per_grp * 2
    n_clusters <- ceiling(n_tot / m)

    interp <- paste0(
      "For a cluster RCT (cluster size m = ", m, ", ICC = ", icc, ", DEFF = ", sprintf("%.2f", deff), "):\n",
      "Requires ", n_clusters, " clusters (Total N = ", n_tot, " individual subjects)."
    )
  }

  res <- tibble::tibble(
    design = design,
    p0 = p0,
    p1 = p1,
    target_effect = rr_or,
    alpha = alpha,
    power = power,
    n_group1 = ifelse(design %in% c("cohort", "cluster_trial"), ceiling(n_tot / (1 + ratio)), ceiling(n_tot / (1 + ratio))),
    n_group2 = ifelse(design %in% c("cohort", "cluster_trial"), ceiling(n_tot * ratio / (1 + ratio)), ceiling(n_tot * ratio / (1 + ratio))),
    n_total = n_tot,
    n_clusters = n_clusters,
    interpretation = interp
  )

  class(res) <- c("kk_sample_size_epi", class(res))
  return(res)
}
