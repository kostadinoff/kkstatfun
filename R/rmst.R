# ============================================================
# RESTRICTED MEAN SURVIVAL TIME
# ============================================================

#' Restricted Mean Survival Time (KK)
#'
#' @description Computes the restricted mean survival time (RMST) up to a
#'   horizon `tau` for each group and the between-group difference and ratio.
#'   RMST is the area under the Kaplan-Meier curve up to `tau` (mean event-free
#'   time within the first `tau` units) and is a useful, assumption-light
#'   alternative to the hazard ratio when the proportional-hazards assumption
#'   is in doubt.
#'
#' @param data Data frame.
#' @param time Follow-up time column (bare name or string).
#' @param status Event indicator column (bare name or string); 0/1.
#' @param group Optional two-level grouping column (bare name or string). If
#'   omitted, a single overall RMST is returned.
#' @param tau Restriction horizon. If NULL (default), the largest time at which
#'   all groups still have subjects at risk (the minimum of the group-specific
#'   maximum follow-up times) is used.
#' @param conf.level Confidence level (default 0.95).
#'
#' @return Tibble with the RMST and CI for each group and, when two groups are
#'   supplied, a row for the RMST difference and ratio with CIs and p-values.
#'
#' @examples
#' \dontrun{
#' library(survival)
#' # Time-to-death by sex, restricted to 1 year
#' kk_rmst(lung, time, status, sex, tau = 365)
#' }
#'
#' @export
kk_rmst <- function(data, time, status, group = NULL, tau = NULL,
                    conf.level = 0.95) {
              validate_data_frame(data)
              if (!requireNamespace("survival", quietly = TRUE)) {
                            stop("Package 'survival' is required for kk_rmst().")
              }

              time_name <- .kk_colname(rlang::enquo(time))
              status_name <- .kk_colname(rlang::enquo(status))
              group_quo <- rlang::enquo(group)
              has_group <- !rlang::quo_is_null(group_quo)
              group_name <- if (has_group) .kk_colname(group_quo) else NULL

              need <- c(time_name, status_name, group_name)
              missing_cols <- setdiff(need, names(data))
              if (length(missing_cols) > 0) {
                            stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
              }

              tt <- data[[time_name]]
              ss <- data[[status_name]]
              z <- stats::qnorm(1 - (1 - conf.level) / 2)

              # Determine tau: largest time at which every group is still at risk
              if (is.null(tau)) {
                            if (has_group) {
                                          tau <- min(tapply(tt, data[[group_name]], max, na.rm = TRUE))
                            } else {
                                          tau <- max(tt, na.rm = TRUE)
                            }
              }

              # RMST for a single group via survival's built-in restricted mean
              # (area under the KM curve up to tau, with its standard error).
              rmst_one <- function(t, s) {
                            fit <- survival::survfit(survival::Surv(t, s) ~ 1)
                            tab <- summary(fit, rmean = tau)$table
                            rmst <- tab[["rmean"]]
                            se_rmst <- tab[["se(rmean)"]]
                            c(rmst = rmst, se = se_rmst)
              }

              if (!has_group) {
                            r <- rmst_one(tt, ss)
                            return(tibble::tibble(
                                          group = "overall",
                                          tau = tau,
                                          rmst = r[["rmst"]],
                                          se = r[["se"]],
                                          conf.low = r[["rmst"]] - z * r[["se"]],
                                          conf.high = r[["rmst"]] + z * r[["se"]],
                                          conf.level = conf.level
                            ))
              }

              g <- data[[group_name]]
              levs <- if (is.factor(g)) levels(g) else sort(unique(stats::na.omit(g)))
              if (length(levs) != 2) {
                            stop("`group` must have exactly two levels for a difference; found ",
                                 length(levs), ".")
              }

              r1 <- rmst_one(tt[g == levs[1]], ss[g == levs[1]])
              r2 <- rmst_one(tt[g == levs[2]], ss[g == levs[2]])

              per_group <- tibble::tibble(
                            group = as.character(levs),
                            tau = tau,
                            rmst = c(r1[["rmst"]], r2[["rmst"]]),
                            se = c(r1[["se"]], r2[["se"]]),
                            conf.low = c(r1[["rmst"]] - z * r1[["se"]], r2[["rmst"]] - z * r2[["se"]]),
                            conf.high = c(r1[["rmst"]] + z * r1[["se"]], r2[["rmst"]] + z * r2[["se"]]),
                            p.value = NA_real_,
                            conf.level = conf.level
              )

              # Difference (group2 - group1)
              diff_est <- r2[["rmst"]] - r1[["rmst"]]
              diff_se <- sqrt(r1[["se"]]^2 + r2[["se"]]^2)
              diff_p <- 2 * stats::pnorm(-abs(diff_est / diff_se))

              # Ratio (group2 / group1), CI on log scale
              ratio_est <- r2[["rmst"]] / r1[["rmst"]]
              log_ratio_se <- sqrt((r1[["se"]] / r1[["rmst"]])^2 + (r2[["se"]] / r2[["rmst"]])^2)
              ratio_p <- 2 * stats::pnorm(-abs(log(ratio_est) / log_ratio_se))

              contrasts <- tibble::tibble(
                            group = c(
                                          sprintf("RMST difference (%s - %s)", levs[2], levs[1]),
                                          sprintf("RMST ratio (%s / %s)", levs[2], levs[1])
                            ),
                            tau = tau,
                            rmst = c(diff_est, ratio_est),
                            se = c(diff_se, NA_real_),
                            conf.low = c(diff_est - z * diff_se,
                                         exp(log(ratio_est) - z * log_ratio_se)),
                            conf.high = c(diff_est + z * diff_se,
                                          exp(log(ratio_est) + z * log_ratio_se)),
                            p.value = c(diff_p, ratio_p),
                            conf.level = conf.level
              )

              dplyr::bind_rows(per_group, contrasts)
}

#' Competing-Risks Cumulative Incidence (KK)
#'
#' @description Estimates the cause-specific cumulative incidence function (CIF)
#'   for competing risks using the Aalen-Johansen estimator (`survival::survfit`
#'   for a multi-state outcome). When groups are supplied and the `cmprsk`
#'   package is installed, Gray's test for equality of CIFs across groups is
#'   also reported.
#'
#' @param data Data frame.
#' @param time Follow-up time column (bare name or string).
#' @param status Event-type column (bare name or string). 0 = censored, 1 = event
#'   of interest, 2+ = competing events. May also be a factor whose first level
#'   is censoring.
#' @param group Optional grouping column (bare name or string).
#' @param cause Code of the event of interest (default 1).
#' @param times Optional vector of time points at which to report the CIF; if
#'   NULL, the CIF at the largest common event time is returned.
#'
#' @return Tibble of cumulative incidence estimates (with standard errors) for
#'   the event of interest, by group and time point, plus an attribute
#'   `gray_test` containing Gray's test result when available.
#'
#' @examples
#' \dontrun{
#' # 0 = censored, 1 = relapse (of interest), 2 = death without relapse
#' df <- data.frame(
#'   time = rexp(200, 0.1),
#'   status = sample(0:2, 200, replace = TRUE),
#'   arm = rep(c("A", "B"), each = 100)
#' )
#' kk_cuminc(df, time, status, arm, cause = 1)
#' }
#'
#' @export
kk_cuminc <- function(data, time, status, group = NULL, cause = 1, times = NULL) {
              validate_data_frame(data)
              if (!requireNamespace("survival", quietly = TRUE)) {
                            stop("Package 'survival' is required for kk_cuminc().")
              }

              time_name <- .kk_colname(rlang::enquo(time))
              status_name <- .kk_colname(rlang::enquo(status))
              group_quo <- rlang::enquo(group)
              has_group <- !rlang::quo_is_null(group_quo)
              group_name <- if (has_group) .kk_colname(group_quo) else NULL

              need <- c(time_name, status_name, group_name)
              missing_cols <- setdiff(need, names(data))
              if (length(missing_cols) > 0) {
                            stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
              }

              tt <- data[[time_name]]
              raw_status <- data[[status_name]]
              # Build a competing-risks factor: first level = censor
              status_fac <- factor(raw_status)
              event_state <- as.character(cause)

              extract_cif <- function(idx, glabel) {
                            t_i <- tt[idx]
                            # Competing-risks factor: first level is treated as
                            # censoring (avoids the deprecated type = "mstate").
                            s_i <- factor(raw_status[idx])
                            if ("0" %in% levels(s_i)) s_i <- stats::relevel(s_i, ref = "0")
                            fit <- survival::survfit(survival::Surv(t_i, s_i) ~ 1)
                            # pstate columns correspond to states; find the cause column
                            states <- fit$states
                            col <- match(event_state, states)
                            if (is.na(col)) {
                                          stop("Cause '", event_state, "' not found among event states: ",
                                               paste(states, collapse = ", "))
                            }
                            cif <- fit$pstate[, col]
                            se <- if (!is.null(fit$std.err)) fit$std.err[, col] else NA_real_
                            ftimes <- fit$time
                            report_at <- if (is.null(times)) max(ftimes) else times
                            out <- lapply(report_at, function(tp) {
                                          j <- sum(ftimes <= tp)
                                          tibble::tibble(
                                                        group = glabel,
                                                        time = tp,
                                                        cif = if (j == 0) 0 else cif[j],
                                                        se = if (j == 0) NA_real_ else se[j],
                                                        cause = event_state
                                          )
                            })
                            dplyr::bind_rows(out)
              }

              if (has_group) {
                            g <- data[[group_name]]
                            levs <- if (is.factor(g)) levels(g) else sort(unique(stats::na.omit(g)))
                            res <- purrr::map_dfr(levs, function(lv) extract_cif(g == lv, as.character(lv)))
              } else {
                            res <- extract_cif(rep(TRUE, nrow(data)), "overall")
              }

              # Gray's test if grouped and cmprsk available
              gray <- NULL
              if (has_group && requireNamespace("cmprsk", quietly = TRUE)) {
                            num_status <- suppressWarnings(as.integer(as.character(raw_status)))
                            if (!any(is.na(num_status))) {
                                          gt <- cmprsk::cuminc(ftime = tt, fstatus = num_status,
                                                               group = data[[group_name]])
                                          tst <- gt$Tests
                                          gray <- tibble::as_tibble(as.data.frame(tst),
                                                                    rownames = "cause")
                            }
              }

              attr(res, "gray_test") <- gray
              res
}
