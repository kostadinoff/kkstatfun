# ============================================================
# SURVIVAL ANALYSIS
# ============================================================

#' Survival Analysis Plot
#'
#' @description Creates a Kaplan-Meier survival plot with risk table
#'
#' @param data Data frame
#' @param time_col Time variable name
#' @param status_col Status variable name (0=censored, 1=event)
#' @param group_col Optional grouping variable
#' @param conf.int Show confidence intervals
#'
#' @return ggplot object
#'
#' @export
survival_plot <- function(data, time_col, status_col, group_col = NULL, conf.int = TRUE) {
              validate_data_frame(data)

              if (!requireNamespace("survival", quietly = TRUE) ||
                            !requireNamespace("ggsurvfit", quietly = TRUE)) {
                            stop("Packages 'survival' and 'ggsurvfit' are required")
              }

              time_sym <- rlang::ensym(time_col)
              status_sym <- rlang::ensym(status_col)

              if (!is.null(group_col)) {
                            group_sym <- rlang::ensym(group_col)
                            fml <- stats::as.formula(paste0(
                                          "survival::Surv(", rlang::as_string(time_sym), ", ",
                                          rlang::as_string(status_sym), ") ~ ", rlang::as_string(group_sym)
                            ))
              } else {
                            fml <- stats::as.formula(paste0(
                                          "survival::Surv(", rlang::as_string(time_sym), ", ",
                                          rlang::as_string(status_sym), ") ~ 1"
                            ))
              }

              fit <- survival::survfit2(fml, data = data)

              p <- ggsurvfit::ggsurvfit(fit) +
                            ggsurvfit::add_confidence_interval() +
                            ggsurvfit::add_risktable() +
                            ggplot2::labs(
                                          y = "Survival Probability",
                                          title = "Kaplan-Meier Survival Curve"
                            )

              if (!conf.int) {
                            # Remove CI layer if not requested (re-plotting without it is easier)
                            p <- ggsurvfit::ggsurvfit(fit) +
                                          ggsurvfit::add_risktable() +
                                          ggplot2::labs(
                                                        y = "Survival Probability",
                                                        title = "Kaplan-Meier Survival Curve"
                                          )
              }

              p
}

#' Survival Plot (KK)
#'
#' @description Kaplan-Meier plot with risk table and p-value.
#'
#' @param data Data frame
#' @param time Time column
#' @param status Status column (0/1)
#' @param group Grouping column (optional)
#' @param risk_table Show risk table? (default TRUE)
#' @param p_val Show p-value? (default TRUE)
#' @param conf_int Show confidence interval? (default TRUE)
#' @param palette Color palette (optional)
#'
#' @return ggplot object
#' @export
kk_survival_plot <- function(data, time, status, group = NULL,
                             risk_table = TRUE, p_val = TRUE, conf_int = TRUE,
                             palette = NULL) {
              if (!requireNamespace("survival", quietly = TRUE) ||
                            !requireNamespace("ggsurvfit", quietly = TRUE)) {
                            stop("Packages 'survival' and 'ggsurvfit' are required")
              }

              time_enquo <- rlang::enquo(time)
              status_enquo <- rlang::enquo(status)
              group_enquo <- rlang::enquo(group)

              # Construct formula
              # We need to handle if group is provided or not

              # Check if group is provided (not null and not missing)
              has_group <- !rlang::quo_is_null(group_enquo)

              if (has_group) {
                            fml <- stats::as.formula(paste0(
                                          "survival::Surv(", rlang::as_name(time_enquo), ", ",
                                          rlang::as_name(status_enquo), ") ~ ", rlang::as_name(group_enquo)
                            ))
              } else {
                            fml <- stats::as.formula(paste0(
                                          "survival::Surv(", rlang::as_name(time_enquo), ", ",
                                          rlang::as_name(status_enquo), ") ~ 1"
                            ))
              }

              fit <- ggsurvfit::survfit2(fml, data = data)

              p <- ggsurvfit::ggsurvfit(fit) +
                            ggplot2::labs(
                                          y = "Survival Probability",
                                          title = "Kaplan-Meier Survival Curve"
                            )

              if (conf_int) {
                            p <- p + ggsurvfit::add_confidence_interval()
              }

              if (risk_table) {
                            p <- p + ggsurvfit::add_risktable()
              }

              if (p_val && has_group) {
                            p <- p + ggsurvfit::add_pvalue()
              }

              if (!is.null(palette)) {
                            p <- p + ggplot2::scale_color_manual(values = palette) +
                                          ggplot2::scale_fill_manual(values = palette)
              }

              return(p)
}
