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
