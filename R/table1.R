# ============================================================
# TABLE 1 GENERATION
# ============================================================

#' Create Publication-Ready Table 1
#'
#' @description Generates a summary table of descriptive statistics, stratified by a group
#'   variable, with p-values.
#'
#' @param data Data frame
#' @param by Grouping variable (character)
#' @param variables Vector of variables to include (optional, defaults to all)
#' @param label_list Named list of labels for variables
#'
#' @return gtsummary object
#'
#' @export
table1_summary <- function(data, by = NULL, variables = NULL, label_list = NULL) {
              validate_data_frame(data)

              if (!requireNamespace("gtsummary", quietly = TRUE)) {
                            stop("Package 'gtsummary' is required")
              }

              if (!is.null(variables)) {
                            data <- data %>% dplyr::select(dplyr::all_of(c(variables, by)))
              }

              tbl <- gtsummary::tbl_summary(
                            data,
                            by = by,
                            missing = "ifany",
                            label = label_list
              ) %>%
                            gtsummary::add_n() %>%
                            gtsummary::add_p() %>%
                            gtsummary::bold_labels()

              tbl
}
