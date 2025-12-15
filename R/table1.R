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
#' @examples
#' \dontrun{
#' library(gtsummary)
#' table1_summary(mtcars, by = "am", variables = c("mpg", "hp", "wt"))
#' }
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

#' Wrapper for Table 1 Generation
#'
#' @description A wrapper around `table1_summary` to generate a publication-ready descriptive table.
#'
#' @examples
#' \dontrun{
#' kk_table1(mtcars, by = "am", variables = c("mpg", "hp"))
#' }
#' @export
kk_table1 <- function(data, by = NULL, variables = NULL, label_list = NULL,
                      digits = 2, p_value = TRUE) {
        # Wrapper around table1_summary with kk_ prefix and potentially more options

        # Ensure data is a data frame
        if (!is.data.frame(data)) stop("Input must be a data frame")

        # Handle 'by' argument if it's a tidy selection (enquo)
        by_enquo <- rlang::enquo(by)
        by_str <- tryCatch(rlang::as_name(by_enquo), error = function(e) NULL)

        # If by_str is NULL or empty string, check if 'by' was passed as string
        if (is.null(by_str) || by_str == "") {
                if (!is.null(by) && is.character(by)) {
                        by_str <- by
                } else {
                        by_str <- NULL
                }
        }

        # Call internal or existing logic
        # We can reuse table1_summary logic but improve it

        if (!requireNamespace("gtsummary", quietly = TRUE)) {
                stop("Package 'gtsummary' is required for kk_table1")
        }

        # Select variables if provided
        if (!is.null(variables)) {
                # If variables is a character vector
                if (is.character(variables)) {
                        data_subset <- data %>% dplyr::select(dplyr::all_of(c(variables, by_str)))
                } else {
                        # If variables is a tidy selection, this is harder to pass directly without more complex handling
                        # For now assume character vector or NULL
                        warning("variables argument should be a character vector of column names.")
                        data_subset <- data
                }
        } else {
                data_subset <- data
        }

        tbl <- gtsummary::tbl_summary(
                data_subset,
                by = if (!is.null(by_str)) by_str else NULL,
                missing = "ifany",
                label = label_list,
                digits = gtsummary::all_continuous() ~ digits
        ) %>%
                gtsummary::add_n() %>%
                gtsummary::bold_labels()

        if (p_value && !is.null(by_str)) {
                tbl <- tbl %>% gtsummary::add_p()
        }

        return(tbl)
}
