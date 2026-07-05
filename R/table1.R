# ============================================================
# TABLE 1 GENERATION
# ============================================================

#' Create Publication-Ready Table 1
#'
#' @description Generates a summary table of descriptive statistics, stratified by a group
#'   variable, with p-values, returned as a standard tibble.
#'
#' @param data Data frame
#' @param by Grouping variable (character)
#' @param variables Vector of variables to include (optional, defaults to all)
#' @param label_list Named list of labels for variables
#'
#' @return Tibble containing summary statistics
#'
#' @examples
#' \dontrun{
#' library(gtsummary)
#' trial <- data.frame(
#'   arm = sample(c("Control", "Treatment"), 100, replace = TRUE),
#'   age = rnorm(100, 60, 10), bmi = rnorm(100, 27, 4), sbp = rnorm(100, 135, 15)
#' )
#' table1_summary(trial, by = "arm", variables = c("age", "bmi", "sbp"))
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

        res_tibble <- gtsummary::as_tibble(tbl)
        names(res_tibble) <- gsub("\\*\\*", "", names(res_tibble))
        names(res_tibble) <- gsub("\\n", " ", names(res_tibble))

        res_tibble
}

#' Wrapper for Table 1 Generation
#'
#' @description A wrapper around `table1_summary` to generate a publication-ready descriptive table
#'   returned as a standard tibble.
#'
#' @examples
#' \dontrun{
#' trial <- data.frame(
#'   arm = sample(c("Control", "Treatment"), 100, replace = TRUE),
#'   age = rnorm(100, 60, 10), bmi = rnorm(100, 27, 4)
#' )
#' kk_table1(trial, by = "arm", variables = c("age", "bmi"))
#' }
#' @export
kk_table1 <- function(data, by = NULL, variables = NULL, label_list = NULL,
                      digits = 2, p_value = TRUE) {
        # Ensure data is a data frame
        if (!is.data.frame(data)) stop("Input must be a data frame")

        # Resolve 'by' argument safely (handles strings, symbols, or NULL)
        resolve_arg <- function(arg_expr, pf = parent.frame()) {
                if (is.null(arg_expr)) return(NULL)
                if (is.character(arg_expr)) return(arg_expr)
                if (is.symbol(arg_expr)) {
                        val <- tryCatch(eval(arg_expr, pf), error = function(e) NULL)
                        if (is.character(val) && length(val) == 1) return(val)
                        return(as.character(arg_expr))
                }
                dep_val <- deparse(arg_expr)
                if (identical(dep_val, "NULL")) return(NULL)
                return(dep_val)
        }

        by_expr <- substitute(by)
        by_str <- resolve_arg(by_expr, parent.frame())

        if (is.null(by_str) || by_str == "") {
                by_str <- NULL
        }

        # Call internal or existing logic

        if (!requireNamespace("gtsummary", quietly = TRUE)) {
                stop("Package 'gtsummary' is required for kk_table1")
        }

        # Select variables if provided
        if (!is.null(variables)) {
                # If variables is a character vector
                if (is.character(variables)) {
                        data_subset <- data %>% dplyr::select(dplyr::all_of(c(variables, by_str)))
                } else {
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

        res_tibble <- gtsummary::as_tibble(tbl)
        names(res_tibble) <- gsub("\\*\\*", "", names(res_tibble))
        names(res_tibble) <- gsub("\\n", " ", names(res_tibble))

        return(res_tibble)
}
