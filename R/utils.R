# ============================================================
# VALIDATION UTILITIES
# ============================================================

#' Validate Data Frame Input
#'
#' @param data Object to validate
#' @param var_name Name of variable for error messages
#'
#' @return NULL (invisibly) or stops with error
#'
#' @keywords internal
validate_data_frame <- function(data, var_name = "data") {
              if (!is.data.frame(data)) {
                            stop(sprintf(
                                          "'%s' must be a data frame or tibble, got %s",
                                          var_name, class(data)[1]
                            ))
              }
              invisible(NULL)
}

#' Validate Numeric Column
#'
#' @param data Data frame to check
#' @param col_name Column name to validate
#' @param allow_na Whether to allow NA values
#'
#' @return NULL (invisibly) or stops with error
#'
#' @keywords internal
validate_numeric_column <- function(data, col_name, allow_na = TRUE) {
              if (!col_name %in% names(data)) {
                            stop(sprintf("Column '%s' not found in data", col_name))
              }

              col <- data[[col_name]]
              if (!is.numeric(col)) {
                            stop(sprintf(
                                          "Column '%s' must be numeric, got %s",
                                          col_name, class(col)[1]
                            ))
              }

              if (!allow_na && any(is.na(col))) {
                            stop(sprintf("Column '%s' contains NA values", col_name))
              }

              invisible(NULL)
}

#' Validate Date Column
#'
#' @param data Data frame to check
#' @param col_name Column name to validate
#'
#' @return NULL (invisibly) or stops with error
#'
#' @keywords internal
validate_date_column <- function(data, col_name) {
              if (!col_name %in% names(data)) {
                            stop(sprintf("Column '%s' not found in data", col_name))
              }

              col <- data[[col_name]]
              if (!inherits(col, c("Date", "POSIXct"))) {
                            stop(sprintf(
                                          "Column '%s' must be Date or POSIXct, got %s",
                                          col_name, class(col)[1]
                            ))
              }

              invisible(NULL)
}

#' Validate Categorical Column
#'
#' @param data Data frame to check
#' @param col_name Column name to validate
#'
#' @return NULL (invisibly) or stops with error
#'
#' @keywords internal
validate_categorical_column <- function(data, col_name) {
              if (!col_name %in% names(data)) {
                            stop(sprintf("Column '%s' not found in data", col_name))
              }

              col <- data[[col_name]]
              if (!is.character(col) && !is.factor(col)) {
                            stop(sprintf(
                                          "Column '%s' must be character or factor, got %s",
                                          col_name, class(col)[1]
                            ))
              }

              invisible(NULL)
}

# ============================================================
# UTILITY FUNCTIONS
# ============================================================

#' Format Tibble for Display
#'
#' @param data Tibble with numeric values
#' @param digits Number of decimal places
#'
#' @return Tibble with formatted display column
#'
#' @export
format_tibble <- function(data, digits = 2) {
              data %>%
                            dplyr::mutate(
                                          Value_display = sapply(Value, function(x) {
                                                        if (is.na(x)) {
                                                                      "NA"
                                                        } else {
                                                                      format(round(x, digits), nsmall = digits, scientific = FALSE, big.mark = ",")
                                                        }
                                          })
                            )
}

#' Round Numeric Columns in Tibble
#'
#' @param data Data frame or tibble
#' @param digits Number of decimal places
#'
#' @return Data with rounded numeric columns
#'
#' @export
mutate_round <- function(data, digits = 2) {
              if (!is.data.frame(data)) {
                            stop(sprintf("Input must be a data frame or tibble, got %s", class(data)[1]))
              }

              data %>%
                            dplyr::mutate(dplyr::across(
                                          where(is.numeric),
                                          ~ janitor::round_half_up(., digits)
                            ))
}
