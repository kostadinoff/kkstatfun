# ============================================================
# UTILITIES
# ============================================================

#' Validate Data Frame
#'
#' @param data Input object
#' @param var_name Variable name for error message
#'
#' @return Invisible NULL or error
#' @noRd
validate_data_frame <- function(data, var_name = "data") {
              if (!is.data.frame(data)) {
                            stop(sprintf(
                                          "'%s' must be a data frame or tibble, got %s",
                                          var_name, class(data)[1]
                            ))
              }
              invisible(NULL)
}

#' Format Tibble for Display
#'
#' @description Formats numeric columns in a tibble for display with specified digits
#'
#' @param data Tibble or data frame
#' @param digits Number of digits to round to (default: 2)
#'
#' @return Formatted tibble
#'
#' @export
format_tibble <- function(data, digits = 2) {
              data %>%
                            dplyr::mutate(
                                          Value_display = sapply(Value, function(x) {
                                                        if (is.na(x)) "NA" else format(round(x, digits), nsmall = digits, scientific = FALSE, big.mark = ",")
                                          })
                            )
}

#' Round Numeric Columns
#'
#' @description Rounds all numeric columns in a data frame
#'
#' @param data Tibble or data frame
#' @param digits Number of digits (default: 2)
#'
#' @return Data frame with rounded numeric columns
#'
#' @export
mutate_round <- function(data, digits = 2) {
              if (!is.data.frame(data)) {
                            stop("Input must be a data frame or tibble.")
              }
              data |>
                            dplyr::mutate(dplyr::across(where(is.numeric), ~ janitor::round_half_up(., digits)))
}
