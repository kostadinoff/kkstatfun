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

#' Initialize KK Stat Fun Environment
#'
#' @description Sets preferred options for backend, parallel processing, and display.
#' @param cores Number of cores to use (default: detectCores() - 1)
#' @param scipen Scientific notation penalty (default: 999)
#' @export
kk_setup <- function(cores = parallel::detectCores(logical = FALSE) - 1, scipen = 999) {
              # Backend and parallel processing
              options(brms.backend = "cmdstanr")
              options(mc.cores = max(1, cores)) # Ensure at least 1 core

              # Display options
              options(ggplot2.messages = FALSE)
              options(dplyr.width = Inf)
              options(scipen = scipen)
              options(warn = 1)

              # Memory
              options(expressions = 5000)

              message("✅ kkstatfun environment configured:")
              message(sprintf("   - Cores: %d", getOption("mc.cores")))
              message(sprintf("   - Backend: %s", getOption("brms.backend")))
              message("   - Scientific notation disabled")
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
#' @examples
#' library(dplyr)
#' df <- tibble(Value = c(10.567, 2.3, NA))
#' format_tibble(df)
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
#' @examples
#' df <- data.frame(a = c(1.234, 5.678), b = c("x", "y"))
#' mutate_round(df, 1)
#'
#' @export
mutate_round <- function(data, digits = 2) {
              if (!is.data.frame(data)) {
                            stop("Input must be a data frame or tibble.")
              }
              data |>
                            dplyr::mutate(dplyr::across(where(is.numeric), ~ janitor::round_half_up(., digits)))
}
