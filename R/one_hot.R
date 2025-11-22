# ============================================================
# ONE-HOT ENCODING
# ============================================================

#' One-Hot Encoding for Categorical Variables
#'
#' @description Creates binary dummy variables for categorical column
#'
#' @param data Tibble or data frame
#' @param column Column name (character or unquoted)
#'
#' @return Data with new binary columns added
#'
#' @export
one_hot_encode <- function(data, column) {
              if (!inherits(data, c("tbl_df", "data.frame"))) {
                            stop(sprintf("Input must be tibble or data frame, got %s", class(data)[1]))
              }

              col_name <- if (is.symbol(substitute(column))) {
                            rlang::as_string(rlang::ensym(column))
              } else {
                            as.character(column)
              }

              if (!col_name %in% names(data)) {
                            stop(sprintf("Column '%s' not found", col_name))
              }

              var <- data[[col_name]]

              if (!is.character(var) && !is.factor(var)) {
                            stop(sprintf("'%s' must be character or factor", col_name))
              }

              if (is.character(var)) var <- as.factor(var)

              levels_var <- levels(var)

              for (level in levels_var) {
                            new_col_name <- paste0(col_name, "_", level)
                            data[[new_col_name]] <- dplyr::if_else(var == level, 1L, 0L)
              }

              tibble::as_tibble(data)
}
