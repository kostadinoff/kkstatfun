# ============================================================
# ONE-HOT ENCODING
# ============================================================

#' One-Hot Encoding (KK One Hot)
#'
#' @description Creates binary dummy variables for categorical column
#'
#' @param data Tibble or data frame
#' @param column Column name (character)
#'
#' @return Data with new binary columns added
#'
#' @examples
#' data <- tibble::tibble(id = 1:3, color = c("red", "blue", "red"))
#' kkonehot(data, "color")
#'
#' @export
kkonehot <- function(data, column) {
              # Check if input is a tibble
              if (!inherits(data, "tbl_df")) {
                            stop("Input 'data' must be a tibble")
              }

              # Check if column exists in data
              if (!column %in% names(data)) {
                            stop("Specified column not found in the dataset")
              }

              # Get the variable
              var <- data[[column]]

              # Check if variable is character or factor
              if (!(is.character(var) || is.factor(var))) {
                            stop("Column must be character or factor type for one-hot encoding")
              }

              # Convert character to factor if needed
              if (is.character(var)) {
                            var <- as.factor(var)
              }

              # Get levels of the factor
              levels <- levels(var)

              # Create one-hot encoded columns
              for (level in levels) {
                            new_col_name <- paste0(column, "_", level)
                            data[[new_col_name]] <- ifelse(var == level, 1, 0)
              }

              # Return the modified dataset
              return(data)
}

#' @export
one_hot_encode <- kkonehot
