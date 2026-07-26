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

#' Re-dispatch a Call Across the Groups of a Grouped Data Frame
#'
#' @description Generic `group_by()` support for the analysis functions. A
#'   function opts in with a single line at the top of its body:
#'
#'   ```
#'   if (dplyr::is_grouped_df(data)) {
#'     return(.kk_by_group(data, match.call(), parent.frame()))
#'   }
#'   ```
#'
#'   The original call is captured with `match.call()`, so tidy-eval arguments
#'   (bare column names, formulas, `...`) survive untouched. For each group the
#'   `data` argument is swapped for that group's rows -- ungrouped, so the
#'   re-entrant call takes the normal path and cannot recurse -- and the result
#'   is prefixed with the grouping columns.
#'
#'   Results that are not data frames (models, lists, plots) are returned in a
#'   list-column named `result`, keeping the output rectangular.
#'
#' @param data The grouped data frame.
#' @param call The result of `match.call()` in the calling function.
#' @param env The calling function's `parent.frame()`, used to resolve the
#'   function name and any variables referenced in the call.
#'
#' @return A tibble of the per-group results with the grouping columns first.
#' @noRd
.kk_by_group <- function(data, call, env) {
              group_cols <- dplyr::group_vars(data)
              keys <- dplyr::group_keys(data)
              pieces <- dplyr::group_split(data, .keep = TRUE)

              # The data argument may have been matched by name or by position;
              # match.call() normalises it to the formal's name.
              data_arg <- names(call)[2]
              if (is.null(data_arg) || !nzchar(data_arg)) data_arg <- 2L

              out <- lapply(seq_along(pieces), function(i) {
                            piece <- dplyr::ungroup(pieces[[i]])
                            sub_call <- call
                            sub_call[[data_arg]] <- quote(.kk_piece)
                            eval_env <- rlang::new_environment(
                                          list(.kk_piece = piece),
                                          parent = env
                            )
                            res <- eval(sub_call, eval_env)

                            if (!is.data.frame(res)) {
                                          res <- tibble::tibble(result = list(res))
                            }
                            if (nrow(res) == 0L) {
                                          return(NULL)
                            }

                            key_row <- keys[rep(i, nrow(res)), , drop = FALSE]
                            # Drop any same-named column the callee produced so the
                            # grouping keys are unambiguous.
                            res <- res[, setdiff(names(res), group_cols), drop = FALSE]
                            dplyr::bind_cols(tibble::as_tibble(key_row), tibble::as_tibble(res))
              })

              dplyr::bind_rows(out)
}

#' Extract Confidence Limits from an emmeans Summary
#'
#' @description `emmeans` names its interval columns `lower.CL`/`upper.CL` when
#'   the reference grid carries finite degrees of freedom, but `asymp.LCL`/
#'   `asymp.UCL` for asymptotic (z-based) grids such as those from a `glm`.
#'   Reading only one pair silently yields `NULL`, which `tibble()` then drops
#'   without warning. This helper accepts either and fails loudly if neither is
#'   present.
#'
#' @param x A data frame from `summary(<emmGrid>, infer = TRUE)`.
#'
#' @return A list with numeric elements `lower` and `upper`.
#' @noRd
.kk_emm_cl <- function(x) {
              lo <- if ("lower.CL" %in% names(x)) {
                            x[["lower.CL"]]
              } else if ("asymp.LCL" %in% names(x)) {
                            x[["asymp.LCL"]]
              } else {
                            NULL
              }
              hi <- if ("upper.CL" %in% names(x)) {
                            x[["upper.CL"]]
              } else if ("asymp.UCL" %in% names(x)) {
                            x[["asymp.UCL"]]
              } else {
                            NULL
              }
              if (is.null(lo) || is.null(hi)) {
                            stop(
                                          "No confidence-limit columns found in the emmeans summary ",
                                          "(expected 'lower.CL'/'upper.CL' or 'asymp.LCL'/'asymp.UCL'). ",
                                          "Columns present: ", paste(names(x), collapse = ", "),
                                          call. = FALSE
                            )
              }
              list(lower = as.numeric(lo), upper = as.numeric(hi))
}

#' Initialize KK Stat Fun Environment
#'
#' @description Sets preferred options for backend, parallel processing, and display.
#' @param cores Number of cores to use (default: detectCores() - 1)
#' @param scipen Scientific notation penalty (default: 999)
#' @examples
#' \dontrun{
#' kk_setup()
#' }
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

              message("\u2705 kkstatfun environment configured:")
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
                                          Value_display = sapply(.data$Value, function(x) {
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
                            dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ janitor::round_half_up(., digits)))
}
