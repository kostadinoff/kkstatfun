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
                            mods <- attr(res, "models", exact = TRUE)

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
                            combined <- dplyr::bind_cols(tibble::as_tibble(key_row),
                                                         tibble::as_tibble(res))
                            # Strip any model attributes that rode along from the callee:
                            # left in place, the first group's model would survive the
                            # bind and masquerade as an overall fit.
                            attr(combined, "model") <- NULL
                            attr(combined, "models") <- NULL
                            attr(combined, "model_data") <- NULL
                            # Carry the fitted models out separately, keyed by group label.
                            attr(combined, ".kk_group_models") <- mods
                            combined
              })

              labels <- apply(as.data.frame(keys), 1L, paste, collapse = " | ")
              per_group <- lapply(out, function(z) attr(z, ".kk_group_models"))
              names(per_group) <- labels[seq_along(per_group)]
              per_group <- per_group[!vapply(per_group, is.null, logical(1))]

              final <- dplyr::bind_rows(lapply(out, function(z) {
                            attr(z, ".kk_group_models") <- NULL
                            z
              }))
              attr(final, "model") <- NULL
              attr(final, "models") <- NULL
              attr(final, "model_data") <- NULL
              if (length(per_group)) {
                            attr(final, "group_models") <- per_group
              }
              final
}

#' Attach Fitted Models to a Results Tibble
#'
#' @description Stores the fitted model objects on a results tibble so callers
#'   can reach them with [kk_model()] and pass them to `emmeans`, `marginaleffects`,
#'   `anova()`, `plot()` and friends. Attaching costs nothing: the model already
#'   retains the fitting environment, so the data is referenced either way.
#'
#' @param x The results tibble to annotate.
#' @param multivariable The adjusted/full model (may be `NULL`).
#' @param univariate Named list of single-predictor models (may be empty).
#' @param data The data frame the models were fitted to.
#'
#' @return `x`, with `model`, `models` and `model_data` attributes.
#' @noRd
.kk_attach_models <- function(x, multivariable = NULL, univariate = list(), data = NULL) {
              attr(x, "model") <- multivariable
              attr(x, "models") <- list(
                            multivariable = multivariable,
                            univariate = univariate
              )
              attr(x, "model_data") <- data
              x
}

#' Extract a Fitted Model from a kkstatfun Result
#'
#' @description The modelling functions ([kk_reg()], [kk_coxph()], [kk_rr_reg()],
#'   [kk_rate_reg()], [kk_firth()]) return a tidy tibble of coefficients, but
#'   also keep the underlying fitted model. `kk_model()` retrieves it so you can
#'   continue with the wider modelling ecosystem -- most usefully `emmeans` for
#'   estimated marginal means, contrasts and trends.
#'
#' @param x A tibble returned by a kkstatfun modelling function.
#' @param which Which model to return: `"multivariable"` (default, the adjusted
#'   model), `"univariate"` (requires `predictor`), or `"all"` for the full
#'   named list.
#' @param predictor Predictor name, when `which = "univariate"`.
#' @param group For a result produced from grouped data, the group label whose
#'   model you want (see `names(attr(x, "group_models"))`). `NULL` (default)
#'   for ungrouped results.
#'
#' @return The fitted model object (`lm`, `glm`, `coxph`, `polr`, ...), or a
#'   named list when `which = "all"`.
#'
#' @details
#' `emmeans` recovers the model frame from the fitted object, so the returned
#' model works directly:
#'
#' ```
#' fit <- kk_reg(d, sbp, c("age", "arm"))
#' emmeans::emmeans(kk_model(fit), ~ arm)
#' ```
#'
#' If a model is ever moved between sessions and `emmeans` cannot recover the
#' data, pass it explicitly with `data = kk_model_data(fit)`.
#'
#' @seealso [kk_model_data()], [kk_emmeans()]
#'
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   age = rnorm(100, 60, 10),
#'   arm = factor(sample(c("Control", "Treatment"), 100, TRUE))
#' )
#' d$sbp <- 100 + 0.4 * d$age - 6 * (d$arm == "Treatment") + rnorm(100, 0, 8)
#'
#' fit <- kk_reg(d, sbp, c("age", "arm"))
#' summary(kk_model(fit))
#' names(kk_model(fit, "all")$univariate)
#'
#' @export
kk_model <- function(x, which = c("multivariable", "univariate", "all"),
                     predictor = NULL, group = NULL) {
              which <- match.arg(which)
              # exact = TRUE throughout: attr() partial-matches by default, so
              # attr(x, "models") would otherwise silently return
              # "group_models" on a grouped result.
              by_group <- attr(x, "group_models", exact = TRUE)

              if (!is.null(group)) {
                            if (is.null(by_group)) {
                                          stop("This result was not produced from grouped data, ",
                                               "so `group` does not apply.", call. = FALSE)
                            }
                            if (!group %in% names(by_group)) {
                                          stop("No models for group '", group, "'. Available: ",
                                               paste(names(by_group), collapse = ", "), call. = FALSE)
                            }
                            models <- by_group[[group]]
              } else {
                            models <- attr(x, "models", exact = TRUE)
              }

              if (is.null(models)) {
                            if (!is.null(by_group)) {
                                          stop("This result came from grouped data, so there is one model per ",
                                               "group. Pass group = one of: ",
                                               paste(names(by_group), collapse = ", "),
                                               call. = FALSE)
                            }
                            stop("No fitted model attached to this object. ",
                                 "kk_model() works on results from kk_reg(), kk_coxph(), ",
                                 "kk_rr_reg(), kk_rate_reg() and kk_firth().",
                                 call. = FALSE)
              }
              if (which == "all") {
                            return(models)
              }
              if (which == "multivariable") {
                            if (is.null(models$multivariable)) {
                                          stop("No multivariable model was fitted for this result.", call. = FALSE)
                            }
                            return(models$multivariable)
              }
              uni <- models$univariate
              if (is.null(predictor)) {
                            stop("`predictor` is required when which = \"univariate\". Available: ",
                                 paste(names(uni), collapse = ", "), call. = FALSE)
              }
              if (!predictor %in% names(uni)) {
                            stop("No univariate model for predictor '", predictor, "'. Available: ",
                                 paste(names(uni), collapse = ", "), call. = FALSE)
              }
              uni[[predictor]]
}

#' Extract the Model-Fitting Data from a kkstatfun Result
#'
#' @description Returns the data frame the models in `x` were fitted to. Useful
#'   as the `data =` argument to `emmeans` in the rare case where automatic
#'   recovery fails (typically after saving and reloading a model).
#'
#' @param x A tibble returned by a kkstatfun modelling function.
#'
#' @return The data frame used for fitting, or `NULL` if none was recorded.
#'
#' @seealso [kk_model()], [kk_emmeans()]
#'
#' @examples
#' set.seed(1)
#' d <- data.frame(y = rnorm(50), g = factor(rep(c("a", "b"), 25)))
#' fit <- kk_reg(d, y, "g")
#' nrow(kk_model_data(fit))
#'
#' @export
kk_model_data <- function(x) {
              attr(x, "model_data", exact = TRUE)
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
