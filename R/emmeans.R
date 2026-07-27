# ============================================================
# ESTIMATED MARGINAL MEANS (emmeans BRIDGE)
# ============================================================

#' Tidy Estimated Marginal Means from a kkstatfun Model
#'
#' @description Runs [emmeans::emmeans()] on the model kept inside a kkstatfun
#'   modelling result and returns a **tibble**, keeping the package's tidy-out
#'   contract. Optionally computes pairwise (or other) contrasts in the same
#'   call.
#'
#'   This is a thin convenience layer. For anything beyond marginal means and
#'   contrasts -- `emtrends()`, `joint_tests()`, `emmip()`, custom contrast
#'   matrices -- pull the model out with [kk_model()] and call `emmeans`
#'   directly.
#'
#' @param x A tibble returned by [kk_reg()], [kk_coxph()], [kk_rr_reg()],
#'   [kk_rate_reg()] or [kk_firth()].
#' @param specs A formula or character vector naming the predictors to average
#'   over, passed straight to [emmeans::emmeans()] (e.g. `~ arm`, `~ arm | sex`).
#' @param contrast Contrast method to apply, or `NULL` (default) for marginal
#'   means only. Any method [emmeans::contrast()] accepts, e.g. `"pairwise"`,
#'   `"trt.vs.ctrl"`, `"poly"`.
#' @param type Passed to `emmeans`: `"link"` (default) keeps the model's link
#'   scale, `"response"` back-transforms to odds ratios, rate ratios, or
#'   probabilities as appropriate.
#' @param adjust Multiplicity adjustment for contrasts (default `"tukey"` for
#'   pairwise; see [emmeans::summary.emmGrid()]).
#' @param conf.level Confidence level (default `0.95`).
#' @param which Which stored model to use -- see [kk_model()].
#' @param predictor Predictor name when `which = "univariate"`.
#' @param group Group label when `x` came from grouped data -- see [kk_model()].
#' @param ... Further arguments passed to [emmeans::emmeans()].
#'
#' @return A tibble of estimated marginal means, or of contrasts when
#'   `contrast` is supplied. The underlying `emmGrid` is attached as
#'   `attr(x, "emmGrid")` for follow-up work.
#'
#' @details
#' Confidence-limit columns are normalised to `conf.low`/`conf.high`, because
#' `emmeans` names them `lower.CL`/`upper.CL` for models with finite degrees of
#' freedom but `asymp.LCL`/`asymp.UCL` for asymptotic ones such as a `glm` --
#' a difference that is easy to trip over when post-processing.
#'
#' @seealso [kk_model()] to get the raw model, [kk_model_data()] for its data.
#'
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   age = rnorm(200, 60, 10),
#'   arm = factor(sample(c("Control", "Treatment"), 200, TRUE)),
#'   sex = factor(sample(c("F", "M"), 200, TRUE))
#' )
#' d$sbp <- 100 + 0.4 * d$age - 6 * (d$arm == "Treatment") + rnorm(200, 0, 8)
#'
#' fit <- kk_reg(d, sbp, c("age", "arm"))
#'
#' # Adjusted marginal means by arm
#' kk_emmeans(fit, ~ arm)
#'
#' # Pairwise contrast between arms
#' kk_emmeans(fit, ~ arm, contrast = "pairwise")
#'
#' @export
kk_emmeans <- function(x, specs, contrast = NULL, type = c("link", "response"),
                       adjust = NULL, conf.level = 0.95,
                       which = c("multivariable", "univariate"),
                       predictor = NULL, group = NULL, ...) {
              if (!requireNamespace("emmeans", quietly = TRUE)) {
                            stop("Package 'emmeans' is required for kk_emmeans().", call. = FALSE)
              }
              type <- match.arg(type)
              which <- match.arg(which)

              model <- kk_model(x, which = which, predictor = predictor, group = group)

              emm <- emmeans::emmeans(model, specs = specs, type = type, ...)

              if (is.null(contrast)) {
                            obj <- emm
                            summ <- if (is.null(adjust)) {
                                          summary(obj, infer = TRUE, level = conf.level)
                            } else {
                                          summary(obj, infer = TRUE, level = conf.level, adjust = adjust)
                            }
              } else {
                            obj <- if (is.null(adjust)) {
                                          emmeans::contrast(emm, method = contrast)
                            } else {
                                          emmeans::contrast(emm, method = contrast, adjust = adjust)
                            }
                            summ <- summary(obj, infer = TRUE, level = conf.level)
              }

              out <- tibble::as_tibble(as.data.frame(summ))

              # Normalise the CI column names: emmeans uses lower.CL/upper.CL when
              # the grid has finite df and asymp.LCL/asymp.UCL when it does not.
              names(out)[names(out) %in% c("lower.CL", "asymp.LCL")] <- "conf.low"
              names(out)[names(out) %in% c("upper.CL", "asymp.UCL")] <- "conf.high"
              names(out)[names(out) == "p.value"] <- "p.value"

              out$conf.level <- conf.level
              attr(out, "emmGrid") <- obj
              out
}
