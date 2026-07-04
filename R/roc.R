# ============================================================
# ROC ANALYSIS
# ============================================================

#' ROC / AUC with Optimal Cutoff (KK)
#'
#' @description Builds an ROC curve for a continuous marker against a binary
#'   truth, returning the area under the curve (AUC) with a DeLong confidence
#'   interval and the Youden-optimal cutoff together with the sensitivity and
#'   specificity achieved there.
#'
#' @param data Data frame.
#' @param truth Binary outcome column (bare name or string).
#' @param predictor Continuous marker / predicted-probability column
#'   (bare name or string).
#' @param conf.level Confidence level for the AUC CI (default 0.95).
#' @param direction Comparison direction passed to `pROC::roc`
#'   (default "auto").
#'
#' @return Tibble with the AUC and its CI, the Youden index, the optimal
#'   threshold, and the sensitivity / specificity / PPV / NPV at that threshold.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   disease = rbinom(200, 1, 0.4),
#'   marker = rnorm(200)
#' )
#' kk_roc(df, disease, marker)
#' }
#'
#' @export
kk_roc <- function(data, truth, predictor, conf.level = 0.95, direction = "auto") {
              validate_data_frame(data)
              if (!requireNamespace("pROC", quietly = TRUE)) {
                            stop("Package 'pROC' is required for kk_roc().")
              }

              truth_name <- .kk_colname(rlang::enquo(truth))
              pred_name <- .kk_colname(rlang::enquo(predictor))

              missing_cols <- setdiff(c(truth_name, pred_name), names(data))
              if (length(missing_cols) > 0) {
                            stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
              }

              roc_obj <- pROC::roc(
                            response = data[[truth_name]],
                            predictor = data[[pred_name]],
                            direction = direction,
                            quiet = TRUE
              )

              auc_ci <- as.numeric(pROC::ci.auc(roc_obj, conf.level = conf.level))
              # ci.auc returns c(lower, median/auc, upper)
              best <- pROC::coords(
                            roc_obj, "best", best.method = "youden",
                            ret = c("threshold", "sensitivity", "specificity", "ppv", "npv"),
                            transpose = FALSE
              )
              best <- best[1, , drop = FALSE]

              tibble::tibble(
                            auc = as.numeric(pROC::auc(roc_obj)),
                            auc_low = auc_ci[1],
                            auc_high = auc_ci[3],
                            youden_j = best$sensitivity + best$specificity - 1,
                            optimal_threshold = best$threshold,
                            sensitivity = best$sensitivity,
                            specificity = best$specificity,
                            ppv = best$ppv,
                            npv = best$npv,
                            n = length(roc_obj$cases) + length(roc_obj$controls),
                            conf.level = conf.level
              )
}

#' Compare Two ROC Curves (DeLong Test) (KK)
#'
#' @description Compares the AUCs of two markers measured on the same subjects
#'   using DeLong's test for paired ROC curves (via `pROC::roc.test`).
#'
#' @param data Data frame.
#' @param truth Binary outcome column (bare name or string).
#' @param predictor1 First marker column (bare name or string).
#' @param predictor2 Second marker column (bare name or string).
#' @param method Test method passed to `pROC::roc.test` (default "delong").
#' @param paired Whether the markers are paired on the same subjects
#'   (default TRUE).
#' @param conf.level Confidence level (default 0.95).
#'
#' @return Tibble with the two AUCs, their difference, the test statistic,
#'   and the p-value.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   disease = rbinom(200, 1, 0.4),
#'   marker_a = rnorm(200),
#'   marker_b = rnorm(200)
#' )
#' kk_compare_roc(df, disease, marker_a, marker_b)
#' }
#'
#' @export
kk_compare_roc <- function(data, truth, predictor1, predictor2,
                           method = "delong", paired = TRUE, conf.level = 0.95) {
              validate_data_frame(data)
              if (!requireNamespace("pROC", quietly = TRUE)) {
                            stop("Package 'pROC' is required for kk_compare_roc().")
              }

              truth_name <- .kk_colname(rlang::enquo(truth))
              p1_name <- .kk_colname(rlang::enquo(predictor1))
              p2_name <- .kk_colname(rlang::enquo(predictor2))

              missing_cols <- setdiff(c(truth_name, p1_name, p2_name), names(data))
              if (length(missing_cols) > 0) {
                            stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "))
              }

              roc1 <- pROC::roc(data[[truth_name]], data[[p1_name]], quiet = TRUE)
              roc2 <- pROC::roc(data[[truth_name]], data[[p2_name]], quiet = TRUE)

              test <- pROC::roc.test(roc1, roc2, method = method, paired = paired,
                                     conf.level = conf.level)

              auc1 <- as.numeric(pROC::auc(roc1))
              auc2 <- as.numeric(pROC::auc(roc2))

              tibble::tibble(
                            marker1 = p1_name,
                            marker2 = p2_name,
                            auc1 = auc1,
                            auc2 = auc2,
                            auc_difference = auc1 - auc2,
                            statistic = unname(test$statistic),
                            p_value = test$p.value,
                            method = test$method,
                            conf.level = conf.level
              )
}
