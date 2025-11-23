# ============================================================
# CONFUSION MATRIX METRICS
# ============================================================

#' Confusion Matrix Metrics with Confidence Intervals
#'
#' @description Computes comprehensive classification metrics with exact
#'   binomial CIs, log-normal CIs for ratios, and optional bootstrap CIs.
#'
#' @param x Input confusion matrix as named vector, list, tibble, or data frame
#'   - Must contain: TP (true positives), FP (false positives),
#'     FN (false negatives), TN (true negatives)
#' @param conf Confidence level (default: 0.95)
#' @param boot Calculate bootstrap CIs for point estimates (default: FALSE)
#' @param B Number of bootstrap replications (default: 5000)
#' @param seed Random seed for reproducibility (default: 1)
#' @param add_0_5_for_lr Apply Haldane-Anscombe +0.5 correction for zero cells
#'   in likelihood ratios (default: TRUE)
#'
#' @return Tibble with metrics, estimates, lower/upper CIs, CI method, and notes
#'
#' @details
#' Includes 22+ classification metrics:
#' - **Binomial CIs:** Sensitivity, Specificity, PPV, NPV, Accuracy, Prevalence
#' - **Derived CIs:** FPR, FNR, FDR, FOR
#' - **Likelihood Ratios:** LR+, LR−, DOR (with log-normal CIs)
#' - **Point Estimates:** F1, Balanced Accuracy, MCC, Youden's J, etc.
#'
#' Input formats:
#' ```
#' # Named vector
#' confusion_metrics_ci(c(tp=100, fp=20, fn=10, tn=870))
#'
#' # Tibble with columns: metric/label, value
#' tibble(label = c("TP","FP","FN","TN"), value = c(100,20,10,870)) %>%
#'   confusion_metrics_ci()
#'
#' # Data frame
#' data.frame(tp=100, fp=20, fn=10, tn=870) %>%
#'   confusion_metrics_ci()
#' ```
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' confusion_metrics_ci(c(tp = 85, fp = 10, fn = 15, tn = 890))
#'
#' # With bootstrap CIs
#' confusion_metrics_ci(c(tp = 85, fp = 10, fn = 15, tn = 890), boot = TRUE, B = 10000)
#'
#' # Higher confidence
#' confusion_metrics_ci(c(tp = 85, fp = 10, fn = 15, tn = 890), conf = 0.99)
#' }
#'
#' @export
confusion_metrics_ci <- function(x,
                                 conf = 0.95,
                                 boot = FALSE,
                                 B = 5000,
                                 seed = 1,
                                 add_0_5_for_lr = TRUE) {
              # ----------------------------- helpers ---------------------------------
              get_counts <- function(x) {
                            if (is.data.frame(x)) {
                                          nm <- tolower(trimws(names(x)[1]))
                                          if (ncol(x) == 2 && (nm %in% c("name", "metric", "label") || is.character(x[[1]]))) {
                                                        kv <- setNames(as.numeric(x[[2]]), tolower(gsub("[^a-z ]", "", x[[1]])))
                                          } else if (all(c("tp", "fp", "fn", "tn") %in% tolower(names(x)))) {
                                                        take <- tolower(names(x)) %in% c("tp", "fp", "fn", "tn")
                                                        kv <- setNames(as.numeric(x[1, take]), tolower(names(x))[take])
                                          } else {
                                                        stop("Could not parse counts. Provide two columns (label,value) or columns named tp,fp,fn,tn.")
                                          }
                            } else if (is.list(x) || is.vector(x)) {
                                          kv <- setNames(as.numeric(x), tolower(names(x)))
                            } else {
                                          stop("Unsupported input type.")
                            }

                            map <- c(
                                          "true positives" = "tp", "tp" = "tp",
                                          "false positives" = "fp", "fp" = "fp",
                                          "false negatives" = "fn", "fn" = "fn",
                                          "true negatives" = "tn", "tn" = "tn"
                            )
                            out <- c(tp = NA_real_, fp = NA_real_, fn = NA_real_, tn = NA_real_)
                            for (k in names(kv)) if (k %in% names(map)) out[map[[k]]] <- kv[[k]]
                            if (any(is.na(out))) stop("Missing one or more of TP/FP/FN/TN.")
                            as.list(out)
              }

              binom_ci <- function(success, total, conf) {
                            if (total == 0) {
                                          return(c(est = NA_real_, lwr = NA_real_, upr = NA_real_))
                            }
                            bt <- stats::binom.test(success, total, conf.level = conf)
                            c(est = unname(bt$estimate), lwr = bt$conf.int[1], upr = bt$conf.int[2])
              }

              ci_complement <- function(ci_named) {
                            if (any(is.na(ci_named))) {
                                          return(c(est = NA_real_, lwr = NA_real_, upr = NA_real_))
                            }
                            est <- as.numeric(1 - ci_named[["est"]])
                            lwr <- as.numeric(1 - ci_named[["upr"]])
                            upr <- as.numeric(1 - ci_named[["lwr"]])
                            c(est = est, lwr = lwr, upr = upr)
              }

              lognorm_ci_ratio <- function(estimate, se_log, conf) {
                            if (is.na(estimate) || is.na(se_log) || estimate <= 0) {
                                          return(c(NA_real_, NA_real_))
                            }
                            z <- stats::qnorm(1 - (1 - conf) / 2)
                            c(exp(log(estimate) - z * se_log), exp(log(estimate) + z * se_log))
              }

              as_row <- function(metric, est, lwr, upr, method, note = NA_character_) {
                            data.frame(
                                          metric = metric,
                                          estimate = as.numeric(est),
                                          lower = as.numeric(lwr),
                                          upper = as.numeric(upr),
                                          ci_level = conf,
                                          ci_method = method,
                                          note = note,
                                          stringsAsFactors = FALSE
                            )
              }

              # ----------------------------- counts & basic metrics -------------------
              cts <- get_counts(x)
              TP <- cts$tp
              FP <- cts$fp
              FN <- cts$fn
              TN <- cts$tn

              P <- TP + FN
              N <- TN + FP
              Tn <- P + N

              sens <- if (P > 0) TP / P else NA_real_
              spec <- if (N > 0) TN / N else NA_real_
              ppv <- if ((TP + FP) > 0) TP / (TP + FP) else NA_real_
              npv <- if ((TN + FN) > 0) TN / (TN + FN) else NA_real_
              acc <- if (Tn > 0) (TP + TN) / Tn else NA_real_
              prev <- if (Tn > 0) P / Tn else NA_real_

              fpr <- if (!is.na(spec)) 1 - spec else NA_real_
              fnr <- if (!is.na(sens)) 1 - sens else NA_real_
              fdr <- if (!is.na(ppv)) 1 - ppv else NA_real_
              forr <- if (!is.na(npv)) 1 - npv else NA_real_

              ba <- mean(c(sens, spec), na.rm = TRUE)
              mk <- if (!is.na(ppv) && !is.na(npv)) ppv + npv - 1 else NA_real_
              bm <- if (!is.na(sens) && !is.na(spec)) sens + spec - 1 else NA_real_

              f1 <- if (!is.na(ppv) && !is.na(sens) && (ppv + sens) > 0) 2 * ppv * sens / (ppv + sens) else NA_real_
              fm <- if (!is.na(ppv) && !is.na(sens)) sqrt(ppv * sens) else NA_real_
              ts <- if ((TP + FN + FP) > 0) TP / (TP + FN + FP) else NA_real_

              denom_mcc <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
              mcc <- if (denom_mcc > 0) ((TP * TN) - (FP * FN)) / denom_mcc else NA_real_

              PT <- if (!is.na(sens) && !is.na(fpr) && (sens - fpr) != 0) {
                            (sqrt(sens * fpr) - fpr) / (sens - fpr)
              } else {
                            NA_real_
              }

              # Haldane–Anscombe for LR/DOR if any zero
              TPc <- TP
              FPc <- FP
              FNc <- FN
              TNc <- TN
              if (add_0_5_for_lr && any(c(TP, FP, FN, TN) == 0)) {
                            TPc <- TP + 0.5
                            FPc <- FP + 0.5
                            FNc <- FN + 0.5
                            TNc <- TN + 0.5
              }
              sens_c <- if ((TPc + FNc) > 0) TPc / (TPc + FNc) else NA_real_
              spec_c <- if ((TNc + FPc) > 0) TNc / (TNc + FPc) else NA_real_
              fpr_c <- if (!is.na(spec_c)) 1 - spec_c else NA_real_

              lr_pos <- if (!is.na(sens_c) && !is.na(fpr_c) && fpr_c > 0) sens_c / fpr_c else NA_real_
              lr_neg <- if (!is.na(spec_c) && !is.na(sens_c) && spec_c > 0) (1 - sens_c) / spec_c else NA_real_
              dor <- if (!is.na(lr_pos) && !is.na(lr_neg) && lr_neg > 0) lr_pos / lr_neg else NA_real_

              # ----------------------------- CIs -------------------------------------
              ci_sens <- binom_ci(TP, P, conf)
              ci_spec <- binom_ci(TN, N, conf)
              ci_ppv <- binom_ci(TP, TP + FP, conf)
              ci_npv <- binom_ci(TN, TN + FN, conf)
              ci_acc <- binom_ci(TP + TN, Tn, conf)
              ci_prev <- binom_ci(P, Tn, conf)

              ci_fpr <- ci_complement(ci_spec)
              ci_fnr <- ci_complement(ci_sens)
              ci_fdr <- ci_complement(ci_ppv)
              ci_for <- ci_complement(ci_npv)

              se_log_lr_pos <- if (all(c(TPc, FPc, TPc + FNc, TNc + FPc) > 0)) {
                            sqrt(1 / TPc - 1 / (TPc + FNc) + 1 / FPc - 1 / (TNc + FPc))
              } else {
                            NA_real_
              }
              se_log_lr_neg <- if (all(c(FNc, TNc, TPc + FNc, TNc + FPc) > 0)) {
                            sqrt(1 / FNc - 1 / (TPc + FNc) + 1 / TNc - 1 / (TNc + FPc))
              } else {
                            NA_real_
              }
              se_log_dor <- if (all(c(TPc, FPc, FNc, TNc) > 0)) {
                            sqrt(1 / TPc + 1 / FPc + 1 / FNc + 1 / TNc)
              } else {
                            NA_real_
              }

              ci_lr_pos <- if (!is.na(lr_pos) && !is.na(se_log_lr_pos)) {
                            c(est = lr_pos, lognorm_ci_ratio(lr_pos, se_log_lr_pos, conf))
              } else {
                            c(est = NA_real_, NA_real_, NA_real_)
              }
              ci_lr_neg <- if (!is.na(lr_neg) && !is.na(se_log_lr_neg)) {
                            c(est = lr_neg, lognorm_ci_ratio(lr_neg, se_log_lr_neg, conf))
              } else {
                            c(est = NA_real_, NA_real_, NA_real_)
              }
              ci_dor <- if (!is.na(dor) && !is.na(se_log_dor)) {
                            c(est = dor, lognorm_ci_ratio(dor, se_log_dor, conf))
              } else {
                            c(est = NA_real_, NA_real_, NA_real_)
              }

              # ----------------------------- collect rows ----------------------------
              out <- do.call(rbind, list(
                            as_row("prevalence", ci_prev[["est"]], ci_prev[["lwr"]], ci_prev[["upr"]], "exact binomial"),
                            as_row("accuracy", ci_acc[["est"]], ci_acc[["lwr"]], ci_acc[["upr"]], "exact binomial"),
                            as_row("sensitivity (TPR)", ci_sens[["est"]], ci_sens[["lwr"]], ci_sens[["upr"]], "exact binomial"),
                            as_row("specificity (TNR)", ci_spec[["est"]], ci_spec[["lwr"]], ci_spec[["upr"]], "exact binomial"),
                            as_row("PPV (precision)", ci_ppv[["est"]], ci_ppv[["lwr"]], ci_ppv[["upr"]], "exact binomial"),
                            as_row("NPV", ci_npv[["est"]], ci_npv[["lwr"]], ci_npv[["upr"]], "exact binomial"),
                            as_row(
                                          "FPR", ci_fpr[["est"]], ci_fpr[["lwr"]], ci_fpr[["upr"]],
                                          "complement of specificity CI"
                            ),
                            as_row(
                                          "FNR", ci_fnr[["est"]], ci_fnr[["lwr"]], ci_fnr[["upr"]],
                                          "complement of sensitivity CI"
                            ),
                            as_row(
                                          "FDR", ci_fdr[["est"]], ci_fdr[["lwr"]], ci_fdr[["upr"]],
                                          "complement of PPV CI"
                            ),
                            as_row(
                                          "FOR", ci_for[["est"]], ci_for[["lwr"]], ci_for[["upr"]],
                                          "complement of NPV CI"
                            ),
                            as_row(
                                          "LR+", ci_lr_pos[[1]], ci_lr_pos[[2]], ci_lr_pos[[3]],
                                          "log-normal (approx.)",
                                          if (add_0_5_for_lr && any(c(TP, FP, FN, TN) == 0)) "Haldane–Anscombe +0.5 used" else NA_character_
                            ),
                            as_row(
                                          "LR−", ci_lr_neg[[1]], ci_lr_neg[[2]], ci_lr_neg[[3]],
                                          "log-normal (approx.)",
                                          if (add_0_5_for_lr && any(c(TP, FP, FN, TN) == 0)) "Haldane–Anscombe +0.5 used" else NA_character_
                            ),
                            as_row("DOR", ci_dor[[1]], ci_dor[[2]], ci_dor[[3]], "log-normal (approx.)"),
                            as_row("Prevalence threshold (PT)", PT, NA, NA, "point estimate"),
                            as_row("balanced accuracy", ba, NA, NA, "point estimate"),
                            as_row("F1 score", f1, NA, NA, "point estimate"),
                            as_row("Fowlkes–Mallows", fm, NA, NA, "point estimate"),
                            as_row("Threat score (CSI)", ts, NA, NA, "point estimate"),
                            as_row("Markedness (Δp)", mk, NA, NA, "point estimate"),
                            as_row("Bookmaker informedness (BM/Youden)", bm, NA, NA, "point estimate"),
                            as_row("Matthews corr. coeff. (MCC)", mcc, NA, NA, "point estimate")
              ))

              # ----------------------------- bootstrap CIs (optional) ----------------
              if (boot) {
                            set.seed(seed)
                            probs <- c(TP, FP, FN, TN)
                            if (sum(probs) == 0) stop("All counts are zero; cannot bootstrap.")
                            probs <- probs / sum(probs)
                            total <- Tn

                            one_metric <- function(tp, fp, fn, tn) {
                                          P <- tp + fn
                                          N <- tn + fp
                                          sens <- if (P > 0) tp / P else NA_real_
                                          spec <- if (N > 0) tn / N else NA_real_
                                          ppv <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
                                          npv <- if ((tn + fn) > 0) tn / (tn + fn) else NA_real_
                                          ba <- mean(c(sens, spec), na.rm = TRUE)
                                          f1 <- if (!is.na(ppv) && !is.na(sens) && (ppv + sens) > 0) 2 * ppv * sens / (ppv + sens) else NA_real_
                                          fm <- if (!is.na(ppv) && !is.na(sens)) sqrt(ppv * sens) else NA_real_
                                          ts <- if ((tp + fn + fp) > 0) tp / (tp + fn + fp) else NA_real_
                                          mk <- if (!is.na(ppv) && !is.na(npv)) ppv + npv - 1 else NA_real_
                                          bm <- if (!is.na(sens) && !is.na(spec)) sens + spec - 1 else NA_real_
                                          denom <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
                                          mcc <- if (denom > 0) ((tp * tn) - (fp * fn)) / denom else NA_real_
                                          c(ba = ba, f1 = f1, fm = fm, ts = ts, mk = mk, bm = bm, mcc = mcc)
                            }

                            boot_mat <- matrix(NA_real_, nrow = B, ncol = 7)
                            colnames(boot_mat) <- c(
                                          "balanced accuracy", "F1 score", "Fowlkes–Mallows",
                                          "Threat score (CSI)", "Markedness (Δp)",
                                          "Bookmaker informedness (BM/Youden)", "Matthews corr. coeff. (MCC)"
                            )

                            for (b in seq_len(B)) {
                                          draw <- as.vector(stats::rmultinom(1, size = total, prob = probs))
                                          boot_mat[b, ] <- do.call(one_metric, as.list(draw))
                            }

                            add_ci <- function(metric_name, point_est) {
                                          vals <- boot_mat[, metric_name]
                                          vals <- vals[is.finite(vals)]
                                          if (length(vals) < 10L) {
                                                        return(NULL)
                                          }
                                          qs <- stats::quantile(vals, probs = c((1 - conf) / 2, 1 - (1 - conf) / 2), na.rm = TRUE, type = 6)
                                          as_row(
                                                        metric_name, point_est, unname(qs[1]), unname(qs[2]),
                                                        sprintf("nonparametric bootstrap (B=%d, multinomial on 2x2)", B)
                                          )
                            }

                            boot_rows <- do.call(rbind, lapply(colnames(boot_mat), function(nm) {
                                          pe <- out$estimate[out$metric == nm][1]
                                          add_ci(nm, pe)
                            }))
                            if (!is.null(boot_rows) && nrow(boot_rows) > 0) {
                                          keep <- !(out$metric %in% colnames(boot_mat))
                                          out <- rbind(out[keep, ], boot_rows)
                            }
              }

              tibble::as_tibble(out)
}

#' Diagnostic Test Accuracy Summary
#'
#' @description A wrapper around `confusion_metrics_ci` to produce a summary table
#'   from raw data columns (Truth, Test).
#'
#' @param data Data frame
#' @param truth Column with true status (binary)
#' @param test Column with test result (binary or numeric)
#' @param cutoff Cutoff for numeric test results (default: 0.5)
#' @param positive Value indicating positive case (optional, auto-detected)
#' @param ... Additional arguments passed to `confusion_metrics_ci`
#'
#' @return Tibble with diagnostic metrics
#' @export
diagnostic_summary <- function(data, truth, test, cutoff = 0.5, positive = NULL, ...) {
              truth_enquo <- rlang::enquo(truth)
              test_enquo <- rlang::enquo(test)

              truth_vec <- data %>% dplyr::pull(!!truth_enquo)
              test_vec <- data %>% dplyr::pull(!!test_enquo)

              # Handle numeric predictions
              if (is.numeric(test_vec) && length(unique(test_vec)) > 2) {
                            test_class <- ifelse(test_vec >= cutoff, 1, 0)
                            # If truth is not numeric 0/1, we might need to be careful, but let's assume 0/1 for numeric test
              } else {
                            test_class <- test_vec
              }

              # Determine positive class
              levels_truth <- levels(factor(truth_vec))
              if (is.null(positive)) {
                            # Assume the last level is positive (standard R behavior) or '1' or 'Yes'
                            positive <- levels_truth[length(levels_truth)]
              }

              # Calculate Confusion Matrix
              tp <- sum(test_class == positive & truth_vec == positive, na.rm = TRUE)
              tn <- sum(test_class != positive & truth_vec != positive, na.rm = TRUE)
              fp <- sum(test_class == positive & truth_vec != positive, na.rm = TRUE)
              fn <- sum(test_class != positive & truth_vec == positive, na.rm = TRUE)

              # Call confusion_metrics_ci
              confusion_metrics_ci(c(tp = tp, fp = fp, fn = fn, tn = tn), ...)
}
