#' Relative Excess Risk due to Interaction (RERI)
#'
#' @description Calculates RERI (Relative Excess Risk due to Interaction) on the additive scale
#'   using a logistic regression model.
#'   Assumes the model contains two binary exposures and their interaction.
#'   Model formula should be roughly: outcome ~ exp1 * exp2 + covariates
#'
#' @param model A glm object (family = binomial)
#' @param exp1 Name of first exposure variable (string)
#' @param exp2 Name of second exposure variable (string)
#' @param conf.level Confidence level (default 0.95)
#'
#' @return Tibble with RERI estimate and CIs (using delta method)
#' @export
#' @examples
#' \dontrun{
#' model <- glm(outcome ~ exp1 * exp2 + age, family = binomial, data = df)
#' kk_reri(model, "exp1", "exp2")
#' }
kk_reri <- function(model, exp1, exp2, conf.level = 0.95) {
              if (!inherits(model, "glm")) stop("Model must be a glm object")

              coefs <- stats::coef(model)
              vcov_mat <- stats::vcov(model)

              # Identify coefficients
              # We need b1 (exp1), b2 (exp2), and b3 (exp1:exp2)
              # Names might vary depending on factor levels or interaction syntax

              # Try to find exact matches
              b1_name <- exp1
              b2_name <- exp2
              b3_name <- paste0(exp1, ":", exp2)
              b3_alt <- paste0(exp2, ":", exp1)

              if (!b3_name %in% names(coefs) && b3_alt %in% names(coefs)) {
                            b3_name <- b3_alt
              }

              if (!all(c(b1_name, b2_name, b3_name) %in% names(coefs))) {
                            stop("Could not find exact coefficients for exposures and interaction. Ensure variables are numeric/binary 0/1 or check names(coef(model)).")
              }

              b1 <- coefs[[b1_name]]
              b2 <- coefs[[b2_name]]
              b3 <- coefs[[b3_name]]

              # RERI = RR_11 - RR_10 - RR_01 + 1
              # In logistic regression (assuming rare disease), OR approx RR
              # RR_11 = exp(b1 + b2 + b3)
              # RR_10 = exp(b1)
              # RR_01 = exp(b2)
              # RERI = exp(b1 + b2 + b3) - exp(b1) - exp(b2) + 1

              reri_est <- exp(b1 + b2 + b3) - exp(b1) - exp(b2) + 1

              # Delta Method for Variance
              # Gradient vector g = [dRERI/db1, dRERI/db2, dRERI/db3]

              # d/db1 = exp(b1+b2+b3) - exp(b1)
              # d/db2 = exp(b1+b2+b3) - exp(b2)
              # d/db3 = exp(b1+b2+b3)

              g1 <- exp(b1 + b2 + b3) - exp(b1)
              g2 <- exp(b1 + b2 + b3) - exp(b2)
              g3 <- exp(b1 + b2 + b3)

              grad <- matrix(c(g1, g2, g3), nrow = 1)

              # Extract relevant covariance matrix (3x3)
              relevant_indices <- c(b1_name, b2_name, b3_name)
              sub_vcov <- vcov_mat[relevant_indices, relevant_indices]

              var_reri <- as.numeric(grad %*% sub_vcov %*% t(grad))
              se_reri <- sqrt(var_reri)

              z <- stats::qnorm(1 - (1 - conf.level) / 2)

              reri_low <- reri_est - z * se_reri
              reri_high <- reri_est + z * se_reri

              tibble::tibble(
                            Metric = "RERI",
                            Estimate = reri_est,
                            Lower = reri_low,
                            Upper = reri_high,
                            SE = se_reri,
                            Conf_Level = conf.level
              )
}
