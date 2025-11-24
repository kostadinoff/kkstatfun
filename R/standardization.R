#' Direct Standardization of Rates (KK)
#'
#' @description Performs direct standardization of rates using a standard population.
#'
#' @param data Data frame containing count and population columns
#' @param count Column name for event counts
#' @param pop Column name for population (person-years or count)
#' @param std_pop Vector of standard population weights/counts. Must match number of rows in data.
#' @param conf.level Confidence level (default 0.95)
#' @param multiplier Multiplier for rates (e.g., 1000 or 100000)
#'
#' @return Tibble with crude and adjusted rates
#' @export
#' @examples
#' df <- data.frame(
#'               age_group = c("0-19", "20-39", "40-59", "60+"),
#'               cases = c(10, 25, 50, 100),
#'               pop = c(5000, 8000, 6000, 4000)
#' )
#' std_pop <- c(4000, 7000, 6000, 3000) # WHO standard or similar
#' kk_std_rates(df, cases, pop, std_pop, multiplier = 1000)
kk_std_rates <- function(data, count, pop, std_pop, conf.level = 0.95, multiplier = 1000) {
              count_enquo <- rlang::enquo(count)
              pop_enquo <- rlang::enquo(pop)

              d <- data %>%
                            dplyr::mutate(
                                          obs = !!count_enquo,
                                          n = !!pop_enquo,
                                          w = std_pop
                            )

              # Crude Rate
              total_obs <- sum(d$obs)
              total_pop <- sum(d$n)
              crude_rate <- (total_obs / total_pop) * multiplier

              # Adjusted Rate (Direct Standardization)
              # Rate_adj = sum(rate_i * w_i) / sum(w_i)
              d <- d %>%
                            dplyr::mutate(
                                          rate_i = obs / n,
                                          expected_i = rate_i * w
                            )

              total_w <- sum(d$w)
              adj_rate_val <- sum(d$expected_i) / total_w

              # Variance of adjusted rate (Gamma approximation or normal)
              # Var(R_adj) = sum(w_i^2 * var(r_i)) / (sum w_i)^2
              # var(r_i) = obs / n^2 (Poisson assumption)

              d <- d %>%
                            dplyr::mutate(
                                          var_ri = obs / (n^2)
                            )

              var_adj <- sum((d$w^2) * d$var_ri) / (total_w^2)
              se_adj <- sqrt(var_adj)

              z <- stats::qnorm(1 - (1 - conf.level) / 2)

              adj_rate <- adj_rate_val * multiplier
              adj_low <- (adj_rate_val - z * se_adj) * multiplier
              adj_high <- (adj_rate_val + z * se_adj) * multiplier

              # Ensure non-negative lower bound
              adj_low <- pmax(0, adj_low)

              tibble::tibble(
                            Type = c("Crude Rate", "Adjusted Rate"),
                            Rate = c(crude_rate, adj_rate),
                            Lower = c(NA, adj_low),
                            Upper = c(NA, adj_high),
                            Multiplier = multiplier,
                            Conf_Level = conf.level
              )
}
