#' Bland-Altman Plot and Analysis (KK)
#'
#' @description Performs Bland-Altman analysis to assess agreement between two methods of measurement.
#'
#' @param method1 Vector of measurements from method 1
#' @param method2 Vector of measurements from method 2
#' @param conf.level Confidence level for limits of agreement (default 0.95)
#' @param plot Return a ggplot object? (default TRUE)
#' @param title Plot title
#'
#' @return If plot=TRUE, a ggplot object. If plot=FALSE, a tibble with stats.
#' @export
#' @examples
#' m1 <- c(10, 12, 15, 20, 25)
#' m2 <- c(11, 13, 14, 21, 24)
#' kk_bland_altman(m1, m2)
kk_bland_altman <- function(method1, method2, conf.level = 0.95, plot = TRUE, title = "Bland-Altman Plot") {
              if (length(method1) != length(method2)) stop("Vectors must be same length")

              df <- data.frame(
                            m1 = method1,
                            m2 = method2
              ) %>%
                            dplyr::mutate(
                                          diff = m1 - m2,
                                          mean = (m1 + m2) / 2
                            )

              mean_diff <- mean(df$diff, na.rm = TRUE)
              sd_diff <- stats::sd(df$diff, na.rm = TRUE)
              n <- sum(!is.na(df$diff))

              # Limits of Agreement (LoA)
              # 1.96 * SD
              z <- stats::qnorm(1 - (1 - conf.level) / 2) # Usually 1.96 for 95%

              upper_loa <- mean_diff + z * sd_diff
              lower_loa <- mean_diff - z * sd_diff

              # CIs for the LoA (approximate, Bland & Altman 1999)
              se_loa <- sqrt(3 * sd_diff^2 / n)

              stats_tbl <- tibble::tibble(
                            Metric = c("Mean Difference (Bias)", "Upper LoA", "Lower LoA"),
                            Value = c(mean_diff, upper_loa, lower_loa),
                            SD = sd_diff,
                            N = n
              )

              if (!plot) {
                            return(stats_tbl)
              }

              if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")

              p <- ggplot2::ggplot(df, ggplot2::aes(x = mean, y = diff)) +
                            ggplot2::geom_point(alpha = 0.6) +
                            ggplot2::geom_hline(yintercept = mean_diff, color = "blue", size = 1) +
                            ggplot2::geom_hline(yintercept = upper_loa, color = "red", linetype = "dashed") +
                            ggplot2::geom_hline(yintercept = lower_loa, color = "red", linetype = "dashed") +
                            ggplot2::labs(
                                          title = title,
                                          x = "Mean of two methods",
                                          y = "Difference (Method 1 - Method 2)",
                                          caption = paste0(
                                                        "Bias: ", round(mean_diff, 3),
                                                        " | LoA: [", round(lower_loa, 3), ", ", round(upper_loa, 3), "]"
                                          )
                            ) +
                            ggplot2::theme_minimal()

              return(p)
}
