#' Bland-Altman Plot and Analysis (KK)
#'
#' @description Performs Bland-Altman analysis to assess agreement between two methods of measurement.
#'   Supports both vector inputs and passing a data frame directly with column symbols or strings.
#'
#' @param method1 Vector of measurements from method 1, or a data frame. If a data frame is provided, the
#'   column names representing the two methods must follow.
#' @param method2 Vector of measurements from method 2, or column name/symbol if `method1` is a data frame.
#' @param conf.level Confidence level for limits of agreement (default 0.95)
#' @param plot Return a ggplot object? (default TRUE)
#' @param title Plot title
#' @param ... Additional arguments passed to methods.
#'
#' @return If plot=TRUE, a ggplot object. If plot=FALSE, a tibble with stats.
#' @export
#' @examples
#' # Vector mode
#' m1 <- c(10, 12, 15, 20, 25)
#' m2 <- c(11, 13, 14, 21, 24)
#' kk_bland_altman(m1, m2)
#'
#' # DataFrame mode
#' df_bp <- data.frame(
#'   arterial = rnorm(50, 120, 15),
#'   cuff = rnorm(50, 118, 14)
#' )
#' kk_bland_altman(df_bp, arterial, cuff)
#'
#' @export
kk_bland_altman <- function(method1, method2 = NULL, ...) {
  # Helper to resolve argument names (handles strings, symbols, or variable contents)
  resolve_arg <- function(arg_expr, pf = parent.frame()) {
    if (is.character(arg_expr)) {
      return(arg_expr)
    }
    if (is.symbol(arg_expr)) {
      val <- tryCatch(eval(arg_expr, pf), error = function(e) NULL)
      if (is.character(val) && length(val) == 1) {
        return(val)
      }
      return(as.character(arg_expr))
    }
    return(deparse(arg_expr))
  }

  cl_list <- as.list(match.call(expand.dots = TRUE))
  pf <- parent.frame()
  
  # Detect if a data frame is passed anywhere
  data_arg_name <- NULL
  data <- NULL
  
  for (name in names(cl_list)[-1]) {
    val <- tryCatch(eval(cl_list[[name]], pf), error = function(e) NULL)
    if (inherits(val, "data.frame")) {
      data <- val
      data_arg_name <- name
      break
    }
  }

  if (!is.null(data)) {
    # DataFrame Mode
    param_names <- c("conf.level", "plot", "title")
    col_args <- list()
    
    for (i in seq_along(cl_list)[-1]) {
      arg_name <- names(cl_list)[i]
      if (identical(arg_name, data_arg_name)) next
      if (!is.null(arg_name) && arg_name %in% param_names) next
      col_args[[length(col_args) + 1]] <- cl_list[[i]]
    }
    
    if (length(col_args) < 2) {
      stop("Must specify two column names for Bland-Altman analysis.")
    }
    
    m1_name <- resolve_arg(col_args[[1]])
    m2_name <- resolve_arg(col_args[[2]])
    
    if (!all(c(m1_name, m2_name) %in% names(data))) {
      stop("Specified columns not found in data frame.")
    }
    
    vec1 <- data[[m1_name]]
    vec2 <- data[[m2_name]]
    
    conf.level <- if ("conf.level" %in% names(cl_list)) eval(cl_list[["conf.level"]], pf) else 0.95
    plot <- if ("plot" %in% names(cl_list)) eval(cl_list[["plot"]], pf) else TRUE
    title <- if ("title" %in% names(cl_list)) eval(cl_list[["title"]], pf) else "Bland-Altman Plot"
  } else {
    # Vector Mode
    vec1 <- method1
    vec2 <- method2
    
    conf.level <- if ("conf.level" %in% names(cl_list)) eval(cl_list[["conf.level"]], pf) else 0.95
    plot <- if ("plot" %in% names(cl_list)) eval(cl_list[["plot"]], pf) else TRUE
    title <- if ("title" %in% names(cl_list)) eval(cl_list[["title"]], pf) else "Bland-Altman Plot"
    
    # Handle positional arguments in ...
    unnamed_args <- list()
    for (i in seq_along(cl_list)[-1]) {
      arg_name <- names(cl_list)[i]
      if (identical(arg_name, "method1") || identical(arg_name, "method2")) next
      if (is.null(arg_name) || arg_name == "") {
        unnamed_args[[length(unnamed_args) + 1]] <- eval(cl_list[[i]], pf)
      }
    }
    
    if (length(unnamed_args) >= 1 && is.numeric(unnamed_args[[1]])) {
      conf.level <- unnamed_args[[1]]
    }
    if (length(unnamed_args) >= 2 && is.logical(unnamed_args[[2]])) {
      plot <- unnamed_args[[2]]
    }
    if (length(unnamed_args) >= 3 && is.character(unnamed_args[[3]])) {
      title <- unnamed_args[[3]]
    }
  }

  if (length(vec1) != length(vec2)) stop("Vectors must be same length")

  df <- data.frame(
    m1 = vec1,
    m2 = vec2
  ) %>%
    dplyr::mutate(
      diff = m1 - m2,
      mean = (m1 + m2) / 2
    )

  mean_diff <- mean(df$diff, na.rm = TRUE)
  sd_diff <- stats::sd(df$diff, na.rm = TRUE)
  n <- sum(!is.na(df$diff))

  # Limits of Agreement (LoA)
  z <- stats::qnorm(1 - (1 - conf.level) / 2)

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
