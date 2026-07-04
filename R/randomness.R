# ============================================================
# RANDOMNESS TESTS
# ============================================================

#' Runs Test for Randomness
#'
#' @description Performs runs tests to evaluate the randomness of a sequence.
#'   Supports both the Single-Sample Runs Test (Wald-Wolfowitz test for nominal/categorical data)
#'   and the Runs Test for Serial Randomness (up-down runs test for quantitative data).
#'   The single-sample test is generalized to handle two or more categories.
#'
#' @param data A data frame or a vector. If a data frame is provided, the column
#'   to analyze must be specified in the `x` argument.
#' @param x Column name (as symbol or string) to analyze if `data` is a data frame.
#'   Ignored if `data` is a vector.
#' @param method Character string specifying the test method. Either `"single_sample"`
#'   (default, Wald-Wolfowitz runs test for categorical data) or `"up_down"`
#'   (Wallis-Moore runs test for serial randomness on numeric data).
#' @param alternative Character string specifying the alternative hypothesis.
#'   Must be one of `"two.sided"` (default), `"less"` (too few runs), or `"greater"` (too many runs).
#' @param threshold Used only when `method = "single_sample"` and the input is numeric/continuous.
#'   Can be a numeric value, `"median"`, `"mean"`, or `NULL` (default). If a value is provided,
#'   continuous data is dichotomized. If `NULL`, the unique values are treated as distinct categories.
#' @param tie_action Used only when `method = "up_down"`. Action to take when consecutive values are equal (no change).
#'   Must be one of `"omit"` (default, removes ties), `"plus"` (treats ties as increases),
#'   `"minus"` (treats ties as decreases), or `"both"` (runs tests for both `"plus"` and `"minus"` and returns both).
#' @param continuity_correction Logical, whether to apply the correction for continuity
#'   (subtracting 0.5 from the absolute difference between observed and expected runs) in the normal approximation.
#'   Defaults to `TRUE`.
#' @param conf.level Confidence level for the interval (default: 0.95).
#'
#' @return A tibble with the following columns:
#'   \itemize{
#'     \item \code{method}: The test method used.
#'     \item \code{alternative}: The alternative hypothesis.
#'     \item \code{n_obs}: The effective number of observations.
#'     \item \code{observed_runs}: The total number of runs ($r$).
#'     \item \code{expected_runs}: The expected number of runs under the null hypothesis ($\mu$).
#'     \item \code{variance_runs}: The variance of the number of runs ($\sigma^2$).
#'     \item \code{z_statistic}: The standard normal $Z$-statistic.
#'     \item \code{p_value}: The p-value of the test.
#'     \item \code{tie_action}: (Only for \code{"up_down"}) The tie-handling action applied.
#'   }
#'
#' @examples
#' # Single-Sample Runs Test (Wald-Wolfowitz) - binary data
#' coin_tosses <- c("H", "H", "H", "T", "T", "T", "H", "H", "T", "T",
#'                  "H", "T", "H", "T", "H", "T", "T", "T", "H", "H")
#' kk_runs_test(coin_tosses)
#'
#' # Multi-category Runs Test
#' die_rolls <- c("A", "A", "B", "C", "C", "B", "A", "A", "B", "B",
#'                "C", "C", "A", "C", "B", "A", "A", "B", "B", "B")
#' kk_runs_test(die_rolls, method = "single_sample")
#'
#' # Up-down Runs Test for Serial Randomness
#' milk_dispensed <- c(1.90, 1.99, 2.00, 1.78, 1.77, 1.76, 1.98, 1.90, 1.65, 1.76,
#'                     2.01, 1.78, 1.99, 1.76, 1.94, 1.78, 1.67, 1.87, 1.91, 1.91, 1.89)
#' kk_runs_test(milk_dispensed, method = "up_down", tie_action = "plus")
#'
#' # Tidy evaluation with a data frame
#' df <- data.frame(val = milk_dispensed)
#' kk_runs_test(df, val, method = "up_down")
#'
#' @export
kk_runs_test <- function(data, x = NULL,
                         method = c("single_sample", "up_down"),
                         alternative = c("two.sided", "less", "greater"),
                         threshold = NULL,
                         tie_action = c("omit", "plus", "minus", "both"),
                         continuity_correction = TRUE,
                         conf.level = 0.95) {
  
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  tie_action <- match.arg(tie_action)
  
  # Handle grouped data frame
  if (dplyr::is_grouped_df(data)) {
    x_enquo <- rlang::enquo(x)
    res <- data %>%
      dplyr::group_modify(function(grp_data, grp_keys) {
        rlang::inject(kk_runs_test(
          data = grp_data,
          x = !!x_enquo,
          method = method,
          alternative = alternative,
          threshold = threshold,
          tie_action = tie_action,
          continuity_correction = continuity_correction,
          conf.level = conf.level
        ))
      })
    return(res)
  }
  
  x_enquo <- rlang::enquo(x)
  # Handle data frame vs vector input
  if (is.data.frame(data)) {
    if (rlang::quo_is_null(x_enquo)) {
      stop("Argument 'x' must be specified when 'data' is a data frame.")
    }
    vec <- data %>% dplyr::pull(!!x_enquo)
  } else {
    vec <- data
  }
  
  # Remove NA values
  vec <- vec[!is.na(vec)]
  
  if (length(vec) < 2) {
    stop("Input sequence must have at least 2 non-missing observations.")
  }
  
  if (method == "single_sample") {
    # If numeric and threshold is supplied, dichotomize
    if (is.numeric(vec) && !is.null(threshold)) {
      if (length(unique(vec)) <= 1) {
        stop("Numeric vector has no variation; cannot perform dichotomization.")
      }
      
      threshold_val <- if (is.numeric(threshold)) {
        threshold
      } else if (threshold == "median") {
        stats::median(vec)
      } else if (threshold == "mean") {
        stats::mean(vec)
      } else {
        stop("Invalid threshold. Must be a numeric value, 'median', or 'mean'.")
      }
      
      vec <- ifelse(vec >= threshold_val, ">=threshold", "<threshold")
    }
    
    unique_vals <- unique(vec)
    k <- length(unique_vals)
    
    if (k < 2) {
      stop("Sequence must contain at least 2 categories/values to perform a runs test.")
    }
    
    # Calculate observed runs (r)
    rle_res <- rle(as.character(vec))
    r <- length(rle_res$lengths)
    
    # Calculate category frequencies
    n_table <- table(vec)
    n <- as.numeric(n_table)
    N <- sum(n)
    
    # Expected mean (Wallis and Roberts 1956, Zar 1999)
    sum_n2 <- sum(n^2)
    expected_runs <- N + 1 - (sum_n2 / N)
    
    # Expected variance
    sum_n3 <- sum(n^3)
    numerator_var <- sum_n2 * (sum_n2 + N * (N + 1)) - 2 * N * sum_n3 - N^2 * (N + 1)
    denominator_var <- N^2 * (N - 1)
    variance_runs <- numerator_var / denominator_var
    
    # Compute z-statistic
    sd_runs <- sqrt(variance_runs)
    diff_runs <- r - expected_runs
    
    if (sd_runs == 0) {
      z_stat <- NA_real_
    } else {
      if (continuity_correction) {
        z_stat <- sign(diff_runs) * max(0, abs(diff_runs) - 0.5) / sd_runs
      } else {
        z_stat <- diff_runs / sd_runs
      }
    }
    
    p_val <- if (is.na(z_stat)) {
      NA_real_
    } else {
      switch(alternative,
             two.sided = 2 * stats::pnorm(-abs(z_stat)),
             less = stats::pnorm(z_stat),
             greater = stats::pnorm(z_stat, lower.tail = FALSE))
    }
    
    return(tibble::tibble(
      method = "Single-Sample Runs Test",
      alternative = alternative,
      n_obs = N,
      observed_runs = r,
      expected_runs = expected_runs,
      variance_runs = variance_runs,
      z_statistic = z_stat,
      p_value = p_val
    ))
    
  } else if (method == "up_down") {
    if (!is.numeric(vec)) {
      stop("Input data must be numeric for the 'up_down' runs test.")
    }
    
    run_test_up_down_single <- function(signs, current_tie_action) {
      rle_res <- rle(signs)
      r <- length(rle_res$lengths)
      N_eff <- length(signs) + 1
      
      expected_runs <- (2 * N_eff - 1) / 3
      variance_runs <- (16 * N_eff - 29) / 90
      
      sd_runs <- sqrt(variance_runs)
      diff_runs <- r - expected_runs
      
      if (sd_runs == 0) {
        z_stat <- NA_real_
      } else {
        if (continuity_correction) {
          z_stat <- sign(diff_runs) * max(0, abs(diff_runs) - 0.5) / sd_runs
        } else {
          z_stat <- diff_runs / sd_runs
        }
      }
      
      p_val <- if (is.na(z_stat)) {
        NA_real_
      } else {
        switch(alternative,
               two.sided = 2 * stats::pnorm(-abs(z_stat)),
               less = stats::pnorm(z_stat),
               greater = stats::pnorm(z_stat, lower.tail = FALSE))
      }
      
      tibble::tibble(
        method = "Runs Test for Serial Randomness (Up-Down)",
        alternative = alternative,
        n_obs = N_eff,
        observed_runs = r,
        expected_runs = expected_runs,
        variance_runs = variance_runs,
        z_statistic = z_stat,
        p_value = p_val,
        tie_action = current_tie_action
      )
    }
    
    diffs <- diff(vec)
    signs_raw <- sign(diffs)
    
    # Process ties according to tie_action
    if (tie_action == "both") {
      # Execute both "plus" and "minus"
      signs_plus <- ifelse(signs_raw == 0, 1, signs_raw)
      signs_minus <- ifelse(signs_raw == 0, -1, signs_raw)
      
      res_plus <- run_test_up_down_single(signs_plus, "plus")
      res_minus <- run_test_up_down_single(signs_minus, "minus")
      
      return(dplyr::bind_rows(res_plus, res_minus))
    } else {
      signs_final <- if (tie_action == "omit") {
        signs_raw[signs_raw != 0]
      } else if (tie_action == "plus") {
        ifelse(signs_raw == 0, 1, signs_raw)
      } else if (tie_action == "minus") {
        ifelse(signs_raw == 0, -1, signs_raw)
      }
      
      if (length(signs_final) < 1) {
        stop("No differences remain after removing ties.")
      }
      
      return(run_test_up_down_single(signs_final, tie_action))
    }
  }
}

#' Alias for kk_runs_test
#'
#' @description Direct alias for \code{\link{kk_runs_test}} to check sequence randomness.
#'
#' @inheritParams kk_runs_test
#'
#' @return A tibble with runs test results.
#'
#' @export
kk_random_seq <- kk_runs_test


#' Frequency Test of Randomness (Equidistribution Test)
#'
#' @description Performs the frequency test of randomness on categorical or nominal data.
#'   Uses a Chi-Square Goodness-of-Fit test to evaluate if categories occur with equal
#'   probability (or custom specified probabilities). If the data is binary ($k = 2$),
#'   it also performs the Exact Binomial Sign Test and the Binomial Z-Test (normal approximation)
#'   with continuity correction.
#'
#' @param data A data frame or a vector. If a data frame is provided, the column
#'   to analyze must be specified in the `x` argument.
#' @param x Column name (as symbol or string) to analyze if `data` is a data frame.
#'   Ignored if `data` is a vector.
#' @param p Expected probabilities under the null hypothesis. Can be a single numeric value
#'   for $k=2$ (representing probability of first category), a numeric vector of length $k$ (summing to 1),
#'   or `NULL` (default, assuming equally probable alternatives $p_j = 1/k$).
#' @param alternative Character string specifying the alternative hypothesis for binary tests.
#'   Must be one of `"two.sided"` (default), `"less"`, or `"greater"`. Ignored for $k > 2$ Chi-Square test.
#' @param continuity_correction Logical, whether to apply the correction for continuity in the binomial Z-test (default: `TRUE`).
#' @param conf.level Confidence level for the interval (default: 0.95).
#'
#' @return A tibble with the following columns:
#'   \itemize{
#'     \item \code{method}: The test method used.
#'     \item \code{alternative}: The alternative hypothesis.
#'     \item \code{statistic_name}: Name of the test statistic ("Chi-Square", "Exact", or "Z").
#'     \item \code{statistic}: The test statistic value.
#'     \item \code{df}: The degrees of freedom for the Chi-Square test (or \code{NA} for binomial tests).
#'     \item \code{p_value}: The p-value of the test.
#'     \item \code{observed}: Observed frequencies of categories formatted as a string.
#'     \item \code{expected}: Expected frequencies of categories formatted as a string.
#'   }
#'
#' @examples
#' # Binary sequence frequency test (Example from temp.md: Heads=21, Tails=9, N=30)
#' flips <- c(rep("H", 21), rep("T", 9))
#' kk_frequency_test(flips, p = 0.5)
#'
#' # Multi-category frequency test (Example from temp.md: 120-digit series)
#' digits <- c(8,9,3,7,2,3,0,2,3,1,4,7,8,5,6,2,0,9,6,8,7,5,3,0,7,8,9,6,3,5,9,9,8,4,
#'            6,3,7,9,1,0,8,3,7,6,1,0,0,3,8,9,5,6,6,7,4,1,2,0,3,6,7,8,8,8,9,9,4,5,
#'            3,3,1,1,1,6,0,0,8,7,7,3,9,7,5,2,0,3,8,6,0,4,6,3,0,2,8,6,7,0,0,1,2,5,
#'            0,5,7,9,0,8,6,4,3,2,5,8,9,6,1,0,7,8)
#' kk_frequency_test(digits)
#'
#' @export
kk_frequency_test <- function(data, x = NULL, p = NULL,
                              alternative = c("two.sided", "less", "greater"),
                              continuity_correction = TRUE,
                              conf.level = 0.95) {
  
  alternative <- match.arg(alternative)
  
  # Handle grouped data frame
  if (dplyr::is_grouped_df(data)) {
    x_enquo <- rlang::enquo(x)
    res <- data %>%
      dplyr::group_modify(function(grp_data, grp_keys) {
        rlang::inject(kk_frequency_test(
          data = grp_data,
          x = !!x_enquo,
          p = p,
          alternative = alternative,
          continuity_correction = continuity_correction,
          conf.level = conf.level
        ))
      })
    return(res)
  }
  
  x_enquo <- rlang::enquo(x)
  # Handle data frame vs vector input
  if (is.data.frame(data)) {
    if (rlang::quo_is_null(x_enquo)) {
      stop("Argument 'x' must be specified when 'data' is a data frame.")
    }
    vec <- data %>% dplyr::pull(!!x_enquo)
  } else {
    vec <- data
  }
  
  # Remove NA values
  vec <- vec[!is.na(vec)]
  
  if (length(vec) < 1) {
    stop("Input sequence must have at least 1 non-missing observation.")
  }
  
  counts_table <- table(vec)
  counts <- as.numeric(counts_table)
  cat_names <- names(counts_table)
  k <- length(counts)
  N <- sum(counts)
  
  if (k < 2) {
    stop("Input sequence must contain at least 2 categories/values to perform a frequency test.")
  }
  
  # Parse expected probabilities
  if (is.null(p)) {
    p_expected <- rep(1/k, k)
  } else if (length(p) == 1 && k == 2) {
    p_expected <- c(p, 1 - p)
  } else if (length(p) == k) {
    p_expected <- p
  } else {
    stop(sprintf("Expected probabilities 'p' must be of length %d (or length 1 if binary).", k))
  }
  
  # Validate expected probabilities
  if (abs(sum(p_expected) - 1) > 1e-6) {
    stop("Expected probabilities must sum to 1.")
  }
  if (any(p_expected <= 0)) {
    stop("Expected probabilities must be strictly positive.")
  }
  
  # Calculate Chi-Square test
  E <- N * p_expected
  chi_sq <- sum((counts - E)^2 / E)
  df_chi <- k - 1
  p_val_chi <- stats::pchisq(chi_sq, df = df_chi, lower.tail = FALSE)
  
  obs_str <- paste(paste(cat_names, counts, sep = "="), collapse = ", ")
  exp_str <- paste(paste(cat_names, round(E, 2), sep = "="), collapse = ", ")
  
  if (k == 2) {
    # Calculate binomial exact test and binomial normal approximation Z-test
    x_obs <- counts[1]
    p_1 <- p_expected[1]
    
    # Exact Binomial
    binom_res <- stats::binom.test(x = x_obs, n = N, p = p_1, alternative = alternative, conf.level = conf.level)
    p_val_exact <- binom_res$p.value
    
    # Z-Test normal approximation
    sd_val <- sqrt(N * p_1 * (1 - p_1))
    diff_val <- x_obs - E[1]
    
    if (sd_val == 0) {
      z_stat <- NA_real_
    } else {
      if (continuity_correction) {
        z_stat <- sign(diff_val) * max(0, abs(diff_val) - 0.5) / sd_val
      } else {
        z_stat <- diff_val / sd_val
      }
    }
    
    p_val_z <- if (is.na(z_stat)) {
      NA_real_
    } else {
      switch(alternative,
             two.sided = 2 * stats::pnorm(-abs(z_stat)),
             less = stats::pnorm(z_stat),
             greater = stats::pnorm(z_stat, lower.tail = FALSE))
    }
    
    # Return all 3 tests
    res_chi <- tibble::tibble(
      method = "Chi-Square Goodness-of-Fit Test",
      alternative = "two.sided",
      statistic_name = "Chi-Square",
      statistic = chi_sq,
      df = df_chi,
      p_value = p_val_chi,
      observed = obs_str,
      expected = exp_str
    )
    
    res_exact <- tibble::tibble(
      method = "Exact Binomial Sign Test",
      alternative = alternative,
      statistic_name = "Exact",
      statistic = x_obs,
      df = NA_integer_,
      p_value = p_val_exact,
      observed = obs_str,
      expected = exp_str
    )
    
    res_z <- tibble::tibble(
      method = "Binomial Sign Z-Test (Normal Approx)",
      alternative = alternative,
      statistic_name = "Z",
      statistic = z_stat,
      df = NA_integer_,
      p_value = p_val_z,
      observed = obs_str,
      expected = exp_str
    )
    
    return(dplyr::bind_rows(res_chi, res_exact, res_z))
  } else {
    # Return Chi-Square Goodness-of-Fit Test
    return(tibble::tibble(
      method = "Chi-Square Goodness-of-Fit Test",
      alternative = "two.sided",
      statistic_name = "Chi-Square",
      statistic = chi_sq,
      df = df_chi,
      p_value = p_val_chi,
      observed = obs_str,
      expected = exp_str
    ))
  }
}


#' Von Neumann Mean Square Successive Difference (MSSD) Test
#'
#' @description Performs the Mean Square Successive Difference (MSSD) test for serial randomness
#'   on continuous interval/ratio numeric data. It contrasts the mean of the squares of consecutive
#'   differences with the variance of the numbers, evaluating if observations in the sequence are independent.
#'
#' @param data A data frame or a vector. If a data frame is provided, the column
#'   to analyze must be specified in the `x` argument.
#' @param x Column name (as symbol or string) to analyze if `data` is a data frame.
#'   Ignored if `data` is a vector.
#' @param alternative Character string specifying the alternative hypothesis.
#'   Must be one of `"two.sided"` (default, testing serial correlation),
#'   `"greater"` (testing positive serial correlation, $C > 0$), or
#'   `"less"` (testing negative serial correlation, $C < 0$).
#'
#' @return A tibble with the following columns:
#'   \itemize{
#'     \item \code{method}: The test method used.
#'     \item \code{alternative}: The alternative hypothesis.
#'     \item \code{n_obs}: The number of observations.
#'     \item \code{statistic_C}: The C statistic value ($C = 1 - s_{ms}^2 / s^2$).
#'     \item \code{s_variance}: The standard sample variance ($s^2$).
#'     \item \code{s_ms_difference}: The mean square successive difference ($s_{ms}^2$).
#'     \item \code{z_statistic}: The large sample normal Z-statistic.
#'     \item \code{p_value}: The p-value of the test.
#'   }
#'
#' @examples
#' # Example from temp.md (N=21 continuous observations)
#' milk <- c(1.90, 1.99, 2.00, 1.78, 1.77, 1.76, 1.98, 1.90, 1.65, 1.76,
#'           2.01, 1.78, 1.99, 1.76, 1.94, 1.78, 1.67, 1.87, 1.91, 1.91, 1.89)
#' kk_mssd_test(milk)
#'
#' @export
kk_mssd_test <- function(data, x = NULL, alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  
  # Handle grouped data frame
  if (dplyr::is_grouped_df(data)) {
    x_enquo <- rlang::enquo(x)
    res <- data %>%
      dplyr::group_modify(function(grp_data, grp_keys) {
        rlang::inject(kk_mssd_test(
          data = grp_data,
          x = !!x_enquo,
          alternative = alternative
        ))
      })
    return(res)
  }
  
  x_enquo <- rlang::enquo(x)
  # Handle data frame vs vector input
  if (is.data.frame(data)) {
    if (rlang::quo_is_null(x_enquo)) {
      stop("Argument 'x' must be specified when 'data' is a data frame.")
    }
    vec <- data %>% dplyr::pull(!!x_enquo)
  } else {
    vec <- data
  }
  
  # Remove NA values
  vec <- vec[!is.na(vec)]
  
  n <- length(vec)
  if (n < 3) {
    stop("Input sequence must have at least 3 non-missing numeric observations.")
  }
  
  s2 <- stats::var(vec)
  if (s2 == 0) {
    stop("Input vector has zero variance; cannot perform MSSD test.")
  }
  
  # Mean square successive difference (unbiased estimate)
  diffs_sq <- diff(vec)^2
  s_ms2 <- sum(diffs_sq) / (2 * (n - 1))
  
  # C statistic (Equation 10.10 in temp.md)
  C <- 1 - (s_ms2 / s2)
  
  # Large-sample normal approximation (Equation 10.11 in temp.md)
  se_C <- sqrt((n - 2) / (n^2 - 1))
  z_stat <- C / se_C
  
  # Calculate p-value based on alternative
  p_val <- switch(alternative,
                  two.sided = 2 * stats::pnorm(-abs(z_stat)),
                  greater = stats::pnorm(z_stat, lower.tail = FALSE),
                  less = stats::pnorm(z_stat, lower.tail = TRUE))
  
  return(tibble::tibble(
    method = "Mean Square Successive Difference Test",
    alternative = alternative,
    n_obs = n,
    statistic_C = C,
    s_variance = s2,
    s_ms_difference = s_ms2,
    z_statistic = z_stat,
    p_value = p_val
  ))
}

