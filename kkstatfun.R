###### ============================================================ ######
###### R STATISTICAL ANALYSIS TOOLKIT - IMPROVED VERSION
###### ============================================================ ######
###### Features: Enhanced error handling, documentation, performance
###### Author: Refactored for production use
###### ============================================================ ######

# ============================================================
# 1. CONSTANTS & CONFIGURATION
# ============================================================

CONSTANTS <- list(
  OUTLIER_IQR_MULTIPLIER = 1.5,
  DEFAULT_CONF_LEVEL = 0.95,
  TRIM_PERCENTAGE = 0.1,
  HUBER_C_CONSTANT = 1.345,
  HUBER_MAX_ITER = 100,
  HUBER_TOLERANCE = 1e-6,
  SMALL_CONSTANT = 1e-10,
  MIN_SAMPLE_FOR_TESTS = list(
    shapiro = 3,
    ks = 5001,
    anderson = 7,
    garch = 30,
    hurst = 20,
    acf_pacf = 3,
    adf_kpss = 10
  )
)

# ============================================================
# 2. PACKAGE LOADING
# ============================================================

#' Load Required Packages with Fallback and Error Handling
#'
#' @description Loads packages with automatic installation fallback and
#'   detailed reporting on failures.
#'
#' @return Invisibly returns character vector of packages that failed to load
#'
#' @export
load_packages <- function() {
  required_packages <- list(
    core = c("tidyverse", "haven", "janitor", "readxl"),
    modeling = c("modelsummary", "brms", "quantreg", "tidymodels",
                 "marginaleffects", "emmeans", "broom", "brglm2"),
    statistics = c("rstatix", "finalfit", "gtsummary", "moments",
                   "nortest", "tseries", "rugarch", "forecast"),
    visualization = c("ggplot2", "ggstats", "ggsurvfit", "patchwork",
                      "ggthemes", "monochromeR", "scales", "showtext", "sysfonts"),
    specialized = c("MKinfer", "tinytable", "epitools", "epiR", "entropy",
                    "easystats", "detectseparation", "pracma", "zoo", "sandwich", "pROC")
  )

  if (!requireNamespace("pacman", quietly = TRUE)) {
    message("Installing pacman...")
    install.packages("pacman", quiet = TRUE)
  }

  all_packages <- unlist(required_packages)
  failed <- character()

  for (pkg in all_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      failed <- c(failed, pkg)
    }
  }

  if (length(failed) > 0) {
    message(sprintf("\n⚠️  Failed to load: %s\n   Attempting installation...",
                    paste(failed, collapse = ", ")))
    tryCatch(
      install.packages(failed, quiet = TRUE),
      error = function(e) {
        message(sprintf("   Installation failed: %s", e$message))
      }
    )

    # Retry loading
    still_failed <- character()
    for (pkg in failed) {
      if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        still_failed <- c(still_failed, pkg)
      }
    }

    if (length(still_failed) > 0) {
      warning(sprintf("Could not load: %s\nSome functions may not work properly.",
                      paste(still_failed, collapse = ", ")))
    } else {
      message("✓ Successfully installed missing packages")
    }
  }

  loaded_count <- length(all_packages) - length(failed)
  message(sprintf("✓ Loaded %d of %d packages", loaded_count, length(all_packages)))
  invisible(failed)
}

# ============================================================
# 3. OPTIONS & ENVIRONMENT SETUP
# ============================================================

#' Set Up R Environment for Statistical Analysis
#'
#' @description Configures R options and environment variables for
#'   optimal analysis performance.
#'
#' @return NULL (invisibly)
#'
#' @export
setup_environment <- function() {
  # Backend and parallel processing
  options(brms.backend = "cmdstanr")
  options(mc.cores = parallel::detectCores() - 1)
  options(ggplot2.messages = FALSE)
  options(dplyr.width = Inf)

  # Output formatting
  options(scipen = 999)
  options(stringsAsFactors = FALSE)
  options(warn = 1)

  # Memory management
  options(expressions = 5000)
  gc()

  message("✓ Environment configured")
  invisible(NULL)
}

# Execute setup
load_packages()
setup_environment()

# ============================================================
# 4. VALIDATION UTILITIES
# ============================================================

#' Validate Data Frame Input
#'
#' @param data Object to validate
#' @param var_name Name of variable for error messages
#'
#' @return NULL (invisibly) or stops with error
#'
#' @keywords internal
validate_data_frame <- function(data, var_name = "data") {
  if (!is.data.frame(data)) {
    stop(sprintf("'%s' must be a data frame or tibble, got %s",
                 var_name, class(data)[1]))
  }
  invisible(NULL)
}

#' Validate Numeric Column
#'
#' @param data Data frame to check
#' @param col_name Column name to validate
#' @param allow_na Whether to allow NA values
#'
#' @return NULL (invisibly) or stops with error
#'
#' @keywords internal
validate_numeric_column <- function(data, col_name, allow_na = TRUE) {
  if (!col_name %in% names(data)) {
    stop(sprintf("Column '%s' not found in data", col_name))
  }

  col <- data[[col_name]]
  if (!is.numeric(col)) {
    stop(sprintf("Column '%s' must be numeric, got %s",
                 col_name, class(col)[1]))
  }

  if (!allow_na && any(is.na(col))) {
    stop(sprintf("Column '%s' contains NA values", col_name))
  }

  invisible(NULL)
}

#' Validate Date Column
#'
#' @param data Data frame to check
#' @param col_name Column name to validate
#'
#' @return NULL (invisibly) or stops with error
#'
#' @keywords internal
validate_date_column <- function(data, col_name) {
  if (!col_name %in% names(data)) {
    stop(sprintf("Column '%s' not found in data", col_name))
  }

  col <- data[[col_name]]
  if (!inherits(col, c("Date", "POSIXct"))) {
    stop(sprintf("Column '%s' must be Date or POSIXct, got %s",
                 col_name, class(col)[1]))
  }

  invisible(NULL)
}

#' Validate Categorical Column
#'
#' @param data Data frame to check
#' @param col_name Column name to validate
#'
#' @return NULL (invisibly) or stops with error
#'
#' @keywords internal
validate_categorical_column <- function(data, col_name) {
  if (!col_name %in% names(data)) {
    stop(sprintf("Column '%s' not found in data", col_name))
  }

  col <- data[[col_name]]
  if (!is.character(col) && !is.factor(col)) {
    stop(sprintf("Column '%s' must be character or factor, got %s",
                 col_name, class(col)[1]))
  }

  invisible(NULL)
}

# ============================================================
# 5. UTILITY FUNCTIONS
# ============================================================

#' Format Tibble for Display
#'
#' @param data Tibble with numeric values
#' @param digits Number of decimal places
#'
#' @return Tibble with formatted display column
#'
#' @export
format_tibble <- function(data, digits = 2) {
  data %>%
    dplyr::mutate(
      Value_display = sapply(Value, function(x) {
        if (is.na(x)) {
          "NA"
        } else {
          format(round(x, digits), nsmall = digits, scientific = FALSE, big.mark = ",")
        }
      })
    )
}

#' Round Numeric Columns in Tibble
#'
#' @param data Data frame or tibble
#' @param digits Number of decimal places
#'
#' @return Data with rounded numeric columns
#'
#' @export
mutate_round <- function(data, digits = 2) {
  if (!is.data.frame(data)) {
    stop(sprintf("Input must be a data frame or tibble, got %s", class(data)[1]))
  }

  data %>%
    dplyr::mutate(dplyr::across(where(is.numeric),
                                ~ janitor::round_half_up(., digits)))
}

# ============================================================
# 6. PROPORTION ANALYSIS FUNCTIONS
# ============================================================

#' Calculate Confidence Intervals for Proportions
#'
#' @description Computes binomial confidence intervals for proportions
#'
#' @param data Data frame with numeric columns
#' @param conf.level Confidence level (default: 0.95)
#'
#' @return Tibble with CI columns
#'
#' @export
proportion_ci <- function(data, conf.level = CONSTANTS$DEFAULT_CONF_LEVEL) {
  validate_data_frame(data)

  required_cols <- c("successes", "trials")
  if (!all(required_cols %in% names(data))) {
    missing <- setdiff(required_cols, names(data))
    stop(sprintf("Data must contain: %s", paste(required_cols, collapse = ", ")))
  }

  x <- data$successes
  n <- data$trials

  if (any(n < x, na.rm = TRUE)) {
    stop("Each trial count must be >= success count")
  }

  lower <- numeric(nrow(data))
  upper <- numeric(nrow(data))

  for (i in seq_len(nrow(data))) {
    ci <- stats::binom.test(x = x[i], n = n[i], conf.level = conf.level)$conf.int
    lower[i] <- ci[1]
    upper[i] <- ci[2]
  }

  non_numeric_cols <- names(data)[!sapply(data, is.numeric)]

  tibble::tibble(
    data[, non_numeric_cols, drop = FALSE],
    successes = x,
    trials = n,
    proportion = x / n,
    lower_ci = lower,
    upper_ci = upper,
    conf_level = conf.level
  )
}

#' Alias for proportion_ci (backward compatibility)
#'
#' @description Old name for proportion_ci(). Use proportion_ci() instead.
#'
#' @param data Data frame with numeric columns
#' @param conf.level Confidence level
#'
#' @return Tibble with CI columns
#'
#' @export
pcit <- proportion_ci

#' Compare Proportions Between Groups (GLM-based)
#'
#' @description Tests differences in proportions using GLM with robust SE
#'
#' @param data Data frame with group, x (successes), n (trials)
#' @param group Grouping variable name
#' @param x Success count variable name
#' @param n Trial count variable name
#' @param by Optional stratification variable
#' @param covariates Optional vector of covariate names
#' @param adjust Multiple comparison adjustment method
#' @param conf.level Confidence level
#' @param vcov_type Sandwich VCOV type
#' @param drop_empty Drop empty combinations
#'
#' @return Tibble with pairwise comparisons
#'
#' @export
compare_proportions_glm <- function(data, group, x, n,
                                    by = NULL, covariates = NULL,
                                    adjust = "holm", 
                                    conf.level = CONSTANTS$DEFAULT_CONF_LEVEL,
                                    vcov_type = "HC3", drop_empty = TRUE) {
  validate_data_frame(data)

  for (pkg in c("emmeans", "sandwich")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required", pkg))
    }
  }

  group_sym <- dplyr::ensym(group)
  x_sym <- dplyr::ensym(x)
  n_sym <- dplyr::ensym(n)
  by_sym <- if (is.null(by)) NULL else dplyr::ensym(by)

  group_name <- rlang::as_string(group_sym)
  by_name <- if (is.null(by_sym)) NULL else rlang::as_string(by_sym)

  df <- data %>%
    dplyr::mutate(
      .x = !!x_sym,
      .n = !!n_sym,
      .fail = .n - .x
    )

  if (any(df$.n < df$.x, na.rm = TRUE)) {
    stop("Each 'n' must be >= 'x'")
  }

  rhs_terms <- group_name
  if (!is.null(by_name)) rhs_terms <- paste(rhs_terms, by_name, sep = " * ")
  if (!is.null(covariates) && length(covariates)) {
    rhs_terms <- paste(rhs_terms, paste(covariates, collapse = " + "), sep = " + ")
  }

  fml <- stats::as.formula(paste0("cbind(.x, .fail) ~ ", rhs_terms))
  fit <- stats::glm(fml, family = stats::binomial("logit"), data = df)

  V <- tryCatch(
    sandwich::vcovHC(fit, type = vcov_type),
    error = function(e) stats::vcov(fit)
  )

  hat <- tryCatch(stats::hatvalues(fit), error = function(e) rep(0, nrow(df)))

  if (any(is.finite(hat) & hat > 0.99)) V <- stats::vcov(fit)

  if (is.null(by_name)) {
    emm <- emmeans::emmeans(fit, specs = group_name, vcov. = V)
    emm_resp <- emmeans::regrid(emm, transform = "response")
    cmp <- emmeans::contrast(emm_resp, method = "pairwise", adjust = adjust)
    out <- as.data.frame(summary(cmp, infer = TRUE, level = conf.level))

    tibble::tibble(
      group1 = sub(" - .*", "", out$contrast),
      group2 = sub(".* - ", "", out$contrast),
      estimate = out$estimate,
      conf_low = out$lower.CL,
      conf_high = out$upper.CL,
      p_value = out$p.value,
      adjust_method = adjust,
      conf_level = conf.level
    )
  } else {
    emm <- emmeans::emmeans(fit, specs = group_name, by = by_name, vcov. = V)
    emm_resp <- emmeans::regrid(emm, transform = "response")
    cmp <- emmeans::contrast(emm_resp, method = "pairwise", by = by_name, adjust = adjust)
    tmp <- as.data.frame(summary(cmp, infer = TRUE, level = conf.level))

    out <- tibble::tibble(
      !!by_name := tmp[[by_name]],
      group1 = sub(" - .*", "", tmp$contrast),
      group2 = sub(".* - ", "", tmp$contrast),
      estimate = tmp$estimate,
      conf_low = tmp$lower.CL,
      conf_high = tmp$upper.CL,
      p_value = tmp$p.value,
      adjust_method = adjust,
      conf_level = conf.level
    )

    if (drop_empty) {
      out <- out %>%
        dplyr::group_by(.data[[by_name]]) %>%
        dplyr::filter(dplyr::n() > 0) %>%
        dplyr::ungroup()
    }

    out
  }
}

#' Simple Proportion Comparisons
#'
#' @description Tests pairwise differences in proportions using z-tests
#'
#' @param data Data frame with proportion and trials columns
#' @param conf.level Confidence level
#' @param method P-value adjustment method
#'
#' @return Tibble with pairwise comparisons
#'
#' @export
compare_proportions_simple <- function(data,
                                       conf.level = CONSTANTS$DEFAULT_CONF_LEVEL,
                                       method = "holm") {
  validate_data_frame(data)

  required_cols <- c("proportion", "trials")
  if (!all(required_cols %in% names(data))) {
    stop(sprintf("Data must contain: %s", paste(required_cols, collapse = ", ")))
  }

  non_numeric_cols <- names(data)[!sapply(data, is.numeric)]
  if (length(non_numeric_cols) == 0) {
    stop("No non-numeric columns found for grouping")
  }

  if (nrow(data) < 2) {
    stop("Data must contain at least two groups")
  }

  combinations <- utils::combn(seq_len(nrow(data)), 2, simplify = FALSE)

  calculate_difference <- function(index_pair) {
    i <- index_pair[1]
    j <- index_pair[2]

    group1 <- data[i, ]
    group2 <- data[j, ]

    prop_diff <- group1$proportion - group2$proportion

    pooled_se <- sqrt(
      group1$proportion * (1 - group1$proportion) / group1$trials +
        group2$proportion * (1 - group2$proportion) / group2$trials
    )

    pooled_se <- max(pooled_se, CONSTANTS$SMALL_CONSTANT)

    if (pooled_se == 0) {
      stop("Standard error is zero")
    }

    z_score <- prop_diff / pooled_se
    p_value <- 2 * (1 - stats::pnorm(abs(z_score)))

    z_critical <- stats::qnorm(1 - (1 - conf.level) / 2)
    ci_lower <- prop_diff - z_critical * pooled_se
    ci_upper <- prop_diff + z_critical * pooled_se

    tibble::tibble(
      !!!setNames(
        lapply(non_numeric_cols, function(col) group1[[col]]),
        paste0("group1_", non_numeric_cols)
      ),
      !!!setNames(
        lapply(non_numeric_cols, function(col) group2[[col]]),
        paste0("group2_", non_numeric_cols)
      ),
      prop_diff = prop_diff,
      z_score = z_score,
      p_value = p_value,
      ci_lower = ci_lower,
      ci_upper = ci_upper
    )
  }

  results <- purrr::map_dfr(combinations, calculate_difference)

  results %>%
    dplyr::mutate(adj_p_value = stats::p.adjust(p_value, method = method))
}

# ============================================================
# 7. EGN (BULGARIAN ID) PARSING
# ============================================================

#' Extract Age from Bulgarian EGN
#'
#' @description Validates and extracts demographic information from
#'   Bulgarian EGN numbers (6 or 10 digits)
#'
#' @param egn Character vector of EGN numbers
#' @param admission_date Date of admission for age calculation
#'
#' @return Tibble with age, birth_date, validity, gender, region
#'
#' @export
extract_egn_info <- function(egn, admission_date = Sys.Date()) {
  region_codes <- list(
    "000-043" = "Blagoevgrad", "044-093" = "Burgas", "094-139" = "Varna",
    "140-169" = "Veliko Tarnovo", "170-183" = "Vidin", "184-217" = "Vratsa",
    "218-233" = "Gabrovo", "234-281" = "Kardzhali", "282-301" = "Kyustendil",
    "302-319" = "Lovech", "320-341" = "Montana", "342-377" = "Pazardzhik",
    "378-395" = "Pernik", "396-435" = "Pleven", "436-501" = "Plovdiv",
    "502-527" = "Razgrad", "528-555" = "Ruse", "556-575" = "Silistra",
    "576-601" = "Sliven", "602-623" = "Smolyan", "624-721" = "Sofia City",
    "722-751" = "Sofia Province", "752-789" = "Stara Zagora", "790-821" = "Dobrich",
    "822-843" = "Targovishte", "844-871" = "Haskovo", "872-903" = "Shumen",
    "904-925" = "Yambol", "926-999" = "Other/Unknown"
  )

  weights <- c(2, 4, 8, 5, 10, 9, 7, 3, 6)
  egn <- as.character(egn)
  admission_date <- as.Date(admission_date)

  result <- data.frame(
    age = numeric(length(egn)),
    birth_date = as.Date(character(length(egn))),
    is_valid = logical(length(egn)),
    gender = character(length(egn)),
    region = character(length(egn)),
    birth_order = numeric(length(egn)),
    invalid_egn = character(length(egn)),
    invalid_reason = character(length(egn)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(egn)) {
    output <- list(
      age = NA_real_,
      birth_date = as.Date(NA_character_),
      is_valid = FALSE,
      gender = NA_character_,
      region = NA_character_,
      birth_order = NA_real_,
      invalid_egn = NA_character_,
      invalid_reason = NA_character_
    )

    if (is.na(egn[i]) || nchar(trimws(egn[i])) == 0) {
      output$invalid_reason <- "Missing or empty EGN"
      output$invalid_egn <- egn[i]
      result[i, ] <- output
      next
    }

    egn_length <- nchar(egn[i])

    if (egn_length == 6) {
      year <- as.numeric(substr(egn[i], 1, 2))
      month <- as.numeric(substr(egn[i], 3, 4))
      day <- as.numeric(substr(egn[i], 5, 6))

      month_indicator <- month
      if (month_indicator >= 41 && month_indicator <= 52) {
        year <- year + 2000
        month <- month - 40
      } else if (month_indicator >= 21 && month_indicator <= 32) {
        year <- year + 1800
        month <- month - 20
      } else if (month_indicator >= 1 && month_indicator <= 12) {
        year <- year + 1900
      } else {
        output$invalid_reason <- sprintf("Invalid month: %d", month_indicator)
        output$invalid_egn <- egn[i]
        result[i, ] <- output
        next
      }

      output$birth_date <- tryCatch(
        as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d"),
        error = function(e) as.Date(NA_character_)
      )

      if (is.na(output$birth_date)) {
        output$birth_date <- tryCatch(
          as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d"),
          error = function(e) as.Date(NA_character_)
        )
      }

      if (!is.na(output$birth_date)) {
        current_admission_date <- if (length(admission_date) == 1) admission_date else admission_date[i]
        output$age <- floor(as.numeric(difftime(current_admission_date,
                                                 output$birth_date, units = "days")) / 365.25)
        output$is_valid <- TRUE
      }

      result[i, ] <- output
      next
    }

    if (egn_length == 10 && grepl("^\\d+$", egn[i])) {
      year <- as.numeric(substr(egn[i], 1, 2))
      month <- as.numeric(substr(egn[i], 3, 4))
      day <- as.numeric(substr(egn[i], 5, 6))

      month_indicator <- month
      if (month_indicator >= 41 && month_indicator <= 52) {
        year <- year + 2000
        month <- month - 40
      } else if (month_indicator >= 21 && month_indicator <= 32) {
        year <- year + 1800
        month <- month - 20
      } else if (month_indicator >= 1 && month_indicator <= 12) {
        year <- year + 1900
      } else {
        output$invalid_reason <- sprintf("Invalid month: %d", month_indicator)
        output$invalid_egn <- egn[i]
        result[i, ] <- output
        next
      }

      output$birth_date <- tryCatch(
        as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d"),
        error = function(e) as.Date(NA_character_)
      )

      if (is.na(output$birth_date)) {
        output$invalid_reason <- "Invalid birth date"
        output$invalid_egn <- egn[i]
        result[i, ] <- output
        next
      }

      current_admission_date <- if (length(admission_date) == 1) admission_date else admission_date[i]
      output$age <- floor(as.numeric(difftime(current_admission_date,
                                               output$birth_date, units = "days")) / 365.25)

      digits <- as.numeric(strsplit(egn[i], "")[[1]])
      weighted_sum <- sum(digits[1:9] * weights)
      control_digit <- weighted_sum %% 11
      control_digit <- if (control_digit == 10) 0 else control_digit
      output$is_valid <- control_digit == digits[10]

      if (!output$is_valid) {
        output$invalid_reason <- "Invalid control digit"
      }

      ninth_digit <- as.numeric(substr(egn[i], 9, 9))
      output$gender <- if (ninth_digit %% 2 == 0) "Male" else "Female"

      three_digit_code <- as.numeric(substr(egn[i], 7, 9))
      if (ninth_digit %% 2 == 0) {
        output$birth_order <- (ninth_digit / 2) + 1
      } else {
        output$birth_order <- ((ninth_digit + 1) / 2)
      }

      output$region <- "Other/Unknown"
      for (range in names(region_codes)) {
        range_bounds <- as.numeric(unlist(strsplit(range, "-")))
        if (three_digit_code >= range_bounds[1] && three_digit_code <= range_bounds[2]) {
          output$region <- region_codes[[range]]
          break
        }
      }
    } else {
      output$invalid_reason <- sprintf("Invalid length (%d) or non-numeric", egn_length)
      output$invalid_egn <- egn[i]
    }

    result[i, ] <- output
  }

  tibble::as_tibble(result)
}

#' Alias for extract_egn_info (Original Name)
#'
#' @param egn Character vector of EGN numbers
#' @param admission_date Date of admission
#'
#' @return Tibble with EGN information
#'
#' @export
extract_age_from_egn <- extract_egn_info

# ============================================================
# 8. REGRESSION ANALYSIS (UNIFIED FUNCTION)
# ============================================================

#' Regression Analysis - Univariate and Multivariate
#'
#' @description Performs univariate and multivariate regression with automatic
#'   model selection. Supports linear, logistic, and ordinal regression.
#'
#' @param data Data frame
#' @param outcome Outcome variable name (character)
#' @param predictors Character vector of predictor names
#' @param log_outcome Log-transform outcome for continuous models
#' @param custom_formula Optional custom formula
#' @param include_diagnostics Include model diagnostics
#' @param ... Additional arguments to glm/lm/polr
#'
#' @return Tibble with regression results
#'
#' @details
#' Outcome types detected:
#' - Binary factor (2 levels): Logistic (odds ratios)
#' - Ordered factor: Ordinal regression
#' - Numeric: Linear regression
#'
#' @export
regression_analysis <- function(data, outcome, predictors,
                                log_outcome = FALSE,
                                custom_formula = NULL,
                                include_diagnostics = TRUE, ...) {
  validate_data_frame(data)

  if (!is.character(outcome) || length(outcome) != 1) {
    stop("'outcome' must be a single character string")
  }

  if (!is.character(predictors)) {
    stop("'predictors' must be a character vector")
  }

  if (!is.logical(log_outcome)) {
    stop("'log_outcome' must be TRUE or FALSE")
  }

  if (!is.logical(include_diagnostics)) {
    stop("'include_diagnostics' must be TRUE or FALSE")
  }

  outcome_name <- as.character(outcome)

  if (!outcome_name %in% colnames(data)) {
    stop(sprintf("Outcome variable '%s' not found in data", outcome_name))
  }

  missing_predictors <- setdiff(predictors, colnames(data))
  if (length(missing_predictors) > 0) {
    stop(sprintf("Predictors not found: %s", paste(missing_predictors, collapse = ", ")))
  }

  if (log_outcome) {
    if (!is.numeric(data[[outcome_name]]) || any(data[[outcome_name]] <= 0, na.rm = TRUE)) {
      stop("log_outcome=TRUE requires positive numeric outcome")
    }
    data <- data %>% dplyr::mutate(!!rlang::sym(outcome_name) := log(!!rlang::sym(outcome_name)))
  }

  outcome_type <- case_when(
    is.factor(data[[outcome_name]]) && nlevels(data[[outcome_name]]) == 2 ~ "binary",
    is.ordered(data[[outcome_name]]) ~ "ordinal",
    is.numeric(data[[outcome_name]]) ~ "continuous",
    TRUE ~ NA_character_
  )

  if (is.na(outcome_type)) {
    stop("Outcome must be numeric, binary factor, or ordered factor")
  }

  fit_model <- function(formula) {
    tryCatch(
      {
        if (outcome_type == "binary") {
          stats::glm(formula, data = data, family = stats::binomial(), ...)
        } else if (outcome_type == "ordinal") {
          if (!requireNamespace("MASS", quietly = TRUE)) {
            stop("Package 'MASS' required for ordinal regression")
          }
          MASS::polr(formula, data = data, Hess = TRUE, ...)
        } else if (outcome_type == "continuous") {
          stats::lm(formula, data = data, ...)
        }
      },
      error = function(e) {
        stop(sprintf("Model fitting failed: %s", e$message))
      }
    )
  }

  calculate_diagnostics <- function(model) {
    if (!include_diagnostics) {
      return(tibble::tibble())
    }

    diagnostics <- list()
    summary_model <- summary(model)

    if (outcome_type == "continuous") {
      diagnostics$r_squared <- summary_model$r.squared
      diagnostics$adj_r_squared <- summary_model$adj.r.squared
      diagnostics$residual_std_error <- summary_model$sigma
      diagnostics$model_p_value <- stats::pf(
        summary_model$fstatistic[1],
        summary_model$fstatistic[2],
        summary_model$fstatistic[3],
        lower.tail = FALSE
      )
    } else if (outcome_type == "binary") {
      null_model <- stats::update(model, ~1)
      loglik_model <- as.numeric(stats::logLik(model))
      loglik_null <- as.numeric(stats::logLik(null_model))
      diagnostics$pseudo_r_squared <- as.numeric(1 - (loglik_model / loglik_null))
      diagnostics$nagelkerke_r_squared <- as.numeric(
        (1 - exp(-2 * (loglik_model - loglik_null))) /
          (1 - exp(2 * loglik_null / nrow(data)))
      )
      diagnostics$model_p_value <- stats::pchisq(
        2 * (loglik_model - loglik_null),
        df = length(stats::coef(model)) - 1,
        lower.tail = FALSE
      )

      if (requireNamespace("pROC", quietly = TRUE)) {
        roc_curve <- pROC::roc(model$y, stats::fitted(model), quiet = TRUE)
        diagnostics$auc_roc <- as.numeric(pROC::auc(roc_curve))
      }
    } else if (outcome_type == "ordinal") {
      null_model <- stats::update(model, ~1)
      loglik_model <- as.numeric(stats::logLik(model))
      loglik_null <- as.numeric(stats::logLik(null_model))
      diagnostics$pseudo_r_squared <- as.numeric(1 - (loglik_model / loglik_null))
      diagnostics$nagelkerke_r_squared <- as.numeric(
        (1 - exp(-2 * (loglik_model - loglik_null))) /
          (1 - exp(2 * loglik_null / nrow(data)))
      )
      diagnostics$model_p_value <- stats::pchisq(
        2 * (loglik_model - loglik_null),
        df = length(stats::coef(model)),
        lower.tail = FALSE
      )
    }

    tibble::as_tibble(diagnostics)
  }

  process_results <- function(model, model_type) {
    estimate_label <- if (outcome_type == "binary") {
      "odds_ratio"
    } else if (outcome_type == "continuous" && log_outcome) {
      "exp_coef"
    } else if (outcome_type == "ordinal") {
      "odds_ratio"
    } else {
      "coef"
    }

    results <- broom::tidy(model, conf.int = TRUE) %>%
      dplyr::mutate(
        model_type = model_type,
        outcome_type = outcome_type,
        AIC = AIC(model),
        BIC = BIC(model),
        estimate_label = estimate_label,
        coef_type = if_else(grepl("\\|", term), "scale", "coefficient")
      )

    if (outcome_type %in% c("binary", "ordinal") || 
        (outcome_type == "continuous" && log_outcome)) {
      results <- results %>%
        dplyr::mutate(
          estimate = if_else(coef_type == "coefficient", exp(estimate), estimate),
          conf.low = if_else(coef_type == "coefficient", exp(conf.low), conf.low),
          conf.high = if_else(coef_type == "coefficient", exp(conf.high), conf.high),
          percent_change = if_else(
            coef_type == "coefficient",
            (estimate - 1) * 100,
            NA_real_
          ),
          percent_change_low = if_else(
            coef_type == "coefficient",
            (conf.low - 1) * 100,
            NA_real_
          ),
          percent_change_high = if_else(
            coef_type == "coefficient",
            (conf.high - 1) * 100,
            NA_real_
          )
        )
    } else if (outcome_type == "continuous") {
      results <- results %>%
        dplyr::mutate(
          percent_change = estimate * 100,
          percent_change_low = conf.low * 100,
          percent_change_high = conf.high * 100
        )
    }

    diagnostics <- calculate_diagnostics(model)
    results %>% dplyr::bind_cols(diagnostics)
  }

  # Univariate models
  univariate_results <- purrr::map_dfr(predictors, function(predictor) {
    formula <- stats::as.formula(paste(outcome_name, "~", predictor))
    model <- fit_model(formula)
    process_results(model, "univariate") %>%
      dplyr::mutate(predictor = predictor)
  })

  # Multivariate model
  if (is.null(custom_formula)) {
    multivariate_formula <- stats::as.formula(
      paste(outcome_name, "~", paste(predictors, collapse = " + "))
    )
  } else {
    multivariate_formula <- custom_formula
  }

  multivariate_model <- fit_model(multivariate_formula)
  multivariate_results <- process_results(multivariate_model, "multivariate")

  dplyr::bind_rows(univariate_results, multivariate_results)
}

# ============================================================
# 9. VISUALIZATION FUNCTIONS
# ============================================================

#' Set Plot Font with Comprehensive Search
#'
#' @description Advanced font loading system that searches Google Fonts, system fonts,
#'   and local directories. Automatically falls back to safe alternatives if needed.
#'
#' @param font Font family name (default: "Roboto Condensed")
#' @param size Base font size in points (default: 18)
#' @param search_sources Order to search: "google", "system", "local"
#' @param fallbacks Fallback fonts if primary not found
#' @param update_theme Apply theme to all subsequent plots
#'
#' @return List with font configuration (invisibly)
#'
#' @details
#' This function searches for fonts in the following order:
#' 1. Google Fonts (downloads and caches locally)
#' 2. System fonts (Arial, Verdana, etc.)
#' 3. Local directories (~/.fonts, Windows\\Fonts, /usr/share/fonts, etc.)
#' 4. Fallback fonts (Arial, Helvetica, sans)
#' 5. System default (sans)
#'
#' Uses showtext for consistent rendering across all platforms.
#'
#' @examples
#' \dontrun{
#'   # Use Google Font
#'   set_plot_font("Sofia Sans", size = 14)
#'
#'   # Use system font
#'   set_plot_font("Arial", size = 12)
#'
#'   # Custom search order
#'   set_plot_font("MyFont",
#'     search_sources = c("local", "system", "google"),
#'     fallbacks = c("Courier", "monospace")
#'   )
#' }
#'
#' @export
set_plot_font <- function(font = "Roboto Condensed", size = 18,
                          search_sources = c("google", "system", "local"),
                          fallbacks = c("Arial", "Helvetica", "sans"),
                          update_theme = TRUE) {
  # Input validation
  if (!is.character(font) || length(font) != 1) {
    stop("Font must be a single character string.")
  }

  # Initialize tracking variables
  font_family <- font
  font_loaded <- FALSE
  search_results <- list()

  # Helper function to test if font family is available
  test_font <- function(family) {
    families <- sysfonts::font_families()
    return(family %in% families)
  }

  # Helper function to add font from Google Fonts
  add_google_font <- function(family) {
    tryCatch(
      {
        sysfonts::font_add_google(name = family, family = family, db_cache = FALSE)
        return(test_font(family))
      },
      error = function(e) {
        message(" ✗ Google Fonts: ", e$message)
        return(FALSE)
      }
    )
  }

  # Helper function to check system fonts
  check_system_font <- function(family) {
    return(test_font(family))
  }

  # Helper function to search local font directories
  search_local_fonts <- function(family) {
    # Common font directories
    font_dirs <- c(
      file.path(Sys.getenv("WINDIR"), "Fonts"), # Windows
      file.path(Sys.getenv("HOME"), ".fonts"), # User fonts
      "/System/Library/Fonts", # macOS
      "/Library/Fonts", # macOS
      "/usr/share/fonts", # Linux
      "/usr/local/share/fonts" # Linux
    )

    # Remove duplicates and non-existent dirs
    font_dirs <- unique(font_dirs[dir.exists(font_dirs)])

    # Common font file extensions
    extensions <- c("ttf", "otf", "ttc")
    pattern <- paste0("(?i)", family, ".*\\.(", paste(extensions, collapse = "|"), ")$")

    # Search for font files
    for (dir in font_dirs) {
      font_files <- list.files(dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)

      if (length(font_files) > 0) {
        # Try to add the first matching font file
        tryCatch(
          {
            sysfonts::font_add(family = family, regular = font_files[1])
            if (test_font(family)) {
              message(" ✓ Added '", family, "' from: ", basename(font_files[1]))
              return(TRUE)
            }
          },
          error = function(e) {
            message(" ✗ Failed to add font from ", dir, ": ", e$message)
          }
        )
      }
    }

    return(FALSE)
  }

  # Main search process
  cat("🔍 Searching for font:", font, "\n")

  # Search in specified order
  for (source in search_sources) {
    if (font_loaded) break

    cat(" Checking", source, "source...\n")

    result <- switch(source,
      "google" = {
        search_results[[source]] <- add_google_font(font)
        search_results[[source]]
      },
      "system" = {
        search_results[[source]] <- check_system_font(font)
        search_results[[source]]
      },
      "local" = {
        search_results[[source]] <- search_local_fonts(font)
        search_results[[source]]
      },
      FALSE
    )

    if (result) {
      font_loaded <- TRUE
      message(" ✓ Found in ", source, " source!")
      break
    }
  }

  # Try fallback fonts if original not found
  if (!font_loaded) {
    cat(" No match found. Trying fallbacks...\n")
    for (fb in fallbacks) {
      cat(" Testing fallback:", fb, "\n")
      if (check_system_font(fb)) {
        font_family <- fb
        font_loaded <- TRUE
        message(" ✓ Using fallback: ", fb)
        break
      }
    }
  }

  # Final fallback to system default
  if (!font_loaded) {
    font_family <- "sans"
    message(" ⚠ Using system default 'sans'")
  }

  # Enable showtext for consistent font rendering
  showtext::showtext_auto(enable = TRUE)
  message("✓ Enabled showtext for font rendering")

  # Create and set theme if requested
  if (update_theme) {
    # Define relative font sizes based on the `size` parameter
    title_size <- size + 4
    subtitle_size <- size + 2
    caption_size <- size - 2
    axis_title_size <- size
    axis_text_size <- size
    strip_text_size <- size

    theme_nice <- ggthemes::theme_tufte() +
      ggplot2::theme(
        axis.ticks = ggplot2::element_line(linewidth = 0.5, color = "black"),
        axis.ticks.length = ggplot2::unit(4, "mm"),
        plot.title = ggplot2::element_text(family = font_family, size = title_size, hjust = 0, vjust = 2, margin = ggplot2::margin(t = 10, b = 10)),
        plot.subtitle = ggplot2::element_text(family = font_family, size = subtitle_size),
        plot.caption = ggplot2::element_text(family = font_family, hjust = 0.5, vjust = 1, size = caption_size),
        plot.caption.position = "plot",
        axis.title = ggplot2::element_text(family = font_family, size = axis_title_size),
        axis.text = ggplot2::element_text(family = font_family, size = axis_text_size),
        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
        strip.text = ggplot2::element_text(family = font_family, size = strip_text_size),
        axis.line = ggplot2::element_line()
      )

    ggplot2::theme_set(theme_nice)
    message("✓ Updated ggplot2 theme with font '", font_family, "'")
  }

  # Return comprehensive results
  result <- list(
    requested = font,
    used = font_family,
    loaded = font_loaded,
    source = if (font_family == font) "original" else "fallback",
    search_sources = search_sources,
    search_results = search_results,
    size = size,
    theme_updated = update_theme
  )

  message("✅ Font setup complete. Using: ", font_family)
  return(invisible(result))
}

#' List Available System Fonts
#'
#' @description Shows all fonts available on your system
#'
#' @return Character vector of font names
#'
#' @export
list_system_fonts <- function() {
  fonts <- sysfonts::font_families()
  message(sprintf("Available fonts (%d total):\n%s\n",
                  length(fonts),
                  paste(fonts, collapse = ", ")))
  invisible(fonts)
}

#' Load Google Font
#'
#' @description Downloads and installs a Google Font
#'
#' @param font_name Font name from Google Fonts
#' @param family Family name to use in R
#'
#' @return Logical - TRUE if successful
#'
#' @examples
#' \dontrun{
#'   load_google_font("Sofia Sans", "sofia_sans")
#'   set_plot_theme(font = "sofia_sans")
#' }
#'
#' @export
load_google_font <- function(font_name, family = NULL) {
  if (is.null(family)) {
    family <- tolower(gsub(" ", "_", font_name))
  }

  if (!requireNamespace("sysfonts", quietly = TRUE)) {
    stop("Package 'sysfonts' required")
  }

  message(sprintf("Downloading '%s' from Google Fonts...", font_name))

  result <- tryCatch(
    {
      sysfonts::font_add_google(name = font_name, family = family, db_cache = FALSE)
      message(sprintf("✓ Loaded '%s' as '%s'", font_name, family))
      TRUE
    },
    error = function(e) {
      message(sprintf("✗ Failed to load: %s", e$message))
      FALSE
    }
  )

  invisible(result)
}

#' Load Local Font File
#'
#' @description Loads a font from a local file path
#'
#' @param font_path Full path to font file (.ttf, .otf)
#' @param family Family name to use in R
#'
#' @return Logical - TRUE if successful
#'
#' @examples
#' \dontrun{
#'   load_local_font("/path/to/font.ttf", "my_font")
#'   set_plot_theme(font = "my_font")
#' }
#'
#' @export
load_local_font <- function(font_path, family) {
  if (!file.exists(font_path)) {
    stop(sprintf("Font file not found: %s", font_path))
  }

  message(sprintf("Loading font from: %s", font_path))

  result <- tryCatch(
    {
      sysfonts::font_add(family = family, regular = font_path)
      message(sprintf("✓ Loaded local font as '%s'", family))
      TRUE
    },
    error = function(e) {
      message(sprintf("✗ Failed to load: %s", e$message))
      FALSE
    }
  )

  invisible(result)
}

#' Search and Load Font (Google, System, or Local)
#'
#' @description Smart font loader that searches multiple sources
#'
#' @param font_name Font name to search for
#' @param search_sources Order to search: "google", "system", "local"
#' @param local_dir Directory to search for local fonts
#'
#' @return Logical - TRUE if loaded successfully
#'
#' @examples
#' \dontrun{
#'   load_font_smart("Sofia Sans")
#'   load_font_smart("Roboto", local_dir = "~/fonts")
#' }
#'
#' @export
load_font_smart <- function(font_name,
                            search_sources = c("google", "system", "local"),
                            local_dir = c(
                              file.path(Sys.getenv("HOME"), ".fonts"),
                              file.path(Sys.getenv("WINDIR"), "Fonts"),
                              "/System/Library/Fonts",
                              "/Library/Fonts",
                              "/usr/share/fonts"
                            )) {
  available_fonts <- sysfonts::font_families()

  # Check if already loaded
  if (font_name %in% available_fonts) {
    message(sprintf("✓ Font '%s' already available", font_name))
    return(invisible(TRUE))
  }

  family_name <- tolower(gsub(" ", "_", font_name))

  for (source in search_sources) {
    message(sprintf("Searching %s...", source))

    if (source == "google") {
      if (load_google_font(font_name, family_name)) {
        return(invisible(TRUE))
      }
    } else if (source == "system") {
      if (font_name %in% available_fonts) {
        message(sprintf("✓ Found '%s' in system fonts", font_name))
        return(invisible(TRUE))
      }
    } else if (source == "local") {
      # Search local directories
      for (dir in local_dir) {
        if (!dir.exists(dir)) next

        font_files <- list.files(
          dir,
          pattern = paste0("(?i)", font_name, ".*\\.(ttf|otf)$"),
          full.names = TRUE,
          ignore.case = TRUE
        )

        if (length(font_files) > 0) {
          if (load_local_font(font_files[1], family_name)) {
            return(invisible(TRUE))
          }
        }
      }
    }
  }

  message(sprintf("✗ Could not find '%s' in any source", font_name))
  return(invisible(FALSE))
}

#' Set Plot Theme and Fonts
#'
#' @description Configures ggplot2 theme with complete control over:
#'   fonts (Google, system, local), colors, and layout
#'
#' @param font Font family name (searches Google Fonts, system, and local)
#' @param size Base font size in points (default: 12)
#' @param theme_style Theme base ("tufte", "minimal", "bw", "classic")
#' @param load_google Try to load from Google Fonts (default: TRUE)
#' @param plot_bg Plot background color (default: "white")
#' @param title_color Title color (default: "black")
#' @param text_color Text color (default: "black")
#' @param line_color Axis line color (default: "black")
#' @param grid Include grid lines? (default: FALSE)
#'
#' @return List with theme configuration (invisibly)
#'
#' @examples
#' \dontrun{
#'   # List available fonts
#'   list_system_fonts()
#'
#'   # Use system font
#'   set_plot_theme(font = "Arial", size = 14)
#'
#'   # Load and use Google Font
#'   set_plot_theme(font = "Sofia Sans", size = 13, load_google = TRUE)
#'
#'   # Use local font file
#'   set_plot_theme(font = "my_font", size = 12)
#' }
#'
#' @export
set_plot_theme <- function(font = "sans",
                           size = 12,
                           theme_style = c("tufte", "minimal", "bw", "classic"),
                           load_google = TRUE,
                           plot_bg = "white",
                           title_color = "black",
                           text_color = "black",
                           line_color = "black",
                           grid = FALSE) {

  theme_style <- match.arg(theme_style)

  if (!is.character(font) || length(font) != 1) {
    stop("'font' must be a single character string")
  }

  if (!is.numeric(size) || size <= 0) {
    stop("'size' must be a positive number")
  }

  message(sprintf("🔍 Searching for font: %s", font))

  # Try to load font from multiple sources
  available_fonts <- sysfonts::font_families()
  font_family <- font

  # 1. Check if already available
  if (font %in% available_fonts) {
    message(sprintf("✓ Found '%s' in system fonts", font))
    font_loaded <- TRUE
  } else {
    font_loaded <- FALSE

    # 2. Try Google Fonts
    if (load_google) {
      message(sprintf("Attempting to load '%s' from Google Fonts...", font))
      if (load_google_font(font, tolower(gsub(" ", "_", font)))) {
        font_family <- tolower(gsub(" ", "_", font))
        font_loaded <- TRUE
      }
    }

    # 3. Try local font search
    if (!font_loaded) {
      message("Searching local font directories...")
      if (load_font_smart(font)) {
        font_family <- tolower(gsub(" ", "_", font))
        font_loaded <- TRUE
      }
    }
  }

  # 4. Fallback
  if (!font_loaded) {
    message(sprintf("⚠️  Could not find '%s'. Using 'sans' as fallback", font))
    font_family <- "sans"
  }

  # Base theme selection
  base_theme <- switch(theme_style,
    tufte = ggthemes::theme_tufte(),
    minimal = ggplot2::theme_minimal(),
    bw = ggplot2::theme_bw(),
    classic = ggplot2::theme_classic()
  )

  # Title sizes relative to base
  title_size <- size + 4
  subtitle_size <- size + 2
  caption_size <- size - 2
  axis_label_size <- size + 1
  axis_text_size <- size - 1

  # Build custom theme
  custom_theme <- base_theme +
    ggplot2::theme(
      # Overall appearance
      plot.background = ggplot2::element_rect(fill = plot_bg, color = NA),
      panel.background = ggplot2::element_rect(fill = plot_bg, color = NA),

      # Titles and labels
      plot.title = ggplot2::element_text(
        family = font_family,
        size = title_size,
        color = title_color,
        hjust = 0,
        vjust = 2,
        margin = ggplot2::margin(t = 10, b = 10),
        face = "bold"
      ),
      plot.subtitle = ggplot2::element_text(
        family = font_family,
        size = subtitle_size,
        color = text_color,
        hjust = 0
      ),
      plot.caption = ggplot2::element_text(
        family = font_family,
        size = caption_size,
        color = text_color,
        hjust = 0.5,
        vjust = 1
      ),
      plot.caption.position = "plot",

      # Axis labels
      axis.title = ggplot2::element_text(
        family = font_family,
        size = axis_label_size,
        color = text_color,
        face = "bold"
      ),
      axis.text = ggplot2::element_text(
        family = font_family,
        size = axis_text_size,
        color = text_color
      ),
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 5, b = 10)
      ),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(l = 5, r = 10)
      ),

      # Axes and ticks
      axis.ticks = ggplot2::element_line(
        color = line_color,
        linewidth = 0.5
      ),
      axis.ticks.length = ggplot2::unit(4, "mm"),
      axis.line = ggplot2::element_line(
        color = line_color,
        linewidth = 0.8
      ),

      # Strip (facet labels)
      strip.text = ggplot2::element_text(
        family = font_family,
        size = size,
        color = text_color,
        face = "bold"
      ),
      strip.background = ggplot2::element_rect(
        fill = plot_bg,
        color = line_color
      ),

      # Grid
      panel.grid.major = if (grid) {
        ggplot2::element_line(color = "gray90", linewidth = 0.3)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.minor = ggplot2::element_blank(),

      # Legend
      legend.title = ggplot2::element_text(
        family = font_family,
        size = size,
        color = text_color,
        face = "bold"
      ),
      legend.text = ggplot2::element_text(
        family = font_family,
        size = axis_text_size,
        color = text_color
      ),
      legend.background = ggplot2::element_rect(
        fill = plot_bg,
        color = line_color
      ),
      legend.key = ggplot2::element_rect(
        fill = plot_bg,
        color = NA
      )
    )

  # Enable showtext for font rendering
  showtext::showtext_auto(enable = TRUE)

  # Apply theme globally
  ggplot2::theme_set(custom_theme)

  # Print summary
  message(sprintf("
╔══════════════════════════════════════╗
║        Plot Theme Applied            ║
╠══════════════════════════════════════╣
║  Font: %s
║  Size: %d pt
║  Style: %s
║  Background: %s
║  Grid: %s
║  Text Color: %s
║  Line Color: %s
╚══════════════════════════════════════╝",
    font_family,
    size,
    theme_style,
    plot_bg,
    if (grid) "Yes" else "No",
    text_color,
    line_color
  ))

  invisible(list(
    font = font_family,
    size = size,
    theme_style = theme_style,
    plot_bg = plot_bg,
    title_color = title_color,
    text_color = text_color,
    line_color = line_color,
    grid = grid
  ))
}

#' Enhanced ggplot Constructor with Axes Caps
#'
#' @description Convenience function for creating ggplot with axis caps.
#'   Applies guides that add caps to both x and y axes.
#'
#' @param ... Arguments passed to ggplot2::ggplot()
#'
#' @return ggplot object with axis caps
#'
#' @examples
#' \dontrun{
#'   mtcars %>%
#'     plot_base(ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point()
#' }
#'
#' @export
plot_base <- function(...) {
  ggplot2::ggplot(...) +
    ggplot2::guides(
      x = ggplot2::guide_axis(cap = "both"),
      y = ggplot2::guide_axis(cap = "both")
    )
}

#' Alias for plot_base (Original Name)
#'
#' @description Original function name. Use plot_base() instead.
#'
#' @param ... Arguments passed to ggplot2::ggplot()
#'
#' @return ggplot object with axis caps
#'
#' @export
kkplot <- plot_base

#' Univariate Categorical Plot
#'
#' @param data Data frame
#' @param variable Variable name (unquoted)
#'
#' @return ggplot object
#'
#' @export
univariate_categorical_plot <- function(data, variable) {
  validate_data_frame(data)
  variable <- rlang::ensym(variable)
  var_name <- rlang::as_name(variable)

  validate_categorical_column(data, var_name)

  title <- sprintf("Distribution of %s", var_name)

  data %>%
    dplyr::count({{ variable }}) %>%
    dplyr::filter(!is.na({{ variable }})) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    plot_base(ggplot2::aes(y = forcats::fct_reorder({{ variable }}, prop),
                           x = prop)) +
    ggplot2::geom_col(alpha = 0.6, fill = "gray60", color = "black") +
    ggplot2::geom_label(
      ggplot2::aes(label = sprintf("%d (%.1f%%)", n, prop * 100)),
      color = "black", size = 4, hjust = -0.1
    ) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::expand_limits(x = 1) +
    ggplot2::labs(x = "Proportion", y = "Category", title = title)
}

#' Univariate Continuous Plot
#'
#' @param data Data frame
#' @param variable Variable name (unquoted)
#'
#' @return ggplot object
#'
#' @export
univariate_continuous_plot <- function(data, variable) {
  validate_data_frame(data)
  variable <- rlang::ensym(variable)
  var_name <- rlang::as_name(variable)

  validate_numeric_column(data, var_name)

  title <- sprintf("Distribution of %s", var_name)

  mean_val <- mean(data[[var_name]], na.rm = TRUE)
  median_val <- stats::median(data[[var_name]], na.rm = TRUE)
  density_data <- stats::density(data[[var_name]], na.rm = TRUE)
  max_density <- max(density_data$y)

  data %>%
    plot_base(ggplot2::aes(x = !!variable)) +
    ggplot2::geom_density(
      adjust = 0.5, fill = "gray90", color = "black", alpha = 0.6
    ) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = mean_val),
      color = "red", linetype = 1, linewidth = 1
    ) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = median_val),
      color = "blue", linetype = "dashed", linewidth = 1
    ) +
    ggplot2::annotate(
      "label",
      x = mean_val, y = 0.9 * max_density,
      label = sprintf("Mean: %.2f", mean_val),
      color = "red", size = 4, hjust = -0.1
    ) +
    ggplot2::annotate(
      "label",
      x = median_val, y = 0.8 * max_density,
      label = sprintf("Median: %.2f", median_val),
      color = "blue", size = 4, hjust = -0.1
    ) +
    ggplot2::labs(x = "Value", y = "Density", title = title)
}

#' Univariate Plot (Auto-Detect Type)
#'
#' @param data Data frame
#' @param variable Variable name (unquoted)
#'
#' @return ggplot object
#'
#' @export
univariate_plot <- function(data, variable) {
  validate_data_frame(data)
  variable <- rlang::ensym(variable)
  var_name <- rlang::as_name(variable)

  if (is.numeric(data[[var_name]])) {
    univariate_continuous_plot(data, !!variable)
  } else if (is.character(data[[var_name]]) || is.factor(data[[var_name]])) {
    univariate_categorical_plot(data, !!variable)
  } else {
    stop(sprintf("Variable '%s' must be numeric or categorical", var_name))
  }
}

# ============================================================
# 10. SUMMARY STATISTICS
# ============================================================

#' Comprehensive Summary Statistics
#'
#' @description Calculates extensive descriptive statistics for numeric
#'   or categorical variables with grouping support
#'
#' @param data Data frame
#' @param col Variable to summarize (unquoted)
#' @param var_name Optional name for variable
#' @param verbose Level: "full", "basic", or "custom"
#' @param stats Custom statistics to include (for verbose="custom")
#' @param pairwise Calculate pairwise comparisons
#' @param chi_probs Expected probabilities for chi-square test
#'
#' @return Tibble with summary statistics
#'
#' @export
comprehensive_summary <- function(data, col, var_name = NULL,
                                   verbose = c("full", "basic", "custom"),
                                   stats = NULL, pairwise = TRUE,
                                   chi_probs = NULL) {
  verbose <- match.arg(verbose)
  validate_data_frame(data)

  col_quo <- rlang::enquo(col)
  col_name <- rlang::quo_name(col_quo)
  if (is.null(var_name)) var_name <- col_name

  x <- dplyr::pull(data, {{ col }})

  compute_stats <- function(x, var_name) {
    if (is.null(x)) stop("Input cannot be NULL")
    if (is.data.frame(x) || is.matrix(x)) {
      stop("Input must be vector or factor, not data frame/matrix")
    }

    result <- list()
    n_miss <- sum(is.na(x))
    result$var_name <- var_name
    result$type <- if (is.numeric(x)) "numeric" else "categorical"
    result$n_total <- length(x)
    result$n_miss <- n_miss
    result$n_valid <- length(x) - n_miss
    result$miss_pct <- (n_miss / length(x)) * 100

    if (result$n_valid == 0) {
      result$note <- "All values missing"
      return(result)
    }

    if (verbose == "basic") {
      stats_to_use <- if (result$type == "numeric") {
        c("mean", "median", "sd", "min", "max")
      } else {
        c("n_unique", "mode")
      }
    } else if (verbose == "custom" && !is.null(stats)) {
      stats_to_use <- stats
    } else {
      stats_to_use <- if (result$type == "numeric") {
        c("mean", "median", "sd", "var", "min", "max", "range",
          "q1", "q3", "iqr", "mad", "cv", "skewness", "kurtosis",
          "ci_mean_low", "ci_mean_up", "outlier_count")
      } else {
        c("n_unique", "mode", "entropy", "gini_index", "chi_p")
      }
    }

    if (result$type == "numeric") {
      x_clean <- stats::na.omit(x)

      if ("mean" %in% stats_to_use) result$mean <- mean(x_clean)
      if ("median" %in% stats_to_use) result$median <- stats::median(x_clean)
      if ("sd" %in% stats_to_use) result$sd <- stats::sd(x_clean)
      if ("var" %in% stats_to_use) result$var <- stats::var(x_clean)
      if ("min" %in% stats_to_use) result$min <- min(x_clean)
      if ("max" %in% stats_to_use) result$max <- max(x_clean)
      if ("range" %in% stats_to_use) result$range <- max(x_clean) - min(x_clean)

      if (any(c("q1", "q3", "iqr", "mad", "outlier_count") %in% stats_to_use)) {
        q1_val <- stats::quantile(x_clean, 0.25)
        q3_val <- stats::quantile(x_clean, 0.75)
        if ("q1" %in% stats_to_use) result$q1 <- q1_val
        if ("q3" %in% stats_to_use) result$q3 <- q3_val
        if ("iqr" %in% stats_to_use) result$iqr <- q3_val - q1_val

        if ("outlier_count" %in% stats_to_use) {
          iqr <- q3_val - q1_val
          outliers <- sum(x_clean < (q1_val - CONSTANTS$OUTLIER_IQR_MULTIPLIER * iqr) |
                           x_clean > (q3_val + CONSTANTS$OUTLIER_IQR_MULTIPLIER * iqr))
          result$outlier_count <- outliers
        }
      }

      if ("mad" %in% stats_to_use) {
        result$mad <- stats::mad(x_clean, constant = 1.4826)
      }

      if ("cv" %in% stats_to_use) {
        mean_val <- mean(x_clean)
        if (mean_val != 0) {
          result$cv <- stats::sd(x_clean) / abs(mean_val)
        }
      }

      if ("skewness" %in% stats_to_use && length(x_clean) >= 3) {
        result$skewness <- moments::skewness(x_clean)
      }

      if ("kurtosis" %in% stats_to_use && length(x_clean) >= 4) {
        result$kurtosis <- moments::kurtosis(x_clean)
      }

      if (any(c("ci_mean_low", "ci_mean_up") %in% stats_to_use)) {
        t_test <- stats::t.test(x_clean, conf.level = CONSTANTS$DEFAULT_CONF_LEVEL)
        result$ci_mean_low <- t_test$conf.int[1]
        result$ci_mean_up <- t_test$conf.int[2]
      }

    } else {
      x_cat <- as.factor(x)
      x_cat <- x_cat[!is.na(x_cat)]
      freq_table <- table(x_cat)

      if (any(c("n_unique", "mode", "entropy", "gini_index") %in% stats_to_use)) {
        result$n_unique <- length(unique(x_cat))
      }

      if ("mode" %in% stats_to_use) {
        result$mode <- names(sort(freq_table, decreasing = TRUE))[1]
      }

      if ("entropy" %in% stats_to_use) {
        freqs <- prop.table(freq_table)
        result$entropy <- entropy::entropy(freqs, unit = "log2")
      }

      if ("gini_index" %in% stats_to_use) {
        p <- prop.table(freq_table)
        result$gini_index <- 1 - sum(p^2)
      }

      if ("chi_p" %in% stats_to_use && result$n_unique > 1) {
        expected_prob <- if (!is.null(chi_probs)) {
          if (length(chi_probs) != result$n_unique || sum(chi_probs) != 1) {
            stop("chi_probs must match levels and sum to 1")
          }
          chi_probs
        } else {
          rep(1 / result$n_unique, result$n_unique)
        }

        chi_test <- stats::chisq.test(freq_table, p = expected_prob)
        result$chi_p <- chi_test$p.value
        result$chi_significant <- chi_test$p.value <= 0.05
      }
    }

    return(result)
  }

  if (dplyr::is_grouped_df(data)) {
    result <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(dplyr::group_vars(.)))) %>%
      dplyr::summarise(stats = list(compute_stats({{ col }}, var_name)),
                       .groups = "keep") %>%
      tidyr::unnest_wider(stats)
  } else {
    result <- tibble::as_tibble(compute_stats(x, var_name))
  }

  return(result)
}

# ============================================================
# 11. ONE-HOT ENCODING
# ============================================================

#' One-Hot Encoding for Categorical Variables
#'
#' @description Creates binary dummy variables for categorical column
#'
#' @param data Tibble or data frame
#' @param column Column name (character or unquoted)
#'
#' @return Data with new binary columns added
#'
#' @export
one_hot_encode <- function(data, column) {
  if (!inherits(data, c("tbl_df", "data.frame"))) {
    stop(sprintf("Input must be tibble or data frame, got %s", class(data)[1]))
  }

  col_name <- if (is.symbol(substitute(column))) {
    rlang::as_string(rlang::ensym(column))
  } else {
    as.character(column)
  }

  if (!col_name %in% names(data)) {
    stop(sprintf("Column '%s' not found", col_name))
  }

  var <- data[[col_name]]

  if (!is.character(var) && !is.factor(var)) {
    stop(sprintf("'%s' must be character or factor", col_name))
  }

  if (is.character(var)) var <- as.factor(var)

  levels_var <- levels(var)

  for (level in levels_var) {
    new_col_name <- paste0(col_name, "_", level)
    data[[new_col_name]] <- if_else(var == level, 1L, 0L)
  }

  tibble::as_tibble(data)
}

# ============================================================
# 12. TIME SERIES ANALYSIS
# ============================================================

#' Time Series Metrics and Analysis
#'
#' @description Calculates comprehensive time series metrics including
#'   descriptive statistics, trends, and stationarity tests
#'
#' @param data Data frame with time series
#' @param value_col Value column name (character or unquoted)
#' @param date_col Date column name (default: "date")
#' @param group_cols Optional grouping columns
#' @param include_advanced Include advanced metrics (GARCH, Hurst, etc.)
#' @param round_digits Decimal places to round
#'
#' @return Tibble with time series metrics
#'
#' @export
time_series_analysis <- function(data, value_col = NULL,
                                  date_col = "date",
                                  group_cols = NULL,
                                  include_advanced = TRUE,
                                  round_digits = 4) {
  if (missing(data)) {
    stop("Argument 'data' is missing")
  }

  if (is.null(value_col) && !"value" %in% names(data)) {
    stop("Specify value_col or ensure 'value' column exists")
  }

  if (!is.null(value_col)) {
    if (is.character(value_col)) value_col <- rlang::sym(value_col)
    data <- dplyr::mutate(data, value = !!value_col)
  }

  validate_date_column(data, date_col)

  if (!is.null(group_cols)) {
    group_cols <- rlang::syms(group_cols)
    data <- dplyr::group_by(data, !!!group_cols)
  }

  date_col_sym <- rlang::sym(date_col)

  result <- data %>%
    dplyr::arrange(!!date_col_sym) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_map(~ {
      y <- .x$value
      n <- length(y)

      if (n < 2) {
        stop(sprintf("Need at least 2 observations, got %d", n))
      }

      if (n < 5) {
        warning(sprintf("Small sample size (n=%d). Results may be unreliable.", n))
      }

      # Basic statistics
      tibble::tibble(
        n_obs = n,
        mean = mean(y, na.rm = TRUE),
        median = stats::median(y, na.rm = TRUE),
        sd = stats::sd(y, na.rm = TRUE),
        min = min(y, na.rm = TRUE),
        max = max(y, na.rm = TRUE),
        range = max(y) - min(y),
        trend_slope = if (n >= 3) {
          time_idx <- as.numeric(seq_len(n))
          stats::coef(stats::lm(y ~ time_idx))[2]
        } else {
          NA_real_
        },
        acf_lag1 = if (n >= 3) {
          stats::acf(y, lag.max = 1, plot = FALSE)$acf[2]
        } else {
          NA_real_
        },
        adf_pvalue = if (n >= CONSTANTS$MIN_SAMPLE_FOR_TESTS$adf_kpss) {
          tryCatch(
            tseries::adf.test(y)$p.value,
            error = function(e) NA_real_
          )
        } else {
          NA_real_
        },
        garch_vol = if (include_advanced && n >= CONSTANTS$MIN_SAMPLE_FOR_TESTS$garch) {
          tryCatch(
            {
              y_diff <- if (n > 1) diff(y) else y
              spec <- rugarch::ugarchspec(
                mean.model = list(armaOrder = c(1, 0)),
                variance.model = list(model = "sGARCH", garchOrder = c(1, 1))
              )
              fit <- rugarch::ugarchfit(spec, y_diff, solver = "hybrid")
              mean(rugarch::sigma(fit), na.rm = TRUE)
            },
            error = function(e) NA_real_
          )
        } else {
          NA_real_
        }
      ) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), 
                                    ~ round(., round_digits)))
    }, .keep = TRUE) %>%
    dplyr::bind_rows()

  result
}

# ============================================================
# 14. CONFUSION MATRIX METRICS
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
#'   # Basic usage
#'   confusion_metrics_ci(c(tp=85, fp=10, fn=15, tn=890))
#'
#'   # With bootstrap CIs
#'   confusion_metrics_ci(c(tp=85, fp=10, fn=15, tn=890), boot = TRUE, B = 10000)
#'
#'   # Higher confidence
#'   confusion_metrics_ci(c(tp=85, fp=10, fn=15, tn=890), conf = 0.99)
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
      if (ncol(x) == 2 && (nm %in% c("name","metric","label") || is.character(x[[1]]))) {
        kv <- setNames(as.numeric(x[[2]]), tolower(gsub("[^a-z ]", "", x[[1]])))
      } else if (all(c("tp","fp","fn","tn") %in% tolower(names(x)))) {
        take <- tolower(names(x)) %in% c("tp","fp","fn","tn")
        kv <- setNames(as.numeric(x[1, take]), tolower(names(x))[take])
      } else {
        stop("Could not parse counts. Provide two columns (label,value) or columns named tp,fp,fn,tn.")
      }
    } else if (is.list(x) || is.vector(x)) {
      kv <- setNames(as.numeric(x), tolower(names(x)))
    } else stop("Unsupported input type.")

    map <- c("true positives"="tp","tp"="tp",
             "false positives"="fp","fp"="fp",
             "false negatives"="fn","fn"="fn",
             "true negatives"="tn","tn"="tn")
    out <- c(tp=NA_real_, fp=NA_real_, fn=NA_real_, tn=NA_real_)
    for (k in names(kv)) if (k %in% names(map)) out[ map[[k]] ] <- kv[[k]]
    if (any(is.na(out))) stop("Missing one or more of TP/FP/FN/TN.")
    as.list(out)
  }

  binom_ci <- function(success, total, conf) {
    if (total == 0) return(c(est=NA_real_, lwr=NA_real_, upr=NA_real_))
    bt <- binom.test(success, total, conf.level = conf)
    c(est = unname(bt$estimate), lwr = bt$conf.int[1], upr = bt$conf.int[2])
  }

  ci_complement <- function(ci_named) {
    if (any(is.na(ci_named))) return(c(est=NA_real_, lwr=NA_real_, upr=NA_real_))
    est <- as.numeric(1 - ci_named[["est"]])
    lwr <- as.numeric(1 - ci_named[["upr"]])
    upr <- as.numeric(1 - ci_named[["lwr"]])
    c(est=est, lwr=lwr, upr=upr)
  }

  lognorm_ci_ratio <- function(estimate, se_log, conf) {
    if (is.na(estimate) || is.na(se_log) || estimate <= 0) return(c(NA_real_, NA_real_))
    z <- qnorm(1 - (1 - conf)/2)
    c(exp(log(estimate) - z*se_log), exp(log(estimate) + z*se_log))
  }

  as_row <- function(metric, est, lwr, upr, method, note = NA_character_) {
    data.frame(metric = metric,
               estimate = as.numeric(est),
               lower = as.numeric(lwr),
               upper = as.numeric(upr),
               ci_level = conf,
               ci_method = method,
               note = note,
               stringsAsFactors = FALSE)
  }

  # ----------------------------- counts & basic metrics -------------------
  cts <- get_counts(x)
  TP <- cts$tp; FP <- cts$fp; FN <- cts$fn; TN <- cts$tn

  P  <- TP + FN
  N  <- TN + FP
  Tn <- P + N

  sens <- if (P > 0) TP / P else NA_real_
  spec <- if (N > 0) TN / N else NA_real_
  ppv  <- if ((TP + FP) > 0) TP / (TP + FP) else NA_real_
  npv  <- if ((TN + FN) > 0) TN / (TN + FN) else NA_real_
  acc  <- if (Tn > 0) (TP + TN) / Tn else NA_real_
  prev <- if (Tn > 0) P / Tn else NA_real_

  fpr <- if (!is.na(spec)) 1 - spec else NA_real_
  fnr <- if (!is.na(sens)) 1 - sens else NA_real_
  fdr <- if (!is.na(ppv))  1 - ppv  else NA_real_
  forr<- if (!is.na(npv))  1 - npv  else NA_real_

  ba   <- mean(c(sens, spec), na.rm = TRUE)
  mk   <- if (!is.na(ppv) && !is.na(npv)) ppv + npv - 1 else NA_real_
  bm   <- if (!is.na(sens) && !is.na(spec)) sens + spec - 1 else NA_real_

  f1   <- if (!is.na(ppv) && !is.na(sens) && (ppv + sens) > 0) 2 * ppv * sens / (ppv + sens) else NA_real_
  fm   <- if (!is.na(ppv) && !is.na(sens)) sqrt(ppv * sens) else NA_real_
  ts   <- if ((TP + FN + FP) > 0) TP / (TP + FN + FP) else NA_real_

  denom_mcc <- sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  mcc <- if (denom_mcc > 0) ((TP*TN) - (FP*FN)) / denom_mcc else NA_real_

  PT <- if (!is.na(sens) && !is.na(fpr) && (sens - fpr) != 0) {
    (sqrt(sens * fpr) - fpr) / (sens - fpr)
  } else NA_real_

  # Haldane–Anscombe for LR/DOR if any zero
  TPc <- TP; FPc <- FP; FNc <- FN; TNc <- TN
  if (add_0_5_for_lr && any(c(TP,FP,FN,TN) == 0)) {
    TPc <- TP + 0.5; FPc <- FP + 0.5; FNc <- FN + 0.5; TNc <- TN + 0.5
  }
  sens_c <- if ((TPc + FNc) > 0) TPc / (TPc + FNc) else NA_real_
  spec_c <- if ((TNc + FPc) > 0) TNc / (TNc + FPc) else NA_real_
  fpr_c  <- if (!is.na(spec_c)) 1 - spec_c else NA_real_

  lr_pos <- if (!is.na(sens_c) && !is.na(fpr_c) && fpr_c > 0) sens_c / fpr_c else NA_real_
  lr_neg <- if (!is.na(spec_c) && !is.na(sens_c) && spec_c > 0) (1 - sens_c) / spec_c else NA_real_
  dor    <- if (!is.na(lr_pos) && !is.na(lr_neg) && lr_neg > 0) lr_pos / lr_neg else NA_real_

  # ----------------------------- CIs -------------------------------------
  ci_sens <- binom_ci(TP, P, conf)
  ci_spec <- binom_ci(TN, N, conf)
  ci_ppv  <- binom_ci(TP, TP + FP, conf)
  ci_npv  <- binom_ci(TN, TN + FN, conf)
  ci_acc  <- binom_ci(TP + TN, Tn, conf)
  ci_prev <- binom_ci(P, Tn, conf)

  ci_fpr <- ci_complement(ci_spec)
  ci_fnr <- ci_complement(ci_sens)
  ci_fdr <- ci_complement(ci_ppv)
  ci_for <- ci_complement(ci_npv)

  se_log_lr_pos <- if (all(c(TPc, FPc, TPc+FNc, TNc+FPc) > 0)) {
    sqrt(1/TPc - 1/(TPc+FNc) + 1/FPc - 1/(TNc+FPc))
  } else NA_real_
  se_log_lr_neg <- if (all(c(FNc, TNc, TPc+FNc, TNc+FPc) > 0)) {
    sqrt(1/FNc - 1/(TPc+FNc) + 1/TNc - 1/(TNc+FPc))
  } else NA_real_
  se_log_dor <- if (all(c(TPc, FPc, FNc, TNc) > 0)) {
    sqrt(1/TPc + 1/FPc + 1/FNc + 1/TNc)
  } else NA_real_

  ci_lr_pos <- if (!is.na(lr_pos) && !is.na(se_log_lr_pos))
    c(est = lr_pos, lognorm_ci_ratio(lr_pos, se_log_lr_pos, conf)) else c(est=NA_real_, NA_real_, NA_real_)
  ci_lr_neg <- if (!is.na(lr_neg) && !is.na(se_log_lr_neg))
    c(est = lr_neg, lognorm_ci_ratio(lr_neg, se_log_lr_neg, conf)) else c(est=NA_real_, NA_real_, NA_real_)
  ci_dor <- if (!is.na(dor) && !is.na(se_log_dor))
    c(est = dor, lognorm_ci_ratio(dor, se_log_dor, conf)) else c(est=NA_real_, NA_real_, NA_real_)

  # ----------------------------- collect rows ----------------------------
  out <- do.call(rbind, list(
    as_row("prevalence",           ci_prev[["est"]], ci_prev[["lwr"]], ci_prev[["upr"]], "exact binomial"),
    as_row("accuracy",             ci_acc[["est"]],  ci_acc[["lwr"]],  ci_acc[["upr"]],  "exact binomial"),
    as_row("sensitivity (TPR)",    ci_sens[["est"]], ci_sens[["lwr"]], ci_sens[["upr"]], "exact binomial"),
    as_row("specificity (TNR)",    ci_spec[["est"]], ci_spec[["lwr"]], ci_spec[["upr"]], "exact binomial"),
    as_row("PPV (precision)",      ci_ppv[["est"]],  ci_ppv[["lwr"]],  ci_ppv[["upr"]],  "exact binomial"),
    as_row("NPV",                  ci_npv[["est"]],  ci_npv[["lwr"]],  ci_npv[["upr"]],  "exact binomial"),

    as_row("FPR",                  ci_fpr[["est"]],  ci_fpr[["lwr"]],  ci_fpr[["upr"]],
          "complement of specificity CI"),
    as_row("FNR",                  ci_fnr[["est"]],  ci_fnr[["lwr"]],  ci_fnr[["upr"]],
          "complement of sensitivity CI"),
    as_row("FDR",                  ci_fdr[["est"]],  ci_fdr[["lwr"]],  ci_fdr[["upr"]],
          "complement of PPV CI"),
    as_row("FOR",                  ci_for[["est"]],  ci_for[["lwr"]],  ci_for[["upr"]],
          "complement of NPV CI"),

    as_row("LR+",                  ci_lr_pos[[1]],   ci_lr_pos[[2]],   ci_lr_pos[[3]],
          "log-normal (approx.)",
          if (add_0_5_for_lr && any(c(TP,FP,FN,TN)==0)) "Haldane–Anscombe +0.5 used" else NA_character_),
    as_row("LR−",                  ci_lr_neg[[1]],   ci_lr_neg[[2]],   ci_lr_neg[[3]],
          "log-normal (approx.)",
          if (add_0_5_for_lr && any(c(TP,FP,FN,TN)==0)) "Haldane–Anscombe +0.5 used" else NA_character_),
    as_row("DOR",                  ci_dor[[1]],      ci_dor[[2]],      ci_dor[[3]],      "log-normal (approx.)"),

    as_row("Prevalence threshold (PT)", PT,          NA,               NA,               "point estimate"),
    as_row("balanced accuracy",    ba,               NA,               NA,               "point estimate"),
    as_row("F1 score",             f1,               NA,               NA,               "point estimate"),
    as_row("Fowlkes–Mallows",      fm,               NA,               NA,               "point estimate"),
    as_row("Threat score (CSI)",   ts,               NA,               NA,               "point estimate"),
    as_row("Markedness (Δp)",      mk,               NA,               NA,               "point estimate"),
    as_row("Bookmaker informedness (BM/Youden)", bm, NA,               NA,               "point estimate"),
    as_row("Matthews corr. coeff. (MCC)", mcc,       NA,               NA,               "point estimate")
  ))

  # ----------------------------- bootstrap CIs (optional) ----------------
  if (boot) {
    set.seed(seed)
    probs <- c(TP, FP, FN, TN)
    if (sum(probs) == 0) stop("All counts are zero; cannot bootstrap.")
    probs <- probs / sum(probs)
    total <- Tn

    one_metric <- function(tp, fp, fn, tn) {
      P  <- tp + fn; N <- tn + fp
      sens <- if (P > 0) tp/P else NA_real_
      spec <- if (N > 0) tn/N else NA_real_
      ppv  <- if ((tp+fp) > 0) tp/(tp+fp) else NA_real_
      npv  <- if ((tn+fn) > 0) tn/(tn+fn) else NA_real_
      ba   <- mean(c(sens, spec), na.rm = TRUE)
      f1   <- if (!is.na(ppv) && !is.na(sens) && (ppv+sens)>0) 2*ppv*sens/(ppv+sens) else NA_real_
      fm   <- if (!is.na(ppv) && !is.na(sens)) sqrt(ppv*sens) else NA_real_
      ts   <- if ((tp+fn+fp) > 0) tp/(tp+fn+fp) else NA_real_
      mk   <- if (!is.na(ppv) && !is.na(npv)) ppv+npv-1 else NA_real_
      bm   <- if (!is.na(sens) && !is.na(spec)) sens+spec-1 else NA_real_
      denom <- sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
      mcc  <- if (denom > 0) ((tp*tn)-(fp*fn))/denom else NA_real_
      c(ba=ba,f1=f1,fm=fm,ts=ts,mk=mk,bm=bm,mcc=mcc)
    }

    boot_mat <- matrix(NA_real_, nrow = B, ncol = 7)
    colnames(boot_mat) <- c("balanced accuracy","F1 score","Fowlkes–Mallows",
                            "Threat score (CSI)","Markedness (Δp)",
                            "Bookmaker informedness (BM/Youden)","Matthews corr. coeff. (MCC)")

    for (b in seq_len(B)) {
      draw <- as.vector(rmultinom(1, size = total, prob = probs))
      boot_mat[b, ] <- do.call(one_metric, as.list(draw))
    }

    add_ci <- function(metric_name, point_est) {
      vals <- boot_mat[, metric_name]
      vals <- vals[is.finite(vals)]
      if (length(vals) < 10L) return(NULL)
      qs <- stats::quantile(vals, probs = c((1-conf)/2, 1-(1-conf)/2), na.rm = TRUE, type = 6)
      as_row(metric_name, point_est, unname(qs[1]), unname(qs[2]),
             sprintf("nonparametric bootstrap (B=%d, multinomial on 2x2)", B))
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

message("
╔════════════════════════════════════════════════════════════╗
║   R Statistical Analysis Toolkit - Loaded Successfully     ║
╚════════════════════════════════════════════════════════════╝
")