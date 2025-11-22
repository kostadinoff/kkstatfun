# ============================================================
# EGN (BULGARIAN ID) PARSING
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
                                                                      output$birth_date,
                                                                      units = "days"
                                                        )) / 365.25)
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
                                                        output$birth_date,
                                                        units = "days"
                                          )) / 365.25)

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
