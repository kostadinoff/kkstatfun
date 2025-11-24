# Verification script for advanced epidemiology functions

# Load package functions (simulated by sourcing files)
# In a real package dev environment, we would load_all()
source("R/advanced_epi.R")
source("R/epi_stats.R") # Dependency if any, or just for context
library(dplyr)
library(tibble)

# ============================================================
# 1. Verify kk_stratified_2x2
# ============================================================
cat("\n--- Verifying kk_stratified_2x2 ---\n")

# Create stratified data (Simpson's Paradox example)
df_strat <- tibble(
              stratum = c(rep("A", 100), rep("B", 100)),
              exposure = c(rep(1, 60), rep(0, 40), rep(1, 20), rep(0, 80)),
              outcome = c(
                            rep(1, 50), rep(0, 10), rep(1, 10), rep(0, 30), # Stratum A
                            rep(1, 5), rep(0, 15), rep(1, 15), rep(0, 65)
              ) # Stratum B
)

tryCatch(
              {
                            res_strat <- kk_stratified_2x2(df_strat, exposure, outcome, stratum)
                            print(res_strat$pooled_or)
                            print(res_strat$homogeneity_test)
              },
              error = function(e) {
                            cat("Error in kk_stratified_2x2:", e$message, "\n")
              }
)


# ============================================================
# 2. Verify kk_mcnemar
# ============================================================
cat("\n--- Verifying kk_mcnemar ---\n")

# Matched pair data
df_pair <- tibble(
              pair_id = 1:100,
              exposure = c(rep(1, 50), rep(0, 50)), # Pre-test
              outcome = c(rep(1, 30), rep(0, 20), rep(1, 40), rep(0, 10)) # Post-test (dummy structure)
)
# Better structure for McNemar:
# We need pairs. Let's make 100 pairs.
# Both +: 40
# Both -: 40
# Discordant (+/-): 15
# Discordant (-/+): 5
df_matched <- tibble(
              pair_id = rep(1:100, each = 2),
              time = rep(c("Pre", "Post"), 100),
              status = c(
                            rep(c(1, 1), 40), # Both +
                            rep(c(0, 0), 40), # Both -
                            rep(c(1, 0), 15), # Pre +, Post -
                            rep(c(0, 1), 5) # Pre -, Post +
              )
)

# Reshape to wide to see structure, but function takes long
tryCatch(
              {
                            # We need to identify exposure (e.g., Time Pre vs Post) and Outcome (Status)
                            # Actually, McNemar is usually for paired binary data.
                            # The function signature is: kk_mcnemar(data, exposure, outcome, pair_id)
                            # Here 'exposure' would be the 'time' variable (Pre/Post) and 'outcome' is 'status'.

                            res_mcnemar <- kk_mcnemar(df_matched, time, status, pair_id)
                            print(res_mcnemar)
              },
              error = function(e) {
                            cat("Error in kk_mcnemar:", e$message, "\n")
              }
)


# ============================================================
# 3. Verify kk_trend_test
# ============================================================
cat("\n--- Verifying kk_trend_test ---\n")

# Dose-response data
df_trend <- tibble(
              dose = c(rep("Low", 50), rep("Medium", 50), rep("High", 50)),
              outcome = c(
                            rep(1, 5), rep(0, 45), # 10%
                            rep(1, 15), rep(0, 35), # 30%
                            rep(1, 30), rep(0, 20)
              ) # 60%
)

tryCatch(
              {
                            res_trend <- kk_trend_test(df_trend, outcome, dose)
                            print(res_trend)
              },
              error = function(e) {
                            cat("Error in kk_trend_test:", e$message, "\n")
              }
)


# ============================================================
# 4. Verify kk_agreement
# ============================================================
cat("\n--- Verifying kk_agreement ---\n")

df_agree <- tibble(
              rater1 = c(rep(1, 80), rep(0, 20)),
              rater2 = c(rep(1, 75), rep(0, 5), rep(1, 5), rep(0, 15))
)

tryCatch(
              {
                            res_agree <- kk_agreement(df_agree, rater1, rater2)
                            print(res_agree)
              },
              error = function(e) {
                            cat("Error in kk_agreement:", e$message, "\n")
              }
)


# ============================================================
# 5. Verify kk_sensitivity_analysis
# ============================================================
cat("\n--- Verifying kk_sensitivity_analysis ---\n")

tryCatch(
              {
                            res_sens <- kk_sensitivity_analysis(estimate = 3.0, lower = 1.5, upper = 5.0, type = "RR")
                            print(res_sens)
              },
              error = function(e) {
                            cat("Error in kk_sensitivity_analysis:", e$message, "\n")
              }
)

cat("\n--- Verification Complete ---\n")
