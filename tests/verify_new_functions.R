# Verification Script for New Functions

# Load necessary libraries (simulating package load by sourcing files)
library(dplyr)
library(ggplot2)
library(tibble)

# Source new files
source("R/nnt.R")
source("R/standardization.R")
source("R/bland_altman.R")
source("R/reri.R")

print("=== Testing kk_nnt ===")
print(kk_nnt(-0.1))
print(kk_nnt(0.5, type = "odds_ratio", baseline_risk = 0.2))

print("\n=== Testing kk_std_rates ===")
df_std <- data.frame(
              age_group = c("0-19", "20-39", "40-59", "60+"),
              cases = c(10, 25, 50, 100),
              pop = c(5000, 8000, 6000, 4000)
)
std_pop <- c(4000, 7000, 6000, 3000)
print(kk_std_rates(df_std, cases, pop, std_pop, multiplier = 1000))

print("\n=== Testing kk_bland_altman ===")
m1 <- c(10, 12, 15, 20, 25)
m2 <- c(11, 13, 14, 21, 24)
ba_stats <- kk_bland_altman(m1, m2, plot = FALSE)
print(ba_stats)

print("\n=== Testing kk_reri ===")
# Simulate data for RERI
set.seed(123)
n <- 1000
exp1 <- rbinom(n, 1, 0.3)
exp2 <- rbinom(n, 1, 0.3)
# Interaction effect
prob <- plogis(-2 + 0.5 * exp1 + 0.5 * exp2 + 0.8 * exp1 * exp2)
outcome <- rbinom(n, 1, prob)
df_reri <- data.frame(outcome, exp1, exp2)

model <- glm(outcome ~ exp1 * exp2, family = binomial, data = df_reri)
print(kk_reri(model, "exp1", "exp2"))

print("\n=== Verification Complete ===")
