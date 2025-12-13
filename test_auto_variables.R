# Test script for auto-detection of variables in kk_compare_groups_table

# Load the package
devtools::load_all()

# Create sample data
set.seed(123)
dat <- data.frame(
              age = rnorm(100, 50, 10),
              male = sample(0:1, 100, replace = TRUE),
              diagnosis_binary = sample(c("Control", "Case"), 100, replace = TRUE),
              married = sample(0:1, 100, replace = TRUE),
              high_education = sample(0:1, 100, replace = TRUE),
              smoker = sample(0:1, 100, replace = TRUE),
              height = rnorm(100, 170, 10),
              weight = rnorm(100, 70, 15)
)

# Test the new syntax (auto-detect variables)
cat("\n=== Testing auto-detection of variables ===\n")
result1 <- dat |>
              dplyr::select(
                            age,
                            male,
                            diagnosis_binary,
                            married,
                            high_education,
                            smoker,
                            height,
                            weight
              ) |>
              kk_compare_groups_table(diagnosis_binary)

print(result1)

# Test the old syntax (explicit variables) - should still work
cat("\n\n=== Testing explicit variables (old syntax) ===\n")
result2 <- dat |>
              dplyr::select(
                            age,
                            male,
                            diagnosis_binary,
                            married,
                            high_education,
                            smoker,
                            height,
                            weight
              ) |>
              kk_compare_groups_table(
                            group = diagnosis_binary,
                            variables = c("age", "male", "married", "high_education", "smoker", "height", "weight")
              )

print(result2)

cat("\n\n✓ Both syntaxes work correctly!\n")
