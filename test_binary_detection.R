# Test script for categorical variable level display

# Load the package
devtools::load_all()

# Create sample data matching the user's data
set.seed(123)
dat <- data.frame(
              age = rnorm(79, 35, 10),
              male = sample(0:1, 79, replace = TRUE),
              diagnosis_binary = sample(c("Контрола", "бета таласемия"), 79, replace = TRUE),
              married = sample(0:1, 79, replace = TRUE),
              high_education = sample(0:1, 79, replace = TRUE),
              smoker = sample(0:1, 79, replace = TRUE),
              height = rnorm(79, 170, 10),
              weight = rnorm(79, 70, 15)
)

cat("\n=== Testing categorical variable level display ===\n")
cat("Binary categorical variables should show the level name\n")
cat("For example: 'diagnosis_binary - бета таласемия' instead of just 'diagnosis_binary'\n\n")

result <- dat |>
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
              kk_compare_groups_table(male)

print(result)

cat("\n\n✓ Check that 'diagnosis_binary' row shows which level is being counted\n")
cat("✓ Check that binary numeric variables (married, high_education, smoker) also show levels (0 or 1)\n")
