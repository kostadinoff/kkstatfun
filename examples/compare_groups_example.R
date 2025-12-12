# Example Usage of compare_groups_table

library(kkstatfun)
library(dplyr)

# Create example data similar to your beta thalassemia study
set.seed(123)
n1 <- 49
n2 <- 30

example_data <- tibble(
              group = c(rep("бета таласемия", n1), rep("Контрола", n2)),
              age = c(rnorm(n1, 34.78, 12.88), rnorm(n2, 34.83, 13.42)),
              male = c(rbinom(n1, 1, 0.61), rbinom(n2, 1, 0.63)),
              married = c(rbinom(n1, 1, 0.33), rbinom(n2, 1, 0.33)),
              high_education = c(rbinom(n1, 1, 0.31), rbinom(n2, 1, 0.43)),
              smoker = c(rbinom(n1, 1, 0.31), rbinom(n2, 1, 0.57)),
              height = c(rnorm(n1, 167.57, 9.47), rnorm(n2, 176.07, 11.55)),
              weight = c(rnorm(n1, 62.37, 11.87), rnorm(n2, 79.60, 21.05))
)

# Convert binary variables to factors
example_data <- example_data %>%
              mutate(
                            male = factor(male, levels = 0:1, labels = c("Female", "Male")),
                            married = factor(married, levels = 0:1, labels = c("No", "Yes")),
                            high_education = factor(high_education, levels = 0:1, labels = c("No", "Yes")),
                            smoker = factor(smoker, levels = 0:1, labels = c("No", "Yes"))
              )

# Basic usage - parametric tests
result <- compare_groups_table(
              data = example_data,
              group = group,
              variables = c("age", "male", "married", "high_education", "smoker", "height", "weight")
)

print(result)

# Using nonparametric tests for continuous variables
result_nonparam <- compare_groups_table(
              data = example_data,
              group = group,
              variables = c("age", "height", "weight"),
              nonparametric = TRUE
)

print(result_nonparam)

# Example with categorical variable with >2 levels
example_data2 <- example_data %>%
              mutate(
                            education_level = sample(c("Primary", "Secondary", "University"),
                                          size = n(), replace = TRUE
                            )
              )

result_multilevel <- compare_groups_table(
              data = example_data2,
              group = group,
              variables = c("age", "education_level", "height"),
              adjust_method = "holm" # Adjust p-values for multiple comparisons
)

print(result_multilevel)
