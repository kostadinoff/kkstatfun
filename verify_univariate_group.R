library(kkstatfun)
library(ggplot2)

# Create dummy data for testing
set.seed(123)
test_data <- data.frame(
              id = 1:200,
              cat_var = sample(c("A", "B", "C"), 200, replace = TRUE),
              cont_var = c(rnorm(100, 50, 10), rnorm(100, 60, 15)),
              group_var = factor(rep(c("Group 1", "Group 2"), each = 100)),
              ordered_var = factor(sample(c("Low", "Medium", "High"), 200, replace = TRUE),
                            levels = c("Low", "Medium", "High"), ordered = TRUE
              )
)

# Test 1: Categorical grouping
print("Testing categorical grouping...")
p1 <- univariate_plot(test_data, "cat_var", group = "group_var")
ggsave("verify_cat_group.png", p1, width = 8, height = 6)

# Test 2: Continuous grouping
print("Testing continuous grouping...")
p2 <- univariate_plot(test_data, "cont_var", group = "group_var")
ggsave("verify_cont_group.png", p2, width = 8, height = 6)

# Test 3: Multi-variable plotting with group
print("Testing multi-variable plotting with group...")
p3 <- univariate_plot(test_data, "cat_var", "cont_var", group = "group_var", ncol = 1)
ggsave("verify_multi_group.png", p3, width = 8, height = 10)

# Test 4: Default plotting (no group)
print("Testing default plotting...")
p4 <- univariate_plot(test_data, "cat_var", "cont_var", ncol = 1)
ggsave("verify_default.png", p4, width = 8, height = 10)

print("Verification plots saved to current directory.")
