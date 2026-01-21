library(dplyr)
library(tibble)
# Source the file instead of loading the package if it's not installed
source("R/compare_groups.R")

# 1. Reproduce Chi-square discrepancy
# Case: 15 smoking, 35 non-smoking (Total 50) -> 30% smoking
# Control: 17 smoking, 13 non-smoking (Total 30) -> 56.7% smoking
d_chi <- tibble(
              group = c(rep("case", 50), rep("control", 30)),
              smoking = c(rep(1, 15), rep(0, 35), rep(1, 17), rep(0, 13))
) |>
              mutate(smoking = as.factor(smoking))

print("Testing Chi-square on smoking data (should be 5.56, p=0.018):")
res_chi <- d_chi |> kk_compare_groups_table(group, smoking)
print(res_chi |> select(Characteristic, Test, Statistic, `p-value`))

# 2. Test unquoted group
print("Testing unquoted group and tidyselect variables:")
res_unquoted <- d_chi |> kk_compare_groups_table(group, smoking)
print(res_unquoted |> select(Characteristic, Test, Statistic, `p-value`))

# 3. Test automatic variable selection
print("Testing automatic variable selection (variables = NULL):")
res_auto <- d_chi |> kk_compare_groups_table(group)
print(res_auto)

# 4. Test grouped data behavior
print("Testing grouped data (group_by) with automatic group detection:")
d_grouped <- d_chi |>
              mutate(sex = rep(c("M", "F"), 40)) |>
              group_by(sex, group) # If we group by sex and then the group we want to compare

# The user wants: d |> group_by(smoking) |> kk_compare_groups_table(group)
# Or: d |> group_by(group) |> kk_compare_groups_table()
print("Case: d |> group_by(group) |> kk_compare_groups_table()")
# This should fail if it doesn't have 2 levels in the LAST grouping variable?
# Wait, if I group by 'group', then each slice has only 1 level of 'group'.
# So ikk_compare_groups_table will fail if it's trying to compare across 'group'.
# The user probably means: group by other variables, and compare 'group' within them.

d_test_grouped <- d_chi |>
              mutate(sex = rep(c("M", "F"), 40)) |>
              group_by(sex)

print("Testing d |> group_by(sex) |> kk_compare_groups_table(group):")
res_stratified <- d_test_grouped |> kk_compare_groups_table(group)
print(res_stratified)
