#' @importFrom dplyr %>%
#' @importFrom rlang .data :=
#' @importFrom stats coef complete.cases family median quantile setNames
#' @importFrom grDevices col2rgb rgb
#' @export
dplyr::`%>%`

# Non-standard-evaluation column names referenced inside dplyr/ggplot2 verbs.
# Declaring them here silences the spurious "no visible binding for global
# variable" NOTEs from R CMD check without weakening the checks that matter.
utils::globalVariables(c(
  ".", ".n", ".x", "Var1", "Var2", "adj_p_value", "ci_lower", "ci_upper",
  "coef.type", "comparison", "conf.high", "conf.low", "conf_high", "conf_low",
  "estimate", "group", "group1", "group2", "group_idx", "m", "m1", "m2",
  "med", "n", "obs", "p_value", "prop", "prop_diff", "rate_i", "sig",
  "subgroup1", "subgroup2", "tau", "term", "total_prop", "w", "z_score"
))
