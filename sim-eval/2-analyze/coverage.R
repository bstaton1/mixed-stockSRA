
# THIS SCRIPT CALCULATES THE COVERAGE OF KEY QUANTITIES
# AND GENERATES THE TABLE USED IN THE MAIN TEXT

# PREPARE THE OUTPUT
source("0-prep-output.R"); ls()

# function to help streamline this
get_coverage = function(p, methods = c("lm", "lme", "ssm1", "ssm2", "ssm3", "ssm4"), years = NULL) {
  
  require(dplyr)
  
  if (!is.null(years)) {
    true = param_summ %>% filter(param == p & year %in% years) %>% select(seed, stock, "value")
  } else {
    true = param_summ %>% filter(param == p) %>% select(seed, stock, "value")
  }
  colnames(true)[3] = "true"
  
  if (!is.null(years)) {
    ests = est_summ %>% filter(param == p & method %in% methods & year %in% years) %>% select(seed, stock, method, "X50.")
  } else {
    ests = est_summ %>% filter(param == p & method %in% methods) %>% select(seed, stock, method, "X2.5.", "X97.5.")
  }
  
  combined = merge(true, ests, by = c("seed", "stock"), all = F) %>%
    arrange(seed, stock, method)
  
  combined$cov = sapply(1:nrow(combined), function(i) between(combined$true[i], combined$X2.5.[i], combined$X97.5.[i]))
  
  out = tapply(combined$cov, combined$method, mean)
  if (length(out) == 4) {
    c("lm" = NA, "lme" = NA, out)
  } else {
    out
  }
}

# quantities to calculate coverage for
p = c("alpha", "beta", "U_msy", "S_msy", "sigma_R", "phi", "pi", "mean_rho",
      "Sstar_0.1", "Sstar_0.3", "Sstar_0.5", "S_MSY", 
      "Ustar_0.1", "Ustar_0.3", "Ustar_0.5", "U_MSY",
      "U", "R", "S", "H")

# this takes a while
coverage = t(sapply(p, get_coverage)); round(coverage, 2) * 100

# save the output
write.csv(coverage, "tabs/coverage_table.csv", row.names = T)
