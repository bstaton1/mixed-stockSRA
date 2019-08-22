
# THIS SCRIPT SUMMARIZES THE MCMC CONVERGENCE DIAGNOSTICS

# PREPARE THE OUTPUT
source("0-prep-output.R"); ls()

# keep = sample(1:160, 10, replace = F)
# param_summ = param_summ[param_summ$seed %in% keep,]
# est_summ = est_summ[est_summ$seed %in% keep,]

# function to replace numbers that would round to zero with <1
format = function(x) {
  ifelse(!is.na(x) & x > 0 & x < 0.01, "<1", round(x * 100))
}

p = c("alpha", "beta", "U_msy", "S_msy", "sigma_R", "phi", "pi", "mean_rho",
      "R", "U")

param_summ = param_summ[param_summ$param %in% p,]
est_summ = est_summ[est_summ$param %in% p,]

param_summ$param = factor(param_summ$param, levels = c("alpha", "beta", "U_msy", "S_msy", "sigma_R", "phi", "pi", "mean_rho", "R", "U"))
est_summ$param = factor(est_summ$param, levels = c("alpha", "beta", "U_msy", "S_msy", "sigma_R", "phi", "pi", "mean_rho", "R", "U"))
est_summ$method = factor(est_summ$method)

counts_bad_bgr = table(est_summ[est_summ$bgr > 1.1,c("param", "method")])
counts_bad_ess = table(est_summ[est_summ$ess < 3000,c("param", "method")])
counts_all = table(est_summ[,c("param", "method")])

out_bgr = counts_bad_bgr/counts_all
out_bgr = apply(out_bgr, 2, format)
out_ess = counts_bad_ess/counts_all
out_ess = apply(out_ess, 2, format)

# these weren't estimated parameters so don't require diagnostics
out_bgr[c("U_msy", "S_msy"), c("lm", "lme")] = "NaN"
out_bgr[c("alpha", "beta"), c("ssm1", "ssm2", "ssm3", "ssm4")] = "NaN"

# these weren't estimated parameters so don't require diagnostics
out_ess[c("U_msy", "S_msy"), c("lm", "lme")] = "NaN"
out_ess[c("alpha", "beta"), c("ssm1", "ssm2", "ssm3", "ssm4")] = "NaN"

# this is the % of all parameters that did not meet the desired criteria (<1.1 for bgr and >3000 for ESS)
out = cbind(out_bgr, out_ess); out
