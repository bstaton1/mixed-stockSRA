
##### SESSION INITIALIZATON #####
# clear the workspace
rm(list = ls(all = T))

# accept command line argument specifying which model to run (see 1_Run_Analysis.sh)
# this script runs only one model at a time
args = commandArgs(trailingOnly = T)
model = as.numeric(args[1])      # model
# model = 1 # or set it manually
model = ifelse(model == 0, "lm", model)

# other settings
short = T     # run the model with VERY short MCMC chains?
alt_vuln = F  # use the alternative vulnerability vector?
alt_aESS = F  # use the alternative effective sample size for age composition?

# create the output directory if it doesn't already exist
out_dir = "outputs"
if(!dir.exists(out_dir)) {
  dir.create(out_dir)
  warning(paste("No directory named '", out_dir, "'. One was created.", sep = ""))
} 

# location of data files
in_dir = "inputs"

# determine the vulnerability vector to use
if (alt_vuln) {
  vuln_use = read.csv(file.path(in_dir, "vuln.csv"))
  vuln_use = vuln_use[order(vuln_use$stock),"v"]
} else {
  vuln_use = rep(1, 13)
}

# determine the age effective sample size to use
if (alt_aESS) aESS_use = 100 else aESS_use = NULL

# prepare the kuskokwim data files
kusko_inputs = FitSR::kusko_prep(
  # escapement data by population and year
  S_dat = read.csv(
    stringsAsFactors = F, 
    file = file.path(in_dir, "S_Ests_Oct_18.csv")
  ),
  
  # aggregate harvest data by year
  H_dat = read.csv(
    stringsAsFactors = F, 
    file = file.path(in_dir, "H_Ests_Oct_18.csv")
  ),
  
  # age composition data by population and year
  age_dat = read.csv(
    stringsAsFactors = F,
    file = file.path(in_dir, "Age_Comp_Data.csv")
  ),
  
  # set the vulnerability scenario
  v = vuln_use,
  
  # set the effective sample size scenario
  ESSmax = aESS_use
)

# separate them into observed and params (params are pretty much all dimensions)
kusko_obs = kusko_inputs$obs
kusko_params = kusko_inputs$params

# create broodtables and get brood year recruitment by stock
kusko_obs = SimSR::gen_Rys_obs(params = kusko_params, obs = kusko_obs)

# create model files
if (model == "lm") model_file = FitSR::write_lme_file(dir_name = "models")
if (model == 1)    model_file = FitSR::write_ssm_1_file(dir_name = "models")
if (model == 2)    model_file = FitSR::write_ssm_2_file(dir_name = "models")
if (model == 3)    model_file = FitSR::write_ssm_3_file(dir_name = "models")
if (model == 4)    model_file = FitSR::write_ssm_4_file(dir_name = "models")

# create filenames for the the output
prefix = ifelse(model == "lm", "lm", "ssm")
mod_id = ifelse(model == "lm", "", model)
summ_file = paste(paste(prefix, mod_id, "summ", sep = "_"), ".rds", sep = "")
post_file = paste(paste(prefix, mod_id, "post", sep = "_"), ".rds", sep = "")

# ssm features by model
maturity = c("simple", "simple", "complex", "complex")
covariance = c("simple", "complex", "simple", "complex")

##### PREPARE DATA FOR JAGS #####
if (model == "lm") {
  # main formatting
  jags_dat = FitSR::lm_data_prep(kusko_params, kusko_obs)
  
  # discard populations that have fewer than 3 SR pairs
  keep_stocks = unique(jags_dat$stock)[table(jags_dat$stock) > 2]
  jags_dat$ns = length(keep_stocks)
  jags_dat$obs_log_RPS_lm = jags_dat$obs_log_RPS_lm[jags_dat$stock %in% keep_stocks]
  jags_dat$obs_log_RPS_lme = jags_dat$obs_log_RPS_lme[jags_dat$stock %in% keep_stocks]
  jags_dat$S_obs = jags_dat$S_obs[jags_dat$stock %in% keep_stocks]
  jags_dat$stock = as.numeric(as.factor(jags_dat$stock[jags_dat$stock %in% keep_stocks]))
  jags_dat$nobs = length(jags_dat$S_obs)
} else {
  jags_dat = FitSR::ssm_data_prep(kusko_params, kusko_obs, covariance = covariance[model])
}

##### PARAMETERS TO MONITOR #####

if (model == "lm") {
  jags_params = c("alpha_lme", "alpha_lm", "beta_lme", "beta_lm", "sig_fit_lm", "sig_fit_lme")
} else {
  
  jags_params = c(
    "alpha", "beta", "U_msy", "S_msy", "U", "pi", "R_eq",
    "sigma_R", "rho_mat", "phi",  "S",
    "R", "log_resid", "Sigma_R", "q", "C_tot"
  )
  
  # append if running a model that uses complex maturity
  if (maturity[model] == "complex") {
    jags_params = c(jags_params, c("p", "D_sum"))
  }
}

##### MCMC DIMENSIONS #####
if (short) { # if doing a trial run
  if (model %in% 1:4) { # if a ssm
    dims = c(ni = 50, nb = 10, nt = 1, nc = 3, na = 10)
  } else { # if an lm
    dims = c(ni = 500, nb = 1000, nt = 2, nc = 3, na = 100)
  }
} else { # if not doing a trial run
  if (model %in% 1:4) {  # if a ssm
    dims = c(ni = 800000, nb = 50000, nt = 400, nc = 10, na = 5000)
  } else {   # if a lm
    dims = c(ni = 100000, nb = 20000, nt = 50,  nc = 10, na = 5000)
  }
}

# with(as.list(dims), nc * ni/nt)

##### CREATE INITIAL VALUES #####

if (model == "lm") {
  # no inits necessary for fitting regressions
  jags_inits = NULL
} else {
  jags_inits = FitSR::gen_ssm_inits(params = kusko_params, obs = kusko_obs, maturity = maturity[model], n_chains = dims["nc"])
}

##### RUN JAGS #####
cat("|**----------**| Running Model #", model, " |**----------**|", "\n", sep = "")

start = Sys.time()
# run the sampler
post = jagsUI::jags.basic(
  data = jags_dat,
  inits = jags_inits,
  parameters.to.save = jags_params,
  model.file = model_file,
  n.chains = dims["nc"],
  n.adapt = dims["na"],
  n.iter = sum(dims[c("ni", "nb")]),
  n.burnin = dims["nb"],
  n.thin = dims["nt"],
  parallel = F,
  verbose = T,
  save.model = F
)

##### SUMMARIZE AND SAVE OUTPUT #####
# summarize the key output parameters
if (model == "lm") {
  summ = FitSR::lme_summary(post = post, params = kusko_params, diag_plots = T, plot_dir = out_dir)
} else {
  summ = FitSR::ssm_summary(
    post = post, model = model, params = kusko_params,
    diag_plots = T, plot_dir = out_dir)
}
stop = Sys.time()

# save the output
saveRDS(post, file = file.path(out_dir, post_file))
saveRDS(summ, file = file.path(out_dir, summ_file))

stop - start
