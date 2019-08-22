###################################################################
###### FIT A SPAWNER-RECRUIT MODEL TO A HYPOTHETICAL DATA SET #####
###################################################################

# CLEAR THE WORKSPACE
rm(list = ls(all = T))

# handle command line arguments
args = commandArgs(trailingOnly = T)
# args = c(4, 7) # test out model four, seed seven
model = as.numeric(args[1])     # model for this run
seed = as.numeric(args[2])      # seed for this run

# directories
out_folder = "outputs"
in_folder = "inputs"

# read in utility functions
source("0a_utils.R")

# options
write = T        # write output folders and files?
time_verbose = T # print progress messages from the time on each step?
diag_plots = F   # create mcmc diag plots?
parallel = T     # run jags in parallel?
short = T        # do a really short run to test this out?
error = F        # do you wish to allow JAGS to crash and return error message? 
                   # TRUE turns off a tryCatch() call.

# read in the appropriate input file for this model run
in_file = fileName(file.path(in_folder, "inputs"), seed, ".rds")

if (file.exists(in_file)) {
  inputs = readRDS(in_file)
  params = inputs$params
  obs = inputs$obs
  true = inputs$true
} else {
  stop (paste("Input file:", in_file, "does not exist!"))
}

# models to fit
do_lm   = ifelse(model == 0, T, F)
do_ssm1 = ifelse(model == 1, T, F)
do_ssm2 = ifelse(model == 2, T, F)
do_ssm3 = ifelse(model == 3, T, F)
do_ssm4 = ifelse(model == 4, T, F)

# OUTPUT DIRECTORY
out_dir = file.path(getwd(), out_folder)

# create the output folder if it does not exist already
if(!dir.exists(out_dir) & write) dir.create(out_dir)

# random sleeping ranges: in seconds
minS = 5
maxS = ifelse(short, 10, 200)

# mcmc dimensions
if (short) {
  lm_dims   = c(ni = 500, nb = 1000, nt = 2, nc = 3, na = 100)
  ssm1_dims = c(ni = 50, nb = 10, nt = 1, nc = 3, na = 10)
  ssm2_dims = c(ni = 50, nb = 10, nt = 1, nc = 3, na = 10)
  ssm3_dims = c(ni = 50, nb = 10, nt = 1, nc = 3, na = 10)
  ssm4_dims = c(ni = 50, nb = 10, nt = 1, nc = 3, na = 10)
} else {
  lm_dims   = c(ni = 300000, nb = 20000, nt = 50,  nc = 5, na = 5000)
  ssm1_dims = c(ni = 600000, nb = 75000, nt = 100, nc = 5, na = 5000)
  ssm2_dims = c(ni = 600000, nb = 75000, nt = 100, nc = 5, na = 5000)
  ssm3_dims = c(ni = 600000, nb = 75000, nt = 100, nc = 5, na = 5000)
  ssm4_dims = c(ni = 600000, nb = 75000, nt = 100, nc = 5, na = 5000)
}

# parameters to monitor in all SSMs. will add D_sum for mods 3 and 4.
ssm_jags_params = c(
  "alpha", "beta", "U_msy", "S_msy", "U",
  "sigma_R", "rho_mat", "phi", "pi", "C_tot", "S", "R"
)

cat("------------------------------------------------------------\n")
ctime = 0

##### FIT LM/LME MODEL(S) #####
if (do_lm) {
  cat(  "|**-------**| Fitting Linear Regression Models |**-------**|\n")
  # save the model file
  model_file = FitSR::write_lme_file(dir_name = "model-files")
  
  # step 5a: fit the lme/lm models
  random_sleep(minS = minS, maxS = maxS)
  start = Sys.time()
  lme_post = fit_model(
    jags_data = FitSR::lm_data_prep(params, obs),
    jags_inits = NULL,
    jags_params = c("alpha_lm", "beta_lm", "alpha_lme", "beta_lme"),
    model_file = model_file, 
    dims = lm_dims, parallel = parallel, error = error
  ); ctime = end_timer(start, ctime = ctime)
  
  # step 5b: summarize and export the estimates from the lme/lm models
  start = Sys.time()
  lme_summ = FitSR::lme_summary(
    post = lme_post, params = params, seed = seed,
    plot_dir = out_folder, diag_plots = diag_plots
  )
  if (write) write.csv(
    lme_summ, 
    paste(out_dir, fileName("lme_summary", seed, ".csv"), sep = "/"),
    row.names = F
  ); ctime = end_timer(start, ctime = ctime)
  cat("------------------------------------------------------------\n")
}

##### FIT SSM 1 #####
if (do_ssm1) {
  cat(  "|**-------**| Fitting State-Space Model #1 |**-------**|\n")
  
  # save the model file
  model_file = FitSR::write_ssm_1_file(dir_name = "model-files")
  cov = "simple"; mat = "simple"
  
  # step 5a: fit the lme/lm models
  random_sleep(minS = minS, maxS = maxS)
  start = Sys.time()
  ssm1_post = fit_model(
    jags_data = FitSR::ssm_data_prep(params, obs, covariance = cov),
    jags_inits = FitSR::gen_ssm_inits(params, obs, mat, ssm1_dims["nc"]),
    jags_params = ssm_jags_params,
    model_file = model_file, 
    dims = ssm1_dims, parallel = parallel, error = error
  ); ctime = end_timer(start, ctime = ctime)
  
  # step 6b: summarize and export the estimates from the tsm model
  start = Sys.time()
  ssm1_summ = FitSR::ssm_summary(
    post = ssm1_post, params = params,
    model = 1,
    diag_plots = diag_plots, plot_dir = out_folder, seed = seed
    )
  if (write) write.csv(
    ssm1_summ, 
    paste(out_dir, fileName("ssm_1_summary", seed, ".csv"), sep = "/"),
    row.names = F
  ); ctime = end_timer(start, ctime = ctime)
  cat("------------------------------------------------------------\n")
}

##### FIT SSM 2 #####
if (do_ssm2) {
  cat(  "|**-------**| Fitting State-Space Model #2 |**-------**|\n")
  # save the model file
  model_file = FitSR::write_ssm_2_file(dir_name = "model-files")
  cov = "complex"; mat = "simple"
  
  # step 5a: fit the lme/lm models
  random_sleep(minS = minS, maxS = maxS)
  start = Sys.time()
  ssm2_post = fit_model(
    jags_data = FitSR::ssm_data_prep(params, obs, covariance = cov),
    jags_inits = FitSR::gen_ssm_inits(params, obs, mat, ssm2_dims["nc"]),
    jags_params = ssm_jags_params,
    model_file = model_file, 
    dims = ssm2_dims, parallel = parallel, error = error
  ); ctime = end_timer(start, ctime = ctime)
  
  # step 6b: summarize and export the estimates from the tsm model
  start = Sys.time()
  ssm2_summ = FitSR::ssm_summary(
    post = ssm2_post, params = params,
    model = 2,
    diag_plots = diag_plots, plot_dir = out_folder, seed = seed
  )
  if (write) write.csv(
    ssm2_summ, 
    paste(out_dir, fileName("ssm_2_summary", seed, ".csv"), sep = "/"),
    row.names = F
  ); ctime = end_timer(start, ctime = ctime)
  cat("------------------------------------------------------------\n")
}

##### FIT SSM 3 #####
if (do_ssm3) {
  cat(  "|**-------**| Fitting State-Space Model #3 |**-------**|\n")
  
  # save the model file
  model_file = FitSR::write_ssm_3_file(dir_name = "model-files")
  cov = "simple"; mat = "complex"
  
  # step 5a: fit the lme/lm models
  random_sleep(minS = minS, maxS = maxS)
  start = Sys.time()
  ssm3_post = fit_model(
    jags_data = FitSR::ssm_data_prep(params, obs, covariance = cov),
    jags_inits = FitSR::gen_ssm_inits(params, obs, mat, ssm3_dims["nc"]),
    jags_params = c(ssm_jags_params, "D_sum"),
    model_file = model_file, 
    dims = ssm3_dims, parallel = parallel, error = error
  ); ctime = end_timer(start, ctime = ctime)
  
  # step 6b: summarize and export the estimates from the tsm model
  start = Sys.time()
  ssm3_summ = FitSR::ssm_summary(
    post = ssm3_post, params = params,
    model = 3,
    diag_plots = diag_plots, plot_dir = out_folder, seed = seed
  )
  if (write) write.csv(
    ssm3_summ, 
    paste(out_dir, fileName("ssm_3_summary", seed, ".csv"), sep = "/"),
    row.names = F
  ); ctime = end_timer(start, ctime = ctime)
  cat("------------------------------------------------------------\n")
}

##### FIT SSM 4 #####
if (do_ssm4) {
  cat(  "|**-------**| Fitting State-Space Model #4 |**-------**|\n")
  # save the model file
  model_file = FitSR::write_ssm_4_file(dir_name = "model-files")
  cov = "complex"; mat = "complex"
  
  # step 5a: fit the lme/lm models
  random_sleep(minS = minS, maxS = maxS)
  start = Sys.time()
  ssm4_post = fit_model(
    jags_data = FitSR::ssm_data_prep(params, obs, covariance = cov),
    jags_inits = FitSR::gen_ssm_inits(params, obs, mat, ssm4_dims["nc"]),
    jags_params = c(ssm_jags_params, "D_sum"),
    model_file = model_file, 
    dims = ssm4_dims, parallel = parallel, error = error
  ); ctime = end_timer(start, ctime = ctime)
  
  # step 6b: summarize and export the estimates from the tsm model
  start = Sys.time()
  ssm4_summ = FitSR::ssm_summary(
    post = ssm4_post, params = params,
    model = 4,
    diag_plots = diag_plots, plot_dir = out_folder, seed = seed
  )
  if (write) write.csv(
    ssm4_summ, 
    paste(out_dir, fileName("ssm_4_summary", seed, ".csv"), sep = "/"),
    row.names = F
  ); ctime = end_timer(start, ctime = ctime)
  cat("------------------------------------------------------------\n")
}
