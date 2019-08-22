# load Staton's packages
library(StatonMisc)
library(SimSR)

# clear the workspace
rm(list = ls(all = T))

# read in utility functions
source("0a_utils.R")

# read in command line arguments
args = commandArgs(trailingOnly = T)
seed = as.numeric(args[1])      # seed for this run
if (is.na(seed)) seed = round(runif(1, 1, 10000))
# seed = 1
# directory for inputs to be stored (the output of this script)
in_folder = "inputs"
if (!dir.exists(in_folder)) dir.create(in_folder)

# set the random seed
set.seed(seed)     

# decide how to generate the leading population parameters
gen_params = "random"    # use random posterior samples for U_msy and S_msy
# gen_params = "median"  # use the posterior median for U_msy and S_msy

# make a plot for these simulated data?
do_plot = T

# read in the posterior samples from a Kusko fit
kusko_ests = readRDS("Kusko_U_S_msy.rds")
U_msy_ests = kusko_ests[,stringr::str_detect(colnames(kusko_ests), "U")]
S_msy_ests = kusko_ests[,stringr::str_detect(colnames(kusko_ests), "S")]

# determine which U_msy and S_msy values to use
if (gen_params == "random") {
  i = sample(1:nrow(kusko_ests), 1)
  Umsy = unname(U_msy_ests[i,])
  Smsy = unname(S_msy_ests[i,])
}

if (gen_params == "median") {
  Umsy = unname(apply(U_msy_ests, 2, median))
  Smsy = unname(apply(S_msy_ests, 2, median))
}

# step 1: generate random parameters
params = init_sim(
  # harvest policy
  U_SUM = 100,
  
  # number of years to simulate
  nt = 42,
  
  # population dynamics parameters
  U_msy = Umsy,
  S_msy = Smsy,
  phi = 0.3,
  
  # settings with essentially no observation error
  # min_S_cv = 0.00001,
  # max_S_cv = 0.00002,
  # min_C_cv = 0.00001,
  # max_C_cv = 0.00002,
  # x_ESS = 1e6
  
  # settings with moderate amount of observation error
  min_S_cv = 0.1,
  max_S_cv = 0.2,
  min_C_cv = 0.1,
  max_C_cv = 0.2,
  x_ESS = 100
)

# step 2: simulate population model to generate true states
true = ricker_sim(params, nb = 50, U_strategy = "Ustar_0.3")

# step 3a: generate observed calendar year states if sampled every year
obs = obs_sim(params = params, true = true)

# step 3b: impose a sampling frequency scheme
# currently mimics the kuskokwim sampling scheme
obs = obs_filter(
  params = params, 
  obs = obs, 
  mimic = read.csv("Kusko_obs_freq.csv"), 
  minSRobs = 3
)

# step 3c: obtain observed brood year states
obs = gen_Rys_obs(params, obs)

# step 4: obtain summaries and save output
input_summ = input_summary(params = params, obs = obs, true = true, seed = seed)
write.csv(
  input_summ, 
  paste(in_folder, fileName("input_summary", seed, ".csv"), sep = "/"),
  row.names = F
)

out_list = list(
  params = params,
  obs = obs,
  true = true,
  seed = seed
)

saveRDS(out_list, file.path(in_folder, fileName("inputs", seed, ".rds")))

if (do_plot) {
  pdf(file.path(in_folder, fileName("true_plot", seed, ".pdf")), h = 8, w = 10)
  true_plot(params = params, true = true, ext_device = F, type = "SRA")
  true_plot(params = params, true = true, ext_device = F, type = "spawners")
  junk = dev.off(); rm(junk)
}

cat("\r", "Inputs created and saved for seed ", seed, sep = "")
