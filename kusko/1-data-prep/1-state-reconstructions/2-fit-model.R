###################################################################################################
####### GENERAL SCRIPT FOR PRODUCING SPAWNER/HARVEST ESTIMATES FROM EACH STOCK IN ANALYSIS ########
###################################################################################################

# THIS SCRIPT TAKES STOCK SPECIFIC HARVEST RATES AND RECONSTRUCTS TOTAL ESCAPEMENT AND HARVEST FROM EACH STOCK
# Code writen by B. Staton Dec. 2016
# Updated by B. Connors Mar 2017
# Last updated by B. Staton Oct. 2018

# this verion doesn't attempt to create stock-specific harvests,
# as they aren't needed by the state-space model anyway.
# Instead, it estimates the total harvest accounted for by 
# stocks in this analysis by estimating the fraction of total 
# drainage-wide escapement accounted for, then uses that fraction of the total harvest

##### STEP 0: SESSION SETUP #####
# clear the workspace
rm(list = ls(all = T))

# packages
suppressWarnings(suppressMessages(library(jagsUI, verbose = F, quietly = T)))

# create a directory for output if it doesn't exist
if (!dir.exists("outputs")) dir.create("outputs")

# STEPS 1-3: PREPARE DATA
source("1-data-prep.R")

##### STEP 4: SPECIFY JAGS MODEL #####
mod = function() {
  
  ### ESTIMATE PARAMETERS FOR PAIRED SYSTEMS ###
  # priors for observability correction
  b0 ~ dnorm(0, 0.001)
  b1 ~ dnorm(0, 0.001)
  
  # priors for spatial correction on each system
    # pi is the probability a tag was in the uncounted reach
    # psi is the odds of this
  for (s in 1:np) {
    pi_p[s] ~ dbeta(1,1)
    psi_p[s] <- pi_p[s]/(1 - pi_p[s])
  }
  
  # likelihood on tag sampling
  for (i in 1:n_obs_tags_p) {
    tags_u_p[i] ~ dbin(pi_p[stock_tags_p[i]], tags_n_p[i])
  }

  # residual error in observability correction
  tau_fit ~ dgamma(0.001, 0.001)
  sig_fit <- 1/sqrt(tau_fit)

  # likelihood on temporal expansion
  for (i in 1:n_obs_fish_p) {
    weir_fish_p[i] ~ dnorm(pred_weir_fish_p[i], tau_fit)
    pred_weir_fish_p[i] <- b0 + b1 * A_hat_p[i]
    A_hat_p[i] <- air_fish_p[i] * (1 + psi_p[stock_fish_p[i]])
  }

  # create a fitted line variable
  for (i in 1:n_pred) {
    pred_y[i] <- b0 + b1 * pred_x[i]
  }
  
  ### EXPAND UNPAIRED SYSTEMS ###
  for (s in 1:nup) {
    pi_up[s] ~ dbeta(1,1)
    psi_up[s] <- pi_up[s]/(1 - pi_up[s])
  }
  
  # likelihood on tag sampling
  for (i in 1:n_obs_tags_up) {
    tags_u_up[i] ~ dbin(pi_up[stock_tags_up[i]], tags_n_up[i])
  }

  # perform spatial and observability expansions
  for (s in 1:nup) {
    for (y in 1:nyrs) {
      # observability correction
      S_tot[y,s] <- b0 + b1 * A_hat_up[y,s]

      # spatial correction
      A_hat_up[y,s] <- air_fish_up[y,s] * (1 + psi_up[s])
    }
  }
  
  ### ESTIMATE TOTAL HARVEST FROM THESE STOCKS ###
  for (y in 1:nyrs) {
    # obtain total escapement accounted for
    S_acct[y] <- sum(S_tot[y,]) + weir_tot[y]

    # obtain fraction of drainage-wide escapement included in this analysis
    p_acct[y] <- S_acct[y]/dw_S[y]

    # obtain harvest from stocks in this analysis
    H_acct[y] <- dw_H[y] * p_acct[y]
    
  }
}

model_file = "model.txt"
R2OpenBUGS::write.model(mod, model_file)

##### STEP 5: JAGS SPECIFICATIONS #####

### STEP 5A: PARAMETERS (NODES) TO MONITOR ###
params = c(
  "b0", "b1", "sig_fit", "A_hat_p", "pred_y",
  "pi_p", "psi_p", "pi_up", "psi_up",
  "S_tot", "p_acct", "H_acct", "S_acct"
  )

### STEP 5B: MCMC DIMENSIONS ###
ni = 10000  # number of post-burnin samples
nb = 5000   # number of burnin samples
nt = 2      # thinning interval
nc = 2       # number of chains

##### STEP 6: RUN THE SAMPLER #####
post = jagsUI::jags.basic(
  data = jags.dat,
  inits = NULL,
  parameters.to.save = params,
  model.file = model_file,
  n.chains = nc,
  n.adapt = 1000,
  n.iter = ni,
  n.burnin = nb,
  n.thin = nt,
  parallel = F,
  verbose = F,
  save.model = F
)

##### STEP 7: SAVE THE SAMPLES #####
saveRDS(post, file = "outputs/post.rds")
