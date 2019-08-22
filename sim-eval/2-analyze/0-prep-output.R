
# THIS SCRIPT READS IN ALL OF THE SUMMARIZED OUTPUT FROM EACH MODEL FITTED
# TO EACH RANDOM DATA SET, AND FORMATS IT INTO OBJECTS USED BY EACH OF THE 
# ANALYSIS SCRIPTS (I.E., THOSE THAT MAKE SPECIFIC FIGURES)

# clear the workspace
rm(list = ls(all = T))

# location of the output files
# these files were not uploaded to the repo. 
# they are available upon request from the authors.
output_dir = "C:/Users/bstaton/Desktop/Staton/2_kusko/analyses/z-output-storage/multi-sra/sim-fit-outputs/outputs/"

# read in regression model output and split into two dfs
lme_summ = readRDS(file.path(output_dir, "lme_summ.rds"))
lm_summ = lme_summ[lme_summ$method == "lm",]
lme_summ = lme_summ[lme_summ$method == "lme",]

# read in ssm model outputs
ssm_1_summ = readRDS(file.path(output_dir, "ssm_1_summ.rds"))
ssm_2_summ = readRDS(file.path(output_dir, "ssm_2_summ.rds"))
ssm_3_summ = readRDS(file.path(output_dir, "ssm_3_summ.rds"))
ssm_4_summ = readRDS(file.path(output_dir, "ssm_4_summ.rds"))

# read in the true parameter/states summaries
param_summ = readRDS(file.path(output_dir, "param_summ.rds"))

# add in phi to the param df - it wasn't included before. This is the true value of each iteration
phi_df = data.frame(seed = 1:160, stock = NA, year = NA, param = "phi", value = 0.3, iter = 1:160)
param_summ = rbind(param_summ, phi_df)

# create one big summary object
lm_summ$year = NA; lme_summ$year = NA
lm_summ = lm_summ[,colnames(ssm_1_summ)]
lme_summ = lme_summ[,colnames(ssm_1_summ)]
est_summ = rbind(lm_summ, lme_summ, ssm_1_summ, ssm_2_summ, ssm_3_summ, ssm_4_summ)
est_summ$param[est_summ$param == "C_tot"] = "H"

# clear out all unnecessary objects
rm(list = setdiff(ls(), c("param_summ", "est_summ")))
