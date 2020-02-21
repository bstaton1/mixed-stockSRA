
# THIS SCRIPT CREATES THE OUTPUT FIGURES/TABLES FOR THE MAIN TEXT
# REGARDING THE ANALYSIS OF STATE-SPACE MODELS WITH KUSKOKWIM DATA

##### SESSION SET UP #####
rm(list = ls(all = T))

# resolution for figures (pixels per inch)
ppi = 600

# Staton packages
# running devtools::install_github("bstaton1/FitSR") will install all of these packages
library(FitSR)
library(SimSR)
library(postpack)
library(StatonMisc)

# set working directory
setwd("location/of/this/file")

# directories and file names for posterior samples
# these files are FAR too large to place in a Git repo
# contact the author if you wish to run this code, or run the model yourself to obtain these output files

post_dir = "location/of/posterior/samples"
ssm_post_dir = file.path(post_dir, "ssm_post_base")
lm_post_dir = file.path(post_dir, "lm_post")
postlm_file = "lm__post.rds"
post1_file = "ssm_1_post.rds"
post2_file = "ssm_2_post.rds"
post3_file = "ssm_3_post.rds"
post4_file = "ssm_4_post.rds"

# output directories
out_dir = "location/to/dump/output"
fig_dir = file.path(out_dir, "figs"); if (!dir.exists(fig_dir)) dir.create(fig_dir)

# load posterior samples
postlm = readRDS(file = file.path(lm_post_dir, postlm_file))
post1 = readRDS(file = file.path(ssm_post_dir, post1_file))
post2 = readRDS(file = file.path(ssm_post_dir, post2_file))
post3 = readRDS(file = file.path(ssm_post_dir, post3_file))
post4 = readRDS(file = file.path(ssm_post_dir, post4_file))

##### LOAD/FORMAT DATA #####
in_dir = "../2-model-fit/inputs"

kusko_inputs = kusko_prep(
  # escapement data
  S_dat = read.csv(
    stringsAsFactors = F, 
    file = file.path(in_dir, "S_Ests_Oct_18.csv")
  ),
  
  # harvest data
  H_dat = read.csv(
    stringsAsFactors = F, 
    file = file.path(in_dir, "H_Ests_Oct_18.csv")
  ),
  
  # age data
  age_dat = read.csv(
    stringsAsFactors = F,
    file = file.path(in_dir, "Age_Comp_Data.csv")
  )
)

obs = kusko_inputs$obs
params = kusko_inputs$params
attach(params)

# create broodtables and get brood year recruitment by stock
obs = gen_Rys_obs(params = params, obs = obs)

# create data sets used in regression fitting 
lm_dat = lm_data_prep(params, obs)
lm_stocks = as.numeric(names(which(table(lm_dat$stock) >= 3)))

# number of posterior samples saved for each model
n_save = unname(post_dim(post1)["saved"])

##### ESTIMATE PLOT: PREP #####

# all = all populations
# red = reduced populations; only those fittable with regression

# extract posterior samples of alpha from all models (all and red)
alm = post_subset(postlm, "alpha_lm[", matrix = T)
alme = post_subset(postlm, "alpha_lme", matrix = T)
a1_all = post_subset(post1, "alpha", matrix = T); a1_red = a1_all[,lm_stocks]
a2_all = post_subset(post2, "alpha", matrix = T); a2_red = a2_all[,lm_stocks]
a3_all = post_subset(post3, "alpha", matrix = T); a3_red = a3_all[,lm_stocks]
a4_all = post_subset(post4, "alpha", matrix = T); a4_red = a4_all[,lm_stocks]

# extract posterior samples of beta from all models (all and red)
blm = post_subset(postlm, "beta_lm[", matrix = T)
blme = post_subset(postlm, "beta_lme", matrix = T)
b1_all = post_subset(post1, "beta", matrix = T); b1_red = b1_all[,lm_stocks]
b2_all = post_subset(post2, "beta", matrix = T); b2_red = b2_all[,lm_stocks]
b3_all = post_subset(post3, "beta", matrix = T); b3_red = b3_all[,lm_stocks]
b4_all = post_subset(post4, "beta", matrix = T); b4_red = b4_all[,lm_stocks]

# extract posterior samples of Sigma_R from all models (all and red)
siglm = post_subset(postlm, "sig_fit_lm[", matrix = T)
siglme = matrix(post_subset(postlm, "sig_fit_lme", matrix = T), nrow = nrow(siglm), ncol = ncol(siglm))
sig1_all = post_subset(post1, "sigma_R", matrix = T); sig1_red = sig1_all[,lm_stocks]
sig2_all = post_subset(post2, "sigma_R", matrix = T); sig2_red = sig2_all[,lm_stocks]
sig3_all = post_subset(post3, "sigma_R", matrix = T); sig3_red = sig3_all[,lm_stocks]
sig4_all = post_subset(post4, "sigma_R", matrix = T); sig4_red = sig4_all[,lm_stocks]

# extract posterior samples of Smsy from ssms (regression ones must be approximated)
Smsy1_all = post_subset(post1, "S_msy", matrix = T); Smsy1_red = Smsy1_all[,lm_stocks]
Smsy2_all = post_subset(post2, "S_msy", matrix = T); Smsy2_red = Smsy2_all[,lm_stocks]
Smsy3_all = post_subset(post3, "S_msy", matrix = T); Smsy3_red = Smsy3_all[,lm_stocks]
Smsy4_all = post_subset(post4, "S_msy", matrix = T); Smsy4_red = Smsy4_all[,lm_stocks]

# extract posterior samples of Umsy from ssms (regression ones must be approximated)
Umsy1_all = post_subset(post1, "U_msy", matrix = T); Umsy1_red = Umsy1_all[,lm_stocks]
Umsy2_all = post_subset(post2, "U_msy", matrix = T); Umsy2_red = Umsy2_all[,lm_stocks]
Umsy3_all = post_subset(post3, "U_msy", matrix = T); Umsy3_red = Umsy3_all[,lm_stocks]
Umsy4_all = post_subset(post4, "U_msy", matrix = T); Umsy4_red = Umsy4_all[,lm_stocks]

# get Umsy and Smsy for regression models
Umsylm = Smsylm = Umsylme = Smsylme = matrix(NA, nrow(alm), ncol(alm))
for (i in 1:n_save) {
  cat("\r", paste(round(i/n_save, 2) * 100, "%", sep = ""))
  tmp = gen_lm_mgmt(alpha = alm[i,], beta = blm[i,])
  Umsylm[i,] = tmp$U_msy
  Smsylm[i,] = tmp$S_msy
  tmp = gen_lm_mgmt(alpha = alme[i,], beta = blme[i,])
  Umsylme[i,] = tmp$U_msy
  Smsylme[i,] = tmp$S_msy
}

# get UMSY and SMSY (for the mixed-stock) for all models (all and red for ssms)
# containers
S_MSYlm = U_MSYlm = S_MSYlme = U_MSYlme =
  S_MSY1_all = S_MSY1_red = U_MSY1_all = U_MSY1_red = 
  S_MSY2_all = S_MSY2_red = U_MSY2_all = U_MSY2_red = 
  S_MSY3_all = S_MSY3_red = U_MSY3_all = U_MSY3_red = 
  S_MSY4_all = S_MSY4_red = U_MSY4_all = U_MSY4_red = 
  numeric(n_save)

starttime = Sys.time()
for (i in 1:n_save) {
  cat("\r", paste(round(i/n_save, 3) * 100, "%", sep = ""))
  
  # simple regression
  tmp = gen_mgmt(params = list(alpha = alm[i,], beta = blm[i,], U_msy = Umsylm[i,], S_msy = Smsylm[i,]))
  S_MSYlm[i] = tmp$mgmt["S_MSY"]; U_MSYlm[i] = tmp$mgmt["U_MSY"]; rm(tmp)
  
  # mixed effect regression
  tmp = gen_mgmt(params = list(alpha = alme[i,], beta = blme[i,], U_msy = Umsylme[i,], S_msy = Smsylme[i,]))
  S_MSYlme[i] = tmp$mgmt["S_MSY"]; U_MSYlme[i] = tmp$mgmt["U_MSY"]; rm(tmp)
  
  # ssm1: all stocks
  tmp = gen_mgmt(params = list(alpha = a1_all[i,], beta = b1_all[i,], U_msy = Umsy1_all[i,], S_msy = Smsy1_all[i,]))
  S_MSY1_all[i] = tmp$mgmt["S_MSY"]; U_MSY1_all[i] = tmp$mgmt["U_MSY"]; rm(tmp)
  
  # ssm1: red stocks
  tmp = gen_mgmt(params = list(alpha = a1_red[i,], beta = b1_red[i,], U_msy = Umsy1_red[i,], S_msy = Smsy1_red[i,]))
  S_MSY1_red[i] = tmp$mgmt["S_MSY"]; U_MSY1_red[i] = tmp$mgmt["U_MSY"]; rm(tmp)
  
  # ssm2: all stocks
  tmp = gen_mgmt(params = list(alpha = a2_all[i,], beta = b2_all[i,], U_msy = Umsy2_all[i,], S_msy = Smsy2_all[i,]))
  S_MSY2_all[i] = tmp$mgmt["S_MSY"]; U_MSY2_all[i] = tmp$mgmt["U_MSY"]; rm(tmp)
  
  # ssm2: red stocks
  tmp = gen_mgmt(params = list(alpha = a2_red[i,], beta = b2_red[i,], U_msy = Umsy2_red[i,], S_msy = Smsy2_red[i,]))
  S_MSY2_red[i] = tmp$mgmt["S_MSY"]; U_MSY2_red[i] = tmp$mgmt["U_MSY"]; rm(tmp)
  
  # ssm3: all stocks
  tmp = gen_mgmt(params = list(alpha = a3_all[i,], beta = b3_all[i,], U_msy = Umsy3_all[i,], S_msy = Smsy3_all[i,]))
  S_MSY3_all[i] = tmp$mgmt["S_MSY"]; U_MSY3_all[i] = tmp$mgmt["U_MSY"]; rm(tmp)
  
  # ssm3: red stocks
  tmp = gen_mgmt(params = list(alpha = a3_red[i,], beta = b3_red[i,], U_msy = Umsy3_red[i,], S_msy = Smsy3_red[i,]))
  S_MSY3_red[i] = tmp$mgmt["S_MSY"]; U_MSY3_red[i] = tmp$mgmt["U_MSY"]; rm(tmp)
  
  # ssm4: all stocks
  tmp = gen_mgmt(params = list(alpha = a4_all[i,], beta = b4_all[i,], U_msy = Umsy4_all[i,], S_msy = Smsy4_all[i,]))
  S_MSY4_all[i] = tmp$mgmt["S_MSY"]; U_MSY4_all[i] = tmp$mgmt["U_MSY"]; rm(tmp)
  
  # ssm4: red stocks
  tmp = gen_mgmt(params = list(alpha = a4_red[i,], beta = b4_red[i,], U_msy = Umsy4_red[i,], S_msy = Smsy4_red[i,]))
  S_MSY4_red[i] = tmp$mgmt["S_MSY"]; U_MSY4_red[i] = tmp$mgmt["U_MSY"]; rm(tmp)
}
Sys.time() - starttime

keep = c("50%", "2.5%", "97.5%")

# summarize and create arrays

Umsy_red_summ = cbind(
  lm = summ(rowMeans(Umsylm))[keep],
  lme = summ(rowMeans(Umsylme))[keep],
  ssm1 = summ(rowMeans(Umsy1_red))[keep],
  ssm2 = summ(rowMeans(Umsy2_red))[keep],
  ssm3 = summ(rowMeans(Umsy3_red))[keep],
  ssm4 = summ(rowMeans(Umsy4_red))[keep]
)
UMSY_red_summ = cbind(
  lm = summ(U_MSYlm)[keep],
  lme = summ(U_MSYlme)[keep],
  ssm1 = summ(U_MSY1_red)[keep],
  ssm2 = summ(U_MSY2_red)[keep],
  ssm3 = summ(U_MSY3_red)[keep],
  ssm4 = summ(U_MSY4_red)[keep]
)
Smsy_red_summ = cbind(
  lm = summ(rowMeans(Smsylm))[keep],
  lme = summ(rowMeans(Smsylme))[keep],
  ssm1 = summ(rowMeans(Smsy1_red))[keep],
  ssm2 = summ(rowMeans(Smsy2_red))[keep],
  ssm3 = summ(rowMeans(Smsy3_red))[keep],
  ssm4 = summ(rowMeans(Smsy4_red))[keep]
)
SMSY_red_summ = cbind(
  lm = summ(S_MSYlm)[keep],
  lme = summ(S_MSYlme)[keep],
  ssm1 = summ(S_MSY1_red)[keep],
  ssm2 = summ(S_MSY2_red)[keep],
  ssm3 = summ(S_MSY3_red)[keep],
  ssm4 = summ(S_MSY4_red)[keep]
)
sig_red_summ = cbind(
  lm = summ(rowMeans(siglm))[keep],
  lme = summ(rowMeans(siglme))[keep],
  ssm1 = summ(rowMeans(sig1_red))[keep],
  ssm2 = summ(rowMeans(sig2_red))[keep],
  ssm3 = summ(rowMeans(sig3_red))[keep],
  ssm4 = summ(rowMeans(sig4_red))[keep]
)

red_summ = abind::abind(Umsy_red_summ, UMSY_red_summ, Smsy_red_summ, SMSY_red_summ, sig_red_summ, along = 3)
dimnames(red_summ)[[3]] = c("Umsy", "UMSY", "Smsy", "SMSY", "sig")

Umsy_all_summ = cbind(
  lm = NA,
  lme = NA,
  ssm1 = summ(rowMeans(Umsy1_all))[keep],
  ssm2 = summ(rowMeans(Umsy2_all))[keep],
  ssm3 = summ(rowMeans(Umsy3_all))[keep],
  ssm4 = summ(rowMeans(Umsy4_all))[keep]
)
UMSY_all_summ = cbind(
  lm = NA,
  lme = NA,
  ssm1 = summ(U_MSY1_all)[keep],
  ssm2 = summ(U_MSY2_all)[keep],
  ssm3 = summ(U_MSY3_all)[keep],
  ssm4 = summ(U_MSY4_all)[keep]
)
Smsy_all_summ = cbind(
  lm = NA,
  lme = NA,
  ssm1 = summ(rowMeans(Smsy1_all))[keep],
  ssm2 = summ(rowMeans(Smsy2_all))[keep],
  ssm3 = summ(rowMeans(Smsy3_all))[keep],
  ssm4 = summ(rowMeans(Smsy4_all))[keep]
)
SMSY_all_summ = cbind(
  lm = NA,
  lme = NA,
  ssm1 = summ(S_MSY1_all)[keep],
  ssm2 = summ(S_MSY2_all)[keep],
  ssm3 = summ(S_MSY3_all)[keep],
  ssm4 = summ(S_MSY4_all)[keep]
)
sig_all_summ = cbind(
  lm = NA,
  lme = NA,
  ssm1 = summ(rowMeans(sig1_all))[keep],
  ssm2 = summ(rowMeans(sig2_all))[keep],
  ssm3 = summ(rowMeans(sig3_all))[keep],
  ssm4 = summ(rowMeans(sig4_all))[keep]
)

all_summ = abind::abind(Umsy_all_summ, UMSY_all_summ, Smsy_all_summ, SMSY_all_summ, sig_all_summ, along = 3)
dimnames(all_summ)[[3]] = c("Umsy", "UMSY", "Smsy", "SMSY", "sig")

##### ESTIMATES PLOT: MAKE PLOT #####

# reformat output to plot
meds1 = t(red_summ["50%",,])
meds2 = t(all_summ["50%",,])
lwrs1 = t(red_summ["2.5%",,])
lwrs2 = t(all_summ["2.5%",,])
uprs1 = t(red_summ["97.5%",,])
uprs2 = t(all_summ["97.5%",,])

# attributes of lines and points
pch1 = c(24,24,21,21,21,21)
bg1 = c("white", "black", "black", "white", "black", "white")
bg2 = c("black", "white", "black", "white")
lty1 = c(1,1,1,1,2,2)
col1 = c(rep("black", 2), rep("black", 4))
pch2 = c(NA,NA,22,22,22,22)
lty2 = c(1,1,1,1,2,2)
col2 = c(NA,NA,rep("black", 4))
x1 = 1:6 + c(0, 0, rep(-0.22, 4))
x2 = 1:6 + c(0, 0, rep( 0.22, 4))
cex = 1.4

# xlimits for each plot
xlim1 = rep(0, 5)
xlim2 = c(1, 1, 8000, 95000, 1.5)

# the latex code to create nice labels
latex = c("$\\bar{U}_{MSY,j}$", "$U_{MSY}$", "$\\bar{S}_{MSY,j}$", "$S_{MSY}$", "$\\bar{\\sigma}_{R,j}$")

# the letters to identify each plot
lets = c("a", "b", "c", "d", "e")

# initiate a device
jpeg(file.path(fig_dir, "kusko-ests.jpg"), h = 7 * ppi, w = 2.5 * ppi, res = ppi)

# set the specs of that device
par(mfrow = c(5,1), mar = c(1,4,1,1), mgp = c(2,0.5,0), oma = c(2.5,0,0,0), cex.axis = 0.9)

# loop through parameters, drawing the plot for each
for (p in 1:5) {
  # empty plot with correct dimensions
  plot(x1 ~ meds1[p,],  type = "n", ylim = rev(range(c(x1, x2))) + c(0,-0.25),
       xlim = c(xlim1[p],xlim2[p]), ylab = "",
       yaxt = "n", cex = cex)
  usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
  
  # grey box to delineate regression models
  rect(usr[1], 2.5, usr[2], usr[4], col = "grey80", border = NA)
  abline(h = 2.5, col = "grey50")
  
  # error bars and points for the reduced scenario
  segments(lwrs1[p,], x1, uprs1[p,], x1, lty = lty1, col = col1)
  points(x1 ~ meds1[p,], pch = pch1, col = col1, bg = bg1, cex = cex)
  
  # error bars and points for the all scenario
  segments(lwrs2[p,], x2, uprs2[p,], x2, lty = lty2, col = col2)
  points(x2 ~ meds2[p,], pch = pch2, bg = bg2, cex = cex)
  
  # margin text
  text(x = usr[1] - xdiff * 0.2, y = usr[4] - ydiff * 0.5, latex2exp::TeX(latex[p]), xpd = T, cex = 1.5, srt = 90)
  axis(side = 2, at = 1:6, labels = c("LM", "LME", "vm", "Vm", "vM", "VM"), las = 1, tcl = -0.3, cex.axis = 1)
  
  # draw a legend on the first plot only
  if (p == 1) legend("bottomright", pch = c(16, 15), 
                     legend = c("10", 13), title = latex2exp::TeX("$n_j$"),
                     bty = "n", pt.cex = 1.4, col = "grey50")
  
  # add letters
  text(usr[1] - xdiff * 0.025, usr[3] + ydiff * 0.075, pos = 4, paste("(", lets[p], ")", sep = ""), font = 2, cex = 1.2)
  box()
}
mtext(side = 1, at = 0.55, outer = T, line = 1, "Estimate", cex = 0.9)
dev.off()

##### SRA PLOTS #####

# extract median alpha from each population and model
alm = post_summ(postlm, "alpha_lm[")[3,]
alme = post_summ(postlm, "alpha_lme[")[3,]
a1 = post_summ(post1, "alpha")[3,]
a2 = post_summ(post2, "alpha")[3,]
a3 = post_summ(post3, "alpha")[3,]
a4 = post_summ(post4, "alpha")[3,]

# extract median beta from each population and model
blm = post_summ(postlm, "beta_lm[")[3,]
blme = post_summ(postlm, "beta_lme[")[3,]
b1 = post_summ(post1, "beta")[3,]
b2 = post_summ(post2, "beta")[3,]
b3 = post_summ(post3, "beta")[3,]
b4 = post_summ(post4, "beta")[3,]

# simple function
ricker_pred = function(S, alpha, beta) {
  alpha * S * exp(-beta * S)
}

# indices of SR pairs as they are organized in data and model
S_ind = 1:(nt - a_max)
R_ind = (a_max + 1):(ny - na + 1)

# create the device
jpeg(file.path(fig_dir, "R-v-S.jpg"), h = 8 * ppi, w = 6.5 * ppi, res = ppi)

# set up plotting device
m = matrix(c(1:ns, rep(ns+1,2)), 5, 3, byrow = T)
layout(m)
par(mar = c(1.5,1.5,1.5,1), cex.main = 1.7, xaxs = "i", yaxs = "i", oma = c(3,3,0,0), tcl = -0.3, cex.axis = 1.3, mgp = c(2,0.5,0))

# point, line, and color types for the different models
pch = c(1,16,1,16,2,17)
lty = c(1,1,2,2,1,1)
cex = 1.8
col = c(rep("black", 4), rep("grey50", 2))
# loop through populations and make the ricker SRA plot for each one
for (s in 1:ns) {
  # determine xaxis limits and the values at which recruitment will be predicted
  minx = 0; maxx = max(obs$S_ts_obs[,s], na.rm = T); nx = 100
  predx = seq(minx, maxx, length = nx)
  predx2 = predx[seq(2, length(predx), 20)]
  ps = which(predx %in% predx2) # which ones will have a point associated as well as a line
  
  # if the popoulation was fitable with regression, obtain R curve, if not, skip
  if (s %in% lm_stocks) {
    i = which(lm_stocks == s)
    pRlm = ricker_pred(predx, alm[i], blm[i])
    pRlme = ricker_pred(predx, alme[i], blme[i])
  } else {
    pRlm = NA
    pRlme = NA
  }
  
  # ricker preds under ssms
  pR1 = ricker_pred(predx, a1[s], b1[s])
  pR2 = ricker_pred(predx, a2[s], b2[s])
  pR3 = ricker_pred(predx, a3[s], b3[s])
  pR4 = ricker_pred(predx, a4[s], b4[s])
  
  # determine yaxis limits: upper is the max of all ricker predictions and observed recruitment when spawners were also observed
  miny = 0; maxy = max(pR1, pR2, pR3, pR4, pRlm, pRlme, obs$R_ys_obs[R_ind,s][!is.na(obs$R_ys_obs[R_ind,s]) & !is.na(obs$S_ts_obs[S_ind,s])], na.rm = T)
  
  # plot points and lines
  plot(1,1, type = "n", xlim = c(minx, maxx) * c(0, 1.1), ylim = c(miny, maxy) * c(0, 1.1),
       main = stocks[s], axes = F)
  lines(pR1 ~ predx, lty = lty[1], col = col[1])
  points(pR1[ps] ~ predx[ps], pch = pch[1], col = col[1], cex = cex)
  lines(pR2 ~ predx, lty = lty[2], col = col[2])
  points(pR2[ps] ~ predx[ps], pch = pch[2], col = col[2], cex = cex)
  lines(pR3 ~ predx, lty = lty[3], col = col[3])
  points(pR3[ps] ~ predx[ps], pch = pch[3], col = col[3], cex = cex)
  lines(pR4 ~ predx, lty = lty[4], col = col[4])
  points(pR4[ps] ~ predx[ps], pch = pch[4], col = col[4], cex = cex)
  abline(c(0,1), lty = 3)
  
  if (s %in% lm_stocks) {
    i = which(lm_stocks == s)
    lines(pRlm ~ predx, lty = lty[5], col = col[5])
    points(pRlm[ps] ~ predx[ps], pch = pch[5], col = col[5], cex = cex)
    lines(pRlme ~ predx, lty = lty[6], col = col[6])
    points(pRlme[ps] ~ predx[ps], pch = pch[6], col = col[6], cex = cex)
  }
  points(obs$R_ys_obs[R_ind,s] ~ obs$S_ts_obs[S_ind,s], pch = 4, cex = cex)
  
  # add axes
  atx = axisTicks(usr = par("usr")[1:2], nint = 3, log = F)
  aty = axisTicks(usr = par("usr")[3:4], nint = 3, log = F)
  axis(side = 1, at = atx, labels = atx/1000)
  axis(side = 2, at = aty, labels = aty/1000, las = 2)
  box()
}
# add a legend
plot.new()
box()
legend("bottomleft", legend = c("m", "M"), lty = c(1,2), title = "SSM\nMaturity", bty = "n", cex = 1.6, pt.cex = 1.8)
legend("bottom", legend = c("v", "V"), pch = c(1,16), title = "SSM\nVariance", bty = "n", cex = 1.6, pt.cex = 1.8)
legend("bottomright", legend = c("LM", "LME"), pch = c(2,17), col = "grey50", lty = 1, title = "Regression", pt.cex = 1.8, bty = "n", cex = 1.6)
mtext(side = 1, line = 1, "Spawners (1000s)", outer = T, cex = 1.3)
mtext(side = 2, line = 1, "Recruits (1000s)", outer = T, cex = 1.3)

dev.off()


##### FIT TO ESCAPEMENT DATA #####

# extract median escapement by stock and format it as [year,stock]
S1 = array_format(post_summ(post1, "S[")[3,])
S2 = array_format(post_summ(post2, "S[")[3,])
S3 = array_format(post_summ(post3, "S[")[3,])
S4 = array_format(post_summ(post4, "S[")[3,])

# create the device
jpeg(file.path(fig_dir, "S-fit.jpg"), h = 8 * ppi, w = 6.5 * ppi, res = ppi)

# set up the device
m = matrix(c(1:ns, rep(ns+1,2)), 5, 3, byrow = T)
layout(m)
par(mar = c(1.5,1.5,1.5,1), cex.main = 1.7, yaxs = "i", oma = c(3,3,0,0), tcl = -0.3, cex.axis = 1.3, mgp = c(2,0.5,0))

# point type, line type, and color for the different models
pch = c(1,16,1,16)
lty = c(1,1,2,2)
cex = 1.8
col = c(rep("black", 4))

years = 1976:2017

# x = S1[,stocks == "Holitna"]
# range(x[x > max(obs$S_ts_obs[,stocks == "Holitna"], na.rm = T)]/max(obs$S_ts_obs[,stocks == "Holitna"], na.rm = T))
# x = S2[,stocks == "Pitka"]
# range(x[x > max(obs$S_ts_obs[,stocks == "Pitka"], na.rm = T)]/max(obs$S_ts_obs[,stocks == "Pitka"], na.rm = T))

# loop over stocks and make plot for each
for (s in 1:ns) {
  
  # decide on y-limits
  miny = 0
  if (any(c(S1[,s], S2[,s], S3[,s], S4[,s]) > 1e6 * max(obs$S_ts_obs[,s], na.rm = T))) {
    maxy = 2 * max(obs$S_ts_obs[,s], na.rm = T)
  } else {
    maxy = max(c(obs$S_ts_obs[,s], S1[,s], S2[,s], S3[,s], S4[,s]), na.rm = T)
  }
  
  # make empty plot
  plot(1,1, type = "n", xlim = range(years), ylim = c(miny, maxy) * c(0, 1.05),
       main = stocks[s], axes = F)
  
  # draw information
  points(obs$S_ts_obs[,s] ~ years, pch = 22, cex = cex + 0.2, col = "black", bg = "grey")
  lines(S1[,s] ~ years, type = "o", pch = pch[1], lty = lty[1], col = col[1])
  lines(S2[,s] ~ years, type = "o", pch = pch[2], lty = lty[2], col = col[2])
  lines(S3[,s] ~ years, type = "o", pch = pch[3], lty = lty[3], col = col[3])
  lines(S4[,s] ~ years, type = "o", pch = pch[4], lty = lty[4], col = col[4])
  
  # add axes 
  atx = axisTicks(usr = par("usr")[1:2], nint = 5, log = F)
  aty = axisTicks(usr = par("usr")[3:4], nint = 3, log = F)
  axis(side = 1, at = atx, labels = paste("'", substr(atx, 3, 4), sep = ""))
  axis(side = 2, at = aty, labels = aty/1000, las = 2)
  box()
}
# add legend
plot.new()
box()
legend("bottomleft", legend = c("m", "M"), lty = c(1,2), title = "SSM\nMaturity", bty = "n", cex = 1.6, pt.cex = 1.8)
legend("bottom", legend = c("v", "V"), pch = c(1,16), title = "SSM\nVariance", bty = "n", cex = 1.6, pt.cex = 1.8)
legend("bottomright", legend = c("", ""), pch = c(22, 22), col = c("black", "white"), pt.bg = c("grey", "white"), title = "Observed\nEscapement", pt.cex = 2.5, bty = "n", cex = 1.6)
mtext(side = 1, line = 1, "Year", outer = T, cex = 1.3)
mtext(side = 2, line = 1, "Escapement (1000s)", outer = T, cex = 1.3)
dev.off()


##### CALCULATIONS FOR TRADE-OFF PLOT #####
n_save = post_dim(post1)["saved"]

# keep only populations fitable by regression
keep = lm_stocks

# extract posterior samples from regression
alm = post_subset(postlm, "alpha_lm[", matrix = T)
blm = post_subset(postlm, "beta_lm[", matrix = T)
alme = post_subset(postlm, "alpha_lme", matrix = T)
blme = post_subset(postlm, "beta_lme", matrix = T)

# extract posterior samples from ssms
a1 = post_subset(post1, "alpha", matrix = T)
b1 = post_subset(post1, "beta", matrix = T)
U1 = post_subset(post1, "U_msy", matrix = T)
S1 = post_subset(post1, "S_msy", matrix = T)

a2 = post_subset(post2, "alpha", matrix = T)
b2 = post_subset(post2, "beta", matrix = T)
U2 = post_subset(post2, "U_msy", matrix = T)
S2 = post_subset(post2, "S_msy", matrix = T)

a3 = post_subset(post3, "alpha", matrix = T)
b3 = post_subset(post3, "beta", matrix = T)
U3 = post_subset(post3, "U_msy", matrix = T)
S3 = post_subset(post3, "S_msy", matrix = T)

a4 = post_subset(post4, "alpha", matrix = T)
b4 = post_subset(post4, "beta", matrix = T)
U4 = post_subset(post4, "U_msy", matrix = T)
S4 = post_subset(post4, "S_msy", matrix = T)

# exploitation rates to evaluate equilibrium states at
U_range = seq(0, 1, 0.01); nU = length(U_range)

# containers
Ulm = Ulme = Slm = Slme = matrix(NA, n_save, length(lm_stocks))
Seq = array(NA, dim = c(n_save, nU, 6))
Ceq = array(NA, dim = c(n_save, nU, 6))
ovf = array(NA, dim = c(n_save, nU, 6))
ext = array(NA, dim = c(n_save, nU, 6))

# loop through mcmc samples, calculate equilibrium S and C at each exploitation rate, as well as p_overfished
starttime = Sys.time()
for (i in 1:n_save) {
  cat("\r", paste(round(i/n_save, 3) * 100, "%", sep = ""))
  
  # need
  tmp = gen_lm_mgmt(alpha = alm[i,], beta = blm[i,])
  Ulm[i,] = unname(tmp$U_msy)
  Slm[i,] = unname(tmp$S_msy)
  tmp = gen_lm_mgmt(alpha = alme[i,], beta = blme[i,])
  Ulme[i,] = unname(tmp$U_msy)
  Slme[i,] = unname(tmp$S_msy)
  
  mlm = gen_mgmt(
    params = list(alpha = alm[i,],beta = blm[i,],
                  U_msy = Ulm[i,],S_msy = Slm[i,]),
    U_range = U_range
  )
  mlme = gen_mgmt(
    params = list(alpha = alme[i,],beta = blme[i,],
                  U_msy = Ulme[i,],S_msy = Slme[i,]),
    U_range = U_range
  )
  m1 = gen_mgmt(
    params = list(alpha = a1[i,keep],beta = b1[i,keep],
                  U_msy = U1[i,keep],S_msy = S1[i,keep]),
    U_range = U_range
  )
  m2 = gen_mgmt(
    params = list(alpha = a2[i,keep],beta = b2[i,keep],
                  U_msy = U2[i,keep],S_msy = S2[i,keep]),
    U_range = U_range
  )
  m3 = gen_mgmt(
    params = list(alpha = a3[i,keep],beta = b3[i,keep],
                  U_msy = U3[i,keep],S_msy = S3[i,keep]),
    U_range = U_range
  )
  m4 = gen_mgmt(
    params = list(alpha = a4[i,keep],beta = b4[i,keep],
                  U_msy = U4[i,keep],S_msy = S4[i,keep]),
    U_range = U_range
  )
  
  Seq[i,,1] = m1$Seq; Seq[i,,2] = m2$Seq
  Seq[i,,3] = m3$Seq; Seq[i,,4] = m4$Seq
  Seq[i,,5] = mlm$Seq; Seq[i,,6] = mlme$Seq
  Ceq[i,,1] = m1$Ceq; Ceq[i,,2] = m2$Ceq
  Ceq[i,,3] = m3$Ceq; Ceq[i,,4] = m4$Ceq
  Ceq[i,,5] = mlm$Ceq; Ceq[i,,6] = mlme$Ceq
  ovf[i,,1] = (m1$overfished * ns)/length(keep); ovf[i,,2] = (m2$overfished * ns)/length(keep)
  ovf[i,,3] = (m3$overfished * ns)/length(keep); ovf[i,,4] = (m4$overfished * ns)/length(keep)
  ovf[i,,5] = (mlm$overfished * ns)/length(keep); ovf[i,,6] = (mlme$overfished * ns)/length(keep)
  ext[i,,1] = (m1$extinct * ns)/length(keep); ext[i,,2] = (m2$extinct * ns)/length(keep)
  ext[i,,3] = (m3$extinct * ns)/length(keep); ext[i,,4] = (m4$extinct * ns)/length(keep)
  ext[i,,5] = (mlm$extinct * ns)/length(keep); ext[i,,6] = (mlme$extinct * ns)/length(keep)
}
Sys.time() - starttime

# obtain posterior summaries for each model at each exploitation rate
Seq_mean = apply(Seq, 3, function(x) apply(x, 2, median))
Ceq_mean = apply(Ceq, 3, function(x) apply(x, 2, median))
ovf_mean = apply(ovf, 3, function(x) apply(x, 2, mean))
ext_mean = apply(ext, 3, function(x) apply(x, 2, mean))

##### TRADE-OFF PLOT #####

# which U's will have associated points on plot?
keepU = which(as.character(U_range) %in% as.character(c(0.10, 0.30, 0.50, 0.70)))

# plotting characters, line types, and colors
pch = c(17, 16, 15, 4)
lty = c(1,1,2,2,1,1)
col = c(rep("black", 4), "grey50", "grey50")
cex = 1.2

# create the device
jpeg(file.path(fig_dir, "kusko-trade-off.jpg"), h = 3.4 * ppi, w = 3.4 * ppi, res = ppi)

# set up the device
par(mar = c(3,3,1,1), mgp = c(2, 0.6, 0), xaxs = "i", yaxs = "i")

# create an empty plot
plot(1,1, type = "n", xlab = "", ylab = "", xlim = c(-0.025,1.025), ylim = c(0, 60000), xaxt = "n", yaxt = "n")

# loop through models drawing the info for each
for (m in c(6,5,2,4)) {
  lines(Ceq_mean[,m] ~ ovf_mean[,m], lty = lty[m], col = col[m])
  points(Ceq_mean[keepU,m] ~ ovf_mean[keepU,m], pch = pch, col = col[m], cex = cex)
}

# draw axes
axis(side = 2, at = seq(0, 100000, 10000), seq(0, 100000, 10000)/1000, las = 2, tcl = -0.4)
axis(side = 1, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2), tcl = -0.4)
axis(side = 1, at = seq(0, 1, 0.05), labels = rep("", 21), tcl = -0.2)
mtext(side = 2, line = 2, "Mixed-Stock Harvest (1000s)")
mtext(side = 1, line = 2, "Proportion of Populations Overfished")

# add labels at highly customized locations
text(x = 0.38, y = 14500, labels = "SSM-Vm", cex = 0.8)
text(x = 0.5, y = 23500, labels = "SSM-VM", cex = 0.8)
text(x = 0.65, y = 35500, labels = "LME", cex = 0.8, col = "grey50")
text(x = 0.73, y = 53500, labels = "LM", cex = 0.8, col = "grey50")
text(x = 0.65, y = 35500, labels = "LME", cex = 0.8, col = "grey50")

# add a legend
legend("topleft", legend = c(0.1, 0.3, 0.5, 0.7), pch = pch, pt.cex = 1.3, cex = 0.8, bty = "n", title = "U")
dev.off()
