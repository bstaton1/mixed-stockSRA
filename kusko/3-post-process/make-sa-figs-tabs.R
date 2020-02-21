
# THIS SCRIPT CREATES THE OUTPUT FIGURES/TABLES FOR THE ONLINE SUPPLEMENT
# REGARDING THE SENSITIVITY ANALYSIS OF STATE-SPACE MODELS WITH KUSKOKWIM DATA

##### SESSION SET UP #####
rm(list = ls(all = T))

# resolution for figures (pixels per inch)
ppi = 600

# set working directory
setwd("location/of/this/file")

# Staton packages
# running devtools::install_github("bstaton1/FitSR") will install all of these packages
library(FitSR)
library(SimSR)
library(postpack)
library(StatonMisc)

# directories and file names
post_dir = "location/of/posterior/samples"
base_post_dir = file.path(post_dir, "ssm_post_base")
vuln_post_dir = file.path(post_dir, "ssm_post_alt_vuln")
aess_post_dir = file.path(post_dir, "ssm_post_alt_ess")
post_file = "ssm_4_post.rds"

# output directories
out_dir = "location/to/dump/output"
fig_dir = file.path(out_dir, "figs"); if (!dir.exists(fig_dir)) dir.create(fig_dir)
tab_dir = file.path(out_dir, "tabs"); if (!dir.exists(tab_dir)) dir.create(tab_dir)

# load posterior samples
base_post = readRDS(file = file.path(base_post_dir, post_file))
vuln_post = readRDS(file = file.path(vuln_post_dir, post_file))
aess_post = readRDS(file = file.path(aess_post_dir, post_file))

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
  
  # age comp data
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

# read in alternative vulnerability schedules
alt_vuln_v = read.csv(file.path(in_dir, "vuln.csv"), stringsAsFactors = F)
vstocks = alt_vuln_v$stock
aess_v = rep(1, ns)
base_v = rep(1, ns)
vuln_v = alt_vuln_v$v
names(vuln_v) = names(base_v) = names(aess_v) = vstocks

##### LOAD OTHER FUNCTIONS #####

# same as eq_ricker from the SimSR package, but with an additional vulnerability term
eq_ricker2 = function(alpha, beta, U_msy, S_msy, U_range, v) {
  
  # calculate Seq
  Seq = 1/beta * (log(alpha) + log(1 - U_range * v))
  Seq[Seq < 0] = 0
  
  # calculate Ceq
  Ceq = Seq * U_range/(1 - U_range * v)
  Ceq[is.na(Ceq)] = 0
  
  # determine if U would overfish it or push it to extinction
  overfished = ifelse((U_range * v) > U_msy, 1, 0)
  extinct = ifelse(Seq == 0, 1, 0)
  
  # output
  return(list(S = Seq, C = Ceq, overfished = overfished, extinct = extinct))
}

# same as gen_mgmt from the SimSR package, but with an additional vulnerability term
gen_mgmt2 = function(params, U_range = seq(0, 1, 0.01), v = NULL) {
  
  output = with(params, {
    
    # key parameters for each substock
    sub_params = cbind(alpha, beta, U_msy, S_msy, v)
    
    # determine the equilibrium quantities for each substock at various exploitation rates (U_range)
    Seq_s = apply(sub_params, 1, function(x) eq_ricker2(alpha = x["alpha"], beta = x["beta"], U_msy = x["U_msy"], S_msy = x["S_msy"], U_range = U_range, v = x["v"])$S)
    Ceq_s = apply(sub_params, 1, function(x) eq_ricker2(alpha = x["alpha"], beta = x["beta"], U_msy = x["U_msy"], S_msy = x["S_msy"], U_range = U_range, v = x["v"])$C)
    overfished_s = apply(sub_params, 1, function(x) eq_ricker2(alpha = x["alpha"], beta = x["beta"], U_msy = x["U_msy"], S_msy = x["S_msy"], U_range = U_range, v = x["v"])$overfished)
    extinct_s = apply(sub_params, 1, function(x) eq_ricker2(alpha = x["alpha"], beta = x["beta"], U_msy = x["U_msy"], S_msy = x["S_msy"], U_range = U_range, v = x["v"])$extinct)
    
    # sum across substocks
    Seq = rowSums(Seq_s)
    Ceq = rowSums(Ceq_s)
    overfished = rowSums(overfished_s)/ns
    extinct = rowSums(extinct_s)/ns
    
    # system-wide BRPs
    S_MSY = Seq[which.max(Ceq)]
    U_MSY = U_range[which.max(Ceq)]
    MSY = Ceq[which.max(Ceq)]
    of_MSY = overfished[which(U_range == U_MSY)]
    ex_MSY = extinct[which(U_range == U_MSY)]
    # system-wide MRPs
    if (all(overfished > 0.1)) {
      Sstar_0.1 = NA
      Ustar_0.1 = NA
      Cstar_0.1 = NA
    } else {
      Sstar_0.1 = min(Seq[which(overfished <= 0.1)])
      Ustar_0.1 = max(U_range[which(overfished <= 0.1)])
      Cstar_0.1 = max(Ceq[which(overfished <= 0.1)])
    }
    if (all(overfished > 0.3)) {
      Sstar0.3 = NA
      Ustar_0.3 = NA
      Cstar_0.3 = NA
    } else {
      Sstar_0.3 = min(Seq[which(overfished <= 0.3)])
      Ustar_0.3 = max(U_range[which(overfished <= 0.3)])
      Cstar_0.3 = max(Ceq[which(overfished <= 0.3)])
    }
    if (all(overfished > 0.5)) {
      Sstar_0.5 = NA
      Ustar_0.5 = NA
      Cstar_0.5 = NA
    } else {
      Sstar_0.5 = min(Seq[which(overfished <= 0.5)])
      Ustar_0.5 = max(U_range[which(overfished <= 0.5)])
      Cstar_0.5 = max(Ceq[which(overfished <= 0.5)])
    }
    
    list(
      mgmt = c(
        Sstar_0.1 = Sstar_0.1, Sstar_0.3 = Sstar_0.3, Sstar_0.5 = Sstar_0.5,
        Ustar_0.1 = Ustar_0.1, Ustar_0.3 = Ustar_0.3, Ustar_0.5 = Ustar_0.5,
        Cstar_0.1 = Cstar_0.1, Cstar_0.3 = Cstar_0.3, Cstar_0.5 = Cstar_0.5,
        S_MSY = S_MSY, U_MSY = U_MSY, MSY = MSY, of_MSY = of_MSY, ex_MSY = ex_MSY),
      eq = cbind(
        Seq = Seq,
        Ceq = Ceq,
        # Req_s = log(alpha)/beta,
        overfished = overfished,
        extinct = extinct
      )
    )
  })
  
  return(output)
}

# function to facilitate building of tables
parens = function(x, rnd = 2, pretty = F) {
  x = round(x, rnd)
  if(pretty) x = prettify(x)
  
  paste(x[3], "(", x[4], " -- ", x[5], ")", sep = "")
}

##### MAKE TABLE OF ESTIMATES #####
U_range = seq(0, 1, 0.01)

base_a = post_subset(base_post, "alpha", matrix = T)
base_b = post_subset(base_post, "beta", matrix = T)
base_Smsy = post_subset(base_post, "S_msy", matrix = T)
base_Umsy = post_subset(base_post, "U_msy", matrix = T)

vuln_a = post_subset(vuln_post, "alpha", matrix = T)
vuln_b = post_subset(vuln_post, "beta", matrix = T)
vuln_Smsy = post_subset(vuln_post, "S_msy", matrix = T)
vuln_Umsy = post_subset(vuln_post, "U_msy", matrix = T)

aess_a = post_subset(aess_post, "alpha", matrix = T)
aess_b = post_subset(aess_post, "beta", matrix = T)
aess_Smsy = post_subset(aess_post, "S_msy", matrix = T)
aess_Umsy = post_subset(aess_post, "U_msy", matrix = T)

n = unname(post_dim(base_post)["saved"])
# n = 1000

base_mgmt = vuln_mgmt = aess_mgmt = matrix(NA, n, 14)
base_eq = vuln_eq = aess_eq = array(NA, dim = c(length(U_range),4,n))

starttime = Sys.time()
for (i in 1:n) {
  cat("\r", paste(floor((i/n) * 100), "%", sep = ""))
  
  tmp = gen_mgmt2(params = list(alpha = base_a[i,], beta = base_b[i,], U_msy = base_Umsy[i,], S_msy = base_Smsy[i,]), U_range = U_range, v = base_v[stocks])
  base_mgmt[i,] = tmp$mgmt
  base_eq[,,i] = tmp$eq
  
  tmp = gen_mgmt2(params = list(alpha = vuln_a[i,], beta = vuln_b[i,], U_msy = vuln_Umsy[i,], S_msy = vuln_Smsy[i,]), U_range = U_range, v = vuln_v[stocks])
  vuln_mgmt[i,] = tmp$mgmt
  vuln_eq[,,i] = tmp$eq
  
  tmp = gen_mgmt2(params = list(alpha = aess_a[i,], beta = aess_b[i,], U_msy = aess_Umsy[i,], S_msy = aess_Smsy[i,]), U_range = U_range, v = aess_v[stocks])
  aess_mgmt[i,] = tmp$mgmt
  aess_eq[,,i] = tmp$eq
}
colnames(base_mgmt) = colnames(vuln_mgmt) = colnames(aess_mgmt) = names(tmp$mgmt)
Sys.time() - starttime

# summarize output
base_out = apply(apply(base_mgmt, 2, summ), 2, function(x) {
  if (x[1] < 1) rnd = 2 else rnd = -2
  parens(x, rnd = rnd, pretty = T)
})
vuln_out = apply(apply(vuln_mgmt, 2, summ), 2, function(x) {
  if (x[1] < 1) rnd = 2 else rnd = -2
  parens(x, rnd = rnd, pretty = T)
})
aess_out = apply(apply(aess_mgmt, 2, summ), 2, function(x) {
  if (x[1] < 1) rnd = 2 else rnd = -2
  parens(x, rnd = rnd, pretty = T)
})

tab = cbind(base_out, vuln_out, aess_out)

# calculate average exploitation rate
base_U = parens(summ(rowMeans(post_subset(base_post, "U[", matrix = T))))
vuln_U = parens(summ(rowMeans(post_subset(vuln_post, "U[", matrix = T))))
aess_U = parens(summ(rowMeans(post_subset(aess_post, "U[", matrix = T))))

tab = rbind(tab, U_full_mean = c(base_U, vuln_U, aess_U))
tab = tab[c("Sstar_0.1",   "Sstar_0.3",   "Sstar_0.5", "S_MSY",
            "Cstar_0.1",   "Cstar_0.3",   "Cstar_0.5", "MSY",
            "Ustar_0.1",   "Ustar_0.3",   "Ustar_0.5", "U_MSY", "U_full_mean",
            "of_MSY", "ex_MSY"),]
colnames(tab) = c("base", "vuln", "aess")

write.csv(tab, file.path(tab_dir, "sa-ests-table.csv"), row.names = T)

##### FIGURE SHOWING % CHANGE #####

# get estimates of U_msy and S_eq by population and model scenario
base_lna = log(post_subset(base_post, "alpha", matrix = T))
base_b = post_subset(base_post, "beta", matrix = T)
base_S = apply(base_lna/base_b, 2, summ)[3,]
base_U = post_summ(base_post, "U_msy")[3,]

vuln_lna = log(post_subset(vuln_post, "alpha", matrix = T))
vuln_b = post_subset(vuln_post, "beta", matrix = T)
vuln_S = apply(vuln_lna/vuln_b, 2, summ)[3,]
vuln_U = post_summ(vuln_post, "U_msy")[3,]

aess_lna = log(post_subset(aess_post, "alpha", matrix = T))
aess_b = post_subset(aess_post, "beta", matrix = T)
aess_S = apply(aess_lna/aess_b, 2, summ)[3,]
aess_U = post_summ(aess_post, "U_msy")[3,]

# calculate percent change from the base
vuln_dU = (vuln_U - base_U)/base_U * 100; names(vuln_dU) = stocks; vuln_dU = vuln_dU[vstocks]
vuln_dS = (vuln_S - base_S)/base_S * 100; names(vuln_dS) = stocks; vuln_dS = vuln_dS[vstocks]
aess_dU = (aess_U - base_U)/base_U * 100; names(aess_dU) = stocks; aess_dU = aess_dU[vstocks]
aess_dS = (aess_S - base_S)/base_S * 100; names(aess_dS) = stocks; aess_dS = aess_dS[vstocks]

max(abs(c(vuln_dU, aess_dU)))
max(abs(c(vuln_dS, aess_dS)))
dmax_U = 105
dmax_S = 30


jpeg(file.path(fig_dir, "sa-delta-fig.jpg"), h = 7 * ppi, w = 4 * ppi, res = ppi)
# par(mfcol = c(2,1), mar = c(0.5,1,0.5,1), oma = c(3,4,2,0))
par(mfrow = c(2,1), mar = c(3,6,1,1), lend = 1, tcl = -0.35, cex.axis = 1.1, mgp = c(2,0.5,0), cex.lab = 1.2)
plot(x = vuln_dU, y = 1:ns, type = "n", ylim = c(ns, 1),
     xlim = 0 + c(-1,1) * dmax_U, yaxt = "n", xaxt = "n",
     ylab = "", xlab = latex2exp::TeX("$\\%\\Delta U_{MSY,j}"))
abline(h = 1:ns, col = "grey")
abline(h = c(3.5, 10.5), lty = 2, lwd = 2)
abline(v = 0, lwd = 2)
points(x = vuln_dU, y = 1:ns, cex = 1.2, pch = 21, bg = "black")
points(x = aess_dU, y = 1:ns, cex = 1.2, pch = 21, bg = "white")
axis(side = 2, at = seq(1,ns), labels = vstocks, las = 1)
axis(side = 1, at = seq(-100, 100, 50), labels = seq(-100, 100, 50), las = 1, cex.axis = 1.05)
axis(side = 1, at = seq(-100, 100, 10), labels = rep("", length(seq(-100, 100, 10))), tcl = -0.2, las = 1)

legend("topleft", cex = 0.8, legend = c("VULN", "ESS"), pt.cex = 1.2,
       pch = 21, pt.bg = c("black", "white"), box.col = "white", bg = "white")

usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(x = usr[2] + xdiff * 0.05, y = (3.5 + usr[4])/2, "Lower", srt = 270, xpd = T)
text(x = usr[2] + xdiff * 0.05, y = (3.5 + 10.5)/2, "Middle", srt = 270, xpd = T)
text(x = usr[2] + xdiff * 0.05, y = (usr[3] + 10.5)/2, "Upper", srt = 270, xpd = T)

box()

plot(x = vuln_dS, y = 1:ns, type = "n", ylim = c(ns, 1),
     xlim = 0 + c(-1,1) * dmax_S, yaxt = "n", xaxt = "n",
     ylab = "", xlab = latex2exp::TeX("$\\%\\Delta S_{eq,j}"))
abline(h = 1:ns, col = "grey")
abline(h = c(3.5, 10.5), lty = 2, lwd = 2)
abline(v = 0, lwd = 2)
points(x = vuln_dS, y = 1:ns, cex = 1.2, pch = 21, bg = "black")
points(x = aess_dS, y = 1:ns, cex = 1.2, pch = 21, bg = "white")
axis(side = 2, at = seq(1,ns), labels = vstocks, las = 1)
axis(side = 1, at = seq(-30, 30, 10), labels = seq(-30, 30, 10), las = 1, cex.axis = 1.05)
axis(side = 1, at = seq(-30, 30, 5), labels = rep("", length(seq(-30, 30, 5))), tcl = -0.2, las = 1)
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(x = usr[2] + xdiff * 0.05, y = (3.5 + usr[4])/2, "Lower", srt = 270, xpd = T)
text(x = usr[2] + xdiff * 0.05, y = (3.5 + 10.5)/2, "Middle", srt = 270, xpd = T)
text(x = usr[2] + xdiff * 0.05, y = (usr[3] + 10.5)/2, "Upper", srt = 270, xpd = T)

box()
dev.off()

detach(params)

##### FIGURE SHOWING ALTERNATIVE VULN SCHEDULE #####

jpeg(file.path(fig_dir, "alt-vuln-fig.jpg"), h = 4 * ppi, w = 5 * ppi, res = ppi)
par(mar = c(4.4,3,1.5,1), tcl = -0.2, mgp = c(2,0.25,0), cex.axis = 0.9)
mp = barplot(vuln_v, ylim = c(0.5,1.02), xpd = F, las = 1, space = 0.25, xlim = c(0.55, 15.95), names.arg = rep("", params$ns))
box()
axis(side = 1, at = mp, labels = names(vuln_v), las = 2)
mtext(side = 2, "Alternate Vulnerability", line = 1.5)
segments(sum(mp[3:4])/2, 0.5, sum(mp[3:4])/2, 1.02, xpd = T, lty = 2)
segments(sum(mp[10:11])/2, 0.5, sum(mp[10:11])/2, 1.02, xpd = T, lty = 2)
text(x = mp[2], y = 1.04, "Lower River", font = 2, xpd = T, cex = 0.9)
text(x = mp[7], y = 1.04, "Middle River", font = 2, xpd = T, cex = 0.9)
text(x = mp[12], y = 1.04, "Upper River", font = 2, xpd = T, cex = 0.9)
dev.off()
