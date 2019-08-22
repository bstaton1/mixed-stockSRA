
# THIS SCRIPT GENERATES THE BOXPLOTS THAT SHOW
# THE DISTRIBUTION OF PROPORTIONAL ERROR IN 
# MAIN PARAMETERS OF INTEREST (E.G., ALPHA, BETA, SIGMA_R)

# PREPARE THE OUTPUT
source("0-prep-output.R"); ls()

# FUNCTION TO SUMMARIZE AN INDIVIDUAL PARAMETER ACROSS ALL MODELS FOR
# EASY BOXPLOTTING
summarize_individual = function(p, methods = c("lm", "lme", "ssm1", "ssm2", "ssm3", "ssm4"), years = NULL) {
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
    ests = est_summ %>% filter(param == p & method %in% methods) %>% select(seed, stock, method, "X50.")
  }
  
  colnames(ests)[4] = "est"
  
  combined = merge(true, ests, by = c("seed", "stock"), all = T) %>%
    arrange(seed, stock, method)
  
  combined = combined %>% mutate(e = est - true, pe = e/true)
  
  out_list = tapply(combined$pe, combined$method, function(x) quantile(x, c(0.025, 0.25, 0.5, 0.75, 0.975)))
  out = unlist(out_list) %>% matrix(nrow = length(out_list), ncol = 5, byrow = T)
  rownames(out) = names(out_list)
  colnames(out) = c("2.5%", "25%", "50%", "75%", "97.5%")
  t(out)
}


ppi = 600
jpeg("figs/param-bias.jpg", h = 6 * ppi, w = 4.5 * ppi, res = ppi)
par(mfcol = c(3,2), mar = c(0.25,1.5,0.25,0.25), oma = c(5,2.5,1,2), cex.axis = 1.3,
    mgp = c(2,0.75,0))

## alpha
dummy = matrix(rep(1, 60), 10, 6)
bp = boxplot(dummy, plot = F)
bp$stats = summarize_individual("alpha", c("lm", "lme", "ssm1", "ssm2", "ssm3", "ssm4"))

bxp(z = bp, outline = F, las = 1, boxfill = c("grey90", "grey90", "grey70", "grey70", "grey70", "grey70"),
    xaxt = "n")
abline(h = 0); abline(h = 0, col = "white", lty = 2); box()
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(usr[2], usr[4] - ydiff * 0.1, latex2exp::TeX("$\\alpha_{j}$"), pos = 2, cex = 1.5)

## U_msy
dummy = matrix(rep(1, 60), 10, 6)
bp = boxplot(dummy, plot = F)
bp$stats = summarize_individual("U_msy", c("lm", "lme", "ssm1", "ssm2", "ssm3", "ssm4"))

bxp(z = bp, outline = F, las = 1, boxfill = c("grey90", "grey90", "grey70", "grey70", "grey70", "grey70"),
    xaxt = "n")
abline(h = 0); abline(h = 0, col = "white", lty = 2); box()
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(usr[2], usr[4] - ydiff * 0.1, latex2exp::TeX("$\\U_{MSY,j}$"), pos = 2, cex = 1.5)

## S_msy
dummy = matrix(rep(1, 60), 10, 6)
bp = boxplot(dummy, plot = F)
bp$stats = summarize_individual("S_msy", c("lm", "lme", "ssm1", "ssm2", "ssm3", "ssm4"))

bxp(z = bp, outline = F, las = 1, boxfill = c("grey90", "grey90", "grey70", "grey70", "grey70", "grey70"),
    xaxt = "n")
abline(h = 0); abline(h = 0, col = "white", lty = 2); box()
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(usr[2], usr[4] - ydiff * 0.1, latex2exp::TeX("$\\S_{MSY,j}$"), pos = 2, cex = 1.5)
axis(side = 1, at = 1:6, labels = c("LM", "LME", "SSM-vm", "SSM-Vm", "SSM-vM", "SSM-VM"), las = 2)

par(mar = c(0.25,0.25,0.25,1.5))

## sigma
dummy = matrix(rep(1, 40), 10, 4)
bp = boxplot(dummy, plot = F)
bp$stats = summarize_individual("sigma_R", c("ssm1", "ssm2", "ssm3", "ssm4"))

bxp(z = bp, outline = F, las = 1, boxfill = c("grey70", "grey70", "grey70", "grey70"),
    xaxt = "n", yaxt = "n", ylim = range(bp$stats) * c(1, 1.15))
abline(h = 0); abline(h = 0, col = "white", lty = 2); box()
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(usr[2], usr[4] - ydiff * 0.1, latex2exp::TeX("$\\sigma_{R,j}$"), pos = 2, cex = 1.5)
axis(side = 4, las = 2)

## mean_rho
dummy = matrix(rep(1, 40), 10, 4)
bp = boxplot(dummy, plot = F)
bp$stats = summarize_individual("mean_rho", c("ssm1", "ssm2", "ssm3", "ssm4"))

bxp(z = bp, outline = F, las = 1, boxfill = c("grey70", "grey70", "grey70", "grey70"),
    xaxt = "n", yaxt = "n")
abline(h = 0); abline(h = 0, col = "white", lty = 2); box()
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(usr[2], usr[4] - ydiff * 0.1, latex2exp::TeX("$\\bar{\\rho}_{i,j}$"), pos = 2, cex = 1.5)
axis(side = 4, las = 2)

## phi
dummy = matrix(rep(1, 40), 10, 4)
bp = boxplot(dummy, plot = F)
bp$stats = summarize_individual("phi", c("ssm1", "ssm2", "ssm3", "ssm4"))

bxp(z = bp, outline = F, las = 1, boxfill = c("grey70", "grey70", "grey70", "grey70"),
    xaxt = "n", yaxt = "n")
abline(h = 0); abline(h = 0, col = "white", lty = 2); box()
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(usr[2], usr[4] - ydiff * 0.1, latex2exp::TeX("$\\phi$"), pos = 2, cex = 1.5)
axis(side = 4, las = 2)

axis(side = 1, at = 1:4, labels = c("SSM-vm", "SSM-Vm", "SSM-vM", "SSM-VM"), las = 2)
mtext(side = 2, outer = T, "Proportional Error", line = 0.75, cex = 1.2)
dev.off()
