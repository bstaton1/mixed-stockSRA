
# THIS SCRIPT GENERATES THE BOXPLOTS THAT SHOW
# THE DISTRIBUTION OF PROPORTIONAL ERROR IN 
# MAIN REFERENCE POINTS OF INTEREST

# PREPARE THE OUTPUT
source("0-prep-output.R"); ls()

# FUNCTION TO SUMMARIZE AN PARAMETER ACROSS ALL MODELS FOR EASY BOXPLOTTING

summarize_common = function(p) {
  
  true = param_summ %>% filter(param == p) %>% select(seed, "value")
  colnames(true)[2] = "true"
  
  ests = est_summ %>% filter(param == p) %>% select(seed, method, "X50.")
  colnames(ests)[3] = "est"
  
  out = merge(true, ests, by = "seed", all = T) %>% 
    arrange(seed, method) %>%
    mutate(e = est - true, pe = e/true)
  
  out
}

ppi = 600
jpeg("figs/ref-point-bias.jpg", h = 7 * ppi, w = 5 * ppi, res = ppi)
par(mfcol = c(4,2), mar = c(0.25,1.5,0.25,0.25), oma = c(5,4,1,2), cex.axis = 1.3,
    mgp = c(2,0.75,0))

### S_MSY
out = summarize_common("S_MSY")
head(out)
bp = boxplot(pe ~ method, data = out, plot = F)
bp$stats = sapply(sort(unique(out$method)), function(m) {
  sub = subset(out, method == m)
  StatonMisc::summ(sub$pe, p = c(0.025, 0.25, 0.5, 0.75, 0.975))[3:7]
})
bxp(z = bp, outline = F, las = 1, boxfill = c("grey90", "grey90", "grey70", "grey70", "grey70", "grey70"),
    xaxt = "n")
abline(h = 0); abline(h = 0); abline(h = 0, col = "white", lty = 2); box()
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(usr[2], usr[4] - ydiff * 0.1, latex2exp::TeX("$S_{MSY}$"), pos = 2, cex = 1.5)

### Sstar_0.5
out = summarize_common("Sstar_0.5")
head(out)
bp = boxplot(pe ~ method, data = out, plot = F)
bp$stats = sapply(sort(unique(out$method)), function(m) {
  sub = subset(out, method == m)
  StatonMisc::summ(sub$pe, p = c(0.025, 0.25, 0.5, 0.75, 0.975))[3:7]
})
bxp(z = bp, outline = F, las = 1, boxfill = c("grey90", "grey90", "grey70", "grey70", "grey70", "grey70"),
    xaxt = "n")
abline(h = 0); abline(h = 0); abline(h = 0, col = "white", lty = 2); box()
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(usr[2], usr[4] - ydiff * 0.1, latex2exp::TeX("$S^*_{0.5}$"), pos = 2, cex = 1.5)

### Sstar_0.3
out = summarize_common("Sstar_0.3")
head(out)
bp = boxplot(pe ~ method, data = out, plot = F)
bp$stats = sapply(sort(unique(out$method)), function(m) {
  sub = subset(out, method == m)
  StatonMisc::summ(sub$pe, p = c(0.025, 0.25, 0.5, 0.75, 0.975))[3:7]
})
bxp(z = bp, outline = F, las = 1, boxfill = c("grey90", "grey90", "grey70", "grey70", "grey70", "grey70"),
    xaxt = "n", ylim = range(bp$stats) * c(1, 1.15))
abline(h = 0); abline(h = 0); abline(h = 0, col = "white", lty = 2); box()
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(usr[2], usr[4] - ydiff * 0.1, latex2exp::TeX("$S^*_{0.3}$"), pos = 2, cex = 1.5)

### Sstar_0.1
out = summarize_common("Sstar_0.1")
head(out)
bp = boxplot(pe ~ method, data = out, plot = F)
bp$stats = sapply(sort(unique(out$method)), function(m) {
  sub = subset(out, method == m)
  StatonMisc::summ(sub$pe, p = c(0.025, 0.25, 0.5, 0.75, 0.975))[3:7]
})
bxp(z = bp, outline = F, las = 1, boxfill = c("grey90", "grey90", "grey70", "grey70", "grey70", "grey70"),
    xaxt = "n", ylim = range(bp$stats) * c(1, 1.15))
abline(h = 0); abline(h = 0); abline(h = 0, col = "white", lty = 2); box()
axis(side = 1, at = 1:6, labels = c("LM", "LME", "SSM-vm", "SSM-Vm", "SSM-vM", "SSM-VM"), las = 2)
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(usr[2], usr[4] - ydiff * 0.1, latex2exp::TeX("$S^*_{0.1}$"), pos = 2, cex = 1.5)


par(mar = c(0.25,0.25,0.25,1.5))

### U_MSY
out = summarize_common("U_MSY")
head(out)
bp = boxplot(pe ~ method, data = out, plot = F)
bp$stats = sapply(sort(unique(out$method)), function(m) {
  sub = subset(out, method == m)
  StatonMisc::summ(sub$pe, p = c(0.025, 0.25, 0.5, 0.75, 0.975))[3:7]
})
bxp(z = bp, outline = F, las = 1, boxfill = c("grey90", "grey90", "grey70", "grey70", "grey70", "grey70"),
    xaxt = "n", yaxt = "n")
abline(h = 0); abline(h = 0); abline(h = 0, col = "white", lty = 2); box()
axis(side = 4, las = 2)
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(usr[2], usr[4] - ydiff * 0.1, latex2exp::TeX("$U_{MSY}$"), pos = 2, cex = 1.5)

### Ustar_0.5
out = summarize_common("Ustar_0.5")
head(out)
bp = boxplot(pe ~ method, data = out, plot = F)
bp$stats = sapply(sort(unique(out$method)), function(m) {
  sub = subset(out, method == m)
  StatonMisc::summ(sub$pe, p = c(0.025, 0.25, 0.5, 0.75, 0.975))[3:7]
})
bxp(z = bp, outline = F, las = 1, boxfill = c("grey90", "grey90", "grey70", "grey70", "grey70", "grey70"),
    xaxt = "n", yaxt = "n")
abline(h = 0); abline(h = 0); abline(h = 0, col = "white", lty = 2); box()
axis(side = 4, las = 2)
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(usr[2], usr[4] - ydiff * 0.1, latex2exp::TeX("$U^*_{0.5}$"), pos = 2, cex = 1.5)

### Ustar_0.3
out = summarize_common("Ustar_0.3")
head(out)
bp = boxplot(pe ~ method, data = out, plot = F)
bp$stats = sapply(sort(unique(out$method)), function(m) {
  sub = subset(out, method == m)
  StatonMisc::summ(sub$pe, p = c(0.025, 0.25, 0.5, 0.75, 0.975))[3:7]
})
bxp(z = bp, outline = F, las = 1, boxfill = c("grey90", "grey90", "grey70", "grey70", "grey70", "grey70"),
    xaxt = "n", yaxt = "n")
abline(h = 0); abline(h = 0); abline(h = 0, col = "white", lty = 2); box()
axis(side = 4, las = 2)
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(usr[2], usr[4] - ydiff * 0.1, latex2exp::TeX("$U^*_{0.3}$"), pos = 2, cex = 1.5)

### Ustar_0.1
out = summarize_common("Ustar_0.1")
head(out)
bp = boxplot(pe ~ method, data = out, plot = F)
bp$stats = sapply(sort(unique(out$method)), function(m) {
  sub = subset(out, method == m)
  StatonMisc::summ(sub$pe, p = c(0.025, 0.25, 0.5, 0.75, 0.975))[3:7]
})
bxp(z = bp, outline = F, las = 1, boxfill = c("grey90", "grey90", "grey70", "grey70", "grey70", "grey70"),
    xaxt = "n", yaxt = "n")
abline(h = 0); abline(h = 0); abline(h = 0, col = "white", lty = 2); box()
axis(side = 4, las = 2)
usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
text(usr[2], usr[4] - ydiff * 0.1, latex2exp::TeX("$U^*_{0.1}$"), pos = 2, cex = 1.5)
axis(side = 1, at = 1:6, labels = c("LM", "LME", "SSM-vm", "SSM-Vm", "SSM-vM", "SSM-VM"), las = 2)
mtext(side = 2, outer = T, "Proportional Error", line = 2, cex = 1.2)

dev.off()
