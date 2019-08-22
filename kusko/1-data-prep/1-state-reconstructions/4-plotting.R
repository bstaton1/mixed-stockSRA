rm(list = ls(all = T))

source("1-data-prep.R")
post = readRDS("outputs/post.rds")

out_dir = "outputs/fig-tab"

if (!dir.exists(out_dir)) dir.create(out_dir)

# function to create nice formatting for table output
parens = function(x) paste(x[1], "(", x[2], " -- ", x[3], ")", sep = "")

# determine the range of posterior CV's 
# S_tot = postpack::post_summ(post, "S_tot")
# S_tot_mean = postpack::array_format(S_tot["50%",])
# S_tot_sd = postpack::array_format(S_tot["sd",])
# S_tot_cv = S_tot_sd/S_tot_mean
# range(S_tot_cv)

### TABLE: POSTERIOR ESTIMATES OF SPATIAL EXPANSION FACTORS ###
pi_p = postpack::post_summ(post, "pi_p")
pi_up = postpack::post_summ(post, "pi_up")
psi_up = postpack::post_summ(post, "psi_up")

spat_expan_ests = data.frame(
  pi = apply(t(round(pi_up[c(3,4,5),], 2)), 1, parens),
  psi_p_1 = apply(t(round(psi_up[c(3,4,5),], 2) + 1), 1, parens)
)

write.csv(spat_expan_ests, file.path(out_dir, "spatial_expansion_estimates.csv"), row.names = F)

### TABLE: POSTERIOR ESTIMATES OF TEMPORAL EXPANSION FACTORS ###
temp_expan_ests = data.frame(
  b0 = parens(round(postpack::post_summ(post, "b0")[c(1,4,5)], 2)),
  b1 = parens(round(postpack::post_summ(post, "b1")[c(1,4,5)], 2)),
  sig = parens(round(postpack::post_summ(post, "sig_fit")[c(1,4,5)], 2))
)

write.csv(temp_expan_ests, file.path(out_dir, "temporal_expansion_estimates.csv"), row.names = F)

### FIGURE: TEMPORAL EXPANSION RELATIONSHIP ###

pred_y = postpack::post_summ(post, "pred_y[")
air_expansion_p = postpack::post_summ(post, "A_hat_p[")

lim = c(0,30000)

ppi = 600
png(file.path(out_dir, "obs-correct.png"), h = 5 * ppi, w = 5 * ppi, res = ppi)
par(xaxs = "i", yaxs = "i", mar = c(4,4,1,1))
plot(pred_y[1,] ~ pred_x, type = "l", ylim = lim, xlim = lim,
     xlab = "Spatially-Expanded Aerial Count (1000s)", ylab = "Weir Count (1000s)", xaxt = "n", yaxt = "n")
polygon(x = c(rev(pred_x), pred_x), y = c(rev(pred_y[4,]), pred_y[5,]), col = "grey90", border = NA)
lines(pred_y[4,] ~ pred_x, col = "grey", lty = 1)
lines(pred_y[5,] ~ pred_x, col = "grey", lty = 1)
lines(pred_y[1,] ~ pred_x, col = "black", lwd = 2)
points(weir_fish_p ~ air_expansion_p[1,], pch = 16, col = "black")
arrows(air_expansion_p[4,], weir_fish_p, air_expansion_p[5,], weir_fish_p, length = 0, col = "black")
abline(c(0,1), col = "grey", lwd = 2, lty = 2)
axis(side = 1, at = seq(0, 30000, 5000), labels = seq(0, 30, 5))
axis(side = 2, at = seq(0, 30000, 5000), labels = seq(0, 30, 5), las = 2)
box()
junk = dev.off(); rm(junk)

### FIGURE: PROPORTION OF DRAINAGE-WIDE ESCAPEMENT ACCOUNTED FOR IN THIS ANALYSIS ###
# proportion of drainage wide escapement accounted for
p_acct = postpack::post_summ(post, "p_acct")
S_acct = postpack::post_summ(post, "S_acct"); colnames(S_acct) = NULL
S_tot = postpack::post_subset(post, "S_tot", matrix = T)

png(file.path(out_dir, "obs-fraction.png"), h = 4 * ppi, w = 6 * ppi, res = ppi)
par(mar = c(2,4,1,1))
barplot(dw_S, col = "grey80", ylim = c(0, 350000), yaxt = "n", ylab = "Total Escapement (1000s)")
mp = barplot(S_acct[1,], add = T, col = "grey60", ylim = c(0, 350000), yaxt = "n")
arrows(mp, S_acct[4,], mp, S_acct[5,], lwd = 1.5, length = 0)
axis(side = 2, at = seq(0, 350000, 50000), labels = seq(0, 350, 50), las = 2)

at.x = seq(min(years),max(years),5)
which.x = which(years %in% at.x)

axis(side = 1, at = mp[which.x], labels = at.x)
usr = par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
legend("topright", legend = c("Drainage-Wide", "This Analysis"), fill = c("grey80", "grey60"), cex = 0.8, bty = "n")
junk = dev.off(); rm(junk)

### FIGURE: HOW FREQUENTLY WAS EACH SUBSTOCK SAMPLED? ###
S = read.csv("outputs/S_Ests_Oct_18.csv")

obs = reshape2::dcast(S, year ~ stock, value.var = "obs")

obsx = obs[,-1]

weirs = c("Kogrukluk", "Tuluksak", "George", "Tatlawiksuk", "Takotna", "Kwethluk")

ordered_stocks = names(rev(sort(colSums(obsx))))
p_obs = rev(sort(colSums(obsx)))/nrow(obs)

jpeg(file.path(out_dir, "obs-freq.jpg"), h = 5 * ppi, w = 5 * ppi, res = ppi)
par(mar = c(6,4,1,1))
plot(1,1, type = "n", xlim = c(0.5, 13.5),
     ylim = c(2018, 1976), yaxt = "n", xaxt = "n", xlab = "", ylab = "")
for (s in 1:length(ordered_stocks)) {
  points(obs[,"year"] ~ rep(s, nrow(obs)),
         pch = ifelse(obs[,ordered_stocks[s]] == 0, NA, 16),
         col = ifelse(ordered_stocks[s] %in% weirs, "black", "grey"))
}
abline(v = max(which(p_obs >= 0.5)) + 0.5)
axis(side = 2, at = seq(1976, 2018, 6), labels = seq(1976, 2018, 6), las = 1)
axis(side = 1, at = 1:13, labels = ordered_stocks, las = 2)
legend("topright", legend = c("Weir", "Aerial"), pch = 16, col = c("black", "grey"), cex = 0.8)
junk = dev.off(); rm(junk)
