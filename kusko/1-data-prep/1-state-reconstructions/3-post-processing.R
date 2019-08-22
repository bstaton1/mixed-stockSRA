rm(list = ls(all = T))

source("1-data-prep.R")
post = readRDS("outputs/post.rds")

# the names of the output files
S_file_name = "outputs/S_Ests_Oct_18.csv"
H_file_name = "outputs/H_Ests_Oct_18.csv"
p_file_name = "outputs/P_Assess_Oct_18.csv"

# input what the observation error on weir S should be in the output file
weir_S_cv = 0.05

##### STEP 1: SUMMARIZE AND WRITE ESTIMATED FRACTION OF DRAINAGE-WIDE STOCK THAT IS MONITORED #####
p_acct = postpack::post_subset(post, "p_acct", matrix = T)
mean_p_acct_post = rowMeans(p_acct)
write.csv(data.frame(ave_p_assess = mean_p_acct_post), p_file_name, row.names = F)

##### STEP 2: SUMMARIZE AND WRITE THE HARVEST ACCOUNTED FOR BY THESE STOCKS #####
H_acct = postpack::post_summ(post, "H_acct")
H_ests = data.frame(year = years, mean = H_acct["mean",], sd = H_acct["sd",],
                    median = H_acct["50%",], lwr95CI = H_acct["2.5%",], upr95CI = H_acct["97.5%",],
                    cv = H_acct["sd",]/H_acct["mean",])
write.csv(H_ests, H_file_name, row.names = F)

##### STEP 3: SUMMARIZE AND WRITE THE ESCAPEMENT TO EACH STOCK IN THE ANALYSIS #####

# extract posterior samples
S_tot_post = postpack::post_subset(post, "S_tot", matrix = T)

### STEP 3A: HANDLE THE AERIAL SURVEYED SYSTEMS ###

# THE ANIAK, SWIFT, AND PITKA SYSTEMS ARE AGGREGATIONS OF VARIOUS
# PROJECTS: DEAL WITH THEM DIFFERENTLY. MUST ADD UP POSTERIOR SAMPLES FROM
# EACH PROJECT AND THEN SUMMARIZE THE SUM

# stock indices to be included in each aggregated system
aniak_ind = which(stocks_up %in% c("Salmon_Aniak", "Aniak", "Kipchuk"))
swift_ind = which(stocks_up %in% c("Cheeneetnuk", "Gagaryah"))
pitka_ind = which(stocks_up %in% c("Salmon_Pitka", "Bear", "Upper_Pitka_Fork"))

# columns in S_post object will end with this for the substocks of interest
aniak_endings = paste(",", aniak_ind, "]", sep = "")
swift_endings = paste(",", swift_ind, "]", sep = "")
pitka_endings = paste(pitka_ind, "]", sep = "")

# find those columns for each substock
cnames = colnames(S_tot_post)
aniak_systems_S = S_tot_post[,substr(cnames, nchar(cnames) - 2, nchar(cnames)) %in% aniak_endings]
swift_systems_S = S_tot_post[,substr(cnames, nchar(cnames) - 2, nchar(cnames)) %in% swift_endings]
pitka_systems_S = S_tot_post[,substr(cnames, nchar(cnames) - 2, nchar(cnames)) %in% pitka_endings]

# sum each sample of escapement across projects in each stock
aniak_offset = c(0, nyrs, nyrs * 2)
swift_offset = c(0, nyrs)
pitka_offset = c(0, nyrs, nyrs * 2)

aniak_sum_S = matrix(NA, nrow = nrow(S_tot_post), ncol = nyrs)
swift_sum_S = matrix(NA, nrow = nrow(S_tot_post), ncol = nyrs)
pitka_sum_S = matrix(NA, nrow = nrow(S_tot_post), ncol = nyrs)

for (y in 1:nyrs) {
  aniak_sum_S[,y] = rowSums(aniak_systems_S[,y + aniak_offset])
  swift_sum_S[,y] = rowSums(swift_systems_S[,y + swift_offset])
  pitka_sum_S[,y] = rowSums(pitka_systems_S[,y + pitka_offset])
}

# summarize
aniak_summ_S = apply(aniak_sum_S, 2, StatonMisc::summ)
swift_summ_S = apply(swift_sum_S, 2, StatonMisc::summ)
pitka_summ_S = apply(pitka_sum_S, 2, StatonMisc::summ)

# extract posterior summaries for other systems
kis_ind_S = paste("S_tot[", 1:nyrs, ",", which(stocks_up == "Kisaralik"), "]", sep = "")
hol_ind_S = paste("S_tot[", 1:nyrs, ",", which(stocks_up == "Holokuk"), "]", sep = "")
osk_ind_S = paste("S_tot[", 1:nyrs, ",", which(stocks_up == "Oskawalik"), "]", sep = "")
hlt_ind_S = paste("S_tot[", 1:nyrs, ",", which(stocks_up == "Holitna"), "]", sep = "")

S_tot = postpack::post_summ(post, "S_tot[")

kis_summ_S = S_tot[,kis_ind_S]
hol_summ_S = S_tot[,hol_ind_S]
osk_summ_S = S_tot[,osk_ind_S]
hlt_summ_S = S_tot[,hlt_ind_S]

# number of aerial survey only systems
n_sys = 7

# make an output object
output_S = data.frame(year = rep(years, n_sys), 
                      stock = rep(c("Kisaralik", "Aniak", "Holokuk", "Oskawalik", "Holitna", "Swift", "Pitka"), each = nyrs),
                      mean = NA, sd = NA, median = NA, lwr95CI = NA, upr95CI = NA)
post_cols = c("mean", "sd", "median", "lwr95CI", "upr95CI")

# store the posterior summaries in the appropriate locations
output_S[output_S$stock == "Kisaralik",post_cols] = t(kis_summ_S)
output_S[output_S$stock == "Aniak",post_cols] = t(aniak_summ_S)
output_S[output_S$stock == "Holokuk",post_cols] = t(hol_summ_S)
output_S[output_S$stock == "Oskawalik",post_cols] = t(osk_summ_S)
output_S[output_S$stock == "Holitna",post_cols] = t(hlt_summ_S)
output_S[output_S$stock == "Swift",post_cols] = t(swift_summ_S)
output_S[output_S$stock == "Pitka",post_cols] = t(pitka_summ_S)

output_S$cv = output_S$sd/output_S$mean

### STEP 8B: HANDLE THE WEIR SYSTEMS ###
weir_dat = read.csv("inputs/1_weir_data.csv")
weir_dat = weir_dat[,-which(colnames(weir_dat) == "Salmon_Aniak")]

weir_stocks = colnames(weir_dat)[2:ncol(weir_dat)]
n_weir_stocks = length(weir_stocks)

RR_ests = read.csv("inputs/4_total_run_esc.csv")
S_tot = RR_ests[,c("year", "S.tot")]
k_ests = read.csv("inputs/5_rr_k_params.csv")

# fill in NAs on weirs
for (s in 1:n_weir_stocks) {
  # extract air counts for this system
  weir_sub = weir_dat[,weir_stocks[s]]
  
  # store which years where not operated
  na_yrs = years[is.na(weir_sub)]
  
  # estimate what the project would have counted those years if operated
  est_counts = S_tot[S_tot$year %in% na_yrs,"S.tot"]/k_ests[k_ests$river == weir_stocks[s],"k"]
  
  # store it in the appropriate place
  weir_dat[weir_dat$Year %in% na_yrs,weir_stocks[s]] = round(est_counts)
}

weir_output = data.frame(year = rep(years, n_weir_stocks), stock = rep(weir_stocks, each = nyrs), 
                         mean = NA, sd = NA, median = NA, lwr95CI = NA, upr95CI = NA, cv = weir_S_cv)

for (s in 1:n_weir_stocks) {
  weir_output[weir_output$stock == weir_stocks[s],"mean"] = weir_dat[,weir_stocks[s]]
  weir_output[weir_output$stock == weir_stocks[s],"sd"] = weir_dat[,weir_stocks[s]] * weir_S_cv
  weir_output[weir_output$stock == weir_stocks[s],"median"] = weir_dat[,weir_stocks[s]]
  weir_output[weir_output$stock == weir_stocks[s],"lwr95CI"] = weir_dat[,weir_stocks[s]] - 1.96 * weir_dat[,weir_stocks[s]] * weir_S_cv
  weir_output[weir_output$stock == weir_stocks[s],"upr95CI"] = weir_dat[,weir_stocks[s]] + 1.96 * weir_dat[,weir_stocks[s]] * weir_S_cv
}

### STEP 8C: COMBINE WEIR SYSTEM ESTIMATES WITH AERIAL SURVEY SYSTEM ESTIMATES ###
all_output = rbind(weir_output, output_S)

### STEP 8D: INCLUDE AN OBSERVED FIELD #####
create_key = function(stocks, Sobs, new_name = NULL, p_need = 0.5) {
  
  mat = as.matrix(Sobs[,stocks])
  n_obs = apply(mat, 1, function(x) sum(!is.na(x)))
  
  data.frame(
    year = Sobs[,which(colnames(Sobs) %in% c("Year", "year"))],
    stock = ifelse(is.null(new_name), stocks[1], new_name),
    obs = ifelse((n_obs/ncol(mat)) >= p_need, 1, 0)
  )
}

# do the weir data
weir_dat = read.csv("inputs/1_weir_data.csv")
weir_stocks = c("Kwethluk", "Tuluksak", "Kogrukluk",
                "George", "Tatlawiksuk", "Takotna")
for (s in 1:length(weir_stocks)) {
  if (s == 1) key = NULL
  tmp = create_key(weir_stocks[s], weir_dat)
  key = rbind(key, tmp)
}

# do the aerial data
air_dat = read.csv("inputs/2_air_data.csv")
stocks = names(air_dat)[3:ncol(air_dat)]
air_stocks_new = c("Kisaralik", "Aniak", "Holokuk", "Oskawalik", "Holitna",
                   "Swift", "Pitka")
air_stocks = list(
  "Kisaralik",
  c("Aniak", "Kipchuk", "Salmon_Aniak"),
  "Holokuk",
  "Oskawalik",
  "Holitna",
  c("Cheeneetnuk", "Gagaryah"),
  c("Salmon_Pitka", "Bear", "Upper_Pitka_Fork")
)

for (s in 1:length(air_stocks)) {
  tmp = create_key(air_stocks[[s]], air_dat, new_name = air_stocks_new[s])
  key = rbind(key, tmp)
}

all_output = merge(all_output, key, by = c("stock", "year"))

### STEP 8D: WRITE THE OUTPUT! ###

write.csv(all_output, S_file_name, row.names = F)
