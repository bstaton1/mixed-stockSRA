##### STEP 1: PREPARE DATA FOR PAIRED SYSTEMS #####

### STEP 1A: COUNT DATA (PAIRED SYSTEMS ONLY) ###

# read raw count data
weir_dat = read.csv("inputs/1_weir_data.csv")
air_dat = read.csv("inputs/2_air_data.csv")

# all years
years = air_dat$year
nyrs = length(years)

# stocks with paired weir/aerial counts
stocks_p = c("Kwethluk", "Salmon_Aniak", "Kogrukluk")

air_dat = air_dat[,stocks_p]
weir_dat = weir_dat[,stocks_p]

for (s in 1:length(stocks_p)) {
  # determine which year indices have paired counts
  shared_ind = which(!is.na(air_dat[,stocks_p[s]]) & !is.na(weir_dat[,stocks_p[s]]))
  
  # extract those years for air counts
  temp_air = air_dat[shared_ind,stocks_p[s]]
  
  # extract those years for weir counts
  temp_weir = weir_dat[shared_ind,stocks_p[s]]
  
  temp_stock = rep(s, length(temp_air))
  
  # if this is the first stock, make the output
  if (s == 1) {
    air_fish_p = temp_air
    weir_fish_p = temp_weir
    stock_fish_p = temp_stock
  } else { # otherwise, append the output
    air_fish_p = c(air_fish_p, temp_air)
    weir_fish_p = c(weir_fish_p, temp_weir)
    stock_fish_p = c(stock_fish_p, rep(s, length(temp_air)))
  }
}

n_obs_fish_p = length(stock_fish_p)

### STEP 1B: PREPARE TAG DATA (PAIRED SYSTEMS ONLY) ###
# read raw data
tags = read.csv("inputs/3_tags.csv")

tags_c_p = NULL # num. tags in counted reaches
tags_u_p  = NULL       # num. tags in uncounted reaches
stock_tags_p = NULL    # stock identifier

for (s in 1:length(stocks_p)) {
  # extract tags in each year that were in aerial survey reaches
  temp_tags_c = tags[tags$river == stocks_p[s] & !is.na(tags$counted.reaches),"counted.reaches"]
  
  # extract tags in each year that were in non-aerial survey reaches
  temp_tags_u = tags[tags$river == stocks_p[s] & !is.na(tags$counted.reaches),"uncounted.reaches"]
  
  # create a stock idendifier
  temp_stock = rep(s, length(temp_tags_c))
  
  # combine with data from previous stocks
  tags_c_p = c(tags_c_p, temp_tags_c)
  tags_u_p = c(tags_u_p, temp_tags_u)
  stock_tags_p = c(stock_tags_p, temp_stock)
}

tags_n_p = tags_c_p + tags_u_p  # total tags in stock/year
n_obs_tags_p = length(tags_n_p) # number of paired stock/year tag observations

##### STEP 2: PREPARE DATA FOR UNPAIRED SYSTEMS #####

### STEP 2A: COUNT DATA ###

air_dat = read.csv("inputs/2_air_data.csv")

stocks_up = c("Kisaralik", "Salmon_Aniak", "Aniak", "Kipchuk", "Holokuk",
                    "Oskawalik", "Holitna", "Cheeneetnuk", "Gagaryah",
                    "Salmon_Pitka", "Bear", "Upper_Pitka_Fork")
n_stocks_up = length(stocks_up)
air_dat = air_dat[,c("year", stocks_up)]

# save which years were operated for each project: 0 is no, 1 is yes
operated = matrix(NA, nyrs, n_stocks_up + 1)
colnames(operated) = colnames(air_dat)
operated[is.na(air_dat)] = 0
operated[!is.na(air_dat)] = 1
operated[,1] = air_dat[,"year"]

# fill in years that weren't operated with estimates using run reconstruction estimates
RR_ests = read.csv("inputs/4_total_run_esc.csv")
S_tot = RR_ests[,c("year", "S.tot")]
k_ests = read.csv("inputs/5_rr_k_params.csv")

for (s in 1:n_stocks_up) {
  # extract air counts for this system
  air_sub = air_dat[,stocks_up[s]]

  # store which years where not operated
  na_yrs = years[is.na(air_sub)]

  # estimate what the project would have counted those years if operated
  est_counts = S_tot[S_tot$year %in% na_yrs,"S.tot"]/k_ests[k_ests$river == stocks_up[s],"k"]

  # store it in the appropriate place
  air_dat[air_dat$year %in% na_yrs,stocks_up[s]] = round(est_counts)
}
air_dat = air_dat[-1] # removes year column

# do the same for the weirs (only those included in the analysis)
weir_dat = read.csv("inputs/1_weir_data.csv")
weir_stocks = c("George", "Kogrukluk", "Kwethluk", "Takotna", "Tatlawiksuk", "Tuluksak")
weir_dat = weir_dat[,c("Year", weir_stocks)]
n_weir_stocks = length(weir_stocks)

for (s in 1:n_weir_stocks) {
  # extract weir counts for this system
  weir_sub = weir_dat[,weir_stocks[s]]

  # store which years where not operated
  na_yrs = years[is.na(weir_sub)]

  # estimate what the project would have counted those years if operated
  est_counts = S_tot[S_tot$year %in% na_yrs,"S.tot"]/k_ests[k_ests$river == weir_stocks[s],"k"]

  # store it in the appropriate place
  weir_dat[weir_dat$Year %in% na_yrs, weir_stocks[s]] = round(est_counts)
}

# sum up the total that would have been counted by weirs if all would have operated each year
weir_tot = rowSums(weir_dat[,weir_stocks])

# ### STEP 2B: TAG DATA ###
tags = read.csv("inputs/3_tags.csv")

tags_c_up = NULL
tags_u_up = NULL
stock_tags_up = NULL
for (s in 1:n_stocks_up) {
  # extract tags in each year that were in aerial survey reaches
  temp_tags_c = tags[tags$river == stocks_up[s] & !is.na(tags$counted.reaches),"counted.reaches"]
  
  # extract tags in each year that were in non-aerial survey reaches
  temp_tags_u = tags[tags$river == stocks_up[s] & !is.na(tags$counted.reaches),"uncounted.reaches"]
  
  # create a stock idendifier
  temp_stock = rep(s, length(temp_tags_c))
  
  tags_c_up = c(tags_c_up, temp_tags_c)
  tags_u_up = c(tags_u_up, temp_tags_u)
  stock_tags_up = c(stock_tags_up, temp_stock)
}

tags_n_up = tags_c_up + tags_u_up
n_obs_tags_up = length(tags_n_up)

##### STEP 3: BUNDLE DATA TO PASS TO JAGS MODEL #####
# to plot a fitted regression line
pred_x = seq(0, 40000, 5000)
n_pred = length(pred_x)

jags.dat = list(
  
  ### PAIRED SYSTEMS DATA ###
  # COUNT DATA
  air_fish_p = air_fish_p,        # raw aerial counts
  weir_fish_p = weir_fish_p,      # weir counts
  stock_fish_p = stock_fish_p,    # stock identifier
  n_obs_fish_p = n_obs_fish_p,    # number of paired weir/aerial counts
  np = max(stock_fish_p),         # number of paired weir/aerial systems
  
  # TAG DATA
  tags_u_p = tags_u_p,            # number of tags in uncounted reaches
  tags_n_p = tags_n_p,            # number of tags in counted + uncounted reaches
  stock_tags_p = stock_tags_p,    # stock identifier
  n_obs_tags_p = n_obs_tags_p,    # number of observations
  
  # USED IN FITTED MODEL PLOTTING
  pred_x = pred_x,
  n_pred = n_pred,
  
  ### UNPAIRED SYSTEMS DATA ###
  # COUNT DATA
  air_fish_up = as.matrix(air_dat),
  nup = max(stock_tags_up),
  
  # TAG DATA
  tags_u_up = tags_u_up,            # number of tags in uncounted reaches
  tags_n_up = tags_n_up,            # number of tags in counted + uncounted reaches
  stock_tags_up = stock_tags_up,    # stock identifier
  n_obs_tags_up = n_obs_tags_up,    # number of observations
  
  ### DRAINAGE-WIDE ESTIMATES ###
  dw_H = RR_ests$N.tot - RR_ests$S.tot,
  dw_S = RR_ests$S.tot,
  weir_tot = weir_tot,
  nyrs = length(weir_tot)
)

# bring objects into workspace: essentially attaches it
for (i in 1:length(jags.dat)) {
  assign(x = names(jags.dat)[i], value = jags.dat[[i]])
}
