
rm(list = ls(all = T))

# needed packages
suppressMessages(library(dplyr))
suppressMessages(library(reshape2))
source("0-functions.R")

# do you want to write the output?
write = T

# the range of years observed across all stocks
all_years = 1976:2017

# directories
asl_dir = "inputs/asl"
esc_dir = "inputs/daily-esc/"
out_dir = "outputs"

# create the output directory if it doesn't exist
if (!dir.exists(out_dir)) dir.create(out_dir)

# files
asl_files = dir(asl_dir)
esc_files = dir(esc_dir)

# the stock names in the correct order for this analysis
stocks = unname(sapply(asl_files, function(x) unlist(strsplit(x, "_"))[1]))

# the length of a temporal stratum (days)
stratum_length = 14

for (s in 1:length(stocks)) {
  
  cat("Crunching data for stock:", stocks[s], "\n")
  
  # start with escapement data
  tmp = read.csv(
    paste(esc_dir, esc_files[s], sep = "/"),
    stringsAsFactors = F
  )
  esc_raw = esc_data_prep(tmp)
  
  # then do asl data
  tmp = read.csv(
    paste(asl_dir, asl_files[s], sep = "/"),
    stringsAsFactors = F
  )
  asl_raw = asl_data_prep(tmp); rm(tmp)
  
  # get min and max observed days
  min_doy = min(c(asl_raw$doy, esc_raw$doy))
  max_doy = max(c(asl_raw$doy, esc_raw$doy))
  cat("DOY Range:", min_doy, "-", max_doy, "\n")
  
  # obtain a strata key
  strata_key = 
    create_strata_key(min_doy, max_doy, stratum_length) %>%
    rename(doy = x)
  
  # merge it with the escapement data set
  esc_raw = esc_raw %>%
    merge(strata_key, by = "doy") %>%
    arrange(year, doy)
  
  # merge it with the asl data set
  asl_raw = asl_raw %>%
    merge(strata_key, by = "doy") %>%
    arrange(year, doy, sex, age)
  
  # format again for analysis
  esc = esc_data_prep2(esc_raw)
  asl = asl_data_prep2(asl_raw)
  
  # apply weighted average calculations to each year separately
  out = t(sapply(all_years, function(x) get_wt_avg(yr = x, asl, esc)))
  
  # combine into a data.frame
  dat = data.frame(year = all_years, stock = stocks[s], out)
  colnames(dat) = c("year", "stock", "a4", "a5", "a6", "a7", "ess")
  
  # write the output
  if (write) {
    write.csv(dat, paste("outputs/", stocks[s], "_age_comps.csv", sep = ""), row.names = F)
  }
  
  head(dat); tail(dat)
}
