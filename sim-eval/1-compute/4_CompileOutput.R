# take the many separate .csv files and combine them into saved R objects
rm(list = ls(all = T))

extract_numbers = function(string, follow = NULL, as.num = F) {
  x = unlist(regmatches(string, gregexpr(paste("[[:digit:]]+", follow, sep = ""), string)))
  if (as.num) {
    as.numeric(x)
  } else {
    x
  }
}

# the main folder that houses the input and output
in_folder = "inputs"
out_folder = "outputs"

# the full file path there
in_dir = file.path(getwd(), in_folder)
out_dir = file.path(getwd(), out_folder)

# the individual file names for each type
in_files = dir(in_dir)
out_files = dir(out_dir)

# the first three characters of file names of each type
in_first3 = substr(in_files, 1, 3)
in_last3 = substr(in_files, nchar(in_files) - 2, nchar(in_files))

# the last three characters of the file names of each type
out_first3 = substr(out_files, 1, 3)
out_last3 = substr(out_files, nchar(out_files) - 2, nchar(out_files))

# extract the names of the files for inputs, and for output files of each type
param_files = in_files[in_first3 == "inp" & in_last3 == "csv"]
lme_files = out_files[out_first3 == "lme" & out_last3 == "csv"]
ssm_files = out_files[out_first3 == "ssm" & out_last3 == "csv"]
ssm_1_files = ssm_files[substr(ssm_files, 5, 5) == 1]
ssm_2_files = ssm_files[substr(ssm_files, 5, 5) == 2]
ssm_3_files = ssm_files[substr(ssm_files, 5, 5) == 3]
ssm_4_files = ssm_files[substr(ssm_files, 5, 5) == 4]

# the seeds that were saved for each type
param_seeds = extract_numbers(param_files, as.num = T)
lme_seeds = extract_numbers(lme_files, as.num = T)
ssm_1_seeds = extract_numbers(extract_numbers(ssm_1_files, follow = ".csv"), as.num = T)
ssm_2_seeds = extract_numbers(extract_numbers(ssm_2_files, follow = ".csv"), as.num = T)
ssm_3_seeds = extract_numbers(extract_numbers(ssm_3_files, follow = ".csv"), as.num = T)
ssm_4_seeds = extract_numbers(extract_numbers(ssm_4_files, follow = ".csv"), as.num = T)

# the seeds saved across all types
seeds = sort(unique(c(lme_seeds, ssm_1_seeds, ssm_2_seeds, ssm_3_seeds, ssm_4_seeds, param_seeds)))
n = length(seeds)

# loop over these unique seeds:
lme_summ = NULL
param_summ = NULL
ssm_1_summ = NULL
ssm_2_summ = NULL
ssm_3_summ = NULL
ssm_4_summ = NULL

for (i in 1:n) {
  
  cat("\r", i, "/", n, sep = "")
  
  if (seeds[i] %in% lme_seeds) {
    lme_tmp = read.csv(paste(out_dir, lme_files[lme_seeds == seeds[i]], sep = "/"), stringsAsFactors = F)
    lme_tmp$iter = i
  } else {
    lme_tmp = data.frame(seed = seeds[i], 
                         param = NA, stock = NA, method = c("lme", "lm"),
                         mean = NA, sd = NA, "X50." = NA, "X2.5." = NA, "X97.5." = NA, 
                         bgr = NA, ess = NA, iter = i)
  }
  
  if (seeds[i] %in% ssm_1_seeds) {
    ssm_1_tmp = read.csv(paste(out_dir, ssm_1_files[ssm_1_seeds == seeds[i]], sep = "/"), stringsAsFactors = F)
    ssm_1_tmp$iter = i
  } else {
    ssm_1_tmp = data.frame(seed = seeds[i], 
                           param = NA, stock = NA, year = NA, method = "ssm1",
                           mean = NA, sd = NA, "X50." = NA, "X2.5." = NA, "X97.5." = NA, 
                           bgr = NA, ess = NA, iter = i)
  }
  
  if (seeds[i] %in% ssm_2_seeds) {
    ssm_2_tmp = read.csv(paste(out_dir, ssm_2_files[ssm_2_seeds == seeds[i]], sep = "/"), stringsAsFactors = F)
    ssm_2_tmp$iter = i
  } else {
    ssm_2_tmp = data.frame(seed = seeds[i], 
                           param = NA, stock = NA, year = NA, method = "ssm2",
                           mean = NA, sd = NA, "X50." = NA, "X2.5." = NA, "X97.5." = NA, 
                           bgr = NA, ess = NA, iter = i)
  }
  
  if (seeds[i] %in% ssm_3_seeds) {
    ssm_3_tmp = read.csv(paste(out_dir, ssm_3_files[ssm_3_seeds == seeds[i]], sep = "/"), stringsAsFactors = F)
    ssm_3_tmp$iter = i
  } else {
    ssm_3_tmp = data.frame(seed = seeds[i], 
                           param = NA, stock = NA, year = NA, method = "ssm3",
                           mean = NA, sd = NA, "X50." = NA, "X2.5." = NA, "X97.5." = NA, 
                           bgr = NA, ess = NA, iter = i)
  }
  
  if (seeds[i] %in% ssm_4_seeds) {
    ssm_4_tmp = read.csv(paste(out_dir, ssm_4_files[ssm_4_seeds == seeds[i]], sep = "/"), stringsAsFactors = F)
    ssm_4_tmp$iter = i
  } else {
    ssm_4_tmp = data.frame(seed = seeds[i], 
                           param = NA, stock = NA, year = NA, method = "ssm4",
                           mean = NA, sd = NA, "X50." = NA, "X2.5." = NA, "X97.5." = NA, 
                           bgr = NA, ess = NA, iter = i)
  }
  
  if (seeds[i] %in% param_seeds) {
    param_tmp = read.csv(paste(in_dir, param_files[param_seeds == seeds[i]], sep = "/"), stringsAsFactors = F)
    param_tmp$iter = i
  } else {
    param_tmp = data.frame(seed = seeds[i], stock = NA, param = NA, value = NA, iter = i)
  }
  
  lme_summ = rbind(lme_summ, lme_tmp)
  param_summ = rbind(param_summ, param_tmp)
  ssm_1_summ = rbind(ssm_1_summ, ssm_1_tmp)
  ssm_2_summ = rbind(ssm_2_summ, ssm_2_tmp)
  ssm_3_summ = rbind(ssm_3_summ, ssm_3_tmp)
  ssm_4_summ = rbind(ssm_4_summ, ssm_4_tmp)
  
}

# save the output as R objects
saveRDS(param_summ, file = file.path(out_dir, "param_summ.rds"))
saveRDS(lme_summ, file = file.path(out_dir, "lme_summ.rds"))
saveRDS(ssm_1_summ, file = file.path(out_dir, "ssm_1_summ.rds"))
saveRDS(ssm_2_summ, file = file.path(out_dir, "ssm_2_summ.rds"))
saveRDS(ssm_3_summ, file = file.path(out_dir, "ssm_3_summ.rds"))
saveRDS(ssm_4_summ, file = file.path(out_dir, "ssm_4_summ.rds"))
