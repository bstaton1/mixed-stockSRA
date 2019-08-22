
# script to combine multiple data files into one

rm(list = ls(all = T))

# the location of the raw output files (those for each individual stock)
out_dir = "outputs"

# extract their names
raw_files = dir(out_dir)[stringr::str_detect(dir(out_dir), pattern = "_age_comps.csv")]

# loop through each, read it in, calculate scale frequencies instead of compositions, then combine
out = NULL
for (s in 1:length(raw_files)) {
  # read in age data for this stock
  tmp = read.csv(file.path(out_dir, raw_files[s]))
  
  # determine which columns to keep
  keep = c(paste("a", 4:7, sep = ""), "ess")
  
  # calculate the scale frequencies
  x = t(apply(tmp[,keep], 1, function(x) round(x[keep[1:4]] * x["ess"])))
  # x[is.na(x)] = 0
  n = rowSums(x, na.rm = T)
  
  # create updated data.frame()
  tmp = data.frame(tmp[,c("year", "stock")], x, n)
  out = rbind(out, tmp)
}

write.csv(out, file.path(out_dir, "Age_Comp_Data.csv"), row.names = F)
