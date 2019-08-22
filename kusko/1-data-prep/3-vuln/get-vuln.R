
rm(list = ls(all = T))

# stocks in analysis: ordered from downstream to upstream
stocks = c("Kwethluk", "Kisaralik", "Tuluksak", 
         "Aniak", "Holokuk", "Oskawalik", "George",
         "Holitna", "Kogrukluk", "Swift", "Tatlawiksuk", "Takotna", "Pitka")

# create list to store the village names that each stock swims by
stock_villages = vector("list", length(stocks))
names(stock_villages) = stocks

# all substocks swim by these villages
all = c("Tuntutuliak", "Eek", "Kasigluk", "Nunapitchuk", "Atmautluak", "Napaskiak", "Napakiak",
        "Oscarville", "Bethel", "Kwethluk")

# enter which villages each stock swims by
stock_villages$Kwethluk = all
stock_villages$Kisaralik = all
stock_villages$Tuluksak = c(all, "Akiachak", "Akiak", "Tuluksak")
stock_villages$Aniak = c(all, "Akiachak", "Akiak", "Tuluksak", "Lower Kalskag", "Upper Kalskag", "Aniak")
stock_villages$Holokuk = c(all, "Akiachak", "Akiak", "Tuluksak", "Lower Kalskag", "Upper Kalskag", "Aniak", "Chuathbaluk")
stock_villages$Oskawalik = c(all, "Akiachak", "Akiak", "Tuluksak", "Lower Kalskag", "Upper Kalskag", "Aniak", "Chuathbaluk")
stock_villages$George = c(all, "Akiachak", "Akiak", "Tuluksak", "Lower Kalskag", "Upper Kalskag", "Aniak", "Chuathbaluk", "Crooked Creek")
stock_villages$Holitna = c(all, "Akiachak", "Akiak", "Tuluksak", "Lower Kalskag", "Upper Kalskag", "Aniak", "Chuathbaluk", "Crooked Creek", "Red Devil", "Sleetmute")
stock_villages$Kogrukluk = c(all, "Akiachak", "Akiak", "Tuluksak", "Lower Kalskag", "Upper Kalskag", "Aniak", "Chuathbaluk", "Crooked Creek", "Red Devil", "Sleetmute")
stock_villages$Swift = c(all, "Akiachak", "Akiak", "Tuluksak", "Lower Kalskag", "Upper Kalskag", "Aniak", "Chuathbaluk", "Crooked Creek", "Red Devil", "Sleetmute", "Stony River")
stock_villages$Tatlawiksuk = c(all, "Akiachak", "Akiak", "Tuluksak", "Lower Kalskag", "Upper Kalskag", "Aniak", "Chuathbaluk", "Crooked Creek", "Red Devil", "Sleetmute", "Stony River")
stock_villages$Takotna = c(all, "Akiachak", "Akiak", "Tuluksak", "Lower Kalskag", "Upper Kalskag", "Aniak", "Chuathbaluk", "Crooked Creek", "Red Devil", "Sleetmute", "Stony River", "Takotna", "McGrath")
stock_villages$Pitka = c(all, "Akiachak", "Akiak", "Tuluksak", "Lower Kalskag", "Upper Kalskag", "Aniak", "Chuathbaluk", "Crooked Creek", "Red Devil", "Sleetmute", "Stony River", "Takotna", "McGrath")

# which villages are we keeping? (there are some in the drainage that wouldn't harvest fish from the stocks in our analysis)
  # e.g., Nikolia, Telida, Lime Village
keep_villages = unique(unlist(stock_villages))

hh = read.csv("households_by_type_village_year.csv", stringsAsFactors = F)
hh = hh[hh$com %in% keep_villages,]
hh$tot = rowSums(hh[,c("not_usually", "usually")], na.rm = T)

hh = round(tapply(hh$tot, hh$com, mean))

thh = sum(hh)

t = 1
v = NULL
for (t in 1:length(stocks)) {
  keep_v = stock_villages[[t]]
  
  v[t] = sum(hh[keep_v])/thh
}

out = data.frame(stock = stocks, v = v, stringsAsFactors = F)

write.csv(out, "alt-vuln.csv", row.names = F)
