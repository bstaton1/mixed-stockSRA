
##### CREATE STRATA KEY FROM A RANGE #####
create_strata_key = function(min_x, max_x, len) {

  # number of total x elements (e.g., days)
  n_x = length(min_x:max_x)
  
  # the starting and ending days of each stratum
  s_start = seq(min_x, max_x, len)
  s_end = c(seq(min_x + len, max_x, len), max_x)
  
  # number of strata
  n_s = length(s_start)
  
  # build the key
  strata_key = data.frame(x = min_x:max_x)
  strata_key$stratum = NULL
  
  for (i in 1:n_x) {
    for (j in 1:n_s) {
      if (strata_key[i,"x"] >= s_start[j] & strata_key[i,"x"] < s_end[j]) strata_key[i,"stratum"] = j
    }
  }
  
  strata_key[n_x,"stratum"] = n_s
  
  strata_key
}

##### PREPARE THE RAW ESCAPEMENT FILES #####
esc_data_prep = function(dat) {
  
  # reformat dates
  dat$date = paste(dat$date, dat$year, sep = "/")
  
  # big data manipulation
  dat %>% 
    # remove other species and keep only years in range
    filter(species == "Chinook" & year %in% all_years) %>%
    # create the doy variable
    group_by(year) %>%
    mutate(doy = StatonMisc::date2doy(date)) %>%
    ungroup %>% 
    # get a total passage estimate, estimates + observed
    group_by(year, doy) %>%
    summarise(count = sum(count, na.rm = T)) %>%
    ungroup
}

##### PREPARE THE RAW ASL FILES
asl_data_prep = function(dat) {
  dat %>%
    # remove other species and keep only years in range
    filter(species == "Chinook" & year %in% all_years) %>%
    # create a day of the year variable
    group_by(year) %>%
    mutate(doy = StatonMisc::date2doy(date)) %>%
    ungroup %>%
    # remove fish that weren't aged successfully
    filter(!is.na(fw_age) & !is.na(sw_age)) %>%
    # at a total age variable
    mutate(age = fw_age + sw_age + 1) %>%
    # keep only the variables we are interested in
    select(year, doy, age, sex, length) %>%
    # keep only ages we are interested in
    filter(age %in% 4:7)
}

##### PREPARE THE PREP-ED ESCAPEMENT DATA #####
esc_data_prep2 = function(esc) {
  # calculates counts by year and strata
  esc = esc %>% 
    group_by(year, stratum) %>% 
    summarise(count = sum(count)) %>%
    dcast(year ~ stratum, value.var = "count")
  esc[is.na(esc)] = 0
  esc
}

##### PREPARE THE PREP-ED ASL DATA #####
asl_data_prep2 = function(asl) {
  # calculate age composition by year and stratum
  asl = asl %>%
    group_by(year, stratum, age) %>%
    summarize(n_age = n()) %>%
    ungroup %>%
    group_by(year, stratum) %>%
    mutate(n_tot = sum(n_age)) %>%
    ungroup() %>%
    mutate(age_comp = round(n_age/n_tot, 2))
  
  comp = asl %>%
    dcast(year + stratum ~ age, value.var = "age_comp")
  
  ess = asl %>% 
    dcast(year + stratum ~ age, value.var = "n_age") %>%
    select(-year) %>% select(-stratum) %>% rowSums(na.rm = T)
  
  comp$ess = ess
  comp[is.na(comp)] = 0
  comp
}

##### PERFORM THE TEMPORAL WEIGHTED AVERAGE #####
get_wt_avg = function(yr, asl, esc) {
  if ((yr %in% esc$year & yr %in% asl$year)) {
    asl_strata = filter(asl, year == yr) %>%
      select(stratum) %>%
      unlist %>% unname %>%
      unique
    
    ess_tot = asl %>% filter(year == yr) %>%
      select(ess) %>% unlist %>% unname %>% sum(na.rm = T)
    
    esc_j = filter(esc, year == yr) %>%
      select(-year) %>% select(asl_strata) %>% unlist
    
    pi_j = esc_j/sum(esc_j)
    
    age_comp_j = filter(asl, year == yr) %>%
      select(-year) %>% select(-ess) %>%
      melt(id.vars = "stratum", variable.name = "age", value.name = "comp") %>%
      dcast(age ~ stratum, value.var = "comp") %>% select(-age)
    
    out = apply(age_comp_j, 1, function(x) sum(x * pi_j))
    out = out/sum(out)
    out = c(out, ess_tot)
  } else {
    out = c(rep(NA, 4), 0)
  }
  
  out
}
