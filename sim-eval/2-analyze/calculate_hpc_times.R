
# THIS SCRIPT SUMMARIZES HOW LONG IT TOOK THE DIFFERENT MODELS TO FIT
# ON THE HPC

job_info_dir = "C:/Users/bstaton/Desktop/Staton/2_kusko/analyses/z-output-storage/multi-sra/sim-fit-outputs/job-info"

##### FUNCTION TO HELP WITH THIS EFFORT #####
convert_hpc_duration = function(x, units = "days") {
  # handle days
  if (stringr::str_detect(x, "-")) {
    x_split = unlist(strsplit(x, "-"))
    days = as.numeric(x_split[1])
    x = x_split[2]
  } else {
    days = 0
  }
  
  # handle hours, minutes, seconds
  x_split = unlist(strsplit(x, ":"))
  hours = as.numeric(x_split[1])
  minutes = as.numeric(x_split[2])
  seconds = as.numeric(x_split[3])
  
  if (units == "days") {
    out = days + hours/24 + minutes/1440 + seconds/86400
  }
  
  if (units == "hours") {
    out = days * 24 + hours + minutes/60 + seconds/3600
  }
  
  if (units == "minutes") {
    out = days * 1440 + hours * 60 + minutes + seconds/60
  }
  
  return(out)
  
}

##### READ IN AND FORMAT THE JOB INFO #####
x1 = read.csv(file.path(job_info_dir, "mod-jobs.csv"), stringsAsFactors = F)
x2 = read.csv(file.path(job_info_dir, "mod-jobs2.csv"), stringsAsFactors = F)
x3 = read.csv(file.path(job_info_dir, "mod-jobs3.csv"), stringsAsFactors = F)

x = rbind(x1, x2, x3)

x0 = subset(x, model == 0)  # REGRESSION MODELS
x1 = subset(x, model == 1)  # SSM-vm
x2 = subset(x, model == 2)  # SSM-Vm
x3 = subset(x, model == 3)  # SSM-vM
x4 = subset(x, model == 4)  # SSM-VM

x0$duration = unname(sapply(x0$time_elapse, convert_hpc_duration, units = "minutes"))
x1$duration = unname(sapply(x1$time_elapse, convert_hpc_duration, units = "days"))
x2$duration = unname(sapply(x2$time_elapse, convert_hpc_duration, units = "days"))
x3$duration = unname(sapply(x3$time_elapse, convert_hpc_duration, units = "days"))
x4$duration = unname(sapply(x4$time_elapse, convert_hpc_duration, units = "days"))

xssm = rbind(x1, x2, x3, x4)

# how many fitted successfully by each model?
tapply(xssm$duration, xssm$model, function(x) sum(x > 0.1))

# summaries by model
round(with(xssm[xssm$duration > 0.1,], tapply(duration, model, min)), 1)
round(with(xssm[xssm$duration > 0.1,], tapply(duration, model, mean)), 1)
round(with(xssm[xssm$duration > 0.1,], tapply(duration, model, max)), 1)
round(with(xssm[xssm$duration > 0.1,], tapply(duration, model, max)), 1)

# how long would it have taken if each job was ran back-to-back (total compute time, not by processor)
sum(xssm[xssm$duration > 0.1,"duration"])/365
