
## helper utility functions

fileName = function(base, sub, ext) {
  sep = if(is.null(sub)) NULL else "_"
  paste(base, sep, sub, ext, sep = "")
}

end_timer = function(start, ctime) {
  end = Sys.time(); elapsed = round(as.numeric(end - start, units = "hours"), 2)
  ctime = sum(c(ctime, elapsed))
  if (time_verbose) cat("    Hours Elapsed: ", elapsed, "; Total Hours Elapsed: ", ctime, "\n", sep = "")
  
  ctime
}


random_sleep = function(minS = 15, maxS = 200) {
  seconds = runif(1, minS, maxS)
  cat("  Random Wait Time: ", round(seconds), " Seconds\n", sep = "")
  Sys.sleep(seconds)
}

fit_model = function(jags_data, jags_inits = NULL, jags_params, model_file, dims, parallel, error = F) {
  if (error) {
    jagsUI::jags.basic(
      data = jags_data,
      inits = jags_inits,
      parameters.to.save = jags_params,
      model.file = model_file,
      n.chains = dims["nc"],
      n.adapt = dims["na"],
      n.iter = sum(dims[c("ni", "nb")]),
      n.burnin = dims["nb"],
      n.thin = dims["nt"],
      parallel = parallel,
      verbose = ifelse(parallel, F, T),
      save.model = F
    )
  } else {
    tryCatch({
      jagsUI::jags.basic(
        data = jags_data,
        inits = jags_inits,
        parameters.to.save = jags_params,
        model.file = model_file,
        n.chains = dims["nc"],
        n.adapt = dims["na"],
        n.iter = sum(dims[c("ni", "nb")]),
        n.burnin = dims["nb"],
        n.thin = dims["nt"],
        parallel = parallel,
        verbose = ifelse(parallel, F, T),
        save.model = F
      )},
      error = function(e) NULL
    )
  }
}


