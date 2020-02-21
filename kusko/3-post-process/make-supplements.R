# a script for creating the online supplements for each SSM
rm(list = ls(all = T))

setwd("location/of/this/file")

# names of models
ids = c("vm", "Vm", "vM", "VM")

# do you want to use only a small number of the samples to generate the supplement?
  # useful in making changes to the code and seeing changes quickly
short = F

# turning off mcmc diagnostics can help with computation time
do_diagnostics = T

# loop through models and create output from the supplement template
starttime = Sys.time()
for (model in 1:4) {
  id = ids[model]
  cat("\r", "Rendering Output for SSM-", id, " (model ", model, "/4)", sep = "")
  
  dir.create("Model Files")
  rmarkdown::render(
    input = "supplement-template.Rmd", 
    output_file = paste0("Online Supplement ", LETTERS[model+1], ".html"),
    quiet = T
  ); detach(params)
  
  cat("\nDone.")
}
Sys.time() - starttime
