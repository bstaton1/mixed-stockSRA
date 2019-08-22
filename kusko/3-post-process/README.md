This subdirectory contains all of the code to analyze the output from the regression and state-space models fitted to Kuskokwim River Chinook salmon data, including the code used to generate the figures and tables used in the main text and various online supplements.

* `make-figs-tabs.R` contains code for all main-text figures
* `make-sa-figs-tabs.R` contains code for summarizing the findings of sensitivity analyses
* `supplement-template.R` contains code to generate a separate online supplement for each of the state-space models fitted to Kuskokwim data using the code in `supplement-template.Rmd`

All of these files read in `.rds` files that store the retained posterior samples from each model. These files are far too large to host on Github, but they are available upon request from the authors.

