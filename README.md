# mixed-stockSRA

This repository stores the code and data used to perform the analysis presented in Staton et al.: _Evaluation of Methods for Quantifying Population Diversity in Mixed-stock Pacific Salmon Fisheries_

The analysis contained two primary phases: 

1. Application to the monitored Chinook salmon populations within Kuskokwim River, located in western Alaska (found in the `kusko` subdirectory)
2. Simulation-testing the methods applied to the Kuskokwim (found in the `sim-eval` subdirectory)

## Dependencies

All code and data to conduct the analysis are found in this repository, though the analyses rely on two other packages written specifically for this analysis: `SimSR` (handles the simulation of population dynamics and sampling of a system with similar characteristics as the Kuskokwim) and `FitSR` (handles the fitting of regression and state-space models, regardless of if the data were simulated or empirical). These packages were written by B. Staton, and can be installed using:

```R
devtools::install_github("bstaton1/FitSR")
```

`FitSR` installs `SimSR` automatically, as well as two other dependencies written by B. Staton: `postpack` (for streamlining posterior summarization) and `StatonMisc` (a random assortment of shortcut functions).

Program [JAGS](<http://mcmc-jags.sourceforge.net/>) will also be required to run the code in this repository, and the R package `jagsUI` (available on CRAN) is used to call JAGS from R, and miscellaneous other packages are used for data formatting and preparations (e.g., `dplyr`, `reshape2`, etc.)

