This directory contains the code for simulation-testing the models in the analysis. 

* The `1-compute` subdirectory contains the code for generating simulated data (leaning heavily on the `SimSR` [package](<https://github.com/bstaton1/SimSR>)) and fitting the six assessed spawner-recruit models to them (leaning heavily on the `FitSR` [package](<https://github.com/bstaton1/FitSR>)).
* The `2-analyze` subdirectory contains the code for analyzing the posterior estimates obtained in `1-compute` against the true parameters to assess the precision and accuracy of the models. All plotting code is contained here as well.

