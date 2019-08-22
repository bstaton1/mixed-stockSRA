#!/bin/bash

slptime=1

echo "Preparing Data and Fitting Model"
Rscript 2-fit-model.R
sleep $slptime

echo "Post-Processing MCMC Output"
Rscript 3-post-processing.R
sleep $slptime

echo "Creating Plots"
Rscript 4-plotting.R
sleep $slptime

echo "Done, if it worked correctly you will have the output stored in the outputs folder"