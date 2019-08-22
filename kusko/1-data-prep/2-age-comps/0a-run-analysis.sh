#!/bin/bash

slptime=1

echo "Performing Temporal Weighted Averaging"
echo "  on Each Stock to Get Age Composition each year"
Rscript 1-analysis.R
sleep $slptime

echo "Combining Multiple Output Files into One"
Rscript 2-combine-output.R
sleep $slptime

# echo "Creating Plots"
# Rscript 4-plotting.R
# sleep $slptime

echo "Done, if it worked correctly you will have the output stored in the outputs folder"