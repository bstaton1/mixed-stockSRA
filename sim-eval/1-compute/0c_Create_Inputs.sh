#!/bin/bash

# ask the user for the input seed range
echo "---------------------------------------"
echo "Please enter the first seed:"
read f
echo "---------------------------------------"
echo "Please enter the last seed:"
read l
echo "---------------------------------------"

# create the seeds array
seeds=$(seq $f $l)

# loop over seeds, create and save the input corresponding to each
for seed in ${seeds[@]}
do
  Rscript 0b_data_sim.R $seed
done
