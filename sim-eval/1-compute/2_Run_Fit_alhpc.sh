#!/bin/bash

# seed goes here - this line will be replaced with awk when copied
# model goes here - this line will be replaced with awk when copied

# this file is meant to be used on the AL HPC

# sleep for a random amount of time
  # this is in case two jobs start at the exact same time
  # it will cause it to crash with a module loading error
r=$(($RANDOM % 10))
echo "Random Sleep Time Before Running Model $model on Seed $seed: $r seconds"
sleep $r

# when ran on the HPC, include this to make R findable
# if running on a local machine comment out these lines
# echo "Loading R"
# source /opt/asn/etc/asn-bash-profiles-special/modules.sh
# module load R/3.3.3

# get the date/time of the start of this script
d=$(date)

# $1 is the random seed provided by 1_Run_Analysis.sh
echo "+++++++++++++++++++++++++++++++++++++++++++++++++++"
echo "Running Model $model with Seed $seed"
echo "  Started running at: $d"
Rscript 3_Fit.R $model $seed

# print the end date and time of this script
d=$(date)
echo "  Ended running at:   $d"
echo "+++++++++++++++++++++++++++++++++++++++++++++++++++"
