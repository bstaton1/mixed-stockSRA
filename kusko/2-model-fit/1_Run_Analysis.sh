#!/bin/bash

# this is a master control file meant to send several models to the ALHPC simultaneously
# it can also be executed on any *NIX command line system (or an emulator of such a system). If this is desired,
# comment out the run_script line and use the sh line in the loop below.
# BEWARE: this will **NOT** run the models simultaneously

# model 0 = lm and lme
# model 1 = SSM-vm
# model 2 = SSM-Vm
# model 3 = SSM-vM
# model 4 = SSM-VM

# maximum sleep time between sending models
maxsleep=10

# models to run
f=0
l=4
models=$(seq $f $l)

echo "############################################"
echo "########## Starting jobs ###################"
echo "############################################"

echo " "
echo "Models to run: $models"
echo " "

# loop through seeds: create and execute seed-specific programs
for model in ${models[@]}
do

  # create a copy of 2_Run_Fit.sh with the third line changed to the $model variable
  awk 'NR==3 {$0="model="'$model'""} { print }' 2_Run_Fit_alhpc.sh > 2_Run_Fit_alhpc_$model.sh
  
  # make the new file executable
  chmod +x 2_Run_Fit_alhpc_$model.sh
  
  # execute the temporary verison with specific seed
  # sh 2_Run_Fit_alhpc_$model.sh
  run_script 2_Run_Fit_alhpc_$model.sh
  
  # random sleep to avoid crashes
  r=$(($RANDOM % $maxsleep))
  echo "Random Sleep Time After Model $model: $r seconds"
  sleep $r
  
done
