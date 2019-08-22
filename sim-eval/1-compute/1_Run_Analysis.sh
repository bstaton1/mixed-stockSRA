#!/bin/bash

# this is a master control file meant to send several models/random data sets (seeds)
# to the ALHPC
# it can also be executed on any *NIX command line system (or an emulator of such a system). If this is desired,
# comment out the run_script line and use the sh line in the loop below.
# BEWARE: this will **NOT** run the models simultaneously

# model 0 = lm and lme
# model 1 = SSM-vm
# model 2 = SSM-Vm
# model 3 = SSM-vM
# model 4 = SSM-VM

# make sure you have created the necessary input files for each seed
# using 0c_Create_Inputs.sh

# maximum sleep time between iterations
maxsleep=10

echo "---------------------------------------"
echo "Please enter the model you wish to run:"
read model
echo "---------------------------------------"
echo "Please enter the first seed:"
read f
echo "---------------------------------------"
echo "Please enter the last seed:"
read l
echo "---------------------------------------"

# when ran on the HPC, include this to make R findable
# comment it out if not on HPC
echo "Loading R"
source /opt/asn/etc/asn-bash-profiles-special/modules.sh
module load R/3.3.3

seeds=$(seq $f $l)

echo "############################################"
echo "########## Starting jobs ###################"
echo "############################################"

echo " "
echo "Seeds to use: "
echo "${seeds[@]}"
echo " "

# loop through seeds: create and execute seed-specific programs
for seed in ${seeds[@]}
do

  # create a copy of 2_Run_SimFit.sh with the third line changed to the $seed variable and fourth line changed to the $model variable
  awk 'NR==3 {$0="seed="'$seed'""} { print }' 2_Run_Fit_alhpc.sh > cp_2_Run_Fit_alhpc_$seed.sh
  awk 'NR==4 {$0="model="'$model'""} { print }' cp_2_Run_Fit_alhpc_$seed.sh > cp_2_Run_Fit_alhpc_$model-$seed.sh

  # remove the intermediate file
  rm cp_2_Run_Fit_alhpc_$seed.sh

  # make the new file executable
  chmod +x cp_2_Run_Fit_alhpc_$model-$seed.sh

  # execute the temporary verison with specific seed
  # use sh if not on HPC, use run_script if on HPC
  # sh cp_2_Run_Fit_alhpc_$model-$seed.sh
  run_script 2_Run_Fit_alhpc_$model-$seed.sh

  # random sleep to avoid crashes
  r=$(($RANDOM % $maxsleep))
  echo "Random Sleep Time After Model $model with Seed $seed: $r seconds"
  sleep $r

done
