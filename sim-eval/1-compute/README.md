This subdirectory contains all code for running the simulation of data and estimation of parameters in the analysis. Because of the long model run times for state-space models, this code is intended to be ran on a high performance computing system, in particular one provided by the Alabama Supercomputer Authority. If desired, the code could be edited to run on an individual computer, or on another HPC - this is provided as an example of how it was done for this circumstance. 

A mixture of shell (`*.sh`) and R (`*.R`) scripts are provided. To generate data for some random seeds, open a bash terminal, navigate to this directory, and execute:

```bash
sh 0c_Create_Inputs.sh
```

You will be prompted to enter the first and last seed of random data sets to create. This structure allows the 6 assessment models to be tested on the exact same data sets.

Then, execute:

```bash
sh 1_Run_Analysis.sh
```

And you will be prompted for a model number, and the seeds to run. Each seed is ran as a separate HPC job, allowing many iterations to be fitted simultaneously. You may ignore the `2_Run_Fit_alhpc.sh` file, it is the job file that is sent to the HPC that calls `3_Fit.R` via the shell command `Rscript`  with the `$model` and `$seed` variables defined as command line arguments.

To perform a run of individual models without a HPC, you may run:

```bash
Rscript 3_Fit.R 1 2
```

To fit model 1 (SSM-vm) to the data set simulated when the seed was set to 2. Make sure you have created the necessary input files for seed 2 _first_.

The script `z_DeleteTmp.sh`  is used to remove the intermediate copies of `2_Run_Fit_alhpc.sh` made when executing `1_Run_Analysis.sh`. The R script `4_CompileOutput.R` is used to combine the individual output files from each model and data set to aggregate files before being used by the code in `../2-analyze`.