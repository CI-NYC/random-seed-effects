#!/bin/bash
#
#SBATCH --array=1-1000%500 # this should go from 1 to 1000
#SBATCH --partition=preemptable

cvfold=2
# task=$4

# runindex=$(($1*1000+$SLURM_ARRAY_TASK_ID)) # multiplies the first input argument by 1000 
#SBATCH --output=Array_test.%A_%a.out
#SBATCH --error=Array_test.%A_%a.error

chmod +x revised_sim_20230602.R
srun Rscript revised_sim_20230602.R $cvfold