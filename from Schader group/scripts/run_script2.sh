#!/bin/bash
#
#SBATCH --array=1-1000%500 # this should go from 1 to 1000
#SBATCH --partition=preemptable

ml R/4.0.2  # load R

scen=$2
cvfold=$3
task=$4
learners=$5

runindex=$(($1*1000+$SLURM_ARRAY_TASK_ID)) # multiplies the first input argument by 1000 
#SBATCH --output=Array_test.%A_%a.out
#SBATCH --error=Array_test.%A_%a.error

chmod +x revised_sim_20230602.R
srun Rscript revised_sim_20230602.R $runindex $scen $cvfold $task $learners




