#!/bin/bash
#
#SBATCH --partition=benkeser

ml R/4.0.2  # load R

scen=$2
cvfold=$3
task=$4
learners=$5

chmod +x revised_sim_20230602.R
srun Rscript revised_sim_20230602.R 0 $scen $cvfold $task $learners



