#!/bin/bash
#
#SBATCH --partition=preemptable
ml R  # load R

scen=$1
cvfold=$2
learners=$3

chmod +x process_results.R
srun Rscript process_results.R 1 $scen $cvfold $learners
