#!/bin/bash
#
#SBATCH --partition=benkeser

ml R
scen=$1
cvfold=$2
learners=$3


chmod +x check_run.R

srun Rscript check_run.R  $scen $cvfold $learners
