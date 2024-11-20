#!/bin/bash
#
#SBATCH --partition=preemptable
ml R/4.0.2  # load R

scen=$1
cvfold=$2

chmod +x revised_sim_20230602.R
srun Rscript revised_sim_20230602.R 1 $scen $cvfold calc_truth all
