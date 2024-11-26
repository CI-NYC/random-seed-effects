#!/bin/bash

#SBATCH --mem=5G
#SBATCH --time=1:00:00
#SBATCH --array=1-200

cvfold=2

module load R

Rscript revised_sim_20230602.R $cvfold 

exit 0



