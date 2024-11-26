#!/bin/bash
#SBATCH --account=msph
#SBATCH --job-name=SchaderReplication
#SBATCH --mem=5G
#SBATCH --time=1:00:00
#SBATCH --array=1-200

cvfold=2

Rscript revised_sim_20230602.R $cvfold 

exit 0



