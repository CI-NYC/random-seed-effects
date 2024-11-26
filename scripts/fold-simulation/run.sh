#!/bin/bash
#SBATCH --account=msph
#SBATCH --job-name=SchaderReplication
#SBATCH --mem=5G
#SBATCH --time=2:00:00
#SBATCH --array=1-200

cvfold=$1

Rscript main.R $cvfold 

exit 0



