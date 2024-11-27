#!/bin/bash
#SBATCH --account=msph
#SBATCH --job-name=SchaderReplication
#SBATCH --mem=5G
#SBATCH --time=5:30:00
#SBATCH --array=8-87,95,97-102,104-113,115-200


cvfold=$1

Rscript main.R $cvfold 

exit 0



