#!/bin/bash
#SBATCH --account=msph
#SBATCH --job-name=SchaderReplication
#SBATCH --mem=5G
#SBATCH --time=5:30:00
#SBATCH --array=109,110,111,112,146


cvfold=$1

Rscript main.R $cvfold 

exit 0



