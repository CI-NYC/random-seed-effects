#! /usr/local/R-3.6.0/bin/Rscript

# This file checks the runs of simulation and makes sure that each line of the parameter matrix was exacuted

# get environment variables
rm(list=ls())
print(getwd())
wd <- getwd()

# set or obtain parameters from script
options(echo=TRUE) # if you want to see commands in output file
args=(commandArgs(TRUE))
print(args)

if(length(args)==0){
  
  print("No arguments supplied")
  scen <- 'simple'
  cv_folds <- 1
  learners <- 'all'
  
} else{
  
  names(args) <- c('scenario', 'CVFolds', 'learners')
  scen <- as.character(args['scenario'])
  cv_folds <- as.numeric(args['CVFolds'])
  learners <- as.character(args['learners'])
  
}


print(scen)
print(cv_folds)

# set parameters 
options(echo=TRUE) # if you want to see commands in output file

  # full parm
  nseed <- seq(1: 150)
  ndata <- seq(1: 200)
  nobs <- c(100,500,1000)
  parm <- expand.grid(seed = nseed, data = ndata, n = nobs)
  
  n_out <- 15 * 13 # output length
  
  # directory that output is saved in 
  save_dir <- paste0('./sim_', learners, '_', scen, '_', cv_folds)
  miss_vec <- c()
  files_list <- list.files(path = save_dir, pattern = '.RData', all.files = FALSE,
             full.names = FALSE)
  
  for(i in 1:nrow(parm)){
    if(!(paste0('sim_', learners,'_',i,'.RData') %in% files_list)){
      miss_vec <- c(miss_vec,i)
    }
  }
  
  # save the vector of missing values
  
  save(miss_vec, file='miss_vec.RData')

    

