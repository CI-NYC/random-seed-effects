#! /usr/local/R-3.6.0/bin/Rscript

# This file was used to combine the results 

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

# set parameters 
options(echo=TRUE) # if you want to see commands in output file

  # full parm
  nseed <- seq(1: 150)
  ndata <- seq(1: 200)
  nobs <- c(100,500,1000)
  parm <- expand.grid(seed = nseed, data = ndata, n = nobs)
  
  n_out <- 15 * 13 # output length
  
  # directory that output is saved in 
  save_dir <- paste0("./sim_", learners,'_', scen, "_", cv_folds)

  rslt <- matrix(nrow = nrow(parm), ncol = n_out)
  for(i in 1:nrow(parm)){
    tmp <- tryCatch({
      load(paste0(save_dir, "/sim_", learners, '_', i, ".RData"))
      out
    }, error = function(e){
      rep(NA, n_out)
    })
    rslt[i, ] <- tmp
  }
  out <- data.frame(scen, cv_folds, parm, rslt)
  
  warnings()
  
  # name
  value <- c("pvalue", "est", "cil", "ciu", "cover")
  method <- c("drtmle", "tmle", "aiptw")
  times <- c("1", "5", "10", "20", "40", "60", "80")
  
  methodave <- c("drtmleave", "tmleave", "aiptwave")
  times1 <- c("5", "10", "20", "40", "60", "80")
  
  name <- expand.grid(value, method, times)
  nameave <- expand.grid(value, methodave, times1)
  nameall <- rbind(name, nameave)
  
  colname <- vector(mode = "list", length = n_out)
  for (i in 1 : nrow(nameall)) {
    colname[[i]] <- paste(c(as.character(nameall[i,1]), 
                      as.character(nameall[i,2]), 
                      as.character(nameall[i,3])), collapse = "_")
  }
  colnames(out) <- c("scenario", "cv_folds", "seed", 
                     "ndata", "nobs", unlist(colname))

  save(out, file = paste0(wd, paste0("/all_sim_", scen, "_", learners,'_', cv_folds, ".RData")))


       