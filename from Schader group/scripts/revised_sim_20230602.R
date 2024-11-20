#! /usr/local/R-3.6.0/bin/Rscript

# This file was used to submit the simulation files to 
# a slurm-based Unix system. Using shell script
# one can submit each simulation in sequence. First, data files are
# created for each simulation. Those data files are then analyzed 
# in the 'run' execution. 

# Edits
# used the sim.sh files to submit the simulation script
# used the compile.sh files, along with the post_sim_compiling.R script to merge results of simulation
# 6/2/23 adding code to allow for a single learner to be passed in as an argument from the cluster

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
  end_index <- 10
  start_index <- 1
  scen <- 'simple'
  cv_folds <- 1
  task <- 'sim'
  learners <- 'all'
  
} else{
  
  print(args)
  names(args) <- c('index', 'scenario', 'CVFolds', 'task', 'learners')
  end_index <- as.numeric(args['index'])*10 # each index will give 10 runs
  start_index <- end_index - 9
  scen <- as.character(args['scenario'])
  cv_folds <- as.numeric(args['CVFolds'])
  task <- as.character(args['task'])
  learners <- as.character(args['learners']) # options will be "all" or pass in name of the learner e.g. "SL.ranger"
  
}


print(scen)
print(cv_folds)

# define custom learner for glmnet - added 5/11/2023
# function used to do smarter CV for glmnet
get_fold_id <- function(Y){
  fold_id <- rep(0, length(Y))
  wiY0 <- which(Y == 0)
  wiY1 <- which(Y == 1)
  #if <4 cases, no cv
  if(length(wiY1) == 4){
    #if exactly 4 cases, 4-fold cv
    #1 case per fold
    fold <- 1:4
    fold_id[sample(wiY1)] <- fold
    fold_id[sample(wiY0)] <- rep(fold, length = length(wiY0))
  }else{
    #if >=5 cases, 5 fold cv
    #cases split as evenly as possible
    fold <- 1:5
    fold_id[sample(wiY1)] <- rep(fold, length = length(wiY1))
    fold_id[sample(wiY0)] <- rep(fold, length = length(wiY0))
  }
  return(fold_id)
}


# function to have more robust behavior in SL.glmnet
SL.glmnet.0 <- function (Y, X, newX, family, obsWeights = rep(1, length(Y)), id, alpha = 1, nfolds = 5,
                         nlambda = 100, useMin = TRUE, loss = "deviance", ...) {
  
  SuperLearner:::.SL.require("glmnet")
  if (!is.matrix(X)) {
    X <- model.matrix(~-1 + ., X)
    newX <- model.matrix(~-1 + ., newX)
  }
  
  if(family$family == "binomial"){
    fold_id <- get_fold_id(Y)
    nfolds <- max(fold_id)
    
    if(nfolds != 0){
      fitCV <- glmnet::cv.glmnet(x = X, y = Y, weights = obsWeights,
                                 lambda = NULL, type.measure = loss, nfolds = nfolds,
                                 foldid = fold_id, family = family$family, alpha = alpha, nlambda = nlambda,
                                 ...)
      pred <- predict(fitCV, newx = newX, type = "response", s = ifelse(useMin,
                                                                        "lambda.min", "lambda.1se"))
      fit <- list(object = fitCV, useMin = useMin)
      class(fit) <- "SL.glmnet"
      
    }else{
      # if fewer than 3 cases, just use mean
      meanY <- weighted.mean(Y, w = obsWeights)
      pred <- rep.int(meanY, times = nrow(newX))
      fit <- list(object = meanY)
      out <- list(pred = pred, fit = fit)
      class(fit) <- c("SL.mean")
    }
    
  }else{
    fitCV <- glmnet::cv.glmnet(x = X, y = Y, weights = obsWeights,
                               lambda = NULL, type.measure = loss, nfolds = nfolds,
                               family = family$family, alpha = alpha, nlambda = nlambda,
                               ...)
    pred <- predict(fitCV, newx = newX, type = "response", s = ifelse(useMin,
                                                                      "lambda.min", "lambda.1se"))
    fit <- list(object = fitCV, useMin = useMin)
    class(fit) <- "SL.glmnet"
    
  }
  out <- list(pred = pred, fit = fit)
  return(out)
}

# SL.caret2 with suppression of verbose iteration and passes in user-defined arguments (ex: tuneGrid)
SL.caret.xgboost <- function (Y, X, newX, family, obsWeights, 
                              method = "xgbTree", tuneLength = 5, 
                              trControl = caret::trainControl(method = "cv", number = 10, 
                                                              verboseIter = FALSE), 
                              metric = ifelse(family$family == "gaussian", "RMSE", "Accuracy"), 
                              ...) 
{
  
  if (family$family == "gaussian") {
    fit.train <- caret::train(x = X, y = Y, weights = obsWeights, 
                              metric = metric, method = method, tuneLength = tuneLength, 
                              trControl = trControl, verbose = F, verbosity = 0, ...)
    pred <- predict(fit.train, newdata = newX, type = "raw")
  }
  if (family$family == "binomial") {
    Y.f <- as.factor(Y)
    levels(Y.f) <- c("A0", "A1")
    suppressWarnings(fit.train <- caret::train(x = X, y = Y.f, weights = obsWeights, 
                              metric = metric, method = method, tuneLength = tuneLength, 
                              trControl = trControl, verbose = F, verbosity = 0, ...))
    pred <- predict(fit.train, newdata = newX, type = "prob")[, 
                                                              2]
  }
  fit <- list(object = fit.train)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.caret")
  return(out)
}


# Simulation Parameters
nseed <- 123#seq(1: 150) # random seed for the analysis
ndata <- seq(1: 200) # random seed for generating the dataset
nobs <- 100#c(100,500,1000)
parm <- expand.grid(seed = nseed, data = ndata, n = nobs)

# Create a sim folder if it does not already exist
foldername1 <- paste0('sim_', learners, '_', scen, '_', cv_folds)
if(!file.exists(foldername1)){dir.create(foldername1)}
setwd(paste0(wd,'/',foldername1))

# execute parallel job #################################################


if(task == 'rerun'){
  index_vector <- load('../miss_vec.RData') # load in the seeds that are missing
  ls()
  index_vector <- miss_vec
} else if(task == 'sim'){
  index_vector <- c(start_index:end_index)
} else if(task == 'calc_truth'){
  index_vector <- c(1)
}

print(index_vector)

for (i in index_vector) { 
  
  print(paste(Sys.time(), "i:", i))

  # load parameters
  print(parm[i,])
  
  # # load packages
  # library(randomForest)
  library(drtmle)
  library(SuperLearner)
  library(earth)
  library(ranger)
  library(dplyr)
  library(mlr3misc)

  
  # simulate data - revised
  set.seed(parm$data[i])
  n   <- parm$n[i]
  
  if(task == 'calc_truth'){
    set.seed(123)
    n <- 100 # 100000000
  } else {
    truth_val <- readRDS(paste0('../truth_calc_', scen, '_val.RDS')) #0.09784965
  }
  
  if(learners == 'all'){
    if(scen == 'simple'){
      or_library <- c("SL.ranger", "SL.earth", "SL.glm.interaction")
      ps_library <- c("SL.ranger", "SL.earth", "SL.glm.interaction")
    } else {
      or_library <- c("SL.ranger", "SL.earth", "SL.glm.interaction", "SL.glmnet.0")
      ps_library <- c("SL.ranger", "SL.earth", "SL.glm.interaction", "SL.glmnet.0")
    }
  } else if(learners == 'SL.ranger'){
    or_library = c(learners)
    ps_library = c(learners)
    add_args = list()
  } else if(learners == 'SL.caret.xgboost'){
    or_library = c(learners)
    ps_library = c(learners)
    # grid for xgbTree
    # xgbGrid <- expand.grid(nrounds = c(1,10),
    #                        max_depth = c(1,4),
    #                        gamma = 0,
    #                        eta = c(0.1, 0.4),
    #                        colsample_bytree = 0.7,
    #                        min_child_weight = 1,
    #                        subsample = c(0.8,1))
    add_args = list()
  }
  
  if(scen == 'complex'){
      
      W1  <- runif(n, 0, 2)
      W2  <- runif(n, 0, 1) # used to generate W15 (probability)
      W3  <- rbinom(n, 1, 0.5)
      W4  <- rbinom(n, 1, 0.5)
      W5  <- rbinom(n, 1, 0.5) # correlated with W10
      W6  <- rnorm(n, 1, 0.5)
      
      W <- data.frame(W1  = W1, 
                      W2  = W2, 
                      W3  = W3, 
                      W4  = W4,
                      W5  = W5,
                      W6  = W6,
                      W7  = rnorm(n, W1, sd=0.75),
                      W8  = rbinom(n, 1, W2),
                      W9  = W3 + W4 +rnorm(n, 0, 5),
                      W10 = 0.5*W5 + rpois(n, W1), # correlated with both W1 and W5
                      W11 = rnorm(n, W6, 0.5) + rnorm(n, W5, sd=0.5),
                      W12 = rbinom(n, 1, 0.5),
                      W13 = rbinom(n, 1, 0.5),
                      W14 = runif(n, 0, 2),
                      W15 = runif(n, 0, 2),
                      W16 = runif(n, 0, 2),
                      W17 = rnorm(n, 1, 0.5),
                      W18 = rnorm(n, 1, 0.5),
                      W19 = rnorm(n, 1, 0.5),
                      W20 = rnorm(n, 1, 0.5))
      
      A <- rbinom(n, 1, plogis(-0.7*W$W1 + 0.5*W$W3*W$W4 - 0.7 * W$W5 + 1*W$W6 + 0.3*W$W7))
      Y <- rbinom(n, 1, plogis(-2 + W$W5 + W$W2 * W$W3 + 0.1*W$W7 + 0.5*A))
      
      if(task == 'calc_truth'){
        pA <- summary(plogis(-0.7*W$W1 + 0.5*W$W3*W$W4 - 0.7 * W$W5 + 1*W$W6 + 0.3*W$W7))
        Y_1 <- rbinom(n, 1, plogis(-2 + W$W5 + W$W2 * W$W3 + 0.1*W$W7 + 0.5*1))
        Y_0 <- rbinom(n, 1, plogis(-2 + W$W5 + W$W2 * W$W3 + 0.1*W$W7 + 0.5*0))
        
        truth_calc_val <- mean(Y_1-Y_0)
        truth_calc_Y_1 <- mean(Y_1)
        truth_calc_Y_0 <- mean(Y_0)
        
        # minimum y-value
        print(min(plogis(-2 + W$W5 + W$W2 * W$W3 + 0.1*W$W7 + 0.5*0)))
        # max y-value
        print(max(plogis(-2 + W$W5 + W$W2 * W$W3 + 0.1*W$W7 + 0.5*1)))
        # min A
        print(min(plogis(-0.7*W$W1 + 0.5*W$W3*W$W4 - 0.7 * W$W5 + 1*W$W6 + 0.3*W$W7)))
        # max P(A=1)
        print(max(plogis(-0.7*W$W1 + 0.5*W$W3*W$W4 - 0.7 * W$W5 + 1*W$W6 + 0.3*W$W7)))
        
        print(mean(Y))
        print(mean(A))
      }
      
      
  } else{
      
      W <- data.frame(W1 = runif(n, 0, 2), 
                      W2 = rbinom(n, 1, 0.5), 
                      W3 = rbinom(n, 1, 0.5), 
                      W4 = rbinom(n, 1, 0.5))
      A <- rbinom(n, 1, plogis(W$W1 + W$W2*W$W3 - 2 * W$W4))
      Y <- rbinom(n, 1, plogis(-3 + W$W1 + W$W2*W$W3 + W$W4 * A))
      
      
      # truth_val <- 0.08365
      if(task == 'calc_truth'){
        pA <- summary(plogis(W$W1 + W$W2*W$W3 - 2 * W$W4))
        Y_1 <- rbinom(n, 1, plogis(-3 + W$W1 + W$W2*W$W3 + W$W4 * 1))
        Y_0 <- rbinom(n, 1, plogis(-3 + W$W1 + W$W2*W$W3 + W$W4 * 0))
        truth_calc_val <- mean(Y_1-Y_0)
        truth_calc_Y_1 <- mean(Y_1)
        truth_calc_Y_0 <- mean(Y_0)
        
        print(min(plogis(-3 + W$W1 + W$W2*W$W3 + W$W4 * 0)))
        print(max(plogis(-3 + W$W1 + W$W2*W$W3 + W$W4 * 1)))
        print(min(plogis(W$W1 + W$W2*W$W3 - 2 * W$W4)))
        print(max(plogis(W$W1 + W$W2*W$W3 - 2 * W$W4)))
        print(mean(Y))
        print(mean(A))
        
      }
  
  }
  
  if(task == 'calc_truth'){
    saveRDS(truth_calc_val, file = paste0('../truth_calc_', scen, '_val.RDS'))
    saveRDS(c(truth_calc_Y_1, truth_calc_Y_0, pA), file = paste0('../truth_calc_Y1Y0_', scen, '_val.RDS'))
  } else{
    set.seed(parm$seed[i])
    
    max <- 80
    Qn <- list()
    gn <- list()
    Qn_0 <- matrix(nrow = max, ncol = n)
    Qn_1 <- matrix(nrow = max, ncol = n)
    gn_1 <- matrix(nrow = max, ncol = n)
    gn_0 <- matrix(nrow = max, ncol = n)
    ate_est_tmle <- vector(mode = "list", length = max)
    ate_cov_tmle <- vector(mode = "list", length = max)
    ate_est_drtmle <- vector(mode = "list", length = max)
    ate_cov_drtmle <- vector(mode = "list", length = max)
    ate_est_aiptw <- vector(mode = "list", length = max)
    ate_cov_aiptw <- vector(mode = "list", length = max)
    Qn_list <- vector(mode = "list", length = max)
    gn_list <- vector(mode = "list", length = max)
    
    # specify indices for the CV folds before iteration through random seeds - tried this does not work well
    # df_full <- data.frame(A=A, W, Y, id=c(1:n))
    # Y0_indices <- c(1:length(df_full$Y))[df_full$Y == 0]
    # Y1_indices <- c(1:length(df_full$Y))[df_full$Y == 1]
    # indices_a <- sample(Y0_indices, size = floor(length(Y0_indices)/2), replace = FALSE, prob = NULL) # split the sample in half randomly
    # indices_b <- sample(Y1_indices, size = ceiling(length(Y1_indices)/2), replace = FALSE, prob = NULL)
    # indices_1 <- c(indices_a, indices_b)
    
    # SuperLearner
    time_vec <- c()
    for (j in 1 : max) {
        start_time <- Sys.time()
        if(cv_folds == 2){
              df_full <- data.frame(A=A, W, Y, id=c(1:n))
              Y0_indices <- c(1:length(df_full$Y))[df_full$Y == 0]
              Y1_indices <- c(1:length(df_full$Y))[df_full$Y == 1]
              indices_a <- sample(Y0_indices, size = floor(length(Y0_indices)/2), replace = FALSE, prob = NULL) # split the sample in half randomly
              indices_b <- sample(Y1_indices, size = ceiling(length(Y1_indices)/2), replace = FALSE, prob = NULL)
              indices_1 <- c(indices_a, indices_b)
              
              df_set1 <- df_full %>% filter(id %in% indices_1)
              df_set2 <- df_full %>% filter(!(id %in% indices_1))
              
              # Train the outcome regression on each sample split
              
              # per the Phillips paper, when Y is binary we want each training set to reflect the prevalence of Y in the sample
              # this indicates using stratifyCV
              # by default we set the folds to 10, but stratify CV requires at least one event and at least one non-event in each fold, 
              # when there are less than 10 events this leads to a problem, therefore check if <10 events/no-events in the samples
              # we can set the folds to the number of events or non-events (whichever is smaller) to guarantee that there is an 
              # event/non-event in each fold
              
              # folds <- ifelse(min(length(Y1_indices)/2, length(Y0_indices)/2) < 10, 5, 10)
              # folds <- ifelse(min(length(Y1_indices)/2, length(Y0_indices)/2) < 5, 3, folds)
              # folds <- ifelse(min(length(Y1_indices)/2, length(Y0_indices)/2) < 3, min(length(Y1_indices)/2, length(Y0_indices)/2), folds)
              # 
              if(length(or_library) == 1){
                sl_Q_set1 <- do.call(or_library, 
                                      c(list(Y = df_set1$Y, 
                                           X = data.frame(A = df_set1$A, df_set1[,colnames(W)]),
                                           family = binomial(),
                                           obsWeights = NULL,
                                           newX = data.frame(A = df_set1$A, df_set1[,colnames(W)])), 
                                        add_args))$fit
                sl_Q_set2 <- do.call(or_library, 
                                     c(list(Y = df_set2$Y, 
                                          X = data.frame(A = df_set2$A, df_set2[,colnames(W)]),
                                          family = binomial(),
                                          obsWeights = NULL,
                                          newX = data.frame(A = df_set2$A, df_set2[,colnames(W)])),
                                       add_args))$fit
              } else {
                sl_Q_set1 <- SuperLearner(Y = df_set1$Y, 
                                          X = data.frame(A = df_set1$A, df_set1[,colnames(W)]), 
                                          family = binomial(), 
                                          SL.library = or_library,
                                          cvControl = list(stratifyCV = TRUE, 
                                                           V = min(10, sum(df_set1$Y == 1), sum(df_set1$Y == 0))) # V = folds)
                )
                sl_Q_set2 <- SuperLearner(Y = df_set2$Y, 
                                          X = data.frame(A = df_set2$A, df_set2[,colnames(W)]), 
                                          family = binomial(), 
                                          SL.library = or_library,
                                          cvControl = list(stratifyCV = TRUE, 
                                                           V = min(10, sum(df_set2$Y == 1), sum(df_set2$Y == 0)))# V = folds)
                )
              
              }
              
              Qn_list[[j]] <- list(sl_Q_set1 = sl_Q_set1, sl_Q_set2 = sl_Q_set2)
              
              # Predict on the opposite dataset
              if(length(or_library) == 1){
                Qn_0_set2 <- as.numeric(predict(sl_Q_set1, newdata = data.frame(A = 0, df_set2[,colnames(W)]), family = binomial()))
                Qn_1_set2 <- as.numeric(predict(sl_Q_set1, newdata = data.frame(A = 1, df_set2[,colnames(W)]), family = binomial()))
                Qn_0_set1 <- as.numeric(predict(sl_Q_set2, newdata = data.frame(A = 0, df_set1[,colnames(W)]), family = binomial()))
                Qn_1_set1 <- as.numeric(predict(sl_Q_set2, newdata = data.frame(A = 1, df_set1[,colnames(W)]), family = binomial()))
                
              } else{
                Qn_0_set2 <- as.numeric(predict(sl_Q_set1, newdata = data.frame(A = 0, df_set2[,colnames(W)]), onlySL = TRUE)[[1]])
                Qn_1_set2 <- as.numeric(predict(sl_Q_set1, newdata = data.frame(A = 1, df_set2[,colnames(W)]), onlySL = TRUE)[[1]]) 
                Qn_0_set1 <- as.numeric(predict(sl_Q_set2, newdata = data.frame(A = 0, df_set1[,colnames(W)]), onlySL = TRUE)[[1]])
                Qn_1_set1 <- as.numeric(predict(sl_Q_set2, newdata = data.frame(A = 1, df_set1[,colnames(W)]), onlySL = TRUE)[[1]]) 
                
              }
             
              # arrange in the appropriate order for the prediction vector
              Qn_0_pred <- rep(NA,n)
              Qn_0_pred[df_full$id %in% indices_1] <- Qn_0_set1
              Qn_0_pred[!(df_full$id %in% indices_1)] <- Qn_0_set2
              Qn_1_pred <- rep(NA,n)
              Qn_1_pred[df_full$id %in% indices_1] <- Qn_1_set1
              Qn_1_pred[!(df_full$id %in% indices_1)] <- Qn_1_set2
              
              Qn_0[j, ] <- Qn_0_pred
              Qn_1[j, ] <- Qn_1_pred
              Qn[[j]] <- list(Qn_0[j, ], Qn_1[j, ])
              
              if(length(ps_library) == 1){
                sl_g_set1 <- do.call(ps_library, 
                                     c(list(Y = df_set1$A,
                                          X = df_set1[,colnames(W)], 
                                          family = binomial(),
                                          obsWeights = NULL,
                                          newX = df_set1[,colnames(W)]), 
                                       add_args))$fit
                sl_g_set2 <- do.call(ps_library, 
                                     c(list(Y = df_set2$A,
                                          X = df_set2[,colnames(W)], 
                                          family = binomial(),
                                          obsWeights = NULL,
                                          newX = df_set2[,colnames(W)]), 
                                       add_args))$fit
              } else {
                sl_g_set1 <- SuperLearner(Y = df_set1$A, 
                                          X = df_set1[,colnames(W)], 
                                          family = binomial(),
                                          #cvControl = SuperLearner.CV.control(V = 2L),
                                          SL.library = ps_library,
                                          cvControl = list(stratifyCV = TRUE, V = min(10, sum(df_set1$A == 1), sum(df_set1$A == 0)))
                                          #method = "method.NNloglik"
                )
                sl_g_set2 <- SuperLearner(Y = df_set2$A, 
                                          X = df_set2[,colnames(W)], 
                                          family = binomial(),
                                          #cvControl = SuperLearner.CV.control(V = 2L),
                                          SL.library = ps_library,
                                          cvControl = list(stratifyCV = TRUE, V = min(10, sum(df_set2$A == 1), sum(df_set2$A == 0)))
                                          #method = "method.NNloglik" 
                )
              }
              
              gn_list[[j]] <- list(sl_g_set1 = sl_g_set1, sl_g_set2 = sl_g_set2)
              
              # Predict on the opposite dataset
              if(length(ps_library) == 1){
                gn_1_set2 <- as.numeric(predict(sl_g_set1, newdata = df_set2[,colnames(W)], family = binomial()))
                gn_0_set2 <- 1- gn_1_set2
                gn_1_set1 <- as.numeric(predict(sl_g_set2, newdata = df_set1[,colnames(W)], family = binomial()))
                gn_0_set1 <- 1 - gn_1_set1
              } else{
                gn_1_set2 <- as.numeric(predict(sl_g_set1, newdata = df_set2[,colnames(W)], onlySL = TRUE)[[1]])
                gn_0_set2 <- 1- gn_1_set2
                gn_1_set1 <- as.numeric(predict(sl_g_set2, newdata = df_set1[,colnames(W)], onlySL = TRUE)[[1]])
                gn_0_set1 <- 1 - gn_1_set1
              }
              
              
              # arrange in the appropriate order for the prediction vector
              gn_0_pred <- rep(NA,n)
              gn_0_pred[df_full$id %in% indices_1] <- gn_0_set1
              gn_0_pred[!(df_full$id %in% indices_1)] <- gn_0_set2
              gn_1_pred <- rep(NA,n)
              gn_1_pred[df_full$id %in% indices_1] <- gn_1_set1
              gn_1_pred[!(df_full$id %in% indices_1)] <- gn_1_set2
              
              gn_0[j, ] <- gn_0_pred
              gn_1[j, ] <- gn_1_pred
              gn[[j]] <- list(gn_0[j, ], gn_1[j, ])
              
              
        }else{
        
              folds <- NA
              # fit outcome regression super learner
              if(length(or_library) == 1){
                sl_Q <- do.call(or_library,
                                c(list(Y = Y, 
                                     X = data.frame(A = A, W), 
                                     family = binomial(),
                                     obsWeights = NULL,
                                     newX = data.frame(A = A, W)), 
                                  add_args))$fit
              } else {
                sl_Q <- SuperLearner(Y = Y, 
                                     X = data.frame(A = A, W), 
                                     family = binomial(), 
                                     SL.library = or_library,
                                     cvControl = list(stratifyCV = TRUE, V = min(10, sum(Y == 1), sum(Y == 0)))
                )
              }
              
              Qn_list[[j]] <- list(sl_Q = sl_Q)
              
              if(length(or_library) == 1){
                Qn_0[j, ] <- as.numeric(predict(sl_Q, newdata = data.frame(A = 0, W), family = binomial()))
                Qn_1[j, ] <- as.numeric(predict(sl_Q, newdata = data.frame(A = 1, W), family = binomial()))
              } else {
                Qn_0[j, ] <- as.numeric(predict(sl_Q, newdata = data.frame(A = 0, W), onlySL = TRUE)[[1]])
                Qn_1[j, ] <- as.numeric(predict(sl_Q, newdata = data.frame(A = 1, W), onlySL = TRUE)[[1]])
              }
              Qn[[j]] <- list(Qn_0[j, ], Qn_1[j, ])
              
              # fit propensity score super learner
              if(length(ps_library) == 1){
                sl_g <- do.call(ps_library,
                                c(list(Y = A, 
                                     X = W, 
                                     family = binomial(),
                                     obsWeights = NULL,
                                     newX = W), 
                                  add_args))$fit
              } else {
                sl_g <- SuperLearner(Y = A, 
                                     X = W, 
                                     family = binomial(),
                                     #cvControl = SuperLearner.CV.control(V = 2L),
                                     SL.library = ps_library,
                                     cvControl = list(stratifyCV = TRUE, V = min(10, sum(A == 1), sum(A == 0)))
                                     #method = "method.NNloglik"
                )     
              }
           
              gn_list[[j]] <- list(sl_g = sl_g)
              
              if(length(ps_library)==1){
                gn_1[j, ] <- as.numeric(predict(sl_g, newdata = W, family = binomial()))
                gn_0[j, ] <- 1 - gn_1[j, ]
              } else {
                gn_1[j, ] <- as.numeric(predict(sl_g, newdata = W, onlySL = TRUE)[[1]])
                gn_0[j, ] <- 1 - gn_1[j, ]
              }
              gn[[j]] <- list(gn_0[j, ], gn_1[j, ])
          
          }
        
      # add in call to drtmle
      this_drtmle <- drtmle(Y = Y, 
                       A = A, 
                       W = W, 
                       a_0 = c(0, 1), 
                       Qn = Qn[[j]], 
                       gn = gn[[j]], 
                       SL_Qr = "SL.glm", 
                       SL_gr = "SL.glm",
                       returnModels = TRUE)
        
      ate_est_tmle[[j]] <- this_drtmle$tmle$est
      ate_cov_tmle[[j]] <- this_drtmle$tmle$cov
      ate_est_drtmle[[j]] <- this_drtmle$drtmle$est
      ate_cov_drtmle[[j]] <- this_drtmle$drtmle$cov
      ate_est_aiptw[[j]] <- this_drtmle$aiptw$est
      ate_cov_aiptw[[j]] <- this_drtmle$aiptw$cov
      
      end_time <- Sys.time()
      total_time <- difftime(end_time, start_time, units = c('mins'))
      time_vec[j] <- total_time
      print(j)
    }
    
    print(time_vec)
    
  
    # average outcome
    mean_tmle_est_5 <- Reduce("+", ate_est_tmle[1:5]) / 5
    mean_tmle_cov_5 <- Reduce("+", ate_cov_tmle[1:5]) / 5
    mean_drtmle_est_5 <- Reduce("+", ate_est_drtmle[1:5]) / 5
    mean_drtmle_cov_5 <- Reduce("+", ate_cov_drtmle[1:5]) / 5
    mean_aiptw_est_5 <- Reduce("+", ate_est_aiptw[1:5]) / 5
    mean_aiptw_cov_5 <- Reduce("+", ate_cov_aiptw[1:5]) / 5
    
    mean_tmle_est_10 <- Reduce("+", ate_est_tmle[1:10]) / 10
    mean_tmle_cov_10 <- Reduce("+", ate_cov_tmle[1:10]) / 10
    mean_drtmle_est_10 <- Reduce("+", ate_est_drtmle[1:10]) / 10
    mean_drtmle_cov_10 <- Reduce("+", ate_cov_drtmle[1:10]) / 10
    mean_aiptw_est_10 <- Reduce("+", ate_est_aiptw[1:10]) / 10
    mean_aiptw_cov_10 <- Reduce("+", ate_cov_aiptw[1:10]) / 10
    
    mean_tmle_est_20 <- Reduce("+", ate_est_tmle[1:20]) / 20
    mean_tmle_cov_20 <- Reduce("+", ate_cov_tmle[1:20]) / 20
    mean_drtmle_est_20 <- Reduce("+", ate_est_drtmle[1:20]) / 20
    mean_drtmle_cov_20 <- Reduce("+", ate_cov_drtmle[1:20]) / 20
    mean_aiptw_est_20 <- Reduce("+", ate_est_aiptw[1:20]) / 20
    mean_aiptw_cov_20 <- Reduce("+", ate_cov_aiptw[1:20]) / 20
    
    mean_tmle_est_40 <- Reduce("+", ate_est_tmle[1:40]) / 40
    mean_tmle_cov_40 <- Reduce("+", ate_cov_tmle[1:40]) / 40
    mean_drtmle_est_40 <- Reduce("+", ate_est_drtmle[1:40]) / 40
    mean_drtmle_cov_40 <- Reduce("+", ate_cov_drtmle[1:40]) / 40
    mean_aiptw_est_40 <- Reduce("+", ate_est_aiptw[1:40]) / 40
    mean_aiptw_cov_40 <- Reduce("+", ate_cov_aiptw[1:40]) / 40
    
    mean_tmle_est_60 <- Reduce("+", ate_est_tmle[1:60]) / 60
    mean_tmle_cov_60 <- Reduce("+", ate_cov_tmle[1:60]) / 60
    mean_drtmle_est_60 <- Reduce("+", ate_est_drtmle[1:60]) / 60
    mean_drtmle_cov_60 <- Reduce("+", ate_cov_drtmle[1:60]) / 60
    mean_aiptw_est_60 <- Reduce("+", ate_est_aiptw[1:60]) / 60
    mean_aiptw_cov_60 <- Reduce("+", ate_cov_aiptw[1:60]) / 60
    
    mean_tmle_est_80 <- Reduce("+", ate_est_tmle[1:80]) / 80
    mean_tmle_cov_80 <- Reduce("+", ate_cov_tmle[1:80]) / 80
    mean_drtmle_est_80 <- Reduce("+", ate_est_drtmle[1:80]) / 80
    mean_drtmle_cov_80 <- Reduce("+", ate_cov_drtmle[1:80]) / 80
    mean_aiptw_est_80 <- Reduce("+", ate_est_aiptw[1:80]) / 80
    mean_aiptw_cov_80 <- Reduce("+", ate_cov_aiptw[1:80]) / 80
    
    # these are the fits averaged over the ESTIMATOR (as opposed to the SL)
    average_fit_5 <- list(drtmle = list(est = mean_drtmle_est_5, cov = mean_drtmle_cov_5),
                          tmle = list(est = mean_tmle_est_5, cov = mean_tmle_cov_5),
                          aiptw = list(est = mean_aiptw_est_5, cov = mean_aiptw_cov_5),
                          a_0 = c(0,1))
    
    average_fit_10 <- list(drtmle = list(est = mean_drtmle_est_10, cov = mean_drtmle_cov_10),
                          tmle = list(est = mean_tmle_est_10, cov = mean_tmle_cov_10),
                          aiptw = list(est = mean_aiptw_est_10, cov = mean_aiptw_cov_10),
                          a_0 = c(0,1))
  
    average_fit_20 <- list(drtmle = list(est = mean_drtmle_est_20, cov = mean_drtmle_cov_20),
                          tmle = list(est = mean_tmle_est_20, cov = mean_tmle_cov_20),
                          aiptw = list(est = mean_aiptw_est_20, cov = mean_aiptw_cov_20),
                          a_0 = c(0,1))
    
    average_fit_40 <- list(drtmle = list(est = mean_drtmle_est_40, cov = mean_drtmle_cov_40),
                          tmle = list(est = mean_tmle_est_40, cov = mean_tmle_cov_40),
                          aiptw = list(est = mean_aiptw_est_40, cov = mean_aiptw_cov_40),
                          a_0 = c(0,1))
    
    average_fit_60 <- list(drtmle = list(est = mean_drtmle_est_60, cov = mean_drtmle_cov_60),
                           tmle = list(est = mean_tmle_est_60, cov = mean_tmle_cov_60),
                           aiptw = list(est = mean_aiptw_est_60, cov = mean_aiptw_cov_60),
                           a_0 = c(0,1))
    
    average_fit_80 <- list(drtmle = list(est = mean_drtmle_est_80, cov = mean_drtmle_cov_80),
                           tmle = list(est = mean_tmle_est_80, cov = mean_tmle_cov_80),
                           aiptw = list(est = mean_aiptw_est_80, cov = mean_aiptw_cov_80),
                           a_0 = c(0,1))
    
    class(average_fit_5) <- "drtmle"
    class(average_fit_10) <- "drtmle"
    class(average_fit_20) <- "drtmle"
    class(average_fit_40) <- "drtmle"
    class(average_fit_60) <- "drtmle"
    class(average_fit_80) <- "drtmle"
    
  
    
    # fit drtmle n_sl = 1
    fit1 <- drtmle(Y = Y, 
                   A = A, 
                   W = W, 
                   a_0 = c(0, 1), 
                   Qn = Qn[[1]],
                   gn = gn[[1]], 
                   SL_Qr = "SL.glm", 
                   SL_gr = "SL.glm",
                   returnModels = TRUE)
    
    # fit drtmle n_sl = 5
    Qn_0_5 <- apply(Qn_0[1:5,], 2, mean)
    Qn_1_5 <- apply(Qn_1[1:5,], 2, mean)
    Qn5 <- list(Qn_0_5, Qn_1_5)
    
    gn_1_5 <- apply(gn_1[1:5,], 2, mean)
    gn_0_5 <- apply(gn_0[1:5,], 2, mean)
    gn5 <- list(gn_0_5, gn_1_5) #LS - reversed the order of the A's in the gn to match a_0
    
    fit5 <- drtmle(Y = Y, 
                    A = A, 
                    W = W, 
                    a_0 = c(0, 1), 
                    Qn = Qn5,
                    gn = gn5, 
                    SL_Qr = "SL.glm", 
                    SL_gr = "SL.glm",
                    returnModels = TRUE)
    
    
    
    # fit drtmle n_sl = 10
    Qn_0_10 <- apply(Qn_0[1:10,], 2, mean)
    Qn_1_10 <- apply(Qn_1[1:10,], 2, mean)
    Qn10 <- list(Qn_0_10, Qn_1_10)
    
    gn_1_10 <- apply(gn_1[1:10,], 2, mean)
    gn_0_10 <- apply(gn_0[1:10,], 2, mean)
    gn10 <- list(gn_0_10, gn_1_10)
    
    fit10 <- drtmle(Y = Y, 
                    A = A, 
                    W = W, 
                    a_0 = c(0, 1), 
                    Qn = Qn10, 
                    gn = gn10, 
                    SL_Qr = "SL.glm", 
                    SL_gr = "SL.glm",
                    returnModels = TRUE)
    
    
    # fit drtmle n_sl = 20
    Qn_0_20 <- apply(Qn_0[1:20,], 2, mean)
    Qn_1_20 <- apply(Qn_1[1:20,], 2, mean)
    Qn20 <- list(Qn_0_20, Qn_1_20)
    
    gn_1_20 <- apply(gn_1[1:20,], 2, mean)
    gn_0_20 <- apply(gn_0[1:20,], 2, mean)
    gn20 <- list(gn_0_20, gn_1_20)
    
    fit20 <- drtmle(Y = Y, 
                    A = A, 
                    W = W, 
                    a_0 = c(0, 1), 
                    Qn = Qn20, 
                    gn = gn20, 
                    SL_Qr = "SL.glm", 
                    SL_gr = "SL.glm",
                    returnModels = TRUE)
    
    # fit drtmle n_sl = 40
    Qn_0_40 <- apply(Qn_0[1:40,], 2, mean)
    Qn_1_40 <- apply(Qn_1[1:40,], 2, mean)
    Qn40 <- list(Qn_0_40, Qn_1_40)
    
    gn_1_40 <- apply(gn_1[1:40,], 2, mean)
    gn_0_40 <- apply(gn_0[1:40,], 2, mean)
    gn40 <- list(gn_0_40, gn_1_40)
    
    fit40 <- drtmle(Y = Y, 
                    A = A, 
                    W = W, 
                    a_0 = c(0, 1), 
                    Qn = Qn40, 
                    gn = gn40, 
                    SL_Qr = "SL.glm", 
                    SL_gr = "SL.glm",
                    returnModels = TRUE)
    
    # fit drtmle n_sl = 60
    Qn_0_60 <- apply(Qn_0[1:60,], 2, mean)
    Qn_1_60 <- apply(Qn_1[1:60,], 2, mean)
    Qn60 <- list(Qn_0_60, Qn_1_60)
    
    gn_1_60 <- apply(gn_1[1:60,], 2, mean)
    gn_0_60 <- apply(gn_0[1:60,], 2, mean)
    gn60 <- list(gn_0_60, gn_1_60)
    
    fit60 <- drtmle(Y = Y, 
                    A = A, 
                    W = W, 
                    a_0 = c(0, 1), 
                    Qn = Qn60, 
                    gn = gn60, 
                    SL_Qr = "SL.glm", 
                    SL_gr = "SL.glm",
                    returnModels = TRUE)
    
  
    # fit drtmle n_sl = 80
    Qn_0_80 <- apply(Qn_0, 2, mean)
    Qn_1_80 <- apply(Qn_1, 2, mean)
    Qn80 <- list(Qn_0_80, Qn_1_80)
    
    gn_1_80 <- apply(gn_1, 2, mean)
    gn_0_80 <- apply(gn_0, 2, mean)
    gn80 <- list(gn_0_80, gn_1_80)
    
    fit80 <- drtmle(Y = Y, 
                    A = A, 
                    W = W, 
                    a_0 = c(0, 1), 
                    Qn = Qn80, 
                    gn = gn80, 
                    SL_Qr = "SL.glm", 
                    SL_gr = "SL.glm",
                    returnModels = TRUE)
    
  
    # update truth to reflect new dat gen 11.18.21
    get <- function(object, 
                    which_est = "drtmle", 
                    truth = truth_val){ 
      
      this_ci <- ci(object, contrast = c(-1, 1), est = which_est)[[which_est]]
      cover <- this_ci[2] < truth & this_ci[3] > truth # check CI coverage
      p <- as.data.frame(wald_test(object, contrast = c(1, -1), est = which_est)[1])[1,2] # get p-value
      return(c(p, as.numeric(this_ci), cover)) # the order comes out as: p-value, estimate, cil, ciu, coverage
      
    }
    
    out <- c(
      
      # averaging on the level of the super learner fits
      get(fit1), get(fit1, "tmle"), get(fit1, "aiptw"), 
      get(fit5), get(fit5, "tmle"), get(fit5, "aiptw"), 
      get(fit10), get(fit10, "tmle"), get(fit10, "aiptw"), 
      get(fit20), get(fit20, "tmle"), get(fit20, "aiptw"), 
      get(fit40), get(fit40, "tmle"), get(fit40, "aiptw"),
      get(fit60), get(fit60, "tmle"), get(fit60, "aiptw"),
      get(fit80), get(fit80, "tmle"), get(fit80, "aiptw"),
      
      # averaging on the level of the estimate
      get(average_fit_5), get(average_fit_5, "tmle"), get(average_fit_5, "aiptw"),
      get(average_fit_10), get(average_fit_10, "tmle"), get(average_fit_10, "aiptw"),
      get(average_fit_20), get(average_fit_20, "tmle"), get(average_fit_20, "aiptw"),
      get(average_fit_40), get(average_fit_40, "tmle"), get(average_fit_40, "aiptw"),
      get(average_fit_60), get(average_fit_60, "tmle"), get(average_fit_60, "aiptw"),
      get(average_fit_80), get(average_fit_80, "tmle"), get(average_fit_80, "aiptw")
    )
    
    sl_fits <- list(Qn = Qn, gn = gn)
    
    print(paste0('iteration ', i, ' complete.'))
    wd <- getwd()
    print(wd)
    
    # create file name
    filename1 <- paste0("sim_",learners,"_", i, ".RData") # 6/2/2023 change the file name for storage based on learners used
    filename2 <- paste0("sl_preds_", i, ".RData")
    filename3 <- paste0("sl_fits_", i, ".RData")
    
    # save output
    save(out, file = filename1)
    save(sl_fits, file = filename2)
    #save(list(Qn_list = Qn_list, gn_list = gn_list), file = filename3)
    rm(sl_fits)
    rm(Qn_list, gn_list)
  }
}
