library(ranger)
library(earth)
library(tml3)
library(mlr3extralearners)
library(tictoc)

generate_data <- function(n = 200) {
  W1 <- runif(n, 0, 2)
  W2_4 <- matrix(rbinom(n * 3, 1, 0.5), ncol = 3)
  colnames(W2_4) <- paste0("W", 2:4)
  W <- cbind(data.frame(W1 = W1), W2_4)
  A <- g(W)
  # A <- sample(c(0, 1), size = n, replace = TRUE)
  Y <- Q(cbind(W,A))
  # Y_0 <- Q(cbind(W,0))
  # Y_1 <- Q(cbind(W,1))
  # Y <- A*Y_1 + (1-A)*Y_0
  
  cbind(W, data.frame(A = A), data.frame(Y = Y))#, 
  #data.frame(Y_0 = Y_0), data.frame(Y_1 = Y_1))
}

g <- function(W) {
  p <- with(W, plogis(W1 + W2*W3 - 2*W4))
  rbinom(nrow(W), 1, p)
}

Q <- function(W_A) {
  # expit(W1 + W2xW3 + W4xA - 3)
  p <- with(W_A, plogis(W1 + W2*W3 + W4*A - 3))
  rbinom(nrow(W_A), 1, p)
}

run_analysis <- function(data){
  results <- tml3(
    data = data,
    trt = "A",
    outcome = "Y",
    covar = c("W1","W2","W3","W4"),
    outcome_type="binomial",
    folds=10,
    learners_trt = c("glm","ranger","earth"),
    learners_outcome = c("glm","ranger","earth")
  )
  return(c(results$psis, results$ses))
}


analysis_aiptw <- function(data, n_seed, seed){
  
  set.seed(seed)
  n <- nrow(W)
  # storage objects
  Qn_0 <- matrix(nrow = n_seed, ncol = n) # outcome regression predictions under A = 0
  Qn_1 <- matrix(nrow = n_seed, ncol = n) # outcome regression predictions under A = 1
  gn_0 <- matrix(nrow = n_seed, ncol = n) # propensity score predictions for A = 0
  gn_1 <- matrix(nrow = n_seed, ncol = n) # propensity score predictions for A = 1
  ate_est_aiptw <- vector(mode = "list", length = n_seed) # list of ATE point estimates
  ate_var_aiptw <- vector(mode = "list", length = n_seed) # list of ATE variance estimates
  
  for (j in 1:n_seed) {
    # fit outcome regression with random forest
    sl_Q <- ranger(x = data.frame(A,W), y = Y,
                   mtry = floor(sqrt(ncol(data.frame(A,W)))),
                   min.node.size = 1,
                   probability = T,
                   num.threads = 1) # chose parameters to match SL. ranger in SuperLearner package
    # store outcome regression predictions under A = 0 and A = 1
    class_1 <- which(sl_Q$forest$class.values == 1)
    Qn_0[j, ] <- predict(sl_Q, data = data.frame(A = 0, W))$predictions[,class_1]
    Qn_1[j, ] <- predict(sl_Q, data = data.frame(A = 1, W))$predictions[,class_1]
    # fit propensity score random forest
    sl_g <- ranger(x = data.frame(W), y = A,
                   mtry = floor(sqrt(ncol(data.frame(W)))),
                   min.node.size = 1,
                   probability = T,
                   num.threads = 1)
    # store propensity for A = 1, A = 0
    class_1 <- which(sl_g$forest$class.values == 1)
    6
    gn_1[j, ] <- predict(sl_g, data = data.frame(W))$predictions[,class_1]
    gn_0[j, ] <- 1-gn_1[j,]
    # estimate AIPTW and variance using current outcome regression/propensity score
    aiptw_object <- aiptw_ate_est(A = A, W = W, Y = Y,
                                  Qn_0 = Qn_0[j,], Qn_1 = Qn_1[j,],
                                  gn_1 = gn_1[j,], gn_0 = gn_0[j,])
    # store point and variance estimates
    ate_est_aiptw[[j]] <- aiptw_object$est
    ate_var_aiptw[[j]] <- aiptw_object$var
  }
  return(out)
}


for (i in 1:5) {
  set.seed(i)
  data <- generate_data()
  result <- analysis_aiptw <- function(data, 150)
}