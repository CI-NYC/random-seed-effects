library(ranger)
library(earth)
library(tml3)
library(mlr3extralearners)
library(tictoc)

generate_data <- function(n) {
  W1 <- runif(n, 0, 2)
  W2_4 <- matrix(rbinom(n * 3, 1, 0.5), ncol = 3)
  colnames(W2_4) <- paste0("W", 2:4)
  W <- cbind(data.frame(W1 = W1), W2_4)
  A <- g(W)
  Y <- Q(cbind(W,A))
  cbind(W, data.frame(A = A), data.frame(Y = Y))
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

test_100 <- generate_data(100)

mean(test_100[which(test_100$A==0),"Y"])
mean(test_100[which(test_100$A==1),"Y"])


# head(test_data)

# tml3(
#   test_100,
#   trt = "A",
#   outcome = "Y",
#   covar = c("W1","W2","W3","W4"),
#   outcome_type="binomial",
#   folds=10,
#   learners_trt = c("glm","ranger","earth"),
#   learners_outcome = c("glm","ranger","earth")
# )
# 
# 
# test_500 <- generate_data(500)
# tml3(
#   test_500,
#   trt = "A",
#   outcome = "Y",
#   covar = c("W1","W2","W3","W4"),
#   outcome_type="binomial",
#   folds=10,
#   learners_trt = c("glm","ranger","earth"),
#   learners_outcome = c("glm","ranger","earth")
# )
# 
# test_1000 <- generate_data(1000)
# res <- tml3(
#   test_1000,
#   trt = "A",
#   outcome = "Y",
#   covar = c("W1","W2","W3","W4"),
#   outcome_type="binomial",
#   folds=10,
#   learners_trt = c("glm","ranger","earth","lasso"),
#   learners_outcome = c("glm","ranger","earth","lasso")
# )

# Distinction between "datasets" and "seeds"
# 200 datasets (seeds 1 through 200)
# Run the analysis with 150 seeds (1 through 150)

run_analysis <- function(data){
  results <- tml3(
    data = data,
    trt = "A",
    outcome = "Y",
    covar = c("W1","W2","W3","W4"),
    outcome_type="binomial",
    folds=10,
    learners_trt = c("glm","ranger","earth","lasso"),
    learners_outcome = c("glm","ranger","earth","lasso")
  )
  
  # diff <- results$A1 - results$A0 Maybe don't need the difference?
  # cis <- c(rep(NA,6))
  # for (i in 1:3){
  #   cis[(i*2-1):(i*2)] <- results$psis[i] + c(-1, 1)*qnorm(0.975)*results$ses[i]
  # }
  
  # return vector of c(AA_estimate, AA_low, AA_high, A0_estimate, A0_low, A0_high, A1_estimate, A1_low, A1_high)
  # return(c(results$psis[1], cis[1:2], results$psis[2], cis[3:4], results$psis[3], cis[5:6]))
  
  return(c(results$psis, results$ses))
  
}

results_all <- list()
# for (i in 1:200){
for (i in 4:7){
  set.seed(i)
  data <- generate_data(100)
  
  results <- data.frame(matrix(ncol = 7, nrow = 150))
  # results <- data.frame(matrix(ncol = 9, nrow = 5))
  tic()
  for (j in 1:150){
    # for (k in 1:80) ???
    set.seed(j)
    results[j, ] <- c(i,run_analysis(data))
    # }
  }
  toc()
  names(results) <- c("dataset", "AA_estimate", "A0_estimate", "A1_estimate", "AA_se", "A0_se", "A1_se")
  
  results_all[[i]]<-results
}


# names(results) <- c("AA_estimate", "AA_low", "AA_high", "A0_estimate", "A0_low", "A0_high", "A1_estimate", "A1_low", "A1_high")
results_all <- results_all[4:7]
saveRDS(results_all, "/Users/anton/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/intermediate-results/seeds_4_7.rds")
