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
  # A <- sample(c(0, 1), size = n, replace = TRUE)
  Y <- Q(cbind(W,A))
  # Y_0 <- Q(cbind(W,0))
  # Y_1 <- Q(cbind(W,1))
  # Y <- A*Y_1 + (1-A)*Y_0
  
  cbind(W, data.frame(A = A), data.frame(Y = Y))
}

g <- function(W) {
  p <- with(W, plogis(W1 + W2*W3 - 2*W4))
  rbinom(nrow(W), 1, p)
}

Q <- function(W_A) {
  # expit(W1 + W2xW3 + W4xA - 3)
  p <- with(W_A, plogis(W1 + W2*W3 - 3))
  rbinom(nrow(W_A), 1, p)
}
# test <- generate_data(2000000)
# results <- numeric(1000)
# for (i in 1:1000) {
#   set.seed(i)
#   test_200 <- generate_data(200)
#   results[i] <- mean(test_200[which(test_200$A==0),"Y"]) > mean(test_200[which(test_200$A==1),"Y"])
# }
# table(results)

run_analysis <- function(data){
  results <- tml3(
    data = data,
    trt = "A",
    outcome = "Y",
    covar = c("W1","W2","W3","W4"),
    outcome_type="binomial",
    folds=2,
    learners_trt = c("glm","ranger","earth"),
    learners_outcome = c("glm","ranger","earth")
  )
  return(c(results$psis, results$ses))
}
# set.seed(11)
# data <- generate_data(200)
start <- 1
end <- 1000
results <- data.frame(matrix(ncol = 7, nrow = 5000))

for (i in 1:50){
  # if (i %% 50 == 1) {
    tic(paste("Iteration", i))
  # }
  
  set.seed(i)
  data <- generate_data(200)
  
  for (j in 1:100){
    set.seed(j)
    results[(i-1)*100+j, ] <- c(i,run_analysis(data))
  }
  
  # if (i %% 50 == 0) {
    toc()
  # }
  if (i %% 10 == 0){
    saveRDS(results, "/Users/anton/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/fig_1_results/revised_seeds_1-50.rds")  }
}

# results <- results[start:end, ]

names(results) <- c("dataset", "AA_estimate", "A0_estimate", "A1_estimate", "AA_se", "A0_se", "A1_se")
saveRDS(results, "/Users/anton/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/fig_1_results/revised_seeds_1-50.rds")


# 1 single initial seed for the data
  # 50 times
  # 100 seeds per dataset

# proportion of CIs containing 0

