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

# results <- numeric(1000)
# for (i in 1:1000) {
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
    folds=20,
    learners_trt = c("glm","ranger","earth","lasso"),
    learners_outcome = c("glm","ranger","earth","lasso")
  )
  return(c(results$psis, results$ses))
}
set.seed(1)

data <- generate_data(200)
start <- 301
end <- 450
results <- data.frame(matrix(ncol = 7, nrow = end))

for (i in start:end){
  if (i %% 10 == 1) {
    tic(paste("Iteration", i))
  }
  
  set.seed(i)
  result <- c(i,run_analysis(data))
  results[i, ] <- result
  
  if (i %% 10 == 0) {
    toc()
  }
}

results <- results[start:end, ]

names(results) <- c("dataset", "AA_estimate", "A0_estimate", "A1_estimate", "AA_se", "A0_se", "A1_se")
saveRDS(results, "/Users/anton/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/fig_1_results/seeds-301-450.rds")
