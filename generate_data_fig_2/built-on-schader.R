
rm(list=ls())
print(getwd())
wd <- getwd()

# set or obtain parameters from script
options(echo=TRUE) # if you want to see commands in output file
args=(commandArgs(TRUE))
print(args)

names(args) <- c('CVFolds')
cv_folds <- as.numeric(args['CVFolds'])

n_datasets <- 200
n_runs <- 100

generate_data <- function(n=100){
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
  
  W$A <- A
  W$Y <- Y
  W
}


run_analysis <- function(data){
  results <- tml3(
    data = data,
    trt = "A",
    outcome = "Y",
    covar = c("W1","W2","W3","W4"),
    outcome_type="binomial",
    folds=cv_folds,
    learners_trt = c("glm","ranger","earth"),
    learners_outcome = c("glm","ranger","earth")
  )
  return(c(results$psis, results$ses))
}

results <- data.frame(matrix(ncol = 7, nrow = n_datasets*n_runs))

# data <- generate_data(200)
for (i in 1:n_datasets){
  tic(paste("Iteration", i))
  
  set.seed(i)
  data <- generate(data)
  
  for (j in 1:n_runs){
    results[(i-1)*n_runs+j, ] <- c(i,run_analysis(data))
    
  }
  
  toc()
}

names(results) <- c("dataset", "AA_estimate", "A0_estimate", "A1_estimate", "AA_se", "A0_se", "A1_se")
saveRDS(results, "/Users/anton/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/fig_1_results/seeds_45_40folds.rds")