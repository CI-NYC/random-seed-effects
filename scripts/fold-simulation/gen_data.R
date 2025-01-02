
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
  Y_1 <- plogis(-2 + W$W5 + W$W2 * W$W3 + 0.1 * W$W7 + 0.5 * 1)
  Y_0 <- plogis(-2 + W$W5 + W$W2 * W$W3 + 0.1 * W$W7 + 0.5 * 0)
  # Y <- rbinom(n, 1, plogis(-2 + W$W5 + W$W2 * W$W3 + 0.1*W$W7 + 0.5*A))
  Y <- A*Y_1 + (1-A)*Y_0
  
  W$A <- A
  W$Y <- Y
  W$Y_1 <- Y_1
  W$Y_0 <- Y_0
  W
}


# True ATE
data <- generate_data(10000000)

truth_calc_val <- mean(data$Y_1-data$Y_0)
# 0.09790345

# expected outcome under A = 1
tmp$Y_A1 <- plogis(-2 + tmp$W5 + tmp$W2 * tmp$W3 + 0.1 * tmp$W7 + 0.5 * 1)

# expected outcome under A = 0
tmp$Y_A0 <- plogis(-2 + tmp$W5 + tmp$W2 * tmp$W3 + 0.1 * tmp$W7 + 0.5 * 0)

# ATE
true_ATE <- mean(tmp$Y_A1 - tmp$Y_A0)
