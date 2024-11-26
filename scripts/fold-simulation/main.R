

# set or obtain parameters from script
options(echo=TRUE) # if you want to see commands in output file
args=commandArgs(TRUE)
id=Sys.getenv("SLURM_ARRAY_TASK_ID")

cv_folds <- as.numeric(args[1])

library(tml3)
library(mlr3extralearners)
library(glue)

source("gen_data.R")

set.seed(id)
data <- generate_data()

K <- 100

results <- lapply(1:K, function(k){
  set.seed(k)
  tml3(
    data = data,
    trt = "A",
    outcome = "Y",
    covar = paste0("W", 1:20),
    outcome_type = "binomial",
    folds = cv_folds,
    learners_trt = c("glm", "ranger", "earth"),
    learners_outcome = c("glm", "ranger", "earth")
  )
})

saveRDS(results, glue("results/raw/schader_{cv_folds}_{id}.rds"))