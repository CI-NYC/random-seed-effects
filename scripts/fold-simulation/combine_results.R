
result_dir <- "~/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/scripts/fold-simulation/results/raw"

results_2 <- do.call(rbind, lapply(1:200, function(i) {
  result <- readRDS(file.path(result_dir, paste0("schader_40_", i, ".rds")))
  estimate_se_list <- lapply(result, function(x) c(i, x$psis, x$ses))
  df <- do.call(rbind, estimate_se_list)
}))




