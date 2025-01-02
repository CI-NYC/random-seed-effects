library(ggplot2)
library(dplyr)
library(gridExtra)

result_dir <- "~/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/scripts/fold-simulation/results/raw"

results_2 <- as.data.frame(do.call(rbind, lapply(169, function(i) {
  result <- readRDS(file.path(result_dir, paste0("schader_2_", i, ".rds")))
  estimate_se_list <- lapply(result, function(x) c(2, i, x$psis, x$ses))
  df <- do.call(rbind, estimate_se_list)
})))

results_40 <- as.data.frame(do.call(rbind, lapply(169, function(i) {
  result <- readRDS(file.path(result_dir, paste0("schader_40_", i, ".rds")))
  estimate_se_list <- lapply(result, function(x) c(40, i, x$psis, x$ses))
  df <- do.call(rbind, estimate_se_list)
})))

results <- rbind(results_2, results_40)

names(results) <- c("cv_folds","dataset","AA_estimate","A0_estimate","A1_estimate","AA_se","A0_se","A1_se")

results <- results |>
  mutate(ATE = A1_estimate - A0_estimate,
         ATE_low = ATE - 1.96*sqrt(A0_se^2 + A1_se^2),
         ATE_high = ATE + 1.96*sqrt(A0_se^2 + A1_se^2))

results <- results |>
  group_by(cv_folds, dataset) |>
  arrange(ATE_low) |>
  mutate(rank = row_number())

p_2 <- ggplot(results |> filter(cv_folds==2), aes(x = rank, y = ATE)) +
  # geom_errorbar(aes(ymin = ATE_low, ymax = ATE_high, color=ATE_high<0.073 | ATE_low>0.073)) +
  geom_errorbar(aes(ymin = ATE_low, ymax = ATE_high), color="grey40") +
  geom_point() + 
  xlab("Run") +
  ylim(-0.3, 0.55) +
  ggtitle("Cross-fit folds: 2") +
  # geom_hline(yintercept = 0.073, linetype = "dashed") +
  theme_light() +
  theme(legend.position = "none")


p_40 <- ggplot(results |> filter(cv_folds==40), aes(x = rank, y = ATE)) +
  # geom_errorbar(aes(ymin = ATE_low, ymax = ATE_high, color=ATE_high<0.073 | ATE_low>0.073)) +
  geom_errorbar(aes(ymin = ATE_low, ymax = ATE_high), color="grey40") +
  geom_point() + 
  xlab("Run") +
  ylim(-0.3, 0.55) +
  ggtitle("Cross-fit folds: 40") +
  # geom_hline(yintercept = 0.073, linetype = "dashed") +
  theme_light() +
  theme(legend.position = "none")

plot <- grid.arrange(p_2, p_40, ncol = 2)
ggsave("~/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/scripts/fold-simulation/results/plots/folds_2_40_seed169.png", plot, width = 10, height = 5, units = "in", dpi = 300)
