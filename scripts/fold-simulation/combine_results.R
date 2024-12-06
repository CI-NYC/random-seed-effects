
library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)

result_dir <- "~/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/scripts/fold-simulation/results/raw"

results_2 <- as.data.frame(do.call(rbind, lapply(1:200, function(i) {
  result <- readRDS(file.path(result_dir, paste0("schader_2_", i, ".rds")))
  estimate_se_list <- lapply(result, function(x) c(2, i, x$psis, x$ses))
  df <- do.call(rbind, estimate_se_list)
})))

results_40 <- as.data.frame(do.call(rbind, lapply(1:200, function(i) {
  result <- readRDS(file.path(result_dir, paste0("schader_40_", i, ".rds")))
  estimate_se_list <- lapply(result, function(x) c(40, i, x$psis, x$ses))
  df <- do.call(rbind, estimate_se_list)
})))

results <- rbind(results_2, results_40)

names(results) <- c("cv_folds","dataset","AA_estimate","A0_estimate","A1_estimate","AA_se","A0_se","A1_se")

results <- results |>
  mutate(ATE = A1_estimate - A0_estimate)

dataset_means <- results |>
  group_by(cv_folds, dataset) |>
  summarise(mean_ATE = mean(ATE)) |>
  arrange(mean_ATE) |>
  mutate(rank = row_number())

results <- results |>
  left_join(dataset_means, by = c("cv_folds","dataset"))

p2 <- ggplot(results |> filter(cv_folds == 2), aes(x=rank,y=ATE,group=rank)) +
  geom_boxplot() +
  ylim(-0.1, 0.35) +
  ggtitle("Cross-fit folds: 2") +
  xlab("Dataset") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_light()

p40 <- ggplot(results |> filter(cv_folds == 40), aes(x=rank,y=ATE,group=rank)) +
  geom_boxplot() +
  ylim(-0.1, 0.35) +
  ggtitle("Cross-fit folds: 40") +
  xlab("Dataset") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_light()

plot <- grid.arrange(p2, p40, ncol = 2)
ggsave("~/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/scripts/fold-simulation/results/plots/boxplots_2v40.png", plot, width = 10, height = 5, units = "in", dpi = 300)

results_variances <- results |>
  group_by(cv_folds,dataset) |>
  summarise(dataset_variance = var(ATE))

p2 <- ggplot(results_variances |> filter(cv_folds == 2), aes(x=dataset_variance)) +
  geom_histogram(bins=50, fill = "grey40") +
  ggtitle("Cross-fit folds: 2") +
  xlab("Variance") +
  scale_x_continuous(labels = scales::label_scientific(scale = 1e0),
                     limits = c(NA,0.001)) +
  theme_light()

p40 <- ggplot(results_variances |> filter(cv_folds == 40), aes(x=dataset_variance)) +
  geom_histogram(bins=50, fill = "grey40") +
  ggtitle("Cross-fit folds: 40") +
  xlab("Variance") +
  scale_x_continuous(labels = scales::label_scientific(scale = 1e0),
                     limits = c(NA,0.001)) +
  theme_light()

plot <- grid.arrange(p2, p40, ncol=2)
ggsave("~/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/scripts/fold-simulation/results/plots/variances_2v40.png", plot, width = 10, height = 5, units = "in", dpi = 300)


# Variance calculations
var1 <- var(results_variances |> filter(cv_folds==2) |> pull(dataset_variance))
var2 <- var(results_variances |> filter(cv_folds==40) |> pull(dataset_variance))

var1/var2

results_variances_medians <- results_variances |>
  group_by(cv_folds) |>
  summarise(med = median(dataset_variance))
