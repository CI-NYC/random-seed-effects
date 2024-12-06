library(ggplot2)
library(dplyr)
library(gridExtra)

# 2 folds

results_2 <- rbind(readRDS("~/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/paper/fig_1_results/revised_seeds_1-50.rds"))

results_2 <- results_2 |> 
  mutate(ATE = A1_estimate - A0_estimate,
         ATE_low = ATE - 1.96*sqrt(A0_se^2 + A1_se^2),
         ATE_high = ATE + 1.96*sqrt(A0_se^2 + A1_se^2),
         CI_width = ATE_high-ATE_low)

results_2_45 <- results_2 |>
  filter(dataset==45) |>
  arrange(ATE_low) |> 
  mutate(rank = row_number())

p_2_45 <- ggplot(results_2_45, aes(x = rank, y = ATE)) +
  geom_errorbar(aes(ymin = ATE_low, ymax = ATE_high, color=ATE_high<0 | ATE_low>0)) +
  geom_point() + 
  xlab("Run") +
  ylim(-0.2, 0.4) +
  ggtitle("Cross-fit folds: 2") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_light() +
  theme(legend.position = "none")



results_40 <- rbind(readRDS("~/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/paper/fig_1_results/seeds_45_40folds.rds"))

results_40 <- results_40 |> 
  mutate(ATE = A1_estimate - A0_estimate,
         ATE_low = ATE - 1.96*sqrt(A0_se^2 + A1_se^2),
         ATE_high = ATE + 1.96*sqrt(A0_se^2 + A1_se^2),
         CI_width = ATE_high-ATE_low)

results_40_45 <- results_40 |>
  # filter(dataset==45) |>
  arrange(ATE_low) |> 
  mutate(rank = row_number())

p_40_45 <- ggplot(results_40_45, aes(x = rank, y = ATE)) +
  geom_errorbar(aes(ymin = ATE_low, ymax = ATE_high), color="#00BFC4") +
  geom_point() + 
  xlab("Run") +
  ylim(-0.2, 0.4) +
  ggtitle("Cross-fit folds: 40") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_light() +
  theme(legend.position = "none")

# results_40_12 <- results_40 |>
#   filter(dataset==12) |>
#   arrange(ATE_low) |> 
#   mutate(rank = row_number())
# 
# p_40_12 <- ggplot(results_40_12, aes(x = rank, y = ATE)) +
#   geom_errorbar(aes(ymin = ATE_low, ymax = ATE_high, color=ATE_high<0 | ATE_low>0)) +
#   geom_point() + 
#   ylim(-0.2, 0.4) +
#   ggtitle("Cross-fit folds: 40") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   theme(legend.position = "none")


results_20 <- rbind(readRDS("~/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/paper/fig_1_results/seeds_12_45.rds"))

results_20 <- results_20 |> 
  mutate(ATE = A1_estimate - A0_estimate,
         ATE_low = ATE - 1.96*sqrt(A0_se^2 + A1_se^2),
         ATE_high = ATE + 1.96*sqrt(A0_se^2 + A1_se^2),
         CI_width = ATE_high-ATE_low)

# results_20_12 <- results_20 |>
#   filter(dataset==12) |>
#   arrange(ATE_low) |> 
#   mutate(rank = row_number())
# 
# p_20_12 <- ggplot(results_20_12, aes(x = rank, y = ATE)) +
#   geom_errorbar(aes(ymin = ATE_low, ymax = ATE_high, color=ATE_high<0 | ATE_low>0)) +
#   geom_point() + 
#   ylim(-0.2, 0.4) +
#   ggtitle("Cross-fit folds: 20") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   theme(legend.position = "none")

results_20_45 <- results_20 |>
  filter(dataset==45) |>
  arrange(ATE_low) |> 
  mutate(rank = row_number())

p_20_45 <- ggplot(results_20_45, aes(x = rank, y = ATE)) +
  geom_errorbar(aes(ymin = ATE_low, ymax = ATE_high, color=ATE_high<0 | ATE_low>0)) +
  geom_point() + 
  ylim(-0.2, 0.4) +
  ggtitle("Cross-fit folds: 20") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Run") +
  theme_light() +
  theme(legend.position = "none")

plot <- grid.arrange(p_2_45, p_20_45, p_40_45, ncol = 3, top = ("Dataset seed 45"))
ggsave("~/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/scripts/fold-simulation/results/plots/folds_2_20_40.png", plot, width = 9, height = 4, units = "in", dpi = 300)

plot <- grid.arrange(p_2_45, p_40_45, ncol = 2)
ggsave("~/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/scripts/fold-simulation/results/plots/folds_2_40.png", plot, width = 10, height = 5, units = "in", dpi = 300)

