library(ggplot2)
library(dplyr)

results <- rbind(readRDS("/Users/anton/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/fig_1_results/seeds-1-150.rds"),
                 readRDS("/Users/anton/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/fig_1_results/seeds-151-300.rds"),
                 readRDS("/Users/anton/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/fig_1_results/seeds-301-450.rds"),
                 readRDS("/Users/anton/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/fig_1_results/seeds-451-575.rds")
)

results <- rbind(readRDS("/Users/anton/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/fig_1_results/revised_seeds_1-50.rds"))

results <- results |> 
  mutate(ATE = A1_estimate - A0_estimate,
         ATE_low = ATE - 1.96*sqrt(A0_se^2 + A1_se^2),
         ATE_high = ATE + 1.96*sqrt(A0_se^2 + A1_se^2),
         CI_width = ATE_high-ATE_low)

# summary statistics
results <- results |>
  group_by(dataset)|>
  summarise(mean(ATE_high<0 | ATE_low > 0))

# [1]  6 12 19 34 40 45

results <- results |>
  filter(dataset==45) |>
  arrange(ATE_low) |> 
  mutate(rank = row_number())

ggplot(results, aes(x = rank, y = ATE)) +
  geom_errorbar(aes(ymin = ATE_low, ymax = ATE_high, color=ATE_high<0 | ATE_low>0)) +
  geom_point() + 
  ylim(-0.2, 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed")
  
ggsave("/Users/anton/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/fig_1_results/seed_45.pdf", width = 6, height = 4, units = "in", dpi = 300)



