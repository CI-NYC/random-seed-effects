library(ggplot2)
library(dplyr)

results <- read.csv("/Users/anton/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/intermediate-results/seed_1.csv") |>
  mutate(dataset = 1) |>
  select(dataset, AA_estimate, A0_estimate, A1_estimate, AA_se, A0_se, A1_se)

results_2_3 <- readRDS("/Users/anton/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/intermediate-results/seeds_2_3.rds")

results_4_7 <- readRDS("/Users/anton/Library/CloudStorage/OneDrive-ColumbiaUniversityIrvingMedicalCenter/Q3_2024/random-seed-effects/intermediate-results/seeds_4_7.rds")
######### 1
# within-dataset variability of ATE point estimates and CI width
# ATE = psi(1)- psi(0)
# boxplots
# confidence intervals? (how)
# results$ATE <- results$A1_estimate - results$A0_estimate
# results$CI_width <- 2*1.96*sqrt(results$A0_se^2 + results$A1_se^2)
# 
# ggplot(results, aes(y=ATE)) +
#   geom_boxplot()
# 
# ggplot(results, aes(y=CI_width)) +
#   geom_boxplot()

# rbind the `results` dataframes for each of the seeds 1-200. create a "dataset" group column
results_combined <- rbind(results, do.call(rbind, results_2_3), do.call(rbind, results_4_7))
results_combined$ATE <- results_combined$A1_estimate - results_combined$A0_estimate
results_combined$CI_width <- 2*1.96*sqrt(results_combined$A0_se^2 + results_combined$A1_se^2)

results_combined <- results_combined |> 
  mutate(ATE = A1_estimate - A0_estimate,
         ATE_low = ATE - 1.96*sqrt(A0_se^2 + A1_se^2),
         ATE_high = ATE + 1.96*sqrt(A0_se^2 + A1_se^2),
         CI_width = ATE_high-ATE_low)

# Calculate the mean of each group and rank them
dataset_means <- results_combined |>
  group_by(dataset) |>
  summarise(mean_ATE = mean(ATE)) |>
  arrange(mean_ATE) |>
  mutate(rank = row_number())

# Merge the rank back to the original dataframe
results_combined <- results_combined |>
  left_join(dataset_means, by = "dataset")

ggplot(results_combined, aes(x=rank,y=ATE,group=rank,fill=rank)) +
  geom_boxplot()

ggplot(results_combined, aes(x=rank,y=ATE_high,group=rank,fill=rank)) +
  geom_boxplot()


######## 2
# Relative Within-Dataset Variability of ATE point estimates and CI widths
# get the between-dataset variance using point estimates from analysis 1 (seed 1) of each dataset
between_set_variance <- results_combined |>
  group_by(dataset) |>
  slice(1) |>
  ungroup() |>
  summarise(
    estimate_variance = var(ATE),
    CI_width_variance = var(CI_width)
  )

# Get within_set variance
within_set_variance <- results_combined |>
  group_by(dataset) |>
  summarise(
    estimate_variance = var(ATE),
    CI_width_variance = var(CI_width)
  )

# divide estimated within-dataset variance by the estimated between-dataset variance
relative_within_set_variance <- within_set_variance |>
  mutate(estimate_variance = estimate_variance / between_set_variance$estimate_variance,
         CI_width_variance = CI_width_variance / between_set_variance$CI_width_variance)

# ggplot(results_combined, aes(x=rank,y=ATE_high,group=rank,fill=rank)) +
#   geom_boxplot()


######## 3 
# Maximum, relative within-dataset range of CI bounds
# range of upper bounds. range of lower bounds

within_set_range <- results_combined |>
  # filter(dataset == 1) |>
  group_by(dataset) |>
  mutate(ATE_low = ATE - CI_width/2,
         ATE_high = ATE + CI_width/2) |>
  summarise(range_of_upper_bounds = max(ATE_high) - min(ATE_high),
            mean_width = mean(CI_width)) |>
  mutate(relative_range = range_of_upper_bounds/mean_width)

# mean_width <- results_combined |> 
#   # filter(dataset == 1) |>
#   group_by(dataset) |>
#   summarise(mean_width = mean(CI_width))


# all ranges - then find the mean of all ranges
# divide range of upper bounds by mean of all ranges


# Unstable CIs (not clear on this)
# number of datasets w/ >10% relative range for either upper or lower bounds
(count_unstable_CI <- sum(within_set_range$relative_range > 0.1))
  

# number of datasets w/ non-overlapping CIs across any of the seeds

# ^ unclear if means "pairs" of datasets, or just datasets that don't overlap with the entire group


# Unstable Hypothesis testing
# proportion over 150 seeds where null was accepted
