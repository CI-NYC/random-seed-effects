library(ggplot)

# within-dataset variability of ATE point estimates and CI width
# ATE = psi(1)- psi(0)
# boxplots
# confidence intervals? (how)
ATE <- results$A1_estimate - results$A0_estimate




# Relative Within-Dataset Variability of ATE point estimates and CI widths
# get the between-dataset variance using point estimates from one analysis
# divide estimated within-dataset variance by the estimated between-datset variance


# Maximum, relative within-dataset range of CI bounds
# range of upper bounds. range of lower bounds
# all ranges - then find the mean of all ranges
# divide range of upper bounds by mean of all ranges


# Unstable CIs (not clear on this)
# number of datasets w/ >10% relative range for either upper or lower bounds
# number of datasets w/ non-overlapping CIs across any of the seeds
# ^ unclear if means "pairs" of datasets, or just datasets that don't overlap with the entire group


# Unstable Hypothesis testing (maybe not this one)
# proportion over 150 seeds
