library(targets)
library(tarchetypes)
library(future)
library(future.callr)

plan(callr)

source("R/find_variances.R")
source("R/simulate_clustertrial.R")
source("R/simulate_normal_outcome.R")

# Set target-specific options such as packages:
tar_option_set(packages = c("tidyverse", "lmerTest") )

weight_baseline_corr <- 0.97

weight_outcome_total_var <- 50^2
weight_baseline_total_var <- 50^2

weight_tx_effect <- 7.4

weight_baseline_coef <- weight_baseline_corr * sqrt(weight_outcome_total_var) / sqrt(weight_baseline_total_var)

n_sims <- 1000

# End this file with a list of target objects.
list(

     # weight change power analysis
     tar_target(n_clusters_vec, 10:20), 
     tar_target(n_per_cluster_vec, 10:20), 
     tar_target(icc_vec, c(.05, .1, .15)), 
     tar_target(weight_variances, find_variances(c(weight_tx_effect, weight_baseline_coef), .5, 
						weight_baseline_total_var, weight_outcome_total_var, icc_vec), 
	       pattern = map(icc_vec), iteration = "list"), 
     tar_target(weight_sims, simulate_clustertrials(n_sims,
						    n_clusters_vec, n_per_cluster_vec, n_clusters_tx = n_clusters_vec/2, sqrt(weight_variances[2]), rnorm, list(mean = 0, sd = sqrt(weight_baseline_total_var)), simulate_normal_outcome, I, coefficient_vector = c(1, weight_tx_effect, weight_baseline_coef), 
						    residual_sd = sqrt(weight_variances[1])) %>% 
							    mutate(icc = weight_variances[2]/(weight_variances[1] + weight_variances[2])), 
		pattern = cross(n_clusters_vec, n_per_cluster_vec, weight_variances))


)
