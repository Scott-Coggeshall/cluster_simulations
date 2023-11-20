# Function for simulating a dataset from a cluster
# randomized trial using a data generating mechanism
# corresponding to a random effects model

simulate_clustertrial  <- function(n_clusters, n_per_cluster, n_clusters_tx, 
				   random_intercept_sd, outcome_baseline_func, outcome_baseline_args, 
				   link_function, outcome_transformation, ...){


	# n_per_cluster can be supplied as an integer, in which case cluster sizes are all equal,
	# or a vector of cluster sizes to allow for varying cluster sizes.
	if(length(n_per_cluster) == 1){
		n_per_cluster  <- rep(n_per_cluster, n_clusters)
	}

	if(length(n_per_cluster) != n_clusters) stop("If you are providing a vector of cluster sizes, then length(n_per_cluster) must be equal to n_clusters")

	total_n  <- sum(n_per_cluster)

	# creating vectors of cluster ids and treatment assignment indicators
	cluster_ids <- factor(rep(1:n_clusters, n_per_cluster))

	# if n_clusters_tx isn't an integer (e.g. if the value for n_clusters_tx was obtained by dividing an odd interger by 2)
	# then we take the ceiling
	n_clusters_tx  <- ceiling(n_clusters_tx)

	# vector of treatment assignments
	tx <- c(rep(0, sum(n_per_cluster[(n_clusters_tx + 1):n_clusters])), rep(1, sum(n_per_cluster[1:n_clusters_tx])))

	# simulate cluster-level random intercepts
	# to induce correlation among observations in the same cluster.
	random_intercepts  <- rnorm(n_clusters, mean = 0, sd = random_intercept_sd)

	# replicate so that observations in each cluster have same random intercept
	random_intercepts_vec  <- rep(random_intercepts, n_per_cluster)
	

	# start our simulated dataset with columns for cluster id, (unobserved) random intercepts,  and treatment assignment
	dat  <- data.frame(cluster_id = cluster_ids, random_intercept = random_intercepts_vec, tx = tx)

	# add the baseline outcome measurements using the user-supplied function and arguments, 
	# adding n = total_n to the argument list in the call to do.call
	dat$outcome_baseline  <- do.call(outcome_baseline_func, c(n = total_n, outcome_baseline_args))

	dat$outcome  <- outcome_transformation(link_function(dat, ...))

	dat$n_clusters <- n_clusters
	dat$n_per_cluster <- n_per_cluster
	dat$n_clusters_tx  <- n_clusters_tx


	# analyze data
	model_fit <- lmer(outcome ~ tx + outcome_baseline + (1 | cluster_id), data = dat)

	model_fit_summary <- summary(model_fit)$coef

	output_dat <- data.frame(tx_estimate = model_fit_summary[2, 1], 
				 tx_pval = model_fit_summary[2, 5], 
				 n_clusters = n_clusters, 
				 n_per_cluster = unique(n_per_cluster),
				 n_clusters_tx = n_clusters_tx
				 )

	return(output_dat)

}



simulate_clustertrials <- function(n_sims, n_clusters, n_per_cluster, n_clusters_tx, 
				   random_intercept_sd, outcome_baseline_func, outcome_baseline_args, 
				   link_function, outcome_transformation, ...){


	datasets_list <- lapply(1:n_sims, function(x){
					simulate_clustertrial(n_clusters, n_per_cluster, n_clusters_tx, random_intercept_sd, outcome_baseline_func, outcome_baseline_args, link_function, outcome_transformation, ...) %>% mutate(sim_id = x) 
				   })

	Reduce("rbind", datasets_list)


}
