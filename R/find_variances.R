find_variances <- function(coefficient_vector, p_tx, variance_outcome_baseline, variance_outcome_total, cluster_icc){

	residual_variance  <- find_residual_variance(coefficient_vector, p_tx, variance_outcome_baseline, variance_outcome_total, cluster_icc)

	cluster_variance <- find_cluster_variance(cluster_icc, residual_variance)


	return(c(residual_variance = residual_variance, cluster_variance = cluster_variance))

}











find_residual_variance <- function(coefficient_vector, p_tx, variance_outcome_baseline, variance_outcome_total, cluster_icc){


   (1 - cluster_icc) * (variance_outcome_total - coefficient_vector[1]^2 * p_tx * (1 - p_tx) - 
			coefficient_vector[2]^2 * variance_outcome_baseline)

}


find_cluster_variance <- function(icc, residual_variance){


	icc/(1 - icc) * residual_variance





}
