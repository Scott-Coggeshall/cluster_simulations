# Function for simulating a normally distributed outcome. 
# Intended use is to pass it to simulate_clustertrial function as the link_function argument 
simulate_normal_outcome <- function(dat, coefficient_vector, residual_sd){


	# adding intercept column

	dat$intercept <- rep(1, nrow(dat))

	outcome <- as.matrix(dat[, c("intercept", "tx", "outcome_baseline")]) %*% coefficient_vector + 
		dat$random_intercept + rnorm(nrow(dat), mean = 0, sd = residual_sd)


	outcome




}
