# This script fits the public goods game data without including triangle terms (Table 2).
# The outputs are the maximum likelihood estimates (MLE) and standard errors,
# and the log-likelihood value.

# Libraries 
library(igraph)
library(tidyverse)
library(parallel)

#################### Functions ####################

# A function to calculate log-likelihood for each network
# {eta: parameter,
#  net_list: list for one set of 7 dynamic networks in network objects,
#  adj_matrices: list for one set of 7 dynamic networks in the adjacency matrices,
#  list_time_plus: list for one set of Y(+) networks in the adjacency matrices}
log_likelihood <- function(eta, net_list, adj_matrices, list_time_plus) {
  
  
  # Set parameters
  lambda <- eta[1:4]
  # Set the initial log-likelihood
  loglik <- 0
  # Set the number of time step: 7
  # Note "net_list" has 8 values
  time_step <- length(net_list) - 1
  
  for (t in 1:time_step) {
    
    ########## Formation-Numerator ##########
    
    # Behaviors (t + 1); 1: cooperation, 0: defection, -1: no history
    # Not t, but t + 1 because we the formation and persistence networks 
    # start from t = 2 to t = 8
    next_behavior <- V(net_list[[t + 1]])$behavior
    # Cumulative wealth / 1000 at t + 1
    next_score <- V(net_list[[t + 1]])$score / 1000
    
    # Compute the absolute difference between actors in "next_score" 
    score_outer <- abs(outer(next_score, next_score, "-"))
    
    # Make Y(+) network at time t + 1
    plus_adj <- adj_matrices[[t]] + adj_matrices[[t + 1]]
    plus_adj[plus_adj > 0] <- 1
    
    # Prepare "coop_homo_adj", Y(+)
    coop_homo_adj <- plus_adj
    # Leave only cooperative ties in Y(+)
    coop_homo_adj[next_behavior != 1, ] <- 0
    coop_homo_adj[ , next_behavior != 1] <- 0
    # Compute the number of cooperative ties in Y(+)
    num_coop_homo <- sum(coop_homo_adj) / 2
    
    # Prepare "def_homo_adj", Y(+)
    def_homo_adj <- plus_adj
    # Leave only defective ties in Y(+)
    def_homo_adj[next_behavior != 0, ] <- 0
    def_homo_adj[ , next_behavior != 0] <- 0
    # Compute the number of defective ties in Y(+)
    num_def_homo <- sum(def_homo_adj)/2
    
    # Compute the sum of the absolute difference in the cumulative wealth Y(+)
    num_score <- sum(plus_adj * score_outer)/2
    # Compute the number of triangle in Y(+)
    # num_triangle <- sum(diag(plus_adj %*% plus_adj %*% plus_adj)) / 6
    
    # Compute the numerator for the formation model
    plus_numerator <- exp(
      (sum(plus_adj) / 2) * lambda[1] +
        num_coop_homo * lambda[2] +
        num_def_homo * lambda[3] +
        num_score * lambda[4])
    
    ########## Formation-Denominator ##########
    
    # Compute the denominator for the formation model
    plus_denominator <- sum(
      # Use list_time_plus[[t]], not "t + 1" because "t" means Y(+) from "t"
      unlist(mclapply(list_time_plus[[t]], function(plus_deno) {
        
        # Y(+) 
        plus_deno_adj <- plus_deno
        
        # Prepare "coop_homo_plus_deno_adj", Y(+)
        coop_homo_plus_deno_adj <- plus_deno_adj
        # Leave only cooperative ties in Y(+) 
        coop_homo_plus_deno_adj[next_behavior != 1, ] <- 0
        coop_homo_plus_deno_adj[ , next_behavior != 1] <- 0
        # Compute the number of cooperative ties in Y(+)
        num_coop_homo_plus_deno <- sum(coop_homo_plus_deno_adj)/2
        
        # Prepare "def_homo_adj", Y(+)
        def_homo_plus_deno_adj <- plus_deno_adj
        # Leave only defective ties in Y(+)
        def_homo_plus_deno_adj[next_behavior != 0, ] <- 0
        def_homo_plus_deno_adj[ , next_behavior != 0] <- 0
        # Compute the number of defective ties in Y(+)
        num_def_homo_plus_deno <- sum(def_homo_plus_deno_adj)/2
        
        # Compute the sum of the absolute difference in the cumulative wealth (Y+)
        num_plus_deno_score <- sum(plus_deno_adj * score_outer)/2
        
        # Compute the number of triangle in Y(+)
        #num_plus_deno_triangle <- sum(
          #diag(plus_deno_adj %*% plus_deno_adj %*% plus_deno_adj)) / 6
        
        # Compute the denominator for the formation model
        exp(
          (sum(plus_deno_adj) / 2) * lambda[1] +
            num_coop_homo_plus_deno * lambda[2] + 
            num_def_homo_plus_deno * lambda[3] +
            num_plus_deno_score * lambda[4])
        
      }, mc.cores = parallel::detectCores() - 1)))
    
    # Formation model for time t + 1
    plus_prob <- plus_numerator / plus_denominator
    
    # Update log-likelihood
    loglik <- loglik + log(plus_prob) 
    
  }
  
  return(loglik)
  
}

# Compute the negative log-likelihood for all the dynamic networks (20)
# {eta: parameters, 
#  pgg_data: a list of all the networks (20) in the network objects,
#  pgg_adj: a list of all the networks (20) in the adjacency matrices,
#  pgg_plus_adj: a list of all the Y(+) networks in the adjacency matrices}
negative_log_likelihood <- function(eta, pgg_data, pgg_adj,
                                    pgg_plus_adj) {
  
  # Set the initial pooled log-likelihood
  loglike_pool <- 0
  
  # Iterate length(pgg_data) = 20 times
  for (n in 1:length(pgg_data)) {
    
    # Compute the log-likelihood for each dynamic network
    loglike <- log_likelihood(
      eta,
      pgg_data[[paste0("net_", n)]],
      pgg_adj[[paste0("net_", n)]],
      pgg_plus_adj[[paste0("net_", n)]])
    
    # Update the pooled log-likelihood
    loglike_pool <- loglike_pool + loglike
    
  }
  
  # Compute the negative log-likelihood
  loglike_pool <- - loglike_pool
  
  print(loglike_pool)
  
  return(loglike_pool)
  
}

#################### Run ####################

# Change the directory if necessary
setwd("~/Desktop/pgg_stergm/")

#Load the data
load("data/pgg_data.RData")
load("data/pgg_adj.RData")
load("data/pgg_plus_adj.RData")

# Estimate the parameters.
# Note that this function might take more than 1.5 hours.
# Initial values are set to 0
result <- optim(par = c(0, 0, 0, 0),
                fn = function(eta) negative_log_likelihood (
                  eta, pgg_data, pgg_adj, pgg_plus_adj), 
                hessian = TRUE,
                method = "BFGS",
                control = list(trace = 1, maxit = 100000))

# Extract MLEs
estimated_params <- result$par
# Extract the maximum log-likelihood
maximum_log_likelihood <- - result$value
# Extract the standard errors
cov_matrix <- solve(result$hessian)
standard_errors <- sqrt(diag(cov_matrix))

# Put the results into a tibble
model_data <- tibble(estimate = estimated_params,
                     se = standard_errors,
                     maximum_log_likelihood = maximum_log_likelihood)

# Save the results
write_csv(model_data, "Results/plus_model_no_triangle.csv")


