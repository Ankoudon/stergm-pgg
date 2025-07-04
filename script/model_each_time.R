# This script fits the public goods game data at each time step (Figures 5 and 6).
# The outputs include the maximum likelihood estimates (MLE), standard errors, and
# log-likelihood values.

# Libraries 
library(igraph)
library(tidyverse)
library(parallel)

#################### Functions ####################

# A function to calculate log-likelihood for each network
# {eta: parameter,
#  net_list: list for one set of 7 dynamic networks in network objects,
#  adj_matrices: list for one set of 7 dynamic networks in the adjacency matrices,
#  list_time_plus: list for one set of Y(+) networks in the adjacency matrices,
#  ist_time_minus: list for one set of Y(-) networks in the adjacency matrices}
log_likelihood <- function(eta, net_list, adj_matrices,
                           list_time_plus, list_time_minus, time_step) {
  
  
  # Set parameters
  lambda <- eta[1:10]
  # Set the initial log-likelihood
  loglik <- 0
  
  for (t in time_step:time_step) {
    
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
    num_triangle <- sum(diag(plus_adj %*% plus_adj %*% plus_adj)) / 6
    
    # Compute the numerator for the formation model
    plus_numerator <- exp(
      (sum(plus_adj) / 2) * lambda[1] +
        num_triangle * lambda[2] +
        num_coop_homo * lambda[3] +
        num_def_homo * lambda[4] +
        num_score * lambda[5])
    
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
        num_plus_deno_triangle <- sum(
          diag(plus_deno_adj %*% plus_deno_adj %*% plus_deno_adj)) / 6
        
        # Compute the denominator for the formation model
        exp(
          (sum(plus_deno_adj) / 2) * lambda[1] +
            num_plus_deno_triangle * lambda[2] +
            num_coop_homo_plus_deno * lambda[3] + 
            num_def_homo_plus_deno * lambda[4] +
            num_plus_deno_score * lambda[5])
        
      }, mc.cores = parallel::detectCores() - 1)))
    
    # Formation model for time t + 1
    plus_prob <- plus_numerator / plus_denominator
    
    ########## Persistence-Numerator ##########
    
    # Compute Y(-) at t + 1
    minus_adj <- adj_matrices[[t]] - adj_matrices[[t + 1]]
    minus_adj[minus_adj < 0] <- 0
    minus_adj <- adj_matrices[[t]] - minus_adj
    
    # Prepare "minus_coop_homo_adj", Y(-)
    minus_coop_homo_adj <- minus_adj
    # Leave only cooperative ties in Y(-)
    minus_coop_homo_adj[next_behavior != 1, ] <- 0
    minus_coop_homo_adj[ , next_behavior != 1] <- 0
    # Compute the number of cooperative ties in Y(-)
    num_minus_coop_homo <- sum(minus_coop_homo_adj) / 2
    
    # Prepare "minus_def_homo_adj", Y(-)
    minus_def_homo_adj <- minus_adj
    # Leave only defective ties in Y(-)
    minus_def_homo_adj[next_behavior != 0, ] <- 0
    minus_def_homo_adj[ , next_behavior != 0] <- 0
    # Compute the number of defective ties in Y(-)
    num_minus_def_homo <- sum(minus_def_homo_adj) / 2
    
    # Compute the sum of the absolute difference in the cumulative wealth in Y(-)
    num_minus_score <- sum(minus_adj * score_outer) / 2
    # Compute the number of triangle in Y(-)
    num_minus_triangle <- sum(diag(minus_adj %*% minus_adj %*% minus_adj)) / 6
    
    # Compute the numerator for the persistence model
    minus_numerator <- exp(
      (sum(minus_adj) / 2) * lambda[6] +
        num_minus_triangle * lambda[7] +
        num_minus_coop_homo * lambda[8] +
        num_minus_def_homo * lambda[9] +
        num_minus_score * lambda[10])
    
    ########## Persistence-Denominator ##########
    
    # Compute the denominator for the persistence model
    minus_denominator <- sum(
      # Use list_time_minus[[t]], not "t + 1" because "t" means Y(-) from "t"
      unlist(mclapply(list_time_minus[[t]], function(minus_deno) {
        
        # Y(-) 
        minus_deno_adj <- minus_deno
        
        # Prepare "coop_homo_minus_deno_adj", Y(-)
        coop_homo_minus_deno_adj <- minus_deno_adj
        # Leave only cooperative ties in Y(-) 
        coop_homo_minus_deno_adj[next_behavior != 1, ] <- 0
        coop_homo_minus_deno_adj[ , next_behavior != 1] <- 0
        # Compute the number of cooperative ties in Y(-)
        num_coop_homo_minus_deno <- sum(coop_homo_minus_deno_adj) / 2
        
        # Prepare "def_homo_minus_deno_adj", Y(-)
        def_homo_minus_deno_adj <- minus_deno_adj
        # Leave only defective ties in Y(-)
        def_homo_minus_deno_adj[next_behavior != 0, ] <- 0
        def_homo_minus_deno_adj[ , next_behavior != 0] <- 0
        # Compute the number of defective ties in Y(-)
        num_def_homo_minus_deno <- sum(def_homo_minus_deno_adj) / 2
        
        # Compute the sum of the absolute difference in the cumulative wealth (Y-)
        num_minus_deno_score <- sum(minus_deno_adj * score_outer) / 2
        
        # Compute the number of triangle in Y(-)
        num_minus_deno_triangle <-  sum(
          diag(minus_deno_adj %*% minus_deno_adj %*% minus_deno_adj)) / 6
        
        # Compute the denominator for the persistence model
        exp(
          (sum(minus_deno_adj) / 2) * lambda[6] +
            num_minus_deno_triangle * lambda[7] +
            num_coop_homo_minus_deno * lambda[8] +
            num_def_homo_minus_deno * lambda[9] +
            num_minus_deno_score * lambda[10])
      }, mc.cores = parallel::detectCores() - 1)))
    
    # Persistence model for time t + 1
    minus_prob <- minus_numerator / minus_denominator
    
    # Update log-likelihood
    loglik <- loglik + log(plus_prob) + log(minus_prob)
    
  }
  
  return(loglik)
  
}

# Compute the negative log-likelihood for all the dynamic networks (20)
# {eta: parameters, 
#  pgg_data: a list of all the networks (20) in the network objects,
#  pgg_adj: a list of all the networks (20) in the adjacency matrices,
#  pgg_plus_adj: a list of all the Y(+) networks in the adjacency matrices,
#  pgg_minus_adj: a list of all the Y(-) networks in the adjacency matrices}
negative_log_likelihood <- function(eta, pgg_data, pgg_adj,
                                    pgg_plus_adj, pgg_minus_adj, time_step) {
  
  # Set the initial pooled log-likelihood
  loglike_pool <- 0
  
  # Iterate length(pgg_data) = 20 times
  for (n in 1:length(pgg_data)) {
    
    # Compute the log-likelihood for each dynamic network
    loglike <- log_likelihood(
      eta,
      pgg_data[[paste0("net_", n)]],
      pgg_adj[[paste0("net_", n)]],
      pgg_plus_adj[[paste0("net_", n)]],
      pgg_minus_adj[[paste0("net_", n)]],
      time_step)
    
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
load("data/pgg_minus_adj.RData")

# Chose time step (1-7)
time_step <- 7

# Estimate the parameters.
# Note that this function might take more 10 minutes.
# Initial values are set to 0
result <- optim(par = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                fn = function(eta) negative_log_likelihood (
                  eta, pgg_data, pgg_adj, pgg_plus_adj, pgg_minus_adj, time_step), 
                hessian = TRUE,
                method = "BFGS",
                control = list(trace = 1, maxit = 100000))

# Extract MLEs
estimated_params <- result$par
# Extract the standard errors
cov_matrix <- solve(result$hessian)
standard_error <- sqrt(diag(cov_matrix))

# Put the results into a tibble
model_data <- tibble(estimate = estimated_params,
                     se = standard_error)
# Save the results
write_csv(model_data, paste0("result/model_", time_step, ".csv"))


