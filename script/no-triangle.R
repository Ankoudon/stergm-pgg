# Read the libraries 
library(igraph)
library(tidyverse)
library(parallel)

setwd("~/Desktop/stergm-small-multiple-networks/")

#Load the data: 
load("data/pgg_data.RData")
load("data/pgg_adj.RData")
load("data/pgg_plus_adj.RData")
load("data/pgg_minus_adj.RData")

log_likelihood <- function(eta, net_list, adj_matrices,
                           list_time_plus, list_time_minus) {
  
  
  lambda <- eta[1:8]
  loglik <- 0
  time_step <- length(net_list) - 1
  
  for (t in 1:time_step) {
    
    next_behavior <- V(net_list[[t + 1]])$behavior
    next_score <- V(net_list[[t + 1]])$score / 1000
    
    score_outer <- abs(outer(next_score, next_score, "-"))
    
    plus_adj <- adj_matrices[[t]] + adj_matrices[[t + 1]]
    plus_adj[plus_adj > 0] <- 1
    
    coop_homo_adj <- plus_adj
    coop_homo_adj[next_behavior != 1, ] <- 0
    coop_homo_adj[ , next_behavior != 1] <- 0
    num_coop_homo <- sum(coop_homo_adj) / 2
    
    def_homo_adj <- plus_adj
    def_homo_adj[next_behavior != 0, ] <- 0
    def_homo_adj[ , next_behavior != 0] <- 0
    num_def_homo <- sum(def_homo_adj)/2
    
    num_score <- sum(plus_adj * score_outer)/2
    num_triangle <- sum(diag(plus_adj %*% plus_adj %*% plus_adj)) / 6
    
    plus_numerator <- exp(
      (sum(plus_adj) / 2) * lambda[1] +
        num_coop_homo * lambda[2] +
        num_def_homo * lambda[3] +
        num_score * lambda[4])
    
    plus_denominator <- sum(
      unlist(mclapply(list_time_plus[[t]], function(plus_deno) {
        
        plus_deno_adj <- plus_deno
        
        coop_homo_plus_deno_adj <- plus_deno_adj
        coop_homo_plus_deno_adj[next_behavior != 1, ] <- 0
        coop_homo_plus_deno_adj[ , next_behavior != 1] <- 0
        num_coop_homo_plus_deno <- sum(coop_homo_plus_deno_adj)/2
        
        def_homo_plus_deno_adj <- plus_deno_adj
        def_homo_plus_deno_adj[next_behavior != 0, ] <- 0
        def_homo_plus_deno_adj[ , next_behavior != 0] <- 0
        num_def_homo_plus_deno <- sum(def_homo_plus_deno_adj)/2
        
        num_plus_deno_score <- sum(plus_deno_adj * score_outer)/2
        
        num_plus_deno_triangle <- sum(
          diag(plus_deno_adj %*% plus_deno_adj %*% plus_deno_adj)) / 6
        
        exp(
          (sum(plus_deno_adj) / 2) * lambda[1] +
            num_coop_homo_plus_deno * lambda[2] + 
            num_def_homo_plus_deno * lambda[3] +
            num_plus_deno_score * lambda[4])
        
      }, mc.cores = parallel::detectCores() - 1)))
    
    plus_prob <- plus_numerator / plus_denominator
    
    minus_adj <- adj_matrices[[t]] - adj_matrices[[t + 1]]
    minus_adj[minus_adj < 0] <- 0
    minus_adj <- adj_matrices[[t]] - minus_adj
    
    minus_coop_homo_adj <- minus_adj
    minus_coop_homo_adj[next_behavior != 1, ] <- 0
    minus_coop_homo_adj[ , next_behavior != 1] <- 0
    num_minus_coop_homo <- sum(minus_coop_homo_adj) / 2
    
    minus_def_homo_adj <- minus_adj
    minus_def_homo_adj[next_behavior != 0, ] <- 0
    minus_def_homo_adj[ , next_behavior != 0] <- 0
    num_minus_def_homo <- sum(minus_def_homo_adj) / 2
    
    num_minus_score <- sum(minus_adj * score_outer) / 2
    
    num_minus_triangle <- sum(diag(minus_adj %*% minus_adj %*% minus_adj)) / 6
    
    minus_numerator <- exp(
      (sum(minus_adj) / 2) * lambda[5] +
        num_minus_coop_homo * lambda[6] +
        num_minus_def_homo * lambda[7] +
        num_minus_score * lambda[8])
    
    minus_denominator <- sum(
      unlist(mclapply(list_time_minus[[t]], function(minus_deno) {
        
        minus_deno_adj <- minus_deno
        
        coop_homo_minus_deno_adj <- minus_deno_adj
        coop_homo_minus_deno_adj[next_behavior != 1, ] <- 0
        coop_homo_minus_deno_adj[ , next_behavior != 1] <- 0
        num_coop_homo_minus_deno <- sum(coop_homo_minus_deno_adj) / 2
        
        def_homo_minus_deno_adj <- minus_deno_adj
        def_homo_minus_deno_adj[next_behavior != 0, ] <- 0
        def_homo_minus_deno_adj[ , next_behavior != 0] <- 0
        num_def_homo_minus_deno <- sum(def_homo_minus_deno_adj) / 2
        
        num_minus_deno_score <- sum(minus_deno_adj * score_outer) / 2
        
        num_minus_deno_triangle <-  sum(
          diag(minus_deno_adj %*% minus_deno_adj %*% minus_deno_adj)) / 6
        
        exp(
          (sum(minus_deno_adj) / 2) * lambda[5] +
            num_coop_homo_minus_deno * lambda[6] +
            num_def_homo_minus_deno * lambda[7] +
            num_minus_deno_score * lambda[8])
      }, mc.cores = parallel::detectCores() - 1)))
    
    minus_prob <- minus_numerator / minus_denominator
    
    # Update log-likelihood
    loglik <- loglik + log(plus_prob) + log(minus_prob)
    
  }
  
  return(loglik)
  
}

negative_log_likelihood <- function(eta, pgg_data, pgg_adj,
                                    pgg_plus_adj, pgg_minus_adj) {
  
  loglike_pool <- 0
  
  for (n in 1:length(pgg_data)) {
    
    loglike <- log_likelihood(
      eta,
      pgg_data[[paste0("net_", n)]],
      pgg_adj[[paste0("net_", n)]],
      pgg_plus_adj[[paste0("net_", n)]],
      pgg_minus_adj[[paste0("net_", n)]])
    
    loglike_pool <- loglike_pool + loglike
    
  }
  
  loglike_pool <- - loglike_pool
  
  print(loglike_pool)
  
  return(loglike_pool)
  
}

result <- optim(par = c(0, 0, 0, 0, 0, 0, 0, 0),
                fn = function(eta) negative_log_likelihood (
                  eta, pgg_data, pgg_adj, pgg_plus_adj, pgg_minus_adj), 
                #hessian = TRUE,
                method = "BFGS",
                control = list(trace = 1, maxit = 100000))

estimated_params <- result$par
maximum_log_likelihood <- - result$value
#cov_matrix <- solve(result$hessian)
#standard_errors <- sqrt(diag(cov_matrix))

model_data <- tibble(estimate = estimated_params,
                     #se = standard_errors,
                     maximum_log_likelihood = maximum_log_likelihood)

write_csv(model_data, "result/model-dev.csv")