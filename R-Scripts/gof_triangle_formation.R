# Libraries 
library(igraph)
library(tidyverse)
library(parallel)

# Calculate the triangles  at a given time step (1-7)
triangle_plus_obs <- function(time_step, adj) {
  
  
  sum_tri_obs_list <- 0
  
  for (i in 1:length(adj)) {
    
    adj_matrix <- adj[[paste0("net_", i)]]
    plus_adj <- adj_matrix[[time_step]] + adj_matrix[[time_step + 1]]
    plus_adj[plus_adj > 0] <- 1
    
    tri_obs <- sum(diag(plus_adj %*% plus_adj %*% plus_adj)) / 6
    
    sum_tri_obs_list <- sum_tri_obs_list + tri_obs
    
  }
  
  return(sum_tri_obs_list)
  
}

# Simulate the formation network at a given time step (1-7)
formation_simu <- function(time_step, lambda, pgg, adj, plus_adj, n){
  
  # list for the simulation results 
  plus_simu_li <- list()
  
  for (i in 1:length(pgg)) {
    
    net_list <- pgg[[paste0("net_", i)]]
    adj_matrices <- adj[[paste0("net_", i)]]
    list_time_plus <- plus_adj[[paste0("net_", i)]]
    
    # Behaviors (t + 1); 1: cooperation, 0: defection, -1: no history
    next_behavior <- V(net_list[[time_step + 1]])$behavior
    # Cumulative wealth / 1000 at t + 1
    next_score <- V(net_list[[time_step + 1]])$score / 1000
    # Compute the absolute difference between actors in "next_score" 
    score_outer <- abs(outer(next_score, next_score, "-"))
    
    # Assign the proportion to each formation network
    plus_prop <-
      # Use list_time_plus[[t]], not "t + 1" 
      unlist(mclapply(list_time_plus[[time_step]], function(plus_event) {
        
        # Y(+) 
        plus_event_adj <- plus_event
        
        # Compute the number of cooperative ties in Y(+)
        coop_homo_plus_event_adj <- plus_event_adj
        coop_homo_plus_event_adj[next_behavior != 1, ] <- 0
        coop_homo_plus_event_adj[ ,next_behavior != 1] <- 0
        num_coop_homo_plus_event <- sum(coop_homo_plus_event_adj)/2
        
        # Compute the number of defective ties in Y(+)
        def_homo_plus_event_adj <- plus_event_adj
        def_homo_plus_event_adj[next_behavior != 0, ] <- 0
        def_homo_plus_event_adj[ ,next_behavior != 0] <- 0
        num_def_homo_plus_event <- sum(def_homo_plus_event_adj) / 2
        
        # Compute the sum of the absolute difference in the cumulative wealth (Y+)
        num_plus_event_score <- sum(plus_event_adj * score_outer) / 2
        
        # Compute the number of triangle in Y(+)
        num_plus_event_triangle <- sum(
          diag(plus_event_adj %*% plus_event_adj %*% plus_event_adj)) / 6
        
        # Compute the denominator for the formation model
        exp(
          (sum(plus_event_adj) / 2) * lambda[1] +
            num_plus_event_triangle * lambda[2] +
            num_coop_homo_plus_event * lambda[3] + 
            num_def_homo_plus_event * lambda[4] +
            num_plus_event_score * lambda[5])
        
      }, mc.cores = parallel::detectCores() - 1))
    
    # Assign the probability to each formation network
    plus_prob <- plus_prop / sum(plus_prop)
    
    # Sample based on "plus_prob"
    plus_index <- sample(
      1:length(plus_prob), size = n, prob = plus_prob, replace = TRUE)
    
    plus_adj_sample <- mclapply(plus_index, function(plus_ind) {
      
      list_time_plus[[time_step]][[plus_ind]]
      
    }, mc.cores = parallel::detectCores() - 1)
    
    plus_simu_li[[i]] <- plus_adj_sample
    
    
  }
  
  return(plus_simu_li)
  
}

triangle_simu_tibble_gen <- function(theta, pgg_data, pgg_adj, pgg_plus_adj, n) {
  
  tri_simu_tibble <- tibble(
    value = as.integer(),
    time = as.integer())
  
  for (t in 1:7) {
    
    simu_result <- formation_simu(t, theta, pgg_data, pgg_adj, pgg_plus_adj, n)
    
    tri_simu_vec <- c()
    
    for (s in 1:500) {
      
      sum_tri_simu <- 0
      
      for (g in 1:20) {
        
        simu_result_each <- simu_result[[g]][[s]]
        
        simu_tri_each <- sum(diag(simu_result_each %*% simu_result_each %*% simu_result_each)) / 6
        
        sum_tri_simu <- sum_tri_simu + simu_tri_each
        
      }
      
      tri_simu_vec <- c(tri_simu_vec, sum_tri_simu)
      
    }
    
    tri_simu_new_row <- tibble(
      value = tri_simu_vec,
      time = t
    )
    
    tri_simu_tibble <- bind_rows(tri_simu_tibble, tri_simu_new_row)
    
  }
  
  # Divide the value by 120 to adjust the scale (per participant per time step)
  tri_simu_tibble <- tri_simu_tibble |>
    mutate(value = value / 120) 
  
  return(tri_simu_tibble)
  
}

# Change a directory if necessary
setwd("~/Desktop/pgg_stergm/")

# Load the data
load("data/pgg_data.RData")
load("data/pgg_adj.RData")
load("data/pgg_plus_adj.RData")

# Create the triangle observation tibble
triangle_obs_tibble <- tibble(
  value = c(triangle_plus_obs(1, pgg_adj) / 120,
            triangle_plus_obs(2, pgg_adj) / 120,
            triangle_plus_obs(3, pgg_adj) / 120,
            triangle_plus_obs(4, pgg_adj) / 120,
            triangle_plus_obs(5, pgg_adj) / 120,
            triangle_plus_obs(6, pgg_adj) / 120,
            triangle_plus_obs(7, pgg_adj) / 120),
  time = c(1, 2, 3, 4, 5, 6, 7))

# Parameters for the simulation 
# with the triangle term
theta_1 <- c(-1.358, -0.260,  1.069,  0.438, -1.084)
# without the triangle term
theta_2 <- c(-1.645, 0, 0.983, 0.381, -1.298)

# Set the seed for reproducibility
set.seed(1234)
# Generate the triangle simulation tibble (with the triangle term)
triangle_simu_tibble_1 <- triangle_simu_tibble_gen(
  theta = theta_1, 
  pgg_data = pgg_data, 
  pgg_adj = pgg_adj, 
  pgg_plus_adj = pgg_plus_adj, 
  n = 500)

# set the seed for reproducibility
set.seed(1234)
# Generate the triangle simulation tibble (without the triangle term)
triangle_simu_tibble_2 <- triangle_simu_tibble_gen(
  theta = theta_2, 
  pgg_data = pgg_data, 
  pgg_adj = pgg_adj, 
  pgg_plus_adj = pgg_plus_adj, 
  n = 500)


combined_tri_simu_tibble <- bind_rows(
  triangle_simu_tibble_1 |> mutate(model = "main"),
  triangle_simu_tibble_2 |> mutate(model = "only-covariates")) 

combined_tri_simu_tibble <- combined_tri_simu_tibble |>
  mutate(model = factor(model, levels = c("main", "only-covariates")))

gof_triangle <- ggplot() +
  stat_boxplot(
    data = combined_tri_simu_tibble,
    aes(x = factor(time), y = value, color = model), geom= 'errorbar') +
  geom_boxplot(
    data = combined_tri_simu_tibble,
    aes(x = factor(time), y = value, color = model), outliers = FALSE) +
  scale_color_manual(
    values = c("main" = "darkgreen", "only-covariates" = "lightgreen")) +
  geom_point(
    data = triangle_obs_tibble, aes(x = factor(time), y = value),
    group = 1, color = "red", size = 3) +
  geom_line(
    data = triangle_obs_tibble, aes(x = factor(time), y = value),
    group = 1, color = "red", linewidth = 1) +
  labs(x = "Time step",
       y = "Frequency per participant") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  theme_minimal() + 
  theme(
    legend.position = c(0.8, 0.2),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)) 


