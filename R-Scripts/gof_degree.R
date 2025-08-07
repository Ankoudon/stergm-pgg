# Libraries 
library(igraph)
library(tidyverse)
library(parallel)

# Calculate the degree of participants over 20 games at a given time step (1-7)
degree_obs <- function(time_step, adj) {
  
  deg_obs_list <- c()
  
  for (i in 1:length(adj)) {
    
    adj_matrix <- adj[[paste0("net_", i)]]
    
    deg_obs <- as.vector(colSums(adj_matrix[[time_step]]))
    
    deg_obs_list <- c(deg_obs_list, deg_obs)
    
  }
  
  return(deg_obs_list)
  
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

# Simulate the persistence network at a given time step (1-7)
persistence_simu <- function(time_step, lambda, pgg, adj, minus_adj, n){
  
  # list for the simulation results 
  minus_simu_li <- list()
  
  for (i in 1:length(pgg)) {
    
    net_list <- pgg[[paste0("net_", i)]]
    adj_matrices <- adj[[paste0("net_", i)]]
    list_time_minus <- minus_adj[[paste0("net_", i)]]
    
    # Behaviors (t + 1); 1: cooperation, 0: defection, -1: no history
    next_behavior <- V(net_list[[time_step + 1]])$behavior
    # Cumulative wealth / 1000 at t + 1
    next_score <- V(net_list[[time_step + 1]])$score / 1000
    # Compute the absolute difference between actors in "next_score" 
    score_outer <- abs(outer(next_score, next_score, "-"))
    
    # Assign the proportion to each persistence network
    minus_prop <-
      # Use list_time_minus[[t]], not "t + 1" 
      unlist(mclapply(list_time_minus[[time_step]], function(minus_event) {
        
        # Y(-) 
        minus_event_adj <- minus_event
        
        # Compute the number of cooperative ties in Y(-)
        coop_homo_minus_event_adj <- minus_event_adj
        coop_homo_minus_event_adj[next_behavior != 1, ] <- 0
        coop_homo_minus_event_adj[ ,next_behavior != 1] <- 0
        num_coop_homo_minus_event <- sum(coop_homo_minus_event_adj)/2
        
        # Compute the number of defective ties in Y(-)
        def_homo_minus_event_adj <- minus_event_adj
        def_homo_minus_event_adj[next_behavior != 0, ] <- 0
        def_homo_minus_event_adj[ ,next_behavior != 0] <- 0
        num_def_homo_minus_event <- sum(def_homo_minus_event_adj) / 2
        
        # Compute the sum of the absolute difference in the cumulative wealth (Y-)
        num_minus_event_score <- sum(minus_event_adj * score_outer) / 2
        
        # Compute the number of triangle in Y(-)
        num_minus_event_triangle <- sum(
          diag(minus_event_adj %*% minus_event_adj %*% minus_event_adj)) / 6
        
        # Compute the denominator for the persistence model
        exp(
          (sum(minus_event_adj) / 2) * lambda[6] +
            num_minus_event_triangle * lambda[7] +
            num_coop_homo_minus_event * lambda[8] + 
            num_def_homo_minus_event * lambda[9] +
            num_minus_event_score * lambda[10])
        
      }, mc.cores = parallel::detectCores() - 1))
    
    # Assign the probability to each formation network
    minus_prob <- minus_prop / sum(minus_prop)
    
    # Sample based on "minus_prob"
    minus_index <- sample(
      1:length(minus_prob), size = n, prob = minus_prob, replace = TRUE)
    
    minus_adj_sample <- mclapply(minus_index, function(min_ind) {
      
      list_time_minus[[time_step]][[min_ind]]
      
    }, mc.cores = parallel::detectCores() - 1)
    
    minus_simu_li[[i]] <- minus_adj_sample
    
  }
  
  return(minus_simu_li)
  
}

# Whole simulation
whole_simu <- function(time_step, theta, pgg, adj, plus_adj, minus_adj, n){
  
  f_simu <- formation_simu(time_step, theta, pgg, adj, plus_adj, n)
  p_simu <- persistence_simu(time_step, theta, pgg, adj, minus_adj, n)
  
  whole_net_list <- list()
  
  for (i in 1:length(f_simu)) {
    
    previous_net <- adj[[paste0("net_", i)]][[time_step]]
    f_simu_each <- f_simu[[i]]
    p_simu_each <- p_simu[[i]]
    
    # Combine the formation and persistence networks, generating the whole network
    whole_simu_each <- mclapply(1:n, function(ind) {
      
      f_simu_each[[ind]] - previous_net + p_simu_each[[ind]]
      
    }, mc.cores = parallel::detectCores() - 1)
    
    whole_net_list[[i]] <- whole_simu_each
    
  }
  
  return(whole_net_list)
  
}


# Function to calculate the degree goodness-of-fit plot
degree_gof <- function(t, theta, pgg_data, pgg_adj,
                       pgg_plus_adj, pgg_minus_adj, n) {
  
  simu_result <- whole_simu(t, theta, pgg_data, pgg_adj,
                            pgg_plus_adj, pgg_minus_adj, n)
  
  deg_simu_tibble <- tibble(
    deg_0 = as.integer(),
    deg_1 = as.integer(),
    deg_2 = as.integer(),
    deg_3 = as.integer(),
    deg_4 = as.integer(),
    deg_5 = as.integer())
  
  for (s in 1:500) {
    
    deg_simu_each_vec <- c()
    
    for (g in 1:20) {
      
      simu_result_each <- simu_result[[g]][[s]]
      
      deg_simu_each_vec <- c(deg_simu_each_vec,
                             as.vector(colSums(simu_result_each)))
      
    }
    
    deg_simu_counts <- prop.table(
      table(factor(deg_simu_each_vec, levels = c(0, 1, 2, 3, 4, 5))))
    
    new_row <- tibble(
      deg_0 = deg_simu_counts[[1]],
      deg_1 = deg_simu_counts[[2]],
      deg_2 = deg_simu_counts[[3]],
      deg_3 = deg_simu_counts[[4]],
      deg_4 = deg_simu_counts[[5]],
      deg_5 = deg_simu_counts[[6]])
    
    deg_simu_tibble <- bind_rows(deg_simu_tibble, new_row)
    
  }
  
  deg_simu_tibble_wide <- deg_simu_tibble |>
    pivot_longer(cols = names(deg_simu_tibble),
                 names_to = "degree", values_to = "value") |>
    mutate(degree = as.integer(str_remove(degree, "deg_")))
  
  deg_obs_hist <- prop.table(
    table(factor(degree_obs(t + 1, pgg_adj), levels = c(0, 1, 2, 3, 4, 5))))
  
  deg_obs_tibble <- tibble(
    value = c(deg_obs_hist[[1]], deg_obs_hist[[2]], deg_obs_hist[[3]],
              deg_obs_hist[[4]], deg_obs_hist[[5]], deg_obs_hist[[6]]),
    degree = c(0, 1, 2, 3, 4, 5))
  
  deg_percentiles <- deg_simu_tibble_wide |>
    group_by(degree) |>
    summarise(
      lower = quantile(value, 0.025),
      upper = quantile(value, 0.975)
    )
  
  deg_gof_plot <- ggplot() +
    stat_boxplot(
      data = deg_simu_tibble_wide,
      aes(x = factor(degree), y = value), geom = "errorbar") +
    geom_boxplot(
      data = deg_simu_tibble_wide,
      aes(x = factor(degree), y = value), outliers = FALSE, fill = "gray") + 
    geom_point(
      data = deg_obs_tibble,
      aes(x = factor(degree), y = value), group = 1, color = "red", size = 3) +
    geom_line(
      data = deg_obs_tibble,
      aes(x = factor(degree), y = value), group = 1, color = "red", linewidth = 1) +
    geom_line(
      data = deg_percentiles,
      aes(x = factor(degree), y = lower), color = "black", linetype = "dashed", group = 1) +
    geom_line(
      data = deg_percentiles,
      aes(x = factor(degree), y = upper), color = "black", linetype = "dashed", group = 1) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 0.55), breaks = seq(0, 0.55, by = 0.1)) +
    theme_minimal() + 
    theme(
      axis.line = element_line(colour = "black"),
      panel.background = element_blank(),
      axis.title.x = element_text(size = 25),
      axis.title.y = element_text(size = 25),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20))
}

# Change a directory if necessary
setwd("~/Desktop/pgg_stergm/")

# Load the data
load("data/pgg_data.RData")
load("data/pgg_adj.RData")
load("data/pgg_plus_adj.RData")
load("data/pgg_minus_adj.RData")

# Parameters for the simulation 
theta <- c(-1.358, -0.260,  1.069,  0.438, -1.084,
           1.682, -0.023, 1.677, 0.106, -1.099)


# Set the seed for reproducibility
set.seed(1234)
# Run the degree goodness-of-fit plot function
degree_gof_plot_1 <- degree_gof(1, theta, pgg_data, pgg_adj,
                               pgg_plus_adj, pgg_minus_adj, 500) +
  labs(x = "Degree", y = "Proportion of participants") +
  annotate("text", x = 5, y = 0.5, label = "time 1", size = 10)

# Set the seed for reproducibility
set.seed(1234)
degree_gof_plot_2 <- degree_gof(2, theta, pgg_data, pgg_adj,
                                pgg_plus_adj, pgg_minus_adj, 500) +
  annotate("text", x = 5, y = 0.5, label = "time 2", size = 10)

# Set the seed for reproducibility
set.seed(1234)
degree_gof_plot_3 <- degree_gof(3, theta, pgg_data, pgg_adj,
                                pgg_plus_adj, pgg_minus_adj, 500) +
  annotate("text", x = 5, y = 0.5, label = "time 3", size = 10)

# Set the seed for reproducibility
set.seed(1234)
degree_gof_plot_4 <- degree_gof(4, theta, pgg_data, pgg_adj,
                                pgg_plus_adj, pgg_minus_adj, 500) +
  annotate("text", x = 5, y = 0.5, label = "time 4", size = 10)

# Set the seed for reproducibility
set.seed(1234)
degree_gof_plot_5 <- degree_gof(5, theta, pgg_data, pgg_adj,
                                pgg_plus_adj, pgg_minus_adj, 500) +
  annotate("text", x = 5, y = 0.5, label = "time 5", size = 10)

# Set the seed for reproducibility
set.seed(1234)
degree_gof_plot_6 <- degree_gof(6, theta, pgg_data, pgg_adj,
                                pgg_plus_adj, pgg_minus_adj, 500) +
  annotate("text", x = 5, y = 0.5, label = "time 6", size = 10)

# Set the seed for reproducibility
set.seed(1234)
degree_gof_plot_7 <- degree_gof(7, theta, pgg_data, pgg_adj,
                                pgg_plus_adj, pgg_minus_adj, 500) +
  annotate("text", x = 5, y = 0.5, label = "time 7", size = 10)

# Save the plot (10 by 20)
ggpubr::ggarrange(
  degree_gof_plot_1,
  degree_gof_plot_2,
  degree_gof_plot_3, 
  degree_gof_plot_4, 
  degree_gof_plot_5, 
  degree_gof_plot_6, 
  degree_gof_plot_7,
  ncol = 4,
  nrow = 2
)
