# This script generates all possible Y(+) network for each network. 
# The output files are saved as "pgg_plus_data.RData" and "pgg_plus_adj.RData".

# Library
library(igraph)

#################### Functions ####################

# Calculate all the plus graphs from a network
# net: an igraph object
cal_plus_graphs <- function(net) {
  
  # Number of nodes
  node_num <- vcount(net)
  
  # All the vertex pairs: 15 pairs
  all_vertex_pairs <- t(combn(node_num, 2))
  
  # Check if the vertex pairs are connected
  bool_connected <- apply(all_vertex_pairs, 1, function(pair) {
      igraph::are.connected(net, pair[1], pair[2])})
  
  # Non-connected pairs
  non_connected_pairs <- all_vertex_pairs[!bool_connected, ]
  
  # All the combinations of non-connected pairs
  combinations <- expand.grid(
    rep(list(c(FALSE, TRUE)), sum(!bool_connected)))
  
  # A list for all the plus graphs
  plus_graphs_list <- list()
  
  # If there is no non-connected pairs
  if (sum(!bool_connected) == 0) {
    
    # The plus graph is the same as the input network
    graph_plus_list[[1]] <- net
    
  } else {
    
    # Generate all the plus graphs
    for (j in 1:nrow(combinations)) {
      
      # Copy the input network
      temp_graph <- net
      
      # Delete the edges based on the combinations
      for (k in 1:sum(!bool_connected)) {
        
        # If the combination is TRUE
        if (combinations[j, k]) {
          
          # Define "add_pair_vec" as the non-connected pairs to add
          if (sum(!bool_connected) == 1) {
            
            add_pair_vec <- non_connected_pairs
            
          } else {
            
            add_pair_vec <- non_connected_pairs[k, ]
            
          }
          
          # Add the edge
          temp_graph <- add_edges(temp_graph, add_pair_vec)
          
        }}
      
      # Add the plus graph to the list
      plus_graphs_list[[j]] <- temp_graph
      
    }
    
  }
  
  return(plus_graphs_list)

}

# Calculate the plus graphs in a set of dynamic networks (7)
# nets: a set of dynamic networks
cal_plus_dynamic <- function(nets) {
  
  # Number of time steps -1 : 7
  # Note that there are 8 networks in "nets", but calculate the plus graphs for 7 networks.
  time_step <- length(nets) - 1
  # A list for all the plus graphs
  plus_dynamic_list <- list()
  
  # Calculate the plus graphs for each time step
  for (t in 1:time_step) {
    
    # Calculate the plus graphs
    plus_graphs_list <- cal_plus_graphs(nets[[t]])
    # Save the plus graphs
    plus_dynamic_list[[t]] <- plus_graphs_list
    
  }
  
  return(plus_dynamic_list)
  
}

# Calculate the plus graphs for all the 20 dynamic networks
# net: a list of 20 dynamic networks
cal_plus_set <- function(net) {
  
  # A list for all the plus graphs
  plus_set_list <- list()
  # Number of networks: 20
  network_num <- length(net)
  
  for (n in 1:network_num) {
    
    # Calculate the plus graphs for each dynamic network
    plus_dynamic_list <- cal_plus_dynamic(net[[paste0("net_", n)]])
    # Save the plus graphs
    plus_set_list[[paste0("net_", n)]] <- plus_dynamic_list
    
  }
  
  return(plus_set_list)
  
}

#################### Run ####################

# Change a directory if necessary
setwd("~/Desktop/pgg_stergm/")

# load the data: pgg_data
load("data/pgg_data.RData")

# Run the function
pgg_plus_data <- cal_plus_set(pgg_data)

# Convert the plus graphs to adjacency matrices
pgg_plus_adj <- lapply(pgg_plus_data, function(sublist) {
  
  lapply(sublist, function(subsublist) {
    
    lapply(subsublist, function(graph) {
      igraph::as_adjacency_matrix(graph, sparse = FALSE)})
    })
  
  })


# Save the data
save(pgg_plus_data, file = "data/pgg_plus_data.RData")
save(pgg_plus_adj, file = "data/pgg_plus_adj.RData")
