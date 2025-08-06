# This script generates all possible Y(-) network for each network. The output
# files are saved as "pgg_minus_data.RData" and "pgg_minus_adj.RData".

# Library 
library(igraph)

#################### Functions ####################

# Calculate all the minus graphs from a network
# net: an igraph object
cal_minus_graphs <- function(net) {
  
  # Number of nodes: 6
  node_num <- vcount(net)
  
  # All the vertex pairs: 15 pairs
  all_vertex_pairs <- t(combn(node_num, 2))
  # Check if the vertex pairs are connected in the network, net
  bool_connected <- apply(all_vertex_pairs, 1, function(pair) {
    igraph::are.connected(net, pair[1], pair[2])})
  
  # Extract the connected pairs
  connected_pairs <- all_vertex_pairs[bool_connected, ]
  
  # Generate all the combinations of the connected pairs
  combinations <- expand.grid(
    rep(list(c(FALSE, TRUE)), sum(bool_connected)))
  
  # A list for all the minus graphs
  minus_graphs_list <- list()
  
  # If there is no connected pairs
  if (sum(bool_connected) == 0) {
    
    # The minus graph is the same as the input network
    minus_graphs_list[[1]] <- net
    
  } else {
    
    # Generate all the minus graphs
    for (j in 1:nrow(combinations)) {
      
      # Copy the input network
      temp_graph <- net
      
      # Delete the edges based on the combinations
      for (k in 1:sum(bool_connected)) {
        
        # If the combination is TRUE
        if (combinations[j, k]) {
          
          # Define "remove_pair_vec" as the connected pairs to remove
          if (sum(bool_connected) == 1) {
            
            remove_pair_vec <- connected_pairs
            
          } else {
            
            remove_pair_vec <- connected_pairs[k, ]
            
          }
          
          # Get the edge id
          edge_id <- igraph::get.edge.ids(temp_graph, remove_pair_vec,
                                          directed = FALSE)
          # Delete the edge
          temp_graph <- delete_edges(temp_graph, edge_id)
          
        }}
      
      # Save the minus graph
      minus_graphs_list[[j]] <- temp_graph
      
    }
    
  }
  
  return(minus_graphs_list)
  
}

# Calculate the minus graphs in a set of dynamic networks (7)
# nets: a set of dynamic networks
cal_minus_dynamic <- function(nets) {
  
  # Number of time steps - 1 : 7
  # Note that there are 8 networks in "nets", but calculate the minus graphs for 7 networks.
  time_step <- length(nets) - 1 
  # A list for all the minus graphs
  minus_dynamic_list <- list()
  
  # Calculate the minus graphs for each time step
  for (t in 1:time_step) {
    
    # Calculate the minus graphs
    minus_graphs_list <- cal_minus_graphs(nets[[t]])
    # Save the minus graphs
    minus_dynamic_list[[t]] <- minus_graphs_list
    
  }
  
  return(minus_dynamic_list)
  
}

# Calculate the minus graphs for all the 20 dynamic networks
# net: a list of 20 dynamic networks
cal_minus_set <- function(net) {
  
  # A list for all the minus graphs
  minus_set_list <- list()
  # Number of networks: 20
  network_num <- length(net)
  
  for (n in 1:network_num) {
    
    # Calculate the minus graphs for each dynamic network
    minus_dynamic_list <- cal_minus_dynamic(net[[paste0("net_", n)]])
    # Save the minus graphs
    minus_set_list[[paste0("net_", n)]] <- minus_dynamic_list
    
  }
  
  return(minus_set_list)
  
}

#################### Run ####################

# Change a directory if necessary
setwd("~/Desktop/pgg_stergm/")

# load the data: pgg_data
load("data/pgg_data.RData")

# Run the function
pgg_minus_data <- cal_minus_set(pgg_data)

# Convert the minus graphs to adjacency matrices
pgg_minus_adj <- lapply(pgg_minus_data, function(sublist) {
  
  lapply(sublist, function(subsublist) {
    
    lapply(subsublist, function(graph) {
      igraph::as_adjacency_matrix(graph, sparse = FALSE)})
  })
  
})


# Save the data
save(pgg_minus_data, file = "data/pgg_minus_data.RData")
save(pgg_minus_adj, file = "data/pgg_minus_adj.RData")
  