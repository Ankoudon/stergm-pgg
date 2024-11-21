# read the libraries 
library(igraph)

setwd("~/Desktop/stergm-small-multiple-networks/")

# load the data: pgg_data
load("data/pgg_data.RData")

# calculate all the plus graphs
cal_minus_graphs <- function(net) {
  
  node_num <- vcount(net)
  
  all_vertex_pairs <- t(combn(node_num, 2))
  
  bool_connected <- apply(all_vertex_pairs, 1, function(pair) {
    igraph::are.connected(net, pair[1], pair[2])})
  
  connected_pairs <- all_vertex_pairs[bool_connected, ]
  
  combinations <- expand.grid(
    rep(list(c(FALSE, TRUE)), nrow(connected_pairs)))
  
  minus_graphs_list <- list()
  
  if (nrow(combinations) == 0) {
    
    minus_graphs_list[[1]] <- net
    
  } else {
    
    for (j in 1:nrow(combinations)) {
      
      temp_graph <- net
      
      for (k in 1:nrow(connected_pairs)) {
        
        if (combinations[j, k]) {
          
          edge_id <- igraph::get.edge.ids(temp_graph, connected_pairs[k, ],
                                          directed = FALSE)
          temp_graph <- delete_edges(temp_graph, edge_id)
          
        }}
      
      minus_graphs_list[[j]] <- temp_graph
      
    }
    
  }
  
  return(minus_graphs_list)
  
}

# calculate the plus graphs in the dynamic network
cal_minus_dynamic <- function(nets) {
  
  time_step <- length(nets) - 1 
  minus_dynamic_list <- list()
  
  for (t in 1:time_step) {
    
    minus_graphs_list <- cal_minus_graphs(nets[[t]])
    minus_dynamic_list[[t]] <- minus_graphs_list
    
  }
  
  return(minus_dynamic_list)
  
}

# calculate the plus graphs of all the dynamic networks
cal_minus_set <- function(net) {
  
  minus_set_list <- list()
  network_num <- length(net)
  
  for (n in 1:network_num) {
    
    minus_dynamic_list <- cal_minus_dynamic(net[[paste0("net_", n)]])
    minus_set_list[[paste0("net_", n)]] <- minus_dynamic_list
    
  }
  
  return(minus_set_list)
  
}

# run the function
pgg_minus_data <- cal_minus_set(pgg_data)

pgg_minus_adj <- lapply(pgg_minus_data, function(sublist) {
  
  lapply(sublist, function(subsublist) {
    
    lapply(subsublist, function(graph) {
      igraph::as_adjacency_matrix(graph, sparse = FALSE)})
  })
  
})


# save the data
save(pgg_minus_data, file = "data/pgg_minus_data.RData")
save(pgg_minus_adj, file = "data/pgg_minus_adj.RData")
