# read the libraries 
library(igraph)

# load the data: pgg_data
load("data/pgg_data.RData")

# calculate all the plus graphs
cal_plus_graphs <- function(net) {
  
  node_num <- vcount(net)
  
  all_vertex_pairs <- t(combn(node_num, 2))
  
  bool_connected <- apply(all_vertex_pairs, 1, function(pair) {
      igraph::are.connected(net, pair[1], pair[2])})
  
  non_connected_pairs <- all_vertex_pairs[! bool_connected, ]
  
  combinations <- expand.grid(
    rep(list(c(FALSE, TRUE)), nrow(non_connected_pairs)))
  
  plus_graphs_list <- list()
  
  if (nrow(combinations) == 0) {
    
    graph_plus_list[[1]] <- net
    
  } else {
    
    for (j in 1:nrow(combinations)) {
      
      temp_graph <- net
      
      for (k in 1:nrow(non_connected_pairs)) {
        
        if (combinations[j, k]) {
          
          temp_graph <- add_edges(temp_graph, non_connected_pairs[k, ])
          
        }}
      
      plus_graphs_list[[j]] <- temp_graph
      
    }
    
  }
  
  return(plus_graphs_list)

}

# calculate the plus graphs in the dynamic network
cal_plus_dynamic <- function(nets) {
  
  time_step <- length(nets) - 1
  plus_dynamic_list <- list()
  
  for (t in 1:time_step) {
    
    plus_graphs_list <- cal_plus_graphs(nets[[t]])
    plus_dynamic_list[[t]] <- plus_graphs_list
    
  }
  
  return(plus_dynamic_list)
  
}

# calculate the plus graphs of all the dynamic networks
cal_plus_set <- function(net) {
  
  plus_set_list <- list()
  network_num <- length(net)
  
  for (n in 1:network_num) {
    
    plus_dynamic_list <- cal_plus_dynamic(net[[paste0("net_", n)]])
    plus_set_list[[paste0("net_", n)]] <- plus_dynamic_list
    
  }
  
  return(plus_set_list)
  
}

# run the function
pgg_plus_data <- cal_plus_set(pgg_data)


pgg_plus_adj <- lapply(pgg_plus_data, function(sublist) {
  
  lapply(sublist, function(subsublist) {
    
    lapply(subsublist, function(graph) {
      igraph::as_adjacency_matrix(graph, sparse = FALSE)})
    })
  
  })


# save the data
save(pgg_plus_data, file = "data/pgg_plus_data.RData")
save(pgg_plus_adj, file = "data/pgg_plus_adj.RData")
