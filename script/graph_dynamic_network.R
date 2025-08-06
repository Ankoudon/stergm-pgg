# Library
library(igraph)
library(network)

# Change a directory if necessary
setwd("~/Desktop/pgg_stergm/")

# Read the data
load("data/pgg_data.RData")

# Extract net_1
net <- pgg_data[["net_1"]]

# Set the layout
par(mfrow = c(2, 4), oma = c(1,1,1,1), mar = c(2, 2, 2, 2))

for (t in 1:8) {
  
  # Extract the network for each time step
  net_t <- net[[t]]
  # Convert the network to an adjacency matrix
  net_adj <- as_adjacency_matrix(net_t, sparse = FALSE)
  # Extract the score and behavior
  net_score <- V(net_t)$score 
  net_behave <- V(net_t)$behavior
  
  # Create a network object (network)
  t_object <- network::network(net_adj, directed = FALSE)
  
  # Set the vertex attributes
  network::set.vertex.attribute(
    t_object, "behavior", net_behave)
  
  network::set.vertex.attribute(
    t_object, "score", net_score)
  
  # Plot the network
  plot(t_object, main = paste0("time ", t - 1), 
       displaylabels = T,
       cex.main = 3,
       label.cex = 2,
       pad = 0.5,
       vertex.cex = network::get.vertex.attribute(t_object, "score")*0.01,
       vertex.col = ifelse(
         network::get.vertex.attribute(t_object, "behavior") == 1,
         "#FF6F61", ifelse(
           network::get.vertex.attribute(t_object, "behavior") == 0,
           "#60A3D9", "gray")))
  
}


