library(igraph)
library(network)

load("data/pgg_data.RData")

net <- pgg_data[["net_1"]]

par(mfrow = c(2, 4), oma = c(1,1,1,1), mar = c(4,2,2,2))

for (t in 1:8) {
  
  net_t <- net[[t]]
  net_adj <- as_adjacency_matrix(net_t, sparse = FALSE)
  net_score <- V(net_t)$score 
  net_behave <- V(net_t)$behavior
  
  t_object <- network::network(net_adj, directed = FALSE)
  
  network::set.vertex.attribute(
    t_object, "behavior", net_behave)
  
  network::set.vertex.attribute(
    t_object, "score", net_score)
  
  plot(t_object, main = paste0("time ", t-1), 
       displaylabels = T,
       cex.main = 3,
       label.cex = 1,
       pad = 0.5,
       vertex.cex = network::get.vertex.attribute(t_object, "score")*0.01,
       vertex.col = ifelse(
         network::get.vertex.attribute(t_object, "behavior") == 1,
         "#FF6F61", ifelse(
           network::get.vertex.attribute(t_object, "behavior") == 0,
           "#60A3D9", "gray")))
  
}


