# "Inference in Multiple Small Dynamic Networks"

Hiroyasu Ando and Mark S. Handcock. (2025). 

### Step (1) Data Preparation

##### Data 1

- pgg_data: Data collected from Public Goods Game. Each network is a igraph object.

- pgg_adj: Adjacency matrices of "pgg_data".

##### Data 2

- pgg_plus_data: Formation network data collected from Public Goods Game. Each network is a igraph object. You can also make this by "plus_network.R".

- pgg_plus_adj: Adjacency matrices of "pgg_plus_data". You can also make this by "plus_network.R".

##### Data 3

- pgg_minus_data: Persistence network data collected from Public Goods Game. Each network is a igraph object. You can also make this by "minus_network.R".

- pgg_minus_adj: Adjacency matrices of "pgg_minus_data". You can also make this by "minus_network.R".

### Step (2) Model Fitting

Run "main.py". This script will generate the results of the paper. Note that this script will take a long time, for an hour or more. 


