# Statistical Modeling of Networked Evolutionary Public Goods Games

### Hiroyasu Ando and Mark S. Handcock. 

### 1. Data

**- pgg_data.RData:** 

A collection of 20 independent dynamic networks from the public goods game, where each network has 8 time points and is represented as an igraph object.

**- pgg_adj.RData:**

A collection of 20 independent dynamic networks from the public goods game, where each network has 8 time points and is represented as an adjacency matrix.

**- pgg_plus_data.Rdata**

All possible $Y^+$ network data for each network from the time 1 to 7, structured identically to "pgg_data.RData". Each element in the list is an igraph object. This list can also be generated using the "plus_network.R" script.

**- pgg_plus_adj.Rdata**

All possible $Y^+$ network data for each network from the time 1 to 7, structured identically to "pgg_data.RData". Each element in the list is an adjacency matrix. This list can also be generated using the "plus_network.R" script.


**- pgg_minus_data.Rdata**

All possible $Y^-$ network data for each network from the time 1 to 7, structured identically to "pgg_data.RData". Each element in the list is an igraph object. This list can also be generated using the "minus_network.R" script.


**- pgg_minus_adj.Rdata** 

All possible $Y^-$ network data for each network from the time 1 to 7, structured identically to "pgg_data.RData". Each element in the list is an adjacency matrix. This list can also be generated using the "minus_network.R" script.

### 2. Scripts

**- plus_network.R**

This script generates all possible $Y^+$ network for each network. The output files are saved as "pgg_plus_data.RData" and "pgg_plus_adj.RData".

**- minus_network.R**

This script generates all possible $Y^-$ network for each network. The output files are saved as "pgg_minus_data.RData" and "pgg_minus_adj.RData".

**- main.R**

This script fits the public goods game (Table 1). The outputs include the maximum likelihood estimates (MLE), standard errors, and the log-likelihood value.

**- sensitivity.R**

This script fits the public goods game data at each time step (Figures 5 and 6). The outputs include the maximum likelihood estimates (MLE), standard errors, and log-likelihood values.

**- no-triangle.R**

This script fits the public goods game data without including triangle terms (Table 2). The outputs are the maximum likelihood estimates (MLE) and the log-likelihood values.

**- graph-5-6.R**

This script generates the graphs for the "sensitivity.R" analysis, corresponding to Figures 5 and 6.

**- graph-4.R**

This script illustrates the dynamics of the public goods game, as shown in Figure 4.