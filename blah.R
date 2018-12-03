library(igraph)
library(magrittr)
library(pracma)   # for gcd function
library(igraph)   # graph library

#Figure 11.1

fig11_1 <- matrix(c(
  0,1,1,0,1,1,
  0,0,0,1,1,0,
  0,1,0,1,1,1,
  1,0,0,0,0,0,
  0,0,0,1,0,1,
  0,1,0,1,0,0
),byrow = T,
nrow = 6,
ncol = 6) %>%
  graph_from_adjacency_matrix 


coef_of_consistency <- function(g){
  if(
    length(V(g)) %% 2 == 0
  ){
   d <- get_n_cycles_directed(fig11_1, n = 3, list_cycles = F)$count
   
   n <- length(V(g))
   
   zeta <- 1 - ((24*d) / ((n^3) - 4*n))
   
   return(zeta)
   
  } else   if(
    length(V(fig11_1)) %% 2 != 0
  ){
    d <- get_n_cycles_directed(fig11_1, n = 3, list_cycles = F)$count
    
    zeta <- 1 - ((24*d) / ((n^3) - n))
    
    return(zeta)
  }
}
