#Generate networks
library(igraph)

#1. Fully connected network

#create graph
d <- graph(sapply(1:100, function(i) {rbind(i, ((i+1):(i+99)-1) %% 100 + 1)}))
#convert into edgelist
network <- get.edgelist(d)
#save
save(network, file="fullNet.Rdata")

#2. Locally connected lattice network (degree = 4)

#create graph
d <- graph(sapply(1:100, function(i) {rbind(i, ((i+1):(i+4)-1) %% 100 + 1)}))
#convert into edgelist
network <- get.edgelist(d)
#save
save(network,file="localNet.Rdata")