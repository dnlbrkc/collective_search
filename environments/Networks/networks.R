#Generate networks

library(igraph)
#fully connected network

d <- graph(sapply(1:100, function(i) {
  rbind(i, ((i+1):(i+99)-1) %% 100 + 1)
}))

network <- get.edgelist(d)
save(network, file="fullNet.Rdata")

d <- graph(sapply(1:100, function(i) {
  rbind(i, ((i+1):(i+4)-1) %% 100 + 1)
}))

network <- get.edgelist(d)

save(network,file="localNet.Rdata")