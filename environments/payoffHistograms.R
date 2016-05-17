#Histograms of payoffs for different environments
rm(list=ls())
#replication ID for cluster
v <- as.integer(commandArgs(TRUE)[1])

#Mason and Watts
source("functions.R")

MWrep <- 100
output <- list()
for (iter in 1:MWrep){
    output[[iter]] <- sort(MasonWatts(1001))
}
#average over replications
averagedOutput <- Reduce("+", output) / length(output)
averagedOutput <- averagedOutput^8 #exponential rescaling

#NK environments
setwd("NKlandscapes/5")

NKoutput <- list()
files <- list.files()
for (n in 1:length(files)){
	load(files[n])
	NKoutput[[n]] <- sort(landscape[,2])
}

averagedNK <- Reduce("+", NKoutput) / length(NKoutput)

setwd("..")
setwd("..")
#2D environments

envNames <- c("Ackley", "Cross-in-Tray", "Drop-Wave", "Eggholder", "Griewank", "Holder Table", "Langermann", "Levy", "Levy n.13", "Rastrigin", "Schaffer n.2", "Schaffer n.4", "Schwefel", "Shubert")
load("environments_rescaled.Rdata")
output2D <- list()
for (env in 1:length(fitness)){
	output2D[[env]] <- sort(fitness[[env]])
}



