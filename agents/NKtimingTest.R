rm(list=ls())
ptm<-proc.time()

#Load pre-computed data
#load strategies
source("strategies.R")

#load network matrices
setwd("Networks")
load("fullNet.Rdata")
load("localNet.Rdata")
setwd("..")

#MODEL PARAMETERS
#number of agents, trials, radius of local search, and sample size (for imitation)
#n.agents=100; tsteps=200; RAD=30; samplesize <- 3 
n.agents=100; tsteps=100; RAD=30; samplesize <- 3 
minRange <- 1
maxRange <- 1001
kVec <- c("5") #K values to use in NK environments

#C) NK environments
N=20
load("store20.Rdata") #matrix storing all one digit neighbors for each NK solution
setwd("NKlandscapes")

#load pre-computed NK environments
setwd("5")
landscapes <- list.files()
total_NK <- matrix(0,ncol=length(strategies),nrow=tsteps) #output matrix of performance over time
#loop through landscapes
load(landscapes[1])#load landscape
agents<-list()
agents[[1]]<-landscape[sample(1:2^N,100,replace=F),]#sample initial starting points
#loop through strategies
for(strat in 1:length(strategies)){
  if(strat==1){ #1. imitation
    for (i in 2:tsteps){
      agents[[i]] <- imitation(agents[[i-1]], fullNet, samplesize =  samplesize , n.agents = n.agents, NK=TRUE)}
  } else if (strat==2){ #2. hybrid with local network
    for (i in 2:tsteps){
        agents[[i]] <- hybrid(landscape, agents[[i-1]], localNet, samplesize =  samplesize , n.agents = n.agents, NK=TRUE, RS = 0, RAD = RAD, maxRange = maxRange, minRange=minRange)}
  } else if (strat ==3){#3. hybrid with full network 
    for (i in 2:tsteps){
        agents[[i]] <- hybrid(landscape, agents[[i-1]], fullNet, samplesize =  samplesize , n.agents = n.agents, NK=TRUE, RS = 0, RAD = RAD, maxRange = maxRange, minRange=minRange)}
  } else if (strat ==4){#4. Hybrid with local network and rand=0.2
    for (i in 2:tsteps){
        agents[[i]] <- hybrid(landscape, agents[[i-1]], localNet, samplesize =  samplesize , n.agents = n.agents, NK=TRUE, RS = 0.2, RAD = RAD, maxRange = maxRange, minRange=minRange)}
  }else if (strat ==5){#5. Hybrid with full network and rand=0.2
    for (i in 2:tsteps){
        agents[[i]] <- hybrid(landscape, agents[[i-1]], fullNet, samplesize =  samplesize , n.agents = n.agents, NK=TRUE, RS = 0.2, RAD = RAD, maxRange = maxRange, minRange=minRange)}
  }else if (strat ==6){#6. hill climbing 
    for (i in 2:tsteps){
        agents[[i]]<-indSearch(landscape, agents[[i-1]], RS= 0, NK = TRUE, n.agents = n.agents, RAD = RAD, maxRange = maxRange, minRange=minRange)}
  } else {#7. random search
    for (i in 2:tsteps){
        agents[[i]]<-indSearch(landscape, agents[[i-1]], RS= 1, NK = TRUE, n.agents = n.agents,  RAD = RAD, maxRange = maxRange, minRange=minRange)}  
    }
  }
total_NK[,strat] <- total_NK[,strat] +  sapply(1:tsteps, function(x) mean(agents[[x]][,2])) #sum performance over all landscapes

 