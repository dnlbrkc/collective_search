rm(list=ls())

#replication ID for cluster
v <- as.integer(commandArgs(TRUE)[1])

#Load pre-computed data
#load strategies
source("strategies.R")
#set working directory to "environments" to load fitness matrices
setwd("..")
setwd("environments")
load("environments.Rdata")
source("functions.R") #environment functions
#load network matrices
setwd("Networks")
load("fullNet.Rdata")
load("localNet.Rdata")
setwd("..")

#MODEL PARAMETERS
#number of agents, trials, radius of local search, and sample size (for imitation)
#n.agents=100; tsteps=200; RAD=30; samplesize <- 3 
n.agents=100; tsteps=100; RAD=3; samplesize <- 3 

#list of 2D environments to loop through
environmentList <- c("ackley", "crossit", "drop", "egg", "griewank", "holder", "langer", "levy", "levy13", "rastr", "schaffer2", "schaffer4", "schwef", "shubert")
n_envs <- length(environmentList)
minRange <- 1
maxRange <- 1001
kVec <- c("5","10","16") #K values to use in NK environments
#Model search strategies
strategies <- c("Imitation", "hybridLocal", "hybridFull", "hybridLocalRand", "hybridFullRand", "hillClimbing", "Random")

#output array
output <- array(dim=c(tsteps,n_envs+1+length(kVec),length(strategies)))

#A) 2D rugged landscapes
for (env in 1:length(fitness)){ #loop through 2D landscapes
	#Initialize random starting position for n.agents
	agents <- list()
	choices <- t(replicate(n.agents, sample(1001,2))) #random starting location
	payoffs<- apply(choices, 1, function(x) fitness[[env]][x[1],x[2]]) #look up payoff from fitness matrix
	agents[[1]] <- cbind(choices,payoffs)  #combine choices and payoffs in matrix row: [x, y, payoff], col: n.agents
	#loop through strategies
	for(strat in 1:length(strategies)){
		if(strat==1){ #1. imitation
			for (i in 2:tsteps){
				agents[[i]] <- imitation(agents[[i-1]], fullNet, samplesize =  samplesize , n.agents = n.agents)}
		} else if (strat==2){ #2. hybrid with local network
			for (i in 2:tsteps){
			    agents[[i]] <- hybrid(fitness[[env]], agents[[i-1]], localNet, samplesize =  samplesize , n.agents = n.agents, RS = 0, RAD = RAD, maxRange = maxRange, minRange=minRange)}
		} else if (strat ==3){#3. hybrid with full network 
			for (i in 2:tsteps){
			    agents[[i]] <- hybrid(fitness[[env]], agents[[i-1]], fullNet, samplesize =  samplesize , n.agents = n.agents, RS = 0, RAD = RAD, maxRange = maxRange, minRange=minRange)}
		} else if (strat ==4){#4. Hybrid with local network and rand=0.2
			for (i in 2:tsteps){
			    agents[[i]] <- hybrid(fitness[[env]], agents[[i-1]], localNet, samplesize =  samplesize , n.agents = n.agents, RS = 0.2, RAD = RAD, maxRange = maxRange, minRange=minRange)}
		}else if (strat ==5){#5. Hybrid with full network and rand=0.2
			for (i in 2:tsteps){
			    agents[[i]] <- hybrid(fitness[[env]], agents[[i-1]], fullNet, samplesize =  samplesize , n.agents = n.agents, RS = 0.2, RAD = RAD, maxRange = maxRange, minRange=minRange)}
		}else if (strat ==6){#6. hill climbing 
			for (i in 2:tsteps){
			    agents[[i]]<-indSearch(fitness[[env]], agents[[i-1]], RS= 0, NK = FALSE, n.agents = n.agents, RAD = RAD, maxRange = maxRange, minRange=minRange)}
		} else {#7. random search
			for (i in 2:tsteps){
			    agents[[i]]<-indSearch(fitness[[env]], agents[[i-1]], RS= 1, NK = FALSE, n.agents = n.agents, RAD = RAD, maxRange = maxRange, minRange=minRange)}  
			}
		#save results  
		output[,env,strat]<-sapply(1:tsteps, function(x) mean(agents[[x]][,3]))
	}
}

#B) Mason and Watts environment
#TODO: either save randomization of environments, or pre-compute them (for calculating peaks, mu, and sigma)
env=n_envs+1 #env = 15 for Mason and Watts
total <- matrix(0,ncol=length(strategies),nrow=tsteps) #output matrix
#loop through replications
for(repM in 1:100){
  MasonWattsEnv <- MasonWatts(1001) #each different replication gets a different randomization of the environment
  agents <- list()
  choices <- t(replicate(n.agents, sample(1001,2))) #random starting location
  payoffs<- apply(choices, 1, function(x) MasonWattsEnv[x[1],x[2]]) #look up payoff from fitness matrix
  #combine choices and payoffs in matrix row: [x, y, payoff], col: n.agents
  agents[[1]] <- cbind(choices,payoffs)  #initialization
  #loop through strategies
	for(strat in 1:length(strategies)){
    if(strat==1){ #1. imitation
      for (i in 2:tsteps){
        agents[[i]] <- imitation(agents[[i-1]], fullNet, samplesize =  samplesize , n.agents = n.agents)}
    } else if (strat==2){ #2. hybrid with local network
      for (i in 2:tsteps){
          agents[[i]] <- hybrid(MasonWattsEnv, agents[[i-1]], localNet, samplesize =  samplesize , n.agents = n.agents, RS = 0, RAD = RAD, maxRange = maxRange, minRange=minRange)}
    } else if (strat ==3){#3. hybrid with full network 
      for (i in 2:tsteps){
          agents[[i]] <- hybrid(MasonWattsEnv, agents[[i-1]], fullNet, samplesize =  samplesize , n.agents = n.agents, RS = 0, RAD = RAD, maxRange = maxRange, minRange=minRange)}
    } else if (strat ==4){#4. Hybrid with local network and rand=0.2
      for (i in 2:tsteps){
          agents[[i]] <- hybrid(MasonWattsEnv, agents[[i-1]], localNet, samplesize =  samplesize , n.agents = n.agents, RS = 0.2, RAD = RAD, maxRange = maxRange, minRange=minRange)}
    }else if (strat ==5){#5. Hybrid with full network and rand=0.2
      for (i in 2:tsteps){
          agents[[i]] <- hybrid(MasonWattsEnv, agents[[i-1]], fullNet, samplesize =  samplesize , n.agents = n.agents, RS = 0.2, RAD = RAD, maxRange = maxRange, minRange=minRange)}
    }else if (strat ==6){#6. hill climbing 
      for (i in 2:tsteps){
          agents[[i]]<-indSearch(MasonWattsEnv, agents[[i-1]], RS= 0, NK = FALSE, n.agents = n.agents, RAD = RAD, maxRange = maxRange, minRange=minRange)}
    } else {#7. random search
      for (i in 2:tsteps){
          agents[[i]]<-indSearch(MasonWattsEnv, agents[[i-1]], RS= 1, NK = FALSE, n.agents = n.agents, RAD = RAD, maxRange = maxRange, minRange=minRange)}  
      }
  total[,strat] <- total[,strat] + sapply(1:tsteps, function(x) mean(agents[[x]][,3])) #sums over all replications
  }
}
#calculate mean performance over time
total <- total/100 #calculates averageover replications
for(strat in 1:length(strategies)){
  output[,env,strat]<-total[,strat] #save to output array
}

#C) NK environments
N=20
load("store20.Rdata") #matrix storing all one digit neighbors for each NK solution
setwd("NKlandscapes")
for (k in kVec){
  env=n_envs + 1 + which(kVec==k) #increment counter
  #load pre-computed NK environments
  k <- paste0("",k)
  setwd(k)
  landscapes <- list.files()
  total_NK <- matrix(0,ncol=length(strategies),nrow=tsteps) #output matrix of performance over time
  #loop through landscapes
  for (l in landscapes){
    load(l)#load landscape
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
      total_NK[,strat] <- total_NK[,strat] +  sapply(1:tsteps, function(x) mean(agents[[x]][,2])) #sum performance over all landscapes
      }
   
  }
  total_NK <-  total_NK/length(landscapes) #average over number of landscapes
  for(strat in 1:length(strategies)){
    output[,env,strat] <- total_NK[,strat]
  }
  setwd("..") #jump back out to "/NKlandscapes" folder
}

#Save results and move back to original code folder
setwd(".."); setwd(".."); setwd("analysis")
name<-paste0(v,RAD,'.Rdata',sep="",collapse=NULL)
save(output, file=name)
setwd(".."); setwd("agents")
