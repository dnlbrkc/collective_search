#used to run the simulations in Barkoczi, Analytis, & Wu (2016)
library(rprojroot)
#replication ID for cluster computing
v <- as.integer(commandArgs(TRUE)[1])

path <- function(x) rprojroot::find_root_file(x, criterion = has_file("collective_search.Rproj"))



#Load pre-computed data
#load strategies
source(path("agents/strategies.R")) # the source of the various agent model functions
load(path("environments/environments.Rdata")) #computed using /environments/fitnessMatrix.R
source(path("environments/functions.R")) #environment functions (loaded only for the MasonWatts() function)
#load network matrices

load(path("environments/Networks/fullNet.Rdata"))
load(path("environments/Networks/localNet.Rdata"))


#MODEL PARAMETERS
n.agents=100; tsteps=100; samplesize <- 3 

#list of 2D environments to loop through
environmentList <- c("ackley", "crossit", "drop", "egg", "griewank", "holder", "langer", "levy", "levy13", "rastr", "schaffer2", "schaffer4", "schwef", "shubert")
n_envs <- length(environmentList)
minRange <- 1
maxRange <- 1001
kVec <- c("5","10") #K values to use in NK environments
#Model search strategies
strategies <- c("Imitation", "hybridLocal", "hybridFull", "hybridLocalRand", "hybridFullRand", "hillClimbing", "Random")

#output array storing data for time steps x n_environments x n_models
output <- array(dim=c(tsteps,n_envs+1+length(kVec),length(strategies)))

#SIMULATION CODE: We loop through environments 1-12 (A), then Mason and Watts (B), and then two NK environments (C)

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
			    agents[[i]] <- hybrid(fitness[[env]], agents[[i-1]], localNet, samplesize =  samplesize , n.agents = n.agents, RS = 0, maxRange = maxRange, minRange=minRange)}
		} else if (strat ==3){#3. hybrid with full network 
			for (i in 2:tsteps){
			    agents[[i]] <- hybrid(fitness[[env]], agents[[i-1]], fullNet, samplesize =  samplesize , n.agents = n.agents, RS = 0, maxRange = maxRange, minRange=minRange)}
		} else if (strat ==4){#4. Hybrid with local network and rand=0.2
			for (i in 2:tsteps){
			    agents[[i]] <- hybrid(fitness[[env]], agents[[i-1]], localNet, samplesize =  samplesize , n.agents = n.agents, RS = 0.2, maxRange = maxRange, minRange=minRange)}
		}else if (strat ==5){#5. Hybrid with full network and rand=0.2
			for (i in 2:tsteps){
			    agents[[i]] <- hybrid(fitness[[env]], agents[[i-1]], fullNet, samplesize =  samplesize , n.agents = n.agents, RS = 0.2, maxRange = maxRange, minRange=minRange)}
		}else if (strat ==6){#6. hill climbing 
			for (i in 2:tsteps){
			    agents[[i]]<-indSearch(fitness[[env]], agents[[i-1]], RS= 0, NK = FALSE, n.agents = n.agents, maxRange = maxRange, minRange=minRange)}
		} else {#7. random search
			for (i in 2:tsteps){
			    agents[[i]]<-indSearch(fitness[[env]], agents[[i-1]], RS= 1, NK = FALSE, n.agents = n.agents, maxRange = maxRange, minRange=minRange)}  
			}
		#save results  
		output[,env,strat]<-sapply(1:tsteps, function(x) mean(agents[[x]][,3]))
	}
}

#B) Mason and Watts environment
env=n_envs+1 
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
          agents[[i]] <- hybrid(MasonWattsEnv, agents[[i-1]], localNet, samplesize =  samplesize , n.agents = n.agents, RS = 0, maxRange = maxRange, minRange=minRange)}
    } else if (strat ==3){#3. hybrid with full network 
      for (i in 2:tsteps){
          agents[[i]] <- hybrid(MasonWattsEnv, agents[[i-1]], fullNet, samplesize =  samplesize , n.agents = n.agents, RS = 0, maxRange = maxRange, minRange=minRange)}
    } else if (strat ==4){#4. Hybrid with local network and rand=0.2
      for (i in 2:tsteps){
          agents[[i]] <- hybrid(MasonWattsEnv, agents[[i-1]], localNet, samplesize =  samplesize , n.agents = n.agents, RS = 0.2, maxRange = maxRange, minRange=minRange)}
    }else if (strat ==5){#5. Hybrid with full network and rand=0.2
      for (i in 2:tsteps){
          agents[[i]] <- hybrid(MasonWattsEnv, agents[[i-1]], fullNet, samplesize =  samplesize , n.agents = n.agents, RS = 0.2, maxRange = maxRange, minRange=minRange)}
    }else if (strat ==6){#6. hill climbing 
      for (i in 2:tsteps){
          agents[[i]]<-indSearch(MasonWattsEnv, agents[[i-1]], RS= 0, NK = FALSE, n.agents = n.agents, maxRange = maxRange, minRange=minRange)}
    } else {#7. random search
      for (i in 2:tsteps){
          agents[[i]]<-indSearch(MasonWattsEnv, agents[[i-1]], RS= 1, NK = FALSE, n.agents = n.agents, maxRange = maxRange, minRange=minRange)}  
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
load(path("environments/NKNeighbors.Rdata")) #matrix storing all one digit neighbors for each NK solution (specifically for N=20)
setwd("NKlandscapes") #Folder containing NK environments, separated by subfolders for specific values of K (data generated with '/environments/Generate_NK/generate_NK.R')
#loop through different NK environments with different K values
for (k in kVec){
  env=n_envs + 1 + which(kVec==k) #increment counter
  #load pre-computed NK environments
  wd <- paste0(path("environments/NK_landscapes/"),"",k)
  setwd(wd)
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
            agents[[i]] <- hybrid(landscape, agents[[i-1]], localNet, samplesize =  samplesize , n.agents = n.agents, NK=TRUE, RS = 0, maxRange = maxRange, minRange=minRange)}
      } else if (strat ==3){#3. hybrid with full network 
        for (i in 2:tsteps){
            agents[[i]] <- hybrid(landscape, agents[[i-1]], fullNet, samplesize =  samplesize , n.agents = n.agents, NK=TRUE, RS = 0, maxRange = maxRange, minRange=minRange)}
      } else if (strat ==4){#4. Hybrid with local network and rand=0.2
        for (i in 2:tsteps){
            agents[[i]] <- hybrid(landscape, agents[[i-1]], localNet, samplesize =  samplesize , n.agents = n.agents, NK=TRUE, RS = 0.2, maxRange = maxRange, minRange=minRange)}
      }else if (strat ==5){#5. Hybrid with full network and rand=0.2
        for (i in 2:tsteps){
            agents[[i]] <- hybrid(landscape, agents[[i-1]], fullNet, samplesize =  samplesize , n.agents = n.agents, NK=TRUE, RS = 0.2, maxRange = maxRange, minRange=minRange)}
      }else if (strat ==6){#6. hill climbing 
        for (i in 2:tsteps){
            agents[[i]]<-indSearch(landscape, agents[[i-1]], RS= 0, NK = TRUE, n.agents = n.agents, maxRange = maxRange, minRange=minRange)}
      } else {#7. random search
        for (i in 2:tsteps){
            agents[[i]]<-indSearch(landscape, agents[[i-1]], RS= 1, NK = TRUE, n.agents = n.agents, maxRange = maxRange, minRange=minRange)}  
      }
      total_NK[,strat] <- total_NK[,strat] +  sapply(1:tsteps, function(x) mean(agents[[x]][,2])) #sum performance over all landscapes
      }
   
  }
  total_NK <-  total_NK/length(landscapes) #average over number of landscapes
  for(strat in 1:length(strategies)){
    output[,env,strat] <- total_NK[,strat]
  }
}

#Save results
setwd(path("results"))
name<-paste0(v,'.Rdata',sep="",collapse=NULL)
save(output, file=name)
