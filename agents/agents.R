#Charley: skeleton for running agent simulations, using Rugged Landscapes, Mason & Watts, and NK
rm(list=ls())

#move to Environments folder
setwd("..") #move to parent folder
setwd("environments")

#Load rugged landscapes as a list of matrices called "fitness"
load("environments.Rdata")
source("functions.R") #environment functions

#list of environment names
environmentList <- c("ackley", "crossit", "drop", "egg", "griewank", "holder", "langer", "levy", "levy13", "rastr", "schaffer2", "schaffer4", "schwef", "shubert", "masonWatts", "N=20,K=5", "N=20, K=16")
n_envs <- length(environmentList)

#SIMULATION VARIABLES
#number of agents and trials
n.agents=100; tsteps=200
samplesize <- 3 #change sample size

myopia <-8 #1=only able to evaluate 1 randomly chosen nearest location, 8=all nearby locations 
copy_error <- 0 #parameter to vary, 0 = no copying error 

total.results <- perf.time <- matrix(0,ncol=n_envs,nrow=tsteps)


#replication ID
r <- as.integer( commandArgs(TRUE)[1])

#vector for storing results
perf.time<-matrix(NA,ncol=n_envs,nrow=tsteps)
diversity<-matrix(NA,ncol=n_envs,nrow=tsteps)
no.ind<-matrix(NA,ncol=n_envs,nrow=tsteps)
successful.ind<-matrix(NA,ncol=n_envs,nrow=tsteps)
raw <- array(0,dim=c(100,3,200))
raw.data <- list()



#A) 2D rugged landscapes
for (k in 1:length(fitness)){
	#1. Initialize random starting position for n.agents
	agents <- list()
	choices <- t(replicate(n.agents, sample(1001,2))) #random starting location
	payoffs<- apply(choices, 1, function(x) fitness[[k]][x[1],x[2]]) #look up payoff from fitness matrix
	#combine choices and payoffs in matrix row: [x, y, payoff], col: n.agents
	agents[[1]] <- cbind(choices,payoffs)  #initialization

	#2. Loop through timesteps 
	#charley note: not sure what's happening here
	count.indlearn <- vector()
	successful.indlearn <- vector()
	for (i in 2:tsteps){
		#3. Search decision 
		choices<- #TODO: do search rule
		payoffs<- apply(choices, 1, function(x) fitness[[k]][x[1],x[2]])
		agents[[i]] <- cbind(choices, payoffs)
	}
	#4. Save relevant information
	#TODO: implement statistics to save over learning phase
	envName <- environmentList[k]
}

#B) Mason and Watts environment
#1. Initialize random starting position for n.agents
MasonWattsEnv <- MasonWatts(1001) #each different replication will get a different randomization of the environment
agents <- list()
choices <- t(replicate(n.agents, sample(1001,2))) #random starting location
payoffs<- apply(choices, 1, function(x) MasonWattsEnv[x[1],x[2]]) #look up payoff from fitness matrix
#combine choices and payoffs in matrix row: [x, y, payoff], col: n.agents
agents[[1]] <- cbind(choices,payoffs)  #initialization

#2. Loop through timesteps
#charley note: not sure what's happening here
count.indlearn <- vector()
successful.indlearn <- vector()
for (i in 2:tsteps){
	#3. Search Decision
	choices<- #TODO: do search rule
	payoffs<- apply(choices, 1, function(x) MasonWattsEnv[x[1],x[2]])
	agents[[i]] <- cbind(choices, payoffs)
}
#4. Save relevant information
#TODO: implement statistics to save over learning phase
envName <- "MasonWatts"


#C) NK environments
kVec <- c(5, 16) #16 is a randomly chosen value higher than 5
for (k in kVec){
	NKenv <- NK(n=20, k=k) #TODO make sure this function is programmed in environments/functions.R
	agents <- list()
	choices <- t(replicate(n.agents, sample(1001,2))) #random starting location
	payoffs<- apply(choices, 1, function(x) NKenv[x[1],x[2]]) #look up payoff from fitness matrix
	#combine choices and payoffs in matrix row: [x, y, payoff], col: n.agents
	agents[[1]] <- cbind(choices,payoffs)  #initialization

	#2. Loop through timesteps
	#charley note: not sure what's happening here
	count.indlearn <- vector()
	successful.indlearn <- vector()
	for (i in 2:tsteps){
		#3. Search Decision
		choices<- #TODO: do search rule
		payoffs<- apply(choices, 1, function(x) NKenv[x[1],x[2]])
		agents[[i]] <- cbind(choices, payoffs)
	}
	#4. Save relevant information
	#TODO: implement statistics to save over learning phase
	envName <- cat("N=20,K=", k)


