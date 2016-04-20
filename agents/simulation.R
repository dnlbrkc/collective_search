rm(list=ls())

#set working directory to "environments" to load fitness matrices
setwd("..")
setwd("environments")
load("environments.Rdata")
source("functions.R") #environment functions


#MODEL PARAMETERS

#list of 2D environments to loop through
environmentList <- c("ackley", "crossit", "drop", "egg", "griewank", "holder", "langer", "levy", "levy13", "rastr", "schaffer2", "schaffer4", "schwef", "shubert")
n_envs <- length(environmentList)
range <- 1:1001
n.strat <- 5 #imitation, hybridA, hybridB,local search, random search


#number of agents and trials
n.agents=100; tsteps=200
samplesize <- 3 #change sample size


#replication ID
v <- as.integer(commandArgs(TRUE)[1])


finalResults <- list() #final results
z=0 #network counter, where z /in {1,2}

RAD = 30 #search radius
nets <- c("fullyconnected.Rdata","lattice.Rdata") #connections for each agent

#todo: loop through network types within the environment loop
for(netname in nets){


z=z+1
load(netname)

# perf.time <- matrix(0,ncol=n_envs,nrow=tsteps)
perf.time <- array(dim=c(tsteps,n_envs+2,n.strat))


#A) 2D rugged landscapes
for (env in 1:length(fitness)){
	#1. Initialize random starting position for n.agents
	agents <- list()
	choices <- t(replicate(n.agents, sample(1001,2))) #random starting location
	payoffs<- apply(choices, 1, function(x) fitness[[env]][x[1],x[2]]) #look up payoff from fitness matrix
	#combine choices and payoffs in matrix row: [x, y, payoff], col: n.agents
	agents[[1]] <- cbind(choices,payoffs)  #initialization

	#Todo: define each strategy as a function
	for (strat in 1:n.strat){
	
	#1.IMITATE THE BEST
	if(strat==1){  
		for (i in 2:tsteps){
			agents[[i]] <- imitation(fitness[[env]], agents[[i-1]], network, samplesize =  samplesize , n.agents = n.agents)

}
	#2. HYBRID A
	} else if (strat==2){
	
		for (i in 2:tsteps){
		    
		    agents[[i]]<-agents[[i-1]] #carry over previous choices
		    samples<-sapply(1:100, function(x) sample(network[network[,1]==x,2],samplesize))
		    
		    #best member rule
		    
		    ind.learn<-vector(length=n.agents)
		    k=0
		    for (j in 1:n.agents){k=k+1; al<-agents[[i]][samples[,j],3]; best<-which(al==max(al)); if(length(best)>1){best<-sample(best,1)}  
		    agents[[i]][j,]<-agents[[i]][samples[best,j],]; 
		    if(agents[[i]][j,3]<=agents[[i-1]][j,3]){ind.learn[j]<-k}
		    }
		    ind.learn<-ind.learn[ind.learn!=0] 
		    
		    #switch if better
		    temp <- agents[[i]]
		    
		    if(length(ind.learn)>=1){
		      
		      for (n in c(ind.learn)){
		        
		        #radius based search + payoff store in temp
		        which.digit <- sample(c(1:2),1) 
		        radius <- as.numeric(temp[n,c(which.digit)])
		        vec_radius <- 1:RAD
		        values <- c(radius + vec_radius,radius - vec_radius)
		        # values <- c(radius + vec_radius,radius - vec_radius)
		        values[values>max(range)] <- min(range):((min(range)+length(values[values>max(range)]))-1)
		        values[values< min(range)] <- ((max(range)-length(values[values<min(range)]))+1):max(range)
		        temp[n,which.digit] <- sample(values,1)
		        temp[n,3] <- fitness[[env]][temp[n,1],temp[n,2] ] 
		        
		      }
		      
		      
		      agents[[i]][ind.learn,] <- temp[ind.learn,]
		    }
		    
		    #determine who's switching to the new option and who is keeping the old option
		    switching<-ifelse(agents[[i-1]][,3]<agents[[i]][,3],1,0)*1:n.agents
		    not.switching<-setdiff(1:100,switching[switching!=0])
		    
		    #those who are not switching carry their option from the previous round
		    agents[[i]][not.switching,]<-agents[[i-1]][not.switching,]
		    
		    
		  }
	} else if (strat ==3) { 
	#3. HYBRID B 20% random search
	for (i in 2:tsteps){
	    
	    agents[[i]]<-agents[[i-1]] #carry over previous choices
	    samples<-sapply(1:100, function(x) sample(network[network[,1]==x,2],samplesize))
	    
	    #best member rule
	    
	    ind.learn<-vector(length=n.agents)
	    k=0
	    for (j in 1:n.agents){k=k+1; al<-agents[[i]][samples[,j],3]; best<-which(al==max(al)); if(length(best)>1){best<-sample(best,1)}  
	    agents[[i]][j,]<-agents[[i]][samples[best,j],]; 
	    if(agents[[i]][j,3]<=agents[[i-1]][j,3]){ind.learn[j]<-k}
	    }
	    ind.learn<-ind.learn[ind.learn!=0] 
	    
	    #switch if better
	    temp <- agents[[i]]
	    
	    if(length(ind.learn)>=1){
	      
	      for (n in c(ind.learn)){
	        
	        if(runif(1)>0.2){
	        #radius based search + payoff store in temp
	        which.digit <- sample(c(1:2),1) 
	        radius <- as.numeric(temp[n,c(which.digit)])
	        vec_radius <- 1:RAD
	        values <- c(radius + vec_radius,radius - vec_radius)
	        # values <- c(radius + vec_radius,radius - vec_radius)
	        values[values>max(range)] <- min(range):((min(range)+length(values[values>max(range)]))-1)
	        values[values< min(range)] <- ((max(range)-length(values[values<min(range)]))+1):max(range)
	        temp[n,which.digit] <- sample(values,1)
	        temp[n,3] <- fitness[[env]][temp[n,1],temp[n,2] ] 
	        } else {
	        temp[n,1:2] <- sample(range,2)
	        temp[n,3] <- fitness[[env]][temp[n,1],temp[n,2] ]
	        }

	        
	      }
	      
	      
	      agents[[i]][ind.learn,] <- temp[ind.learn,]
	    }
	    
	    #determine who's switching to the new option and who is keeping the old option
	    switching<-ifelse(agents[[i-1]][,3]<agents[[i]][,3],1,0)*1:n.agents
	    not.switching<-setdiff(1:100,switching[switching!=0])
	    
	    #those who are not switching carry their option from the previous round
	    agents[[i]][not.switching,]<-agents[[i-1]][not.switching,]
	    
	    
	  }
	} else if (strat ==4){
	#4. LOCAL 
	for (i in 2:tsteps){
	    agents[[i]]<-indSearch(fitness[[i]], agents[[i-1]], RS= 0, NK = FALSE, n.agents = n.agents, RAD = RAD, maxRange = max(range), minRange=min(range))
	  }
	} else {  
	#5. RANDOM SEARCH
	for (i in 2:tsteps){
	    
	    agents[[i]]<-agents[[i-1]] #carry over previous choices
	    
	    
	    ind.learn<-1:100
	   
	    temp <- agents[[i]]
	    
	    if(length(ind.learn)>=1){
	      
	      for (n in c(ind.learn)){
	        
	        
	     temp[n,1:2] <- sample(range,2)
	     temp[n,3] <- fitness[[env]][temp[n,1],temp[n,2] ]   
	        
	        
	        
	        
	      }
	      
	      
	      agents[[i]][ind.learn,] <- temp[ind.learn,]
	    }
	    
	    #determine who's switching to the new option and who is keeping the old option
	    switching<-ifelse(agents[[i-1]][,3]<agents[[i]][,3],1,0)*1:n.agents
	    not.switching<-setdiff(1:100,switching[switching!=0])
	    
	    #those who are not switching carry their option from the previous round
	    agents[[i]][not.switching,]<-agents[[i-1]][not.switching,]
	    
	    
	  }  
	}  
	  
	  perf.time[,env,strat]<-sapply(1:tsteps, function(x) mean(agents[[x]][,3]))
	  
	}
	
	
}
#increment env counter
env=env+1 #env = 15 for Mason and Watts

total <- matrix(0,ncol=n.strat,nrow=tsteps)
for(repM in 1:100){
#B) Mason and Watts environment
MasonWattsEnv <- MasonWatts(1001) #each different replication will get a different randomization of the environment
agents <- list()
choices <- t(replicate(n.agents, sample(1001,2))) #random starting location
payoffs<- apply(choices, 1, function(x) MasonWattsEnv[x[1],x[2]]) #look up payoff from fitness matrix
#combine choices and payoffs in matrix row: [x, y, payoff], col: n.agents
agents[[1]] <- cbind(choices,payoffs)  #initialization

	
for (strat in 1:n.strat){
  
  if(strat==1){  
    #IMITATE THE BEST
    for (i in 2:tsteps){
      
      agents[[i]]<-agents[[i-1]] #carry over previous choices
      samples<-sapply(1:100, function(x) sample(network[network[,1]==x,2],samplesize))
      
      
      #best member rule
      #if(strat==1){
      
      ind.learn<-vector(length=n.agents)
      k=0
      for (j in 1:n.agents){k=k+1; al<-agents[[i]][samples[,j],3]; best<-which(al==max(al)); if(length(best)>1){best<-sample(best,1)}  
      agents[[i]][j,]<-agents[[i]][samples[best,j],]; 
      if(agents[[i]][j,3]<=agents[[i-1]][j,3]){ind.learn[j]<-k}
      }
      ind.learn<-ind.learn[ind.learn!=0] 
      
      #switch if better
      temp <- agents[[i]]
      
      # if(length(ind.learn)>=1){
      # 
      # for (n in c(ind.learn)){
      # 
      # #radius based search + payoff store in temp
      #  which.digit <- sample(c(1:2),1) 
      #   radius <- as.numeric(temp[n,c(which.digit)])
      #   vec_radius <- 1:RAD
      #   values <- c(radius + vec_radius,radius - vec_radius)
      #   # values <- c(radius + vec_radius,radius - vec_radius)
      #   values[values>max(range)] <- min(range):((min(range)+length(values[values>max(range)]))-1)
      #   values[values< min(range)] <- ((max(range)-length(values[values<min(range)]))+1):max(range)
      #   temp[n,which.digit] <- sample(values,1)
      #   temp[n,3] <- environment[temp[n,1],temp[n,2] ] 
      # 
      # 
      # 
      # 
      # 
      # }
      # 
      # 
      # agents[[i]][ind.learn,] <- temp[ind.learn,]
      # }
      
      #determine who's switching to the new option and who is keeping the old option
      switching<-ifelse(agents[[i-1]][,3]<agents[[i]][,3],1,0)*1:n.agents
      not.switching<-setdiff(1:100,switching[switching!=0])
      
      #those who are not switching carry their option from the previous round
      agents[[i]][not.switching,]<-agents[[i-1]][not.switching,]
      
      
    }
  } else if (strat==2){
    #HYBRID A
    for (i in 2:tsteps){
      
      agents[[i]]<-agents[[i-1]] #carry over previous choices
      samples<-sapply(1:100, function(x) sample(network[network[,1]==x,2],samplesize))
      
      
      #best member rule
      #if(strat==1){
      
      ind.learn<-vector(length=n.agents)
      k=0
      for (j in 1:n.agents){k=k+1; al<-agents[[i]][samples[,j],3]; best<-which(al==max(al)); if(length(best)>1){best<-sample(best,1)}  
      agents[[i]][j,]<-agents[[i]][samples[best,j],]; 
      if(agents[[i]][j,3]<=agents[[i-1]][j,3]){ind.learn[j]<-k}
      }
      ind.learn<-ind.learn[ind.learn!=0] 
      
      #switch if better
      temp <- agents[[i]]
      
      if(length(ind.learn)>=1){
        
        for (n in c(ind.learn)){
          
          #radius based search + payoff store in temp
          which.digit <- sample(c(1:2),1) 
          radius <- as.numeric(temp[n,c(which.digit)])
          vec_radius <- 1:RAD
          values <- c(radius + vec_radius,radius - vec_radius)
          # values <- c(radius + vec_radius,radius - vec_radius)
          values[values>max(range)] <- min(range):((min(range)+length(values[values>max(range)]))-1)
          values[values< min(range)] <- ((max(range)-length(values[values<min(range)]))+1):max(range)
          temp[n,which.digit] <- sample(values,1)
          temp[n,3] <- MasonWattsEnv[temp[n,1],temp[n,2] ] 
          
          
          
          
          
        }
        
        
        agents[[i]][ind.learn,] <- temp[ind.learn,]
      }
      
      #determine who's switching to the new option and who is keeping the old option
      switching<-ifelse(agents[[i-1]][,3]<agents[[i]][,3],1,0)*1:n.agents
      not.switching<-setdiff(1:100,switching[switching!=0])
      
      #those who are not switching carry their option from the previous round
      agents[[i]][not.switching,]<-agents[[i-1]][not.switching,]
      
      
    }
  } else if (strat ==3) { 
    #HYBRID B 20% random search
    for (i in 2:tsteps){
      
      agents[[i]]<-agents[[i-1]] #carry over previous choices
      samples<-sapply(1:100, function(x) sample(network[network[,1]==x,2],samplesize))
      
      
      #best member rule
      #if(strat==1){
      
      ind.learn<-vector(length=n.agents)
      k=0
      for (j in 1:n.agents){k=k+1; al<-agents[[i]][samples[,j],3]; best<-which(al==max(al)); if(length(best)>1){best<-sample(best,1)}  
      agents[[i]][j,]<-agents[[i]][samples[best,j],]; 
      if(agents[[i]][j,3]<=agents[[i-1]][j,3]){ind.learn[j]<-k}
      }
      ind.learn<-ind.learn[ind.learn!=0] 
      
      #switch if better
      temp <- agents[[i]]
      
      if(length(ind.learn)>=1){
        
        for (n in c(ind.learn)){
          
          if(runif(1)>0.2){
            #radius based search + payoff store in temp
            which.digit <- sample(c(1:2),1) 
            radius <- as.numeric(temp[n,c(which.digit)])
            vec_radius <- 1:RAD
            values <- c(radius + vec_radius,radius - vec_radius)
            # values <- c(radius + vec_radius,radius - vec_radius)
            values[values>max(range)] <- min(range):((min(range)+length(values[values>max(range)]))-1)
            values[values< min(range)] <- ((max(range)-length(values[values<min(range)]))+1):max(range)
            temp[n,which.digit] <- sample(values,1)
            temp[n,3] <- MasonWattsEnv[temp[n,1],temp[n,2] ] 
          } else {
            temp[n,1:2] <- sample(range,2)
            temp[n,3] <- MasonWattsEnv[temp[n,1],temp[n,2] ]
          }
          
          
          
          
        }
        
        
        agents[[i]][ind.learn,] <- temp[ind.learn,]
      }
      
      #determine who's switching to the new option and who is keeping the old option
      switching<-ifelse(agents[[i-1]][,3]<agents[[i]][,3],1,0)*1:n.agents
      not.switching<-setdiff(1:100,switching[switching!=0])
      
      #those who are not switching carry their option from the previous round
      agents[[i]][not.switching,]<-agents[[i-1]][not.switching,]
      
      
    }
  } else if (strat ==4){
    #LOCAL
    for (i in 2:tsteps){
      
      agents[[i]]<-agents[[i-1]] #carry over previous choices
      
      
      ind.learn<-1:100
      
      temp <- agents[[i]]
      
      if(length(ind.learn)>=1){
        
        for (n in c(ind.learn)){
          
          #radius based search + payoff store in temp
          which.digit <- sample(c(1:2),1) 
          radius <- as.numeric(temp[n,c(which.digit)])
          vec_radius <- 1:RAD
          values <- c(radius + vec_radius,radius - vec_radius)
          # values <- c(radius + vec_radius,radius - vec_radius)
          values[values>max(range)] <- min(range):((min(range)+length(values[values>max(range)]))-1)
          values[values< min(range)] <- ((max(range)-length(values[values<min(range)]))+1):max(range)
          temp[n,which.digit] <- sample(values,1)
          temp[n,3] <- MasonWattsEnv[temp[n,1],temp[n,2] ] 
          
          
          
          
          
        }
        
        
        agents[[i]][ind.learn,] <- temp[ind.learn,]
      }
      
      #determine who's switching to the new option and who is keeping the old option
      switching<-ifelse(agents[[i-1]][,3]<agents[[i]][,3],1,0)*1:n.agents
      not.switching<-setdiff(1:100,switching[switching!=0])
      
      #those who are not switching carry their option from the previous round
      agents[[i]][not.switching,]<-agents[[i-1]][not.switching,]
      
      
    }
  } else {  
    #RANDOM SEARCH
    for (i in 2:tsteps){
      
      agents[[i]]<-agents[[i-1]] #carry over previous choices
      
      
      ind.learn<-1:100
      
      temp <- agents[[i]]
      
      if(length(ind.learn)>=1){
        
        for (n in c(ind.learn)){
          
          
          temp[n,1:2] <- sample(range,2)
          temp[n,3] <- MasonWattsEnv[temp[n,1],temp[n,2] ]   
          
          
          
          
        }
        
        
        agents[[i]][ind.learn,] <- temp[ind.learn,]
      }
      
      #determine who's switching to the new option and who is keeping the old option
      switching<-ifelse(agents[[i-1]][,3]<agents[[i]][,3],1,0)*1:n.agents
      not.switching<-setdiff(1:100,switching[switching!=0])
      
      #those who are not switching carry their option from the previous round
      agents[[i]][not.switching,]<-agents[[i-1]][not.switching,]
      
      
    }  
  }  
  
  
  total[,strat] <- total[,strat] + sapply(1:tsteps, function(x) mean(agents[[x]][,3]))
  
  
}

# perf.time[,env,strat]<-sapply(1:tsteps, function(x) mean(agents[[x]][,3]))

#calculate mean performance over time



}
total <- total/100
for(strat in 1:n.strat){
perf.time[,env,strat]<-total[,strat]
}
env=env+1

#C) NK environments
load("store20.Rdata") #A matrix storing all one digit neighbors for each NK solution
setwd("landscapes")
#each landscape is a 2 column matrix, [decimal representation of a binary string, payoff]
landscapes <- list.files()
N=20
total_NK <- matrix(0,ncol=n.strat,nrow=tsteps)
for(l in landscapes){

load(l)

agents<-list()
agents[[1]]<-landscape[sample(1:2^N,100,replace=F),]

for(strat in 1:n.strat){

  if (strat ==1){
  #imitate the best
  for (i in 2:tsteps){
    
    agents[[i]]<-agents[[i-1]]
    samples<-sapply(1:100, function(x) sample(network[network[,1]==x,2],samplesize))
    
    
    
    NE<-rep(0,n.agents)
    ind.learn<-vector(length=n.agents)
    k=0
    for (j in 1:n.agents){k=k+1; al<-agents[[i]][samples[,j],2]; best<-which(al==max(al)); if(length(best)>1){best<-sample(best,1)}  
    agents[[i]][j,]<-agents[[i]][samples[best,j],]; #0
    if(agents[[i]][j,2]<=agents[[i-1]][j,2]){ind.learn[j]<-k} else if (agents[[i]][j,2]>agents[[i-1]][j,2]) {NE[j]<-k}}
    
   
    
    switching<-ifelse(agents[[i-1]][,2] < agents[[i]][,2],1,0)*1:n.agents #smaller or eenvual
    
    # switching.indlearn<-ifelse(agents[[i-1]][c(ind.learn),2] < agents[[i]][c(ind.learn),2],1,0)*1:length(ind.learn)
    
    # not.switching.indlearn<-setdiff(1:length(ind.learn),switching.indlearn[switching.indlearn!=0])
    
    not.switching<-setdiff(1:100,switching[switching!=0])
    
    # noswitch<-not.switching[ind.learn]
    
    agents[[i]][not.switching,]<-agents[[i-1]][not.switching,]
    
  }
  } else if (strat ==2){
  #hybrid A
  for (i in 2:tsteps){
	
agents[[i]]<-agents[[i-1]]
samples<-sapply(1:100, function(x) sample(network[network[,1]==x,2],samplesize))



NE<-rep(0,n.agents)
ind.learn<-vector(length=n.agents)
k=0
for (j in 1:n.agents){k=k+1; al<-agents[[i]][samples[,j],2]; best<-which(al==max(al)); if(length(best)>1){best<-sample(best,1)}  
	agents[[i]][j,]<-agents[[i]][samples[best,j],]; #0
	if(agents[[i]][j,2]<=agents[[i-1]][j,2]){ind.learn[j]<-k} else if (agents[[i]][j,2]>agents[[i-1]][j,2]) {NE[j]<-k}}

ind.learn<-ind.learn[ind.learn!=0]	

if(length(ind.learn)>=1){
h=0
choose<-vector()
for (n in c(ind.learn)){h=h+1; choose[h]<-sample(store[,agents[[i]][n,1]+1],1)}

choose1 <- choose+1

payZ<-sapply(1:length(choose1), function(x) landscape[choose1[x],2])

new<-cbind(choose,payZ)
l=0
for (n in c(ind.learn)){l=l+1; agents[[i]][n,]<-new[l,]}
}

switching<-ifelse(agents[[i-1]][,2] < agents[[i]][,2],1,0)*1:n.agents #smaller or eenvual

switching.indlearn<-ifelse(agents[[i-1]][c(ind.learn),2] < agents[[i]][c(ind.learn),2],1,0)*1:length(ind.learn)

not.switching.indlearn<-setdiff(1:length(ind.learn),switching.indlearn[switching.indlearn!=0])

not.switching<-setdiff(1:100,switching[switching!=0])

noswitch<-not.switching[ind.learn]

agents[[i]][not.switching,]<-agents[[i-1]][not.switching,]

}
} else if (strat==3){
  #hybrid B
  for (i in 2:tsteps){
    
    agents[[i]]<-agents[[i-1]]
    samples<-sapply(1:100, function(x) sample(network[network[,1]==x,2],samplesize))
    
    
    
    NE<-rep(0,n.agents)
    ind.learn<-vector(length=n.agents)
    k=0
    for (j in 1:n.agents){k=k+1; al<-agents[[i]][samples[,j],2]; best<-which(al==max(al)); if(length(best)>1){best<-sample(best,1)}  
    agents[[i]][j,]<-agents[[i]][samples[best,j],]; #0
    if(agents[[i]][j,2]<=agents[[i-1]][j,2]){ind.learn[j]<-k} else if (agents[[i]][j,2]>agents[[i-1]][j,2]) {NE[j]<-k}}
    
    ind.learn<-ind.learn[ind.learn!=0]	
    
    if(length(ind.learn)>=1){
      
      
      h=0
      choose<-vector()
      for (n in c(ind.learn)){
        h=h+1
        if(runif(1)>0.2){ 
          choose[h]<-sample(store[,agents[[i]][n,1]+1],1)} else { choose[h] <- sample(1:2^20,1)-1}
        }
      
      choose1 <- choose+1
      
      payZ<-sapply(1:length(choose1), function(x) landscape[choose1[x],2])
      
      new<-cbind(choose,payZ)
      l=0
      for (n in c(ind.learn)){l=l+1; agents[[i]][n,]<-new[l,]}
    }
    
    switching<-ifelse(agents[[i-1]][,2] < agents[[i]][,2],1,0)*1:n.agents #smaller or eenvual
    
    switching.indlearn<-ifelse(agents[[i-1]][c(ind.learn),2] < agents[[i]][c(ind.learn),2],1,0)*1:length(ind.learn)
    
    not.switching.indlearn<-setdiff(1:length(ind.learn),switching.indlearn[switching.indlearn!=0])
    
    not.switching<-setdiff(1:100,switching[switching!=0])
    
    noswitch<-not.switching[ind.learn]
    
    agents[[i]][not.switching,]<-agents[[i-1]][not.switching,]
    
  }
} else if (strat ==4){
  #local search
  for (i in 2:tsteps){
    
    agents[[i]]<-agents[[i-1]]
   
    ind.learn <- 1:100
    if(length(ind.learn)>=1){
      
      
      h=0
      choose<-vector()
      for (n in c(ind.learn)){h=h+1;choose[h]<-sample(store[,agents[[i]][n,1]+1],1)} # +1 is there a reason. Ask Daniel
      
      choose1 <- choose+1
      
      payZ<-sapply(1:length(choose1), function(x) landscape[choose1[x],2])
      
      new<-cbind(choose,payZ)
      l=0
      for (n in c(ind.learn)){l=l+1; agents[[i]][n,]<-new[l,]}
    }
    
    switching<-ifelse(agents[[i-1]][,2] < agents[[i]][,2],1,0)*1:n.agents #smaller or eenvual
    
    switching.indlearn<-ifelse(agents[[i-1]][c(ind.learn),2] < agents[[i]][c(ind.learn),2],1,0)*1:length(ind.learn)
    
    not.switching.indlearn<-setdiff(1:length(ind.learn),switching.indlearn[switching.indlearn!=0])
    
    not.switching<-setdiff(1:100,switching[switching!=0])
    
    noswitch<-not.switching[ind.learn]
    
    agents[[i]][not.switching,]<-agents[[i-1]][not.switching,]
    
  }
} else {
  #random search
  for (i in 2:tsteps){
    
    agents[[i]]<-agents[[i-1]]
    
    ind.learn <- 1:100
    
    if(length(ind.learn)>=1){
      
      
      h=0
      choose<-vector()
      for (n in c(ind.learn)){h=h+1; choose[h] <- sample(1:2^20,1)-1}
      
      choose1 <- choose+1
      
      payZ<-sapply(1:length(choose1), function(x) landscape[choose1[x],2])
      
      new<-cbind(choose,payZ)
      l=0
      for (n in c(ind.learn)){l=l+1; agents[[i]][n,]<-new[l,]}
    }
    
    switching<-ifelse(agents[[i-1]][,2] < agents[[i]][,2],1,0)*1:n.agents #smaller or eenvual
    
    switching.indlearn<-ifelse(agents[[i-1]][c(ind.learn),2] < agents[[i]][c(ind.learn),2],1,0)*1:length(ind.learn)
    
    not.switching.indlearn<-setdiff(1:length(ind.learn),switching.indlearn[switching.indlearn!=0])
    
    not.switching<-setdiff(1:100,switching[switching!=0])
    
    noswitch<-not.switching[ind.learn]
    
    agents[[i]][not.switching,]<-agents[[i-1]][not.switching,]
    
  }
}
  
  
  total_NK[,strat] <- total_NK[,strat] +  sapply(1:tsteps, function(x) mean(agents[[x]][,2]))
}




}
total_NK <-  total_NK/length(landscapes)
for(strat in 1:n.strat){
perf.time[,env,strat] <- total_NK[,strat]
}
finalResults[[z]] <- perf.time
}

setwd("~/COLLECTIVE_SEARCH")
name<-paste0(v,RAD,'.Rdata',sep="",collapse=NULL)
save(finalResults, file=name)

