#imitation, hybrid, and individual learning

imitation <- function(prevChoices, network, NK = FALSE, samplesize = 3, n.agents = 100){
	#imitate the best strategy
	if (NK==FALSE){#check if NK, because NK fitness matrices have 2 cols and 2D envs have 3 cols
		payoff.id <- 3
	}else{
		payoff.id<-2
	}
	#Sample peers from connected network 
	samples<-sapply(1:100, function(x) sample(network[network[,1]==x,2],samplesize))
	#copy prevChoices as newChoices for storing new iteration of solutions
	newChoices <- prevChoices
	#loop through agents
	for (n in 1:n.agents){
		peers<-prevChoices[samples[,n],payoff.id] #compile payoffs of connected peers
		best<-which(peers==max(peers)) #find the best solution of connected peers
		if(length(best)>1){ #if multiple peers have equally good payoffs
			best<-sample(best,1) #randomly sample one of the tied solutions
		}
		newChoices[n,]<-prevChoices[samples[best,n],] #copy over [x,y,payoff] of the best peer solution into new choices
	}
	#check if new choice is worse than old choice, revert back if it is
	not.switching<-ifelse(prevChoices[,payoff.id]>newChoices[,payoff.id],1,0)*1:n.agents 
	newChoices[not.switching,]<-prevChoices[not.switching,]
	return(newChoices)
}

hybrid <- function(fitnessMatrix, prevChoices, network, NK=FALSE, RS = 0, RAD = 30, samplesize = 3, n.agents = 100, minRange=1, maxRange=1001){
	#Hybrid strategy that tries to learn through imitation first, and if unsuccessful, learns individually (local or random with p(random)=RS)
	if (NK==FALSE){#check if NK, because NK fitness matrices have 2 cols and 2D envs have 3 cols
		payoff.id <- 3
	}else{
		payoff.id<-2
	}
	newChoices <- prevChoices #copy prevChoices as newChoices for storing new iteration of solutions
	samples<-sapply(1:100, function(x) sample(network[network[,1]==x,2],samplesize)) #sample peers from network
	ind.learn<-vector(length=n.agents) #index of imitators or independent learners
	#loop through agents
	for (n in 1:n.agents){
		#1. try social learning	    
		peers<-prevChoices[samples[,n],payoff.id] #compile payoffs of connected peers
		best<-which(peers==max(peers)) #find the best solution of connected peers
		if(length(best)>1){ #if multiple peers have equally good payoffs
			best<-sample(best,1) #randomly sample one of the tied solutions
		}  
		newChoices[n,]<-prevChoices[samples[best,n],] #copy over [x,y,payoff] of the best peer solution into new choices
		#2. if best peer choice was not better than previous choice, attempt individual learning
		if(newChoices[n,payoff.id]<=prevChoices[n,payoff.id]){ 
			#randomly draw a number to see if local or random search (TODO: Is there a faster way to avoid generating it if RS = 1 or 0?)
			if (RS==0 | runif(1) > RS){ #local search
				if (NK==FALSE){#2D environments
				  temp <- prevChoices[n,]
				  possibilities <- rbind(temp[1:2] + c( 1 ,1 ),
				                         temp[1:2] + c(-1 , 1 ),
				                         temp[1:2] + c(1 ,-1 ),
				                         temp[1:2] + c(-1 ,-1 ),
				                         temp[1:2] + c(0, 1 ),
				                         temp[1:2] + c(1 ,0),
				                         temp[1:2] + c(-1 ,0),
				                         temp[1:2] + c(0,-1 ))
				  for(p in 1:8){
				    if(any(possibilities[p,1:2]>maxRange) || any(possibilities[p,1:2]<minRange)){
				      possibilities[p,possibilities[p,1:2]<minRange] <- minRange
				      possibilities[p,possibilities[p,1:2]>maxRange] <- maxRange
				    }
				  }
				  payoffs <- sapply(1:nrow(possibilities), function(x) fitnessMatrix[possibilities[x,1], possibilities[x,2]]  )
				  # choose <- possibilities[sample(1:nrow(possibilities),1),]
				  new_options <- cbind(possibilities,payoffs)
				  choose <- new_options[which.max(new_options[,3]),]
				  
				  newChoices[n,] <- choose #randomly choose one of the possible values, and re-adjust location in temp for agent
				  
				  
				  
				  
				  	}else{#NK
				newChoices[n,1] <- sample(store[,prevChoices[n,1]],1)+1 #sample neighboring solution (+1 is to compensate for decimal value of NK solution starting at 0)
				newChoices[n,2] <- fitnessMatrix[newChoices[n,1],2] #add fitness to new choices matrix
				}
			}
			else{ #random search
				if (NK==FALSE){ #2D environments
					newChoices[n,1:2] <- sample(minRange:maxRange,2) #randomly generate an x,y value within range of function 
		     		newChoices[n,3] <- fitnessMatrix[newChoices[n,1],newChoices[n,2]] #look up fitness
				}else{#NK
					newChoices[n,1] <- sample(1:2^N,1) #randomly sample a new solution
					newChoices[n,2] <- fitnessMatrix[newChoices[n,1],2] #add fitness to new choices matrix
				}
			}
		}
	}
	#check if new choice is worse than old choice, revert back if it is
	not.switching<-ifelse(prevChoices[,payoff.id]>newChoices[,payoff.id],1,0)*1:n.agents 
	newChoices[not.switching,]<-prevChoices[not.switching,]
	return(newChoices)
}

indSearch <- function(fitnessMatrix, prevChoices, RS= 0, NK = FALSE, n.agents = 100, RAD = 30, minRange=1,maxRange = 1001){
	#RS = 0 is hill-climbing, RS = 1 is random search
	if (NK==FALSE){#check if NK, because NK fitness matrices have 2 cols and 2D envs have 3 cols
		payoff.id <- 3
	}else{
		payoff.id <- 2
	}

	newChoices <- prevChoices #copy prevChoices as newChoices for storing new iteration of solutions
	#loop through agents
	for (n in 1:n.agents){
		#randomly draw a number to see if local or random search (TODO: Is there a faster way to avoid generating it if RS = 1 or 0?)
	  if (RS==0 || runif(1) > RS){ #local search
	    if (NK==FALSE){#2D environments
	      temp <- prevChoices[n,]
	      possibilities <- rbind(temp[1:2] + c( 1 ,1 ),
	                             temp[1:2] + c(-1 , 1 ),
	                             temp[1:2] + c(1 ,-1 ),
	                             temp[1:2] + c(-1 ,-1 ),
	                             temp[1:2] + c(0, 1 ),
	                             temp[1:2] + c(1 ,0),
	                             temp[1:2] + c(-1 ,0),
	                             temp[1:2] + c(0,-1 ))
	      for(p in 1:8){
	        if(any(possibilities[p,1:2]>maxRange) || any(possibilities[p,1:2]<minRange)){
	          possibilities[p,possibilities[p,1:2]<minRange] <- minRange
	          possibilities[p,possibilities[p,1:2]>maxRange] <- maxRange
	        }
	      }
	
	      payoffs <- sapply(1:nrow(possibilities), function(x) fitnessMatrix[ possibilities[x,1], possibilities[x,2] ]  )
	      # choose <- possibilities[sample(1:nrow(possibilities),1),]
	      new_options <- cbind(possibilities,payoffs)
	      choose <- unlist(new_options[which.max(new_options[,3]),])
	     
	      newChoices[n,1:2] <- as.vector(choose[1:2]) #randomly choose one of the possible values, and re-adjust location in temp for agent
	      newChoices[n,3] <- as.vector(choose[3])
	      
	      
	      
	    }else{#NK
	      newChoices[n,1] <- sample(store[,prevChoices[n,1]],1)+1 #sample neighboring solution (+1 is to compensate for decimal value of NK solution starting at 0)
	      newChoices[n,2] <- fitnessMatrix[newChoices[n,1],2] #add fitness to new choices matrix
	    }
	  }
		else{ #random search
			if (NK==FALSE){ #2D environments
				newChoices[n,1:2] <- sample(minRange:maxRange,2) #randomly generate an x,y value within range of function 
	     		newChoices[n,3] <- fitnessMatrix[newChoices[n,1],newChoices[n,2]] #look up fitness
			}else{#NK
				newChoices[n,1] <- sample(1:2^N,1) #randomly sample a new solution
				newChoices[n,2] <- fitnessMatrix[newChoices[n,1],2] #add fitness to new choices matrix
			}
		}
	}
	#check if new choice is worse than old choice, revert back if it is
	not.switching<-ifelse(prevChoices[,payoff.id]>newChoices[,payoff.id],1,0)*1:n.agents 
	newChoices[not.switching,]<-prevChoices[not.switching,]
	return(newChoices)
}