#imitation, hybrid, and individual learning

imitation <- function(fitnessMatrix, prevChoices, network, NK = FALSE, samplesize = 3, n.agents = 100){
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
			if (runif(1) > RS){ #local search
				if (NK==FALSE){#2D environments
					which.digit <- sample(c(1:2),1) #randomly sample one of the x or y values to modify
					loc <- as.numeric(prevChoices[n,c(which.digit)]) #retrieve location of x or y from prevChoices for agent n
					vec_radius <- 1:RAD #radius vector
					values <- c(loc + vec_radius,loc - vec_radius) #range of possible locations within radius to choose from
					#Important, wrap location values around edge of landscape (i.e., pacman)
					values[values>maxRange] <- minRange:((minRange+length(values[values>maxRange]))-1)
					values[values< minRange] <- ((maxRange-length(values[values<minRange]))+1):maxRange
					newChoices[n,which.digit] <- sample(values,1) #randomly choose one of the possible values, and re-adjust location in temp for agent
					newChoices[n,3] <- fitnessMatrix[newChoices[n,1],newChoices[n,2] ] #look up fitness
				}else{#NK
				newChoices[n,1] <- sample(store[,prevChoices[n,1]+1],1) #sample neighboring solution (+1 is to compensate for decimal value of NK solution starting at 0)
				newChoices[n,2] <- landscape[newChoices[n,1],2] #add fitness to new choices matrix
				}
			}
			else{ #random search
				if (NK==FALSE){ #2D environments
					newChoices[n,1:2] <- sample(minRange:maxRange,2) #randomly generate an x,y value within range of function 
		     		newChoices[n,3] <- fitnessMatrix[newChoices[n,1],newChoices[n,2]] #look up fitness
				}else{#NK
					newChoices[n,1] <- sample(1:2^N,1) #randomly sample a new solution
					newChoices[n,2] <- landscape[newChoices[n,1],2] #add fitness to new choices matrix
				}
			}
		}
	}
	#check if new choice is worse than old choice, revert back if it is
	not.switching<-ifelse(prevChoices[,payoff.id]>newChoices[,payoff.id],1,0)*1:n.agents 
	newChoices[not.switching,]<-prevChoices[not.switching,]
	return(newChoices)
}

indSearch <- function(fitnessMatrix, prevChoices, RS= 0, NK = FALSE, n.agents = 100, RAD = 30, maxRange = 1001, minRange=1){
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
		if (runif(1) > RS){ #local search
			if (NK==FALSE){#2D environments
				which.digit <- sample(c(1:2),1) #randomly sample one of the x or y values to modify
				loc <- as.numeric(prevChoices[n,c(which.digit)]) #retrieve location of x or y from prevChoices for agent n
				vec_radius <- 1:RAD #radius vector
				values <- c(loc + vec_radius,loc - vec_radius) #range of possible locations within radius to choose from
				#Important, wrap location values around edge of landscape (i.e., pacman)
				values[values>maxRange] <- minRange:((minRange+length(values[values>maxRange]))-1)
				values[values< minRange] <- ((maxRange-length(values[values<minRange]))+1):maxRange
				newChoices[n,which.digit] <- sample(values,1) #randomly choose one of the possible values, and re-adjust location in temp for agent
				newChoices[n,3] <- fitnessMatrix[newChoices[n,1],newChoices[n,2] ] #look up fitness
			}else{#NK
			newChoices[n,1] <- sample(store[,prevChoices[n,1]+1],1) #sample neighboring solution (+1 is to compensate for decimal value of NK solution starting at 0)
			newChoices[n,2] <- landscape[newChoices[n,1],2] #add fitness to new choices matrix
			}
		}
		else{ #random search
			if (NK==FALSE){ #2D environments
				newChoices[n,1:2] <- sample(minRange:maxRange,2) #randomly generate an x,y value within range of function 
	     		newChoices[n,3] <- fitnessMatrix[newChoices[n,1],newChoices[n,2]] #look up fitness
			}else{#NK
				newChoices[n,1] <- sample(1:2^N,1) #randomly sample a new solution
				newChoices[n,2] <- landscape[newChoices[n,1],2] #add fitness to new choices matrix
			}
		}
	}
	#check if new choice is worse than old choice, revert back if it is
	not.switching<-ifelse(prevChoices[,payoff.id]>newChoices[,payoff.id],1,0)*1:n.agents 
	newChoices[not.switching,]<-prevChoices[not.switching,]
	return(newChoices)
}