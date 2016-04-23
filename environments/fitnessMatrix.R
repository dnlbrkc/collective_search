#loop through each environment, and save the fitness as a 1001 x 1001 matrix
ptm <- proc.time()
source("envRange.R")
source("functions.R")


envList <- c(ackley, crossit, drop, egg, griewank, holder, langer, levy, levy13, rastr, schaffer2, schaffer4, schwef, shubert)
rangeList <- list(ackleyRange, crossitRange, dropRange, eggRange, griewankRange, holderRange, langerRange, levyRange, levy13Range, rastrRange, schaffer2Range, schaffer4Range, schwefRange, shubertRange )
envNames <- c("ackley", "crossit", "drop", "egg", "griewank", "holder", "langer", "levy", "levy13", "rastr", "schaffer2", "schaffer4", "schwef", "shubert")


fitness <-list()
for (i in 1:14){#loop through environments
	env<- envList[[i]]
	envRange <- rangeList[[i]]
	envName <- envNames[[i]]
	fitnessMatrix <- matrix(, ncol=1001, nrow =1001)
	#loop through 1:1000 for x and y
	for (x in 1:1001){
		for (y in 1:1001){
			xloc <- envRange[x]
			yloc <- envRange[y]
			fitnessMatrix[x,y] <- env(c(xloc,yloc))
		}
	}
	#invert (convert to maximization problem)
	if (i!=12){#note: Schaffer4 is already a maximization problem
		fitnessMatrix <- fitnessMatrix * -1
	}
	#scale payoffs to between 0 and 1
	fitnessMatrix<- (fitnessMatrix - min(fitnessMatrix))/(max(fitnessMatrix) - min(fitnessMatrix))
	#add to output list
	fitness[[i]] <- fitnessMatrix
}

save(fitness, file="environments.Rdata")

print(proc.time() - ptm)

#todo: calculate environmental statistics