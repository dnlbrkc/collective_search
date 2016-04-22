#Calculate mean, variance, and modality of environments
rm(list=ls())
load("environments.Rdata")

#TODO: add extra NK
envNames <- c("Ackley", "Cross-in-Tray", "Drop-Wave", "Eggholder", "Griewank", "Holder Table", "Langermann", "Levy", "Levy n.13", "Rastrigin", "Schaffer n.2", "Schaffer n.4", "Schwefel", "Shubert", "Mason & Watts (2012)", "N=20, K=5")

statistics <- character(length = length(envNames))#statistics vec, holds strings for each environment

#calculate all neighbors for a 1001 x 1001 matrix
mat <- matrix(1:1001^2, 1001, 1001)
m2<-cbind(NA,rbind(NA,mat,NA),NA)

addresses <- expand.grid(x = 1:1001, y = 1:1001)

ret<-c()
for(i in 1:-1)
  for(j in 1:-1)
    if(i!=0 || j !=0)
      ret<-rbind(ret,m2[addresses$x+i+1+nrow(m2)*(addresses$y+j)]) 


#loop through 2D environments
for (env in 1:length(fitness)){#2D first
    statistics[env] <- envNames[env] #add env name to 
    #calculate mean
    mean <- toString(round(mean(fitness[[env]]), digits=2))
    statistics[env] <- paste(statistics[env], mean, sep=" & ") #concatenate to string
    #calculate variance
    var <- toString(round(sd(fitness[[env]]), digits=2))
    statistics[env] <- paste(statistics[env], var, sep=" & ")
    #calculate modality
    mod <- 0
    for (i in 1:1002001){
        xloc <- addresses$x[i]
        yloc <- addresses$y[i]
        fit <- fitness[[env]][xloc,yloc]
        neighbors <- ret[,i]
        neighborFit <- sapply(neighbors, function(z) fitness[[env]][addresses$x[z], addresses$y[z]])#vector of neighboring fitness values
        bestFit <- max(neighborFit,na.rm=TRUE)
        if (bestFit<fit){
            mod <- mod+1
        }
    }
    statistics[env] <- paste(statistics[env], toString(mod), sep=" & ")
    #finish off latex stirng
    statistics[env] <- paste(statistics[env], "\\ [0.5ex]", sep=" ")
    print(statistics[env])
}

#Mason and Watts
source("functions.R")
env <- length(fitness) + 1

mean <- 0
var <- 0
mod <- 0

for (iter in 1:10000){
    MasonWattsEnv <- MasonWatts(1001) #initialize mason watts environment
    mean<- mean + mean(MasonWattsEnv)
    var <- var + sd(MasonWatts)
    mod_i <- 0
    for (i in 1:1002001){
        xloc <- addresses$x[i]
        yloc <- addresses$y[i]
        fit <- MasonWattsEnv[xloc,yloc]
        neighbors <- ret[,1]
        neighborFit <- sapply(neighbors, function(z) MasonWattsEnv[addresses$x[z], addresses$y[z]])#vector of neighboring fitness values
        bestFit <- max(neighborFit,na.rm=TRUE)
        if (bestFit<fit){
            mod_i <- mod_i+1
        }
    mod <- mod + mod_i
    }
}

mean <- mean/10000
var <-  var/10000
mod <- mod/10000

statistics[env] <- paste(envNames[env], toString(round(mean, digits=2)), sep=" & ")
statistics[env] <- paste(statistics[env], toString(round(var, digits=2)), sep=" & ")
statistics[env] <- paste(statistics[env], toString(mod), sep=" & ")
statistics[env] <- paste(statistics[env], "\\ [0.5ex]", sep=" ")
print(statistics[env])
