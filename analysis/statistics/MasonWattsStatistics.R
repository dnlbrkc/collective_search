#Calculate mean, variance, and modality of environments
rm(list=ls())
#Mason and Watts
source("functions.R")

#replication ID for cluster
v <- as.integer(commandArgs(TRUE)[1])

#number of replications
MWrep<- 100

#calculate all neighbors for a 1001 x 1001 matrix
mat <- matrix(1:1001^2, 1001, 1001)
m2<-cbind(NA,rbind(NA,mat,NA),NA)

addresses <- expand.grid(x = 1:1001, y = 1:1001) #one address for each location in the grid

ret<-c()
for(i in 1:-1)
  for(j in 1:-1)
    if(i!=0 || j !=0)
      ret<-rbind(ret,m2[addresses$x+i+1+nrow(m2)*(addresses$y+j)]) 


output <- matrix(nrow=MWrep,ncol=3)
for (iter in 1:MWrep){
    MasonWattsEnv <- MasonWatts(1001) #initialize mason watts environment
    output[iter,1]<- mean(MasonWattsEnv)
    output[iter,2] <- sd(MasonWattsEnv)
    mod <- 0
    for (i in 1:1002001){
        xloc <- addresses$x[i]
        yloc <- addresses$y[i]
        fit <- MasonWattsEnv[xloc,yloc]
        neighbors <- ret[,i]
        neighborFit <- sapply(neighbors, function(z) MasonWattsEnv[addresses$x[z], addresses$y[z]])#vector of neighboring fitness values
        bestFit <- max(neighborFit,na.rm=TRUE)
        if (bestFit<fit){
            mod <- mod+1
        }
    }
    output[iter,3] <- mod
}

#average over replications
outputVec <- colMeans(output, na.rm=TRUE)

#write file
name<-paste0("MasonWattsStats", v,'.Rdata',sep="",collapse=NULL)
save(outputVec, file=name)


#analyze saved Rdata files
setwd("MasonWattsStats")

files <- list.files()
stats <- matrix(ncol=3,nrow=length(files))
for (n in 1:length(files)){
    load(files[n])
    stats[n,] <- outputVec

}
#calculate Mason and Watts statistics from pre-generated .Rdata files (see MasonWatts)