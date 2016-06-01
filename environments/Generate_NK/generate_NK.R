#used to generate the NK landscapes, where N and K are specified below
source("generate_landscape.R")
library(gtools)
library(fGarch)


#Specify NK parameters
N = 20
K = 10

#Replication Id for cluster computing
r <- as.integer( commandArgs(TRUE)[1])

#List of solution Ids
LS<-as.data.frame(permutations(2,N,v=c(0,1),repeats.allowed=TRUE))

#generate the dependencies between each solution and their fitness values
if (K==0){
	depends<-as.vector(1:N); values<-replicate(N,round(runif(2,0,1),1)); fitness<-values
	} else {
		depends<- rbind(1:N,replicate(N,sample(c(1:N),K,replace=F)))
		combinations<-permutations(2,K+1,v=c(0,1),repeats.allowed=TRUE)
		values<-replicate(N,round(runif(nrow(combinations),0,1),1))
		fitness<-cbind(combinations,values); ids<-depends[1,]+(K+1) 
	}

#Generate landscape using function from generate_landscape.R
landscape<-generate_landscape(N,K,LS,fitness,depends)
#Exponential rescaling as described in our Methods section (also see Lazer & Friedman, 2007)
landscape[,N+1]<-(landscape[,N+1]/max(landscape[,N+1]))^8

#replace solutions Ids with integers
landscape<-cbind(0:(nrow(landscape)-1),landscape[,N+1])

#Move to '/environments/NKlandscapes' folder
setwd(".."); setwd("NKlandscapes")
#Move into folder named after K value
setwd(paste0("",K))

#save output
name<-paste0("N=20,K=",K,"_",r,'.Rdata',sep="",collapse=NULL)
save(landscape, file=name)

#Move back into current directory
setwd(".."); setwd(".."); setwd("Generate_NK")
