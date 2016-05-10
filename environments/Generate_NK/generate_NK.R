rm(list=ls())
#setwd('~/Desktop/GENERATE_NK')
source("generate_landscape.R")
library(gtools)
library(fGarch)


N = 20
K = 10

r <- as.integer( commandArgs(TRUE)[1])

a<-permutations(2,N,v=c(0,1),repeats.allowed=TRUE)
LS<-as.data.frame(a)

if (K==0){depends<-as.vector(1:N); values<-replicate(N,round(runif(2,0,1),1)); fitness<-values} else {depends<- rbind(1:N,replicate(N,sample(c(1:N),K,replace=F))); combinations<-permutations(2,K+1,v=c(0,1),repeats.allowed=TRUE); values<-replicate(N,round(runif(nrow(combinations),0,1),1)); fitness<-cbind(combinations,values); ids<-depends[1,]+(K+1) }
fitness.score<-vector()

	
landscape<-generate_landscape(N,K,LS,fitness,depends,fitness.score)
landscape[,N+1]<-(landscape[,N+1]/max(landscape[,N+1]))^8
fitness.score<-vector()	

landscape<-cbind(0:(nrow(landscape)-1),landscape[,N+1])


name<-paste0("LANDSCAPE",r,'.Rdata',sep="",collapse=NULL)
save(landscape, file=name)
