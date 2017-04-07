#Environment Generate from Gaussian Process prior
#Charley Wu, March 2017

#house keeping
rm(list=ls())
theme_set(theme_bw(base_size=16))# use the b&w theme

#load packages
packages <- c('plyr', 'ggplot2', 'MASS', 'psych')
lapply(packages, require, character.only = TRUE)

################################################################################################################
# Kernel
################################################################################################################

#Radial Basis Kernel
rbf <- function(X1,X2,lambda, signalVar, noiseVar){
  #transfer to matrices
  X1 <- as.matrix(X1)
  X2 <- as.matrix(X2)
  #check dimensions
  if(ncol(X1) != ncol(X2)){
    stop("X1 and X2 must contain input values of the same dimension.")
  }
  #get dimensions
  N1 <- nrow(X1)
  N2 <- nrow(X2)
  d <- ncol(X1)
  #initialize sigma
  sigma <-  matrix(rep(0, N1*N2),nrow=N1)
  #observational variance
  sf <- signalVar
  #noise variance
  sn <- noiseVar
  #loop through
  for(i in 1:d){
    #x-diff
    xdiff <- (outer(X1[,i],X2[,i],function(x,y) x - y)/lambda)^2
    sigma <- sigma + xdiff
  }
  #RBF function
  if(identical(X1,X2)){
    id <- diag(rep(1,N1))
    sigma.final <- sf*exp(-0.5*sigma) + sn*id
  } else {
    sigma.final <- sf*exp(-0.5*sigma)
  }
  #return final covariance matrix
  return(sigma.final)
}
################################################################################################################
# Environment Generator
################################################################################################################
#Environment Parameters
dimensions <- 2
xseq <-  seq(-10,10,1) #the sequence of points along each input dimension
lambda <- 1  #length scale, larger lambdas create smoother environments
n_envs <- 2 #number of environments to generate
kernelFun <- rbf  #which kernel function to use

#GP hyper parameters
signalVariance <- 1 
noiseVariance <- 1

#build grid of all unique positions in environment
grid <- expand.grid(replicate(dimensions, xseq, simplify=FALSE))

#sample environments from multivariate normal distribution with the specified kernel function supplying the covariance matrix
Envs <- mvrnorm(n = n_envs, mu = rep(0,length(xseq)^dimensions), kernelFun(grid,grid,lambda,signalVariance,noiseVariance))
