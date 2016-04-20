

ackley <- function(xx, a=20, b=0.2, c=2*pi)
{
  ##########################################################################
  #
  # ACKLEY FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUTS:
  #
  # xx = c(x1, x2, ..., xd)
  # a = constant (optional), with default value 20
  # b = constant (optional), with default value 0.2
  # c = constant (optional), with default value 2*pi


  #range = seq(-32.768,32.768,(32.768+32.768)/1000) #1:1001
  #
  ##########################################################################
  
  d <- length(xx)
  
  sum1 <- sum(xx^2)
  sum2 <- sum(cos(c*xx))
  
  term1 <- -a * exp(-b*sqrt(sum1/d))
  term2 <- -exp(sum2/d)
  
  y <- term1 + term2 + a + exp(1)

  y <- (-y + 2.232012e+01)/ abs(2.232012e+01 - 0) #convert to maximization problem and scale between 0 and 1
  
  return(y)
}


crossit <- function(xx)
{
  ##########################################################################
  #
  # CROSS-IN-TRAY FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  fact1 <- sin(x1)*sin(x2)
  fact2 <- exp(abs(100 - sqrt(x1^2+x2^2)/pi))
  
  y <- -0.0001 * (abs(fact1*fact2)+1)^0.1
  
  y <- (-y -0.000100)/ abs(-0.000100 - -2.062593) #convert to maximization problem and scale between 0 and 1

  return(y)
}

drop <- function(xx)
{
  ##########################################################################
  #
  # DROP-WAVE FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  frac1 <- 1 + cos(12*sqrt(x1^2+x2^2))
  frac2 <- 0.5*(x1^2+x2^2) + 2
  
  y <- -frac1/frac2
  y <- (-y + -6.036063e-11)/ abs(-6.036063e-11 - 1.000000e+00) #convert to maximization problem and scale between 0 and 1
  
  return(y)
}

egg <- function(xx)
{
  ##########################################################################
  #
  # EGGHOLDER FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  term1 <- -(x2+47) * sin(sqrt(abs(x2+x1/2+47)))
  term2 <- -x1 * sin(sqrt(abs(x1-(x2+47))))
  
  y <- term1 + term2
  y <- (-y + 1049.1316)/ abs(1049.1316 - -959.5705) #convert to maximization problem and scale between 0 and 1
  
  return(y)
}

griewank <- function(xx)
{
  ##########################################################################
  #
  # GRIEWANK FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  ii <- c(1:length(xx))
  sum <- sum(xx^2/4000)
  prod <- prod(cos(xx/sqrt(ii)))
  
  y <- sum - prod + 1
  y <- (-y + 181.0395)/ abs(181.0395 - 0) #convert to maximization problem and scale between 0 and 1
  
  return(y)
}

holder <- function(xx)
{
  ##########################################################################
  #
  # HOLDER TABLE FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  fact1 <- sin(x1)*cos(x2)
  fact2 <- exp(abs(1 - sqrt(x1^2+x2^2)/pi))
  
  y <- -abs(fact1*fact2)
  y <- (-y + 0)/ abs(0 - -19.20805) #convert to maximization problem and scale between 0 and 1
  
  return(y)
}

langer <- function(xx, m=5, cvec, A)
{
  ##########################################################################
  #
  # LANGERMANN FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUTS:
  #
  # xx   = c(x1, x2, ..., xd)
  # m    = constant (optional), with default value 5
  # cvec = m-dimensional vector (optional), with default value c(1, 2, 5, 2, 3)
  #        (when m=5)
  # A    = (mxd)-dimensional matrix (optional), with default value:
  #        [3  5]
  #        [5  2]
  #        [2  1]
  #        [1  4]
  #        [7  9]
  #        (when m=5 and d=2)
  #
  ##########################################################################
  
  d <- length(xx)
  
  if (missing(cvec)) {
    if (m == 5){
      cvec <- c(1,2,5,2,3)
    }
    else {
      stop('Value of the m-dimensional vector cvec is required.')
    }
  }
  
  if (missing(A)) {
    if (m==5 && d==2) {
      A <- matrix(c(3,5,5,2,2,1,1,4,7,9),5,2,byrow=TRUE)
    }
    else {
      stop('Value of the (mxd)-dimensional matrix A is required.')
    }
  }
  
  xxmat <- matrix(rep(xx,times=m), m, d, byrow=TRUE)    
  inner <- rowSums((xxmat-A[,1:d])^2)	
  outer <- sum(cvec * exp(-inner/pi) * cos(pi*inner))
  
  y <- outer
  
  y <- (-y + 5.161699)/ abs(5.161699 - -4.155052) #convert to maximization problem and scale between 0 and 1
  return(y)
}

levy <- function(xx)
{
  ##########################################################################
  #
  # LEVY FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  d <- length(xx)
  w <- 1 + (xx - 1)/4
  
  term1 <- (sin(pi*w[1]))^2 
  term3 <- (w[d]-1)^2 * (1+1*(sin(2*pi*w[d]))^2)
  
  wi <- w[1:(d-1)]
  sum <- sum((wi-1)^2 * (1+10*(sin(pi*wi+1))^2))
  
  y <- term1 + sum + term3
  
  y <- (-y + 9.538281e+01)/ abs(9.538281e+01 - 1.499760e-32) #convert to maximization problem and scale between 0 and 1
  return(y)
}

levy13 <- function(xx)
{
  ##########################################################################
  #
  # LEVY FUNCTION N. 13
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  term1 <- (sin(3*pi*x1))^2
  term2 <- (x1-1)^2 * (1+(sin(3*pi*x2))^2)
  term3 <- (x2-1)^2 * (1+(sin(2*pi*x2))^2)
  
  y <- term1 + term2 + term3
  y <- (-y + 4.530203e+02)/ abs(4.530203e+02 - 1.349784e-31) #convert to maximization problem and scale between 0 and 1
  return(y)
}


rastr <- function(xx)
{
  ##########################################################################
  #
  # RASTRIGIN FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  d <- length(xx)
  
  sum <- sum(xx^2 - 10*cos(2*pi*xx))
  
  y <- 10*d + sum
  y <- (-y + 80.70288)/ abs(80.70288 - 0) #convert to maximization problem and scale between 0 and 1
  
  return(y)
}


schaffer2 <- function(xx)
{
  ##########################################################################
  #
  # SCHAFFER FUNCTION N. 2
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  fact1 <- (sin(x1^2-x2^2))^2 - 0.5
  fact2 <- (1 + 0.001*(x1^2+x2^2))^2
  
  y <- 0.5 + fact1/fact2
  y <- (-y + 0.9968394)/ abs(0.9968394 - 0) #convert to maximization problem and scale between 0 and 1
  
  
  return(y)
}


schaffer4 <- function(xx)
{
  ##########################################################################
  #
  # SCHAFFER FUNCTION N. 4
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  fact1 <- cos(sin(abs(x1^2-x2^2))) - 0.5
  fact2 <- (1 + 0.001*(x1^2+x2^2))^2
  
  y <- 0.5 + fact1/fact2
  y <- (y - 0.5000939)/ abs(1.0000000 - 0.5000939) #ALREADY A MAXIMIZATION PROBLEM, scale between 0 and 1
  
  
  return(y)
}


schwef <- function(xx)
{
  ##########################################################################
  #
  # SCHWEFEL FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################
  
  d <- length(xx)
  
  sum <- sum(xx*sin(sqrt(abs(xx))))
  
  y <- 418.9829*d - sum
  y <- (-y + 1.675931e+03)/ abs(1.675931e+03 - 2.719677e-04) #convert to maximization problem and scale between 0 and 1
  
  
  return(y)
}


shubert <- function(xx)
{
  ##########################################################################
  #
  # SHUBERT FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################
  
  x1 <- xx[1]
  x2 <- xx[2]
  ii <- c(1:5)
  
  sum1 <- sum(ii * cos((ii+1)*x1+ii))
  sum2 <- sum(ii * cos((ii+1)*x2+ii))
  
  y <- sum1 * sum2
  
  y <- (-y + 210.4818)/ abs(210.4818 - -186.7219) #convert to maximization problem and scale between 0 and 1
  
  return(y)
}

unif <- function(xx){
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  set.seed(x1)
  y<-runif(1)
  
  y <- (y - 9.1456e-09) / (1  - 9.1456e-09)
  return(y)
}


unif <- function(xx){
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  set.seed(x1)
  y<-runif(1)
  
  y <- (y - 9.1456e-09) / (1  - 9.1456e-09)
  return(y)
}




unif2 <- function(xx){
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  #set.seed(3)
  #env <- matrix(runif(1001*1001),nrow=1001,ncol=1001)
 # env <- runif(1001*1001)
  #env <- (env - min(env)) / ((max(env)-min(env)))
  #y<-runif(1)
  #load("uniform_matrix.Rdata")
  y <- uniform_matrix[x1,x2]
  #y <- (y - 9.1456e-09) / (1  - 9.1456e-09)
  return(y)
}


#Charley wu: creating a 2D NK fitness landscape
#TODO: check that the environment remains consistent when calling it multiple times
nk <- function(xx){
  library(CEGO) #library for NK function
  x1<- xx[1]
  x2 <- xx[2]

  #TODO: transform into binary vector
  vec<- c(x1,x2)

  #TODO: define N and K parameters
  NKfunc <- benchmarkGeneratorNKL(20,19) #example with N =6 and K =2
  y <- NKfunc(vec)

  return(y) 
}

#environment from Mason & Watts
library(akima)
MasonWatts<-function(L){ #L specifies the size of the output matrix, which is L x L
  R <- 3 
  rho <- 0.7
  #1. create function matrix as a unimodal bivariate Gaussian with the mean randomly chosen, with variance R
  #generate two univariate Gaussians
  X <- dnorm(seq(1,100), mean = runif(1,1,100), sd=sqrt(R))
  Y <- dnorm(seq(1,100), mean = runif(1,1,100), sd=sqrt(R))
  #Bivariate Gaussian is the product of the two univariate Gaussians
  fitnessMatrix <- X %*% t(Y)
  #scale to between 0 and 1 (Not sure if this is what they do)
  fitnessMatrix <- fitnessMatrix * (1/max(fitnessMatrix))
  #2. compute psuedorandom Perlin noise
  #2a loop through octaves
  for (omega in 3:7){
    octave <- 2^omega  
    #create a smaller matrix of size octave x octave, containing  randomly assigned payoffs 
    octaveMatrix <- matrix(runif(octave^2),ncol=octave, nrow=octave)
    #X or Y mapping of octave sequence 
    octaveSeq <- seq(1,100, length.out=octave)
    #2b. smooth values of all cell values using bicubic interpolation; i.e., blow up octave x octave matrix into a 100 x 100 matrix 
    octaveMatrix <- bicubic.grid(octaveSeq, octaveSeq, octaveMatrix, c(1,100), c(1,100),1,1) 
    #2c. scale matrix by the persistence paramter  
    octaveMatrix <- octaveMatrix$z * rho^omega
    #3. sum together
    fitnessMatrix <- fitnessMatrix + octaveMatrix
  }
  #3 continued... transform to L x L grid using bicubic interpolation
  fitnessMatrix <- bicubic.grid(seq(1,L,length.out=100), seq(1,L,length.out=100), fitnessMatrix, c(1,L), c(1,L),1,1) 
  fitnessMatrix <- fitnessMatrix$z
  #...scale fitnessMatrix to between 1 and 100
  fitnessMatrix <- fitnessMatrix * (1/max(fitnessMatrix))

  return(fitnessMatrix)
  #3D plotting example
  #https://cran.r-project.org/web/packages/plot3D/vignettes/volcano.pdf
  #library(plot3D)
  #persp3D(z = test, clab = "m")
}

#Old Mason and Watts function, which directly computes it on the L x L grid
MasonWattsOld<-function(L){
  R <- 3 * (L/100) #variance term
  rho <- 0.7
  #1. create function matrix as a unimodal bivariate Gaussian with the mean randomly chosen, with variance R
  #generate two random means
  X <- dnorm(seq(1,L), mean = runif(1,1,L), sd=sqrt(R))
  Y <- dnorm(seq(1,L), mean = runif(1,1,L), sd=sqrt(R))
  fitnessMatrix <- X %*% t(Y)
  #scale to between 0 and 1 (Not sure if this is what they do)
  fitnessMatrix <- fitnessMatrix * (1/max(fitnessMatrix))
  #2. compute psuedorandom Perlin noise
  #2a loop through octaves and randomly draw values
  for (omega in 3:7){
    octave <- 2^omega  #scale octave to account for grids larger than original 100x100
    #create a smaller matrix, containing only randomly assigned payoffs corresponding to the cells affected by the octave
    octaveMatrix <- matrix(runif(octave^2),ncol=octave, nrow=octave)
    #center octave sequence on median of grid
    octaveSeq <- seq(1,L, length.out=octave)
    #2b. smooth values of all cell values using bicubic interpolation
    octaveMatrix <- bicubic.grid(octaveSeq, octaveSeq, octaveMatrix, c(1,L), c(1,L),1,1) 
    #2c. scale matrix by the persistence paramter  
    octaveMatrix <- octaveMatrix$z * rho^omega
    #3. sum together
    fitnessMatrix <- fitnessMatrix + octaveMatrix
  }
  #3 continued... scale fitnessMatrix to between 1 and 100
  fitnessMatrix <- fitnessMatrix * (1/max(fitnessMatrix))

  return(fitnessMatrix)
  #3D plotting example
  #https://cran.r-project.org/web/packages/plot3D/vignettes/volcano.pdf
  #persp3D(z = test, clab = "m")
}
