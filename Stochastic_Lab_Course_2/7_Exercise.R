setwd("~/Practical-courses/Stochastic_Lab_Course_2")
library("tidyverse") 

children <- read.delim("children3.txt", sep = "")

#Question(a)



#Implement kernel density estimation in a function that depends on the sample, bandwidth and a kernel
i <- function(x){ifelse((abs(x)<=1),1,0)}
#kernels
uniform <- function(x){0.5*i(x)}
triangular <- function(x){(1-abs(x))*i(x)}
epanechnikov <-function(x){(0.75*(1-x**2))*i(x)}
gaussian <- function(x){((2*pi)^(-1/2))*(exp((-x**2)/2))}

k.names <- list("uniform", "triangular", "epanechnikov", "gaussian")
k.funtions <- list(uniform, triangular, epanechnikov, gaussian)

#

local.poly.fit <- function(Y, X, bw, l, kernel = "epanechnikov"){
  # Y = response variable
  # X = covariate
  # bw = bandwidth
  # l = polynomial degree
  # kernel = epanechnikov by default
  
  kernel <- k.funtions[[which(k.names == kernel)]]
  Y <- as.matrix(Y)
  X <- as.matrix(X)
  d <- ncol(X)
  n <- nrow(X)
  X_mat <- matrix(0, d*n, l+1)
  
  A.hat <- function(x, derivative){
    # derivative = f derivative approximated. NB: derivative poly degree <= 1
    term1 <- (X - x)/bw
    term2 <- as.vector(t(X - x))
    
    V <- apply(X, MARGIN = 1, FUN = kernel)
    
    for (i in 0:1){
      X_mat[ ,i+1] <- term2^i
    }
  res <- lm(Y ~ X_mat - 1, weights = V) # weighted least squares (exclude intercept)
  res <- res$cofficients
  res <- factanal(derivative)*res[derivative + 1]
  return(res)
  }
  VA_hat <- Vectorize(A_hat, vectorize.args = "X")
  return(VA_hat)
}



















