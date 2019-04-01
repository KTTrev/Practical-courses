setwd("~/Practical-courses/Stochastic_Lab_Course_2")
library("tidyverse") 
library("NonpModelCheck")

#loading the data
children <- read.delim("children3.txt", sep = "")

#Question(a)
#Write an R function that calculates a local polynomial fit and depends on the response,
#covariate, bandwidth, polynomial degree, kernel and the number of derivatives.

#Implementing kernel density estimation in a function that depends on the sample, bandwidth and a kernel
i <- function(x){ifelse((abs(x)<=1),1,0)}
#kernels
nadaraya <- i
uniform <- function(x){0.5*i(x)}
triangular <- function(x){(1-abs(x))*i(x)}
epanechnikov <-function(x){(0.75*(1-x**2))*i(x)}
gaussian <- function(x){((2*pi)^(-1/2))*(exp((-x**2)/2))}

k.names <- list("uniform", "triangular", "epanechnikov", "gaussian", "nadaraya")
k.funtions <- list(uniform, triangular, epanechnikov, gaussian, nadaraya)

#local polynomial fit function

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
    
    V <- apply(term1, MARGIN = 1, FUN = kernel)
    
    for (i in 0:l){
      X_mat[ ,i+1] <- term2^i
    }
  res <- lm(Y ~ X_mat - 1, weights = V) # weighted least squares (exclude intercept)
  res <- res$coefficients
  res <- factorial(derivative)*res[derivative + 1]
  return(res)
  }
  VA.hat <- Vectorize(A.hat, vectorize.args = "x")
  return(VA.hat)
}

#First fix the polynomial degree to 1 
# and estimate f with 4 different bandwidths
fit.1 <- local.poly.fit(children$zwast, children$hypage, bw = 2, l = 1)
fit.2 <-local.poly.fit(children$zwast, children$hypage, bw = 5, l = 1)
fit.3 <-local.poly.fit(children$zwast, children$hypage, bw = 8, l = 1)
fit.4 <-local.poly.fit(children$zwast, children$hypage, bw = 11, l = 1)

#Fitting on the domain of hypage = 0:59
dfit.1 <- fit.1(x = 0:59, derivative = 0)
dfit.2 <- fit.2(x = 0:59, derivative = 0)
dfit.3 <- fit.3(x = 0:59, derivative = 0)
dfit.4 <- fit.4(x = 0:59, derivative = 0)

#dataframes for ggplot
df1 <- data.frame(a = 0:59, b = dfit.1)
df2 <- data.frame(a = 0:59, b = dfit.2)
df3 <- data.frame(a = 0:59, b = dfit.3)
df4 <- data.frame(a = 0:59, b = dfit.4)
#Then the plot
ggplot(children, aes(x = hypage, y = zwast)) +
  geom_point(color = "gray31") +
  geom_line(data = df1, aes(x = a, y = b, color = "2"), size = 1) +
  geom_line(data = df2, aes(x = a, y = b, color = "5"), size = 1) +
  geom_line(data = df3, aes(x = a, y = b, color = "8"), size = 1) +
  geom_line(data = df3, aes(x = a, y = b, color = "11"), linetype = "dashed", size = 1) +
  labs(color = "Bandwidth") 


#now with bw = 8, we fix the polynomial degree to 1, estimate f with 4 kernel from exercise 6
fit.5 <- local.poly.fit(children$zwast, children$hypage, bw = 8, l = 1, kernel = "epanechnikov")
fit.6 <- local.poly.fit(children$zwast, children$hypage, bw = 8, l = 1, kernel = "uniform")
fit.7 <- local.poly.fit(children$zwast, children$hypage, bw = 8, l = 1, kernel = "triangular")
fit.8 <- local.poly.fit(children$zwast, children$hypage, bw = 8, l = 1, kernel = "gaussian")
#Fitting on the domain of hypage = 0:59
dfit.5 <- fit.5(x = 0:59, derivative = 0)
dfit.6 <- fit.6(x = 0:59, derivative = 0)
dfit.7 <- fit.7(x = 0:59, derivative = 0)
dfit.8 <- fit.8(x = 0:59, derivative = 0)

#dataframes for ggplot
df5 <- data.frame(a = 0:59, b = dfit.5)
df6 <- data.frame(a = 0:59, b = dfit.6)
df7 <- data.frame(a = 0:59, b = dfit.7)
df8 <- data.frame(a = 0:59, b = dfit.8)
#Then the plot
ggplot(children, aes(x = hypage, y = zwast)) +
  geom_point(color = "gray31") +
  geom_line(data = df5, aes(x = a, y = b, color = "epanechnikov"), size = 1) +
  geom_line(data = df6, aes(x = a, y = b, color = "uniform"), size = 1) +
  geom_line(data = df7, aes(x = a, y = b, color = "triangular"), size = 1) +
  geom_line(data = df8, aes(x = a, y = b, color = "gaussian"),  size = 0.5) +
  labs(color = "Kernel") 

#Question (b)
#Write a function that calculates the optimal bandwidth with Generalised Cross Validation (GCV)





GCV <- function(Y, X, bw, l){
  # Y = covariate; X = response variable; bw = bandwidth; l = polynomial degree
  X_values <- unique(sort(X))
  n <- length(Y)
  # Precomputation of matrices X(x_i) in formula for the estimator A_hat
  X_mat_list <- list()
  for (i in X_values){
    index = which(X_values == i)
    X_mat_list[[index]] = matrix(0, n, 4+1)
    aux = (X - i)
    for (j in 0:4){
      X_mat_list[[index]][, j+1] = aux**j
    }
  }
  
  sum_square = rep(0, length(X_values)) # MSE of Y vs fitted values of X
  W_trace = matrix(0, length(X_values)) # Sum of trace values of weight function W
  
  poly_fit = local.poly.fit(Y, X, bw, l)
  
  # Calculate components of W_trace and sum_square
  for (i in X_values){
    index = which(X_values == i)
    aux = (X - i)
    
    X_mat = X_mat_list[[index]][, 1:(l+1)]
    
    V = diag(epanechnikov(aux/bw))
    
    weight_vector = solve(t(X_mat) %*% V %*% X_mat) %*% t(X_mat) %*% V
    weight_vector = factorial(l+1) * weight_vector[l+1, ]
    
    W_trace[index] = sum((X == i) * weight_vector)
    
    sum_square[index] = sum((Y[(X == i)] - poly_fit(i, deriv = 0))**2)
    print(paste0(index, "/60"))
  }
  res = sum(sum_square)/(1 - sum(W_trace)/n)**2
  
  return(res)
  
}

#optimal bandwidth by GCV for polynomial degrees 1 to 4
Y <- children$zwast
X <- children$hypage
GCV1 <- Vectorize(function(bw){GCV(Y, X, bw, l = 1)})
GCV2 <- Vectorize(function(bw){GCV(Y, X, bw, l = 2)})
GCV3 <- Vectorize(function(bw){GCV(Y, X, bw, l = 3)})
GCV4 <- Vectorize(function(bw){GCV(Y, X, bw, l = 4)})

opt.GCV1 <- optimize(GCV1, interval = c(1, 11))$minimum
opt.GCV2 <- optimize(GCV2, interval = c(1, 11))$minimum
opt.GCV3 <- optimize(GCV3, interval = c(1, 11))$minimum
opt.GCV4 <- optimize(GCV4, interval = c(1, 11))$minimum






