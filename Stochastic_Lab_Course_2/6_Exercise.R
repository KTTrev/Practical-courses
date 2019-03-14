setwd("~/Stochastic_Lab/Stochastic_Lab_Course_2")
library("tidyverse") 

#Question (a)
#Implement kernel density estimation in a function that depends on the sample, bandwidth and a kernel


i <- function(x){ifelse((abs(x)<=1),1,0)}
#kernels
uniform <- function(x){0.5*i(x)}
triangular <- function(x){(1-abs(x))*i(x)}
epanechnikov <-function(x){0.75*(1-x**2)}
gaussian <- function(x){((2*pi)^(-1/2))*(exp((x**2)/2))}

k.names <- list("uniform", "triangular", "epanechnikov", "gaussian")
k.funtions <- list(uniform, triangular, epanechnikov, gaussian)

#KDE = Kernel density estimator
KDE <- function(sample, bw, kernel){
  kernel = k.funtions[[which(k.names == kernel)]]
  a <- function(x){
    t <- (x - sample)/bw
    (1/bw*length(sample))*(sum(sapply(t, K)))
  }
  return(Vectorize(t))
}






#loading the data
a <- read.csv("StudentsPerformance.csv")
students <- a %>%
  dplyr::select(c(test.preparation.course, math.score, reading.score, writing.score))


ggplot(students, aes(x = math.score)) + 
  stat_density(aes(color = "nrd0"), kernel = "epanechnikov", bw = "nrd0") + 
  stat_density(aes(color = "nrd"), kernel = "epanechnikov", bw = "nrd") +
  stat_density(aes(color = "SJ"), kernel = "epanechnikov", bw = "SJ") +
  



  
  
  
  

plot( estim.density(s = students$math.score, h = "nrd", K = "epanechnikov"))


