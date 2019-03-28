setwd("~/Stochastic_Lab/Stochastic_Lab_Course_2")
library("tidyverse") 

#Question (a)
#Implement kernel density estimation in a function that depends on the sample, bandwidth and a kernel
i <- function(x){ifelse((abs(x)<=1),1,0)}
#kernels
uniform <- function(x){0.5*i(x)}
triangular <- function(x){(1-abs(x))*i(x)}
epanechnikov <-function(x){(0.75*(1-x**2))*i(x)}
gaussian <- function(x){((2*pi)^(-1/2))*(exp((-x**2)/2))}

k.names <- list("uniform", "triangular", "epanechnikov", "gaussian")
k.funtions <- list(uniform, triangular, epanechnikov, gaussian)

#KDE = Kernel density estimator
KDE <- function(sample, bw, kernel){
  K <- k.funtions[[which(k.names == kernel)]]
  a <- function(x){
  t <- (x - sample)/bw
    (1/(bw*length(sample)))*(sum(K(t)))
  }
  return(Vectorize(a))
}

#loading the data
a <- read.csv("StudentsPerformance.csv")
students <- a %>%
  dplyr::select(c(test.preparation.course, math.score, reading.score, writing.score))

#plot: diffrent bandwidth with epanechnikov kernel
f1 <- KDE(students$math.score, 2, "epanechnikov")
f2 <- KDE(students$math.score, 5, "epanechnikov")
f3 <- KDE(students$math.score, 8, "epanechnikov")
f4 <- KDE(students$math.score, 11, "epanechnikov")

ggplot(students, aes(x = math.score)) + 
  stat_function(fun = f1, aes(colour = "2")) +
  stat_function(fun = f2, aes(colour = "5")) +
  stat_function(fun = f3, aes(colour = "8")) +
  stat_function(fun = f4, aes(colour = "11")) +
  theme(legend.position = c(0.96, 0.95),legend.justification = c("right", "top"), legend.title = element_text()) +
  labs(color = "Bandwidth") #legend title

#plot#plot: bandwidth = 8, with different kernels
g1 <- KDE(students$math.score, 8, "epanechnikov")
g2 <- KDE(students$math.score, 8, "uniform")
g3 <- KDE(students$math.score, 8, "triangular")
g4 <- KDE(students$math.score, 8, "gaussian")

ggplot(students, aes(x = math.score)) + 
  stat_function(fun = g1, aes(colour = "epanechnikov")) +
  stat_function(fun = g2, aes(colour = "uniform")) +
  stat_function(fun = g3, aes(colour = "triangular")) +
  stat_function(fun = g4, aes(colour = "gaussian")) +
  theme(legend.position = c(0.96, 0.95),legend.justification = c("right", "top"), legend.title = element_text()) +
  labs(color = "Kernel") #legend title

#Question(b)
#Implement the cross-validation criterion to find the optimal bandwidth
math = students$math.score
reading = students$reading.score
writing = students$writing.score
#cross-validation function from the lecture 6
CV = function(sample, bw){
  n = length(sample)
  
  f.hat = KDE(sample, bw, "gaussian")
  f.hat.sqr = function(x){f.hat(x)**2} 
  
  A = integrate(f.hat.sqr, lower = Inf, upper = Inf) 
  B = (sum(gaussian(outer(sample, sample, FUN = "-")/bw)) - n*gaussian(0))*2/(n*(n-1)*bw)
  
  C = A$value - B
  return(C)
}

CV.math = Vectorize(function(bw){CV(math, bw)})
CV.reading = Vectorize(function(bw){CV(reading, bw)})
CV.writing = Vectorize(function(bw){CV(writing, bw)})

# Optimize objective function
CV.math.opt = optimize(CV.math, interval = c(1, 20))$minimum
CV.reading.opt = optimize(CV.reading, interval = c(1, 15))$minimum
CV.writing.opt = optimize(CV.writing, interval = c(1, 20))$minimum

#with bw.ucv and bw.bcv
#bw.ucv
bw.ucv.math.opt = bw.ucv(X_math, lower = 1, upper = 20)
bw.ucv.reading.opt = bw.ucv(X_reading, lower = 1, upper = 15)
bw.ucv.writing.opt = bw.ucv(X_writing, lower = 1, upper = 20)

#bw.bcv
bw.bcv.math.opt = bw.bcv(math, lower = 1, upper = 20)
bw.bcv.reading.opt = bw.bcv(reading, lower = 1, upper = 15)
bw.bcv.writing.opt = bw.bcv(writing, lower = 1, upper = 20)

#Question(c)
students.c <- filter(students, test.preparation.course == "completed") # .c = completed
students.n <- filter(students, test.preparation.course == "none") # .n = none
#Repeat the question b for test.preparation.course == completed
math.c = students.c$math.score
reading.c = students.c$reading.score
writing.c = students.c$writing.score

CV.math.c = Vectorize(function(bw){CV(math.c, bw)})
CV.reading.c = Vectorize(function(bw){CV(reading.c, bw)})
CV.writing.c = Vectorize(function(bw){CV(writing.c, bw)})
#optimal bandwidth for the group "completed"
CV.math.opt.c = optimize(CV.math.c, interval = c(1, 20))$minimum
CV.reading.opt.c = optimize(CV.reading.c, interval = c(1, 15))$minimum
CV.writing.opt.c = optimize(CV.writing.c, interval = c(1, 20))$minimum
#KDEs
h1.c <- KDE(students.c$math.score, CV.math.opt.c, "gaussian") 
h2.c <- KDE(students.c$reading.score, CV.reading.opt.c, "gaussian")
h3.c <- KDE(students.c$writing.score, CV.writing.opt.c, "gaussian")

#Repeat the question b for test.preparation.course == none
math.n = students.n$math.score
reading.n = students.n$reading.score
writing.n = students.n$writing.score

CV.math.n = Vectorize(function(bw){CV(math.n, bw)})
CV.reading.n = Vectorize(function(bw){CV(reading.n, bw)})
CV.writing.n = Vectorize(function(bw){CV(writing.n, bw)})
#optimal bandwidth for the group "completed"
CV.math.opt.n = optimize(CV.math.n, interval = c(1, 20))$minimum
CV.reading.opt.n = optimize(CV.reading.n, interval = c(1, 15))$minimum
CV.writing.opt.n = optimize(CV.writing.n, interval = c(1, 20))$minimum
#KDEs
h1.n <- KDE(students.n$math.score, CV.math.opt.n, "gaussian") 
h2.n <- KDE(students.n$reading.score, CV.reading.opt.n, "gaussian")
h3.n <- KDE(students.n$writing.score, CV.writing.opt.n, "gaussian")

#plots
ggplot(students, aes(x = math.score)) + 
  stat_function(fun = h1.c, aes(color = "completed")) +
  stat_function(fun = h1.n, aes(color = "none")) +
  scale_colour_manual(name="Test preparation course", values = c("red", "purple")) +
  theme(legend.position = c(0.96, 0.95),legend.justification = c("right", "top"), legend.title = element_text()) 
  
ggplot(students, aes(x = reading.score)) + 
  stat_function(fun = h2.c, aes(color = "completed")) +
  stat_function(fun = h2.n, aes(color = "none")) +
  scale_colour_manual(name="Test preparation course", values = c("red", "purple")) +
  theme(legend.position = c(0.96, 0.95),legend.justification = c("right", "top"), legend.title = element_text())

ggplot(students, aes(x = writing.score)) + 
  stat_function(fun = h3.c, aes(color = "completed")) +
  stat_function(fun = h3.n, aes(color = "none")) +
  scale_colour_manual(name="Test preparation course", values = c("red", "purple")) +
  theme(legend.position = c(0.96, 0.95),legend.justification = c("right", "top"), legend.title = element_text())
