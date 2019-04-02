setwd("~/Practical-courses/Stochastic_Lab_Course_2")
set.seed(250)
library("tidyverse")
library(tikzDevice) # To export plots as .tex files

##Question (a)
#Switch the default random number generator in R to Wichmann-Hill
RNGkind(kind = "Wichmann-Hill", normal.kind = NULL)

#Inversion method to simulate a binomial random variable
N = 1000
n = 10
p = 0.4
u <- runif(N)
bins <- .bincode(u, breaks = c(0, pbinom(0:10, 10, 0.4)), right = F, include.lowest = T)

bin_inv <- numeric()
for(i in 1:N){
  bin_inv[i] <- bins[i]-1
}
#Simulation of a binomial random variable by simulating corresponding Bernoulli random variables by inversion method
bin_bern <- numeric()
for (i in 1:N){
  v <- runif(n)
  bin_bern[i] <- sum(v < p)
}
#Simulation of a binomial random variable with rbinom
bin_rbin <- rbinom(N, n, p)

#Plot the histograms of all three samples on one panel
bin_inv <- data.frame(bin_inv)
bin_inv$method <- rep("inverse CDF", 1000)
colnames(bin_inv) <- c("rand_num", "method")

bin_bern <- data.frame(bin_bern)
bin_bern$method <- rep("From Bernoulli", 1000)
colnames(bin_bern) <- c("rand_num", "method")

bin_rbin <- data.frame(bin_rbin)
bin_rbin$method <- rep("rbinom", 1000)
colnames(bin_rbin) <- c("rand_num", "method")

df <- rbind(bin_inv, bin_bern, bin_rbin)

plot1 <- ggplot(df, aes(x = rand_num, fill = method)) +
  geom_histogram( binwidth=.5, position="dodge")

#Switch the random number generator back to its default
RNGkind(kind = "default", normal.kind = NULL)

##Question(b)
f <- function(x){
  ((2*pi)^(-1/2))*exp(-(x^2)/2)
}

g <- function(x){
  (pi*(1 + x^2))^(-1)
}

#First determine the best value of the constant c, such that f(x) <= g(x)
x <- -1500:1500
c <- max(f(x)/g(x))

#10000 standard normal random variables using 
#the accept-reject method, generating Cauchy distributed random variables using inversion method
N <- 10000
j <- 0
rand_num <- numeric()
while(length(rand_num) != N){
  w <- runif(1) #step 1
  cauchy <- tan((w-(1/2))*pi) #still step 1
  U <- runif(1) #step 2
  if(U*c*g(cauchy) <= f(cauchy)){
    rand_num[j] <- cauchy #step 3
    j <- j + 1
  }
}

#Histogram of the obtained sample with the standard normal density
k <- rnorm(N)
df <- data.frame(rand_num, k)
plot2 <- ggplot(df) +
  geom_histogram(aes( x = rand_num, y = ..density.., colour = rand_num), colour ="white") +
  geom_density(aes(x = k), colour = "blue") 

#QQ-plot
plot3 <- ggplot(data = df, mapping = aes(sample = rand_num)) +
  stat_qq() 

#it is not possible to simulate from the standard Cauchy density using
#the accept-reject method, the standard normal candidate density
#because max(g(x)/f(x)) == Inf


#exports plots as .tex files
tikz('Ex2plot1.tex',width=3.5, height=3, sanitize=TRUE)
plot1
dev.off()

tikz('Ex2plot2.tex',width=3.5, height=3, sanitize=TRUE)
plot2
dev.off()

tikz('Ex2plot3.tex',width=3.5, height=3, sanitize=TRUE)
plot3
dev.off()
