setwd("~/Practical-courses/Stochastic_Lab_Course_2")
set.seed(1)
library("tidyverse")
library("bootstrap")
library(tikzDevice) # To export plots as .tex files

n <- 100 #the sample size 
R <- 1000 #the number of bootstrap replications
M <- 1000 #the number of Monte Carlo samples 

#Question(a)

#Simulate a sample with size n from the Weibull distribution with the scale parameter lamda = 13 and shape parameter k = 1
xmed <- 9.010913 # calculated with the formula of xmed
sigma <- 13 # calculated with the formula of sigma
l <- 13
k <- 1
#Building a two-sided bootstrap percentile confidence intervals for s = sigma and x_med at the significance level a = 0:95
#We use M Monte Carlo samples to estimate the coverage probability of both confidence intervals.

is_median <- 0
is_sd <- 0
alpha <- 0.95
CI_MED_left <- 0
CI_MED_right <- 0
CI_SD_left <- 0
CI_SD_right <- 0

for (j in 1:M) {
  sample_weibull <- rweibull(n, k, l)
  MED <- 0
  SD <- 0
  for (i in 1:R) {
    myBootstrap <- sample(sample_weibull, n, replace = T)
    MED[i] <- median(myBootstrap)
    SD[i] <- sd(myBootstrap)
  }
  
  MED <- sort(MED)
  SD <- sort(SD)
  
  CI_MED_left[j] <- MED[floor(R*(1-alpha))/2]
  CI_MED_right[j] <- MED[floor(R*(1-(1-alpha)/2))]
  CI_SD_left[j] <- SD[floor(R*(1-alpha))/2]
  CI_SD_right[j] <- SD[floor(R*(1-(1-alpha)/2))]
  df1 <- data.frame(CI_MED_left, CI_MED_right, CI_SD_left, CI_SD_right)
}
df1 <- df1 %>% 
  mutate(is_median = xmed >= CI_MED_left & xmed <= CI_MED_right)
df1 <- df1 %>% 
  mutate(is_sd = sigma >= CI_SD_left & sigma <= CI_SD_right)


#estimation of the coverage probability of both confidence intervals
First_coverage_prob <- c(sum(df1$is_median)/M, sum(df1$is_sd)/M)

#Estimation of the average interval length
First_average_interval_length <- c(sum(df1$CI_MED_right - df1$CI_MED_left)/nrow(df1), sum(df1$CI_SD_right - df1$CI_SD_left)/nrow(df1))

#####if n = R = 1000 
n <- 1000
R <- 1000
is_median <- 0
is_sd <- 0
alpha <- 0.95
CI_MED_left <- 0
CI_MED_right <- 0
CI_SD_left <- 0
CI_SD_right <- 0

for (j in 1:M) {
  sample_weibull <- rweibull(n, k, l)
  MED <- 0
  SD <- 0
  for (i in 1:R) {
    myBootstrap <- sample(sample_weibull, n, replace = T)
    MED[i] <- median(myBootstrap)
    SD[i] <- sd(myBootstrap)
  }
  
  MED <- sort(MED)
  SD <- sort(SD)
  
  CI_MED_left[j] <- MED[floor(R*(1-alpha))/2]
  CI_MED_right[j] <- MED[floor(R*(1-(1-alpha)/2))]
  CI_SD_left[j] <- SD[floor(R*(1-alpha))/2]
  CI_SD_right[j] <- SD[floor(R*(1-(1-alpha)/2))]
  df2 <- data.frame(CI_MED_left, CI_MED_right, CI_SD_left, CI_SD_right)
}
df2 <- df2 %>% 
  mutate(is_median = xmed >= CI_MED_left & xmed <= CI_MED_right)
df2 <- df2 %>% 
  mutate(is_sd = sigma >= CI_SD_left & sigma <= CI_SD_right)


#estimation of the coverage probability of both confidence intervals
Second_coverage_prob <- c(sum(df2$is_median)/M, sum(df2$is_sd)/M)

#Estimation of the average interval length
Second_average_interval_length <- c(sum(df2$CI_MED_right - df2$CI_MED_left)/nrow(df2), sum(df2$CI_SD_right - df2$CI_SD_left)/nrow(df2))

#####if n = 100, R = 5000
n <- 100
R <- 5000
is_median <- 0
is_sd <- 0
alpha <- 0.95
CI_MED_left <- 0
CI_MED_right <- 0
CI_SD_left <- 0
CI_SD_right <- 0

for (j in 1:M) {
  sample_weibull <- rweibull(n, k, l)
  MED <- 0
  SD <- 0
  for (i in 1:R) {
    myBootstrap <- sample(sample_weibull, n, replace = T)
    MED[i] <- median(myBootstrap)
    SD[i] <- sd(myBootstrap)
  }
  
  MED <- sort(MED)
  SD <- sort(SD)
  
  CI_MED_left[j] <- MED[floor(R*(1-alpha))/2]
  CI_MED_right[j] <- MED[floor(R*(1-(1-alpha)/2))]
  CI_SD_left[j] <- SD[floor(R*(1-alpha))/2]
  CI_SD_right[j] <- SD[floor(R*(1-(1-alpha)/2))]
  df3 <- data.frame(CI_MED_left, CI_MED_right, CI_SD_left, CI_SD_right)
}
df3 <- df3 %>% 
  mutate(is_median = xmed >= CI_MED_left & xmed <= CI_MED_right)
df3 <- df3 %>% 
  mutate(is_sd = sigma >= CI_SD_left & sigma <= CI_SD_right)


#estimation of the coverage probability of both confidence intervals
Third_coverage_prob <- c(sum(df3$is_median)/M, sum(df3$is_sd)/M)

#Estimation of the average interval length
Third_average_interval_length <- c(sum(df3$CI_MED_right - df3$CI_MED_left)/nrow(df3), sum(df3$CI_SD_right - df3$CI_SD_left)/nrow(df3))

####bootstrap accelerated 
####with M Monte Carlo samples to assess the coverage probability and the average length
#First whith the median

CI_MED_left <- rep(0, M)
CI_MED_right <- rep(0, M)
z_MED <- 0
a.0_MED <- 0
for (j in 1:M) {
  sample_weibull <- rweibull(100, k, l)
  A <- bcanon(sample_weibull, R, theta = median, alpha = c(0.025, 0.975))
  z_MED[j] <- A$z0
  a.0_MED[j] <- A$acc
  CI_MED_left[j] <- A$confpoints[1,2]
  CI_MED_right[j] <- A$confpoints[2,2]
}

df_bca_point_MED <- data.frame(CI_MED_left, CI_MED_right)

df_bca_point_MED <- df_bca_point_MED %>% 
  mutate(is_median = xmed >= CI_MED_left & xmed <= CI_MED_right)



#Then with the sd
CI_SD_left <- 0
CI_SD_right <- 0
z_SD <- 0
a.0_SD <- 0
for (j in 1:M) {
  sample_weibull <- rweibull(100, k, l)
  A <- bcanon(sample_weibull, R, theta = sd, alpha = c(0.025, 0.975))
  z_SD[j] <- A$z0
  a.0_SD[j] <- A$acc
  CI_SD_left[j] <- A$confpoints[1,2]
  CI_SD_right[j] <- A$confpoints[2,2]
}

df_bca_point_SD <- data.frame(CI_SD_left, CI_SD_right)

df_bca_point_SD <- df_bca_point_SD %>% 
  mutate(is_sd = sigma >= CI_SD_left & sigma <= CI_SD_right)

#Before we continue, we group relevant results into dataframes
estim <-  data.frame(z_SD, a.0_SD, z_MED, a.0_MED) #estimated bias correction and estimated acceleration constant values
df4 <- data.frame(df_bca_point_MED, df_bca_point_SD) 
# Now, estimation of the coverage probability of both confidence intervals

Fourth_coverage_prob <- c(sum(df4$is_median)/M, sum(df4$is_sd)/M)

#Estimation of the average interval length
Fourth_average_interval_length <- c(sum(df4$CI_MED_right - df4$CI_MED_left)/nrow(df4), sum(df4$CI_SD_right - df4$CI_SD_left)/nrow(df4))

####IMPORTANT: take the average of  each z_SD, a.0_SD, z_MED and a.0_MED, to comment on z0 and a^
comment1 <- mean(z_SD)
comment2 <- mean(a.0_SD)
comment3 <- mean(z_MED)
comment4 <- mean(a.0_MED)



#Question(b)
shhs1 <- read.delim("shhs1.txt")

#histogram and empirical distribution of the variable rdi4p
plot <- ggplot(shhs1, aes(x = rdi4p, y = ..density..)) +
  geom_histogram(color = 'white') +
  geom_density(aes(x = rdi4p), colour = "blue")

#bootstrap percentile (two-sided) confidence intervals for the standard deviation and median
rdi4p.med <- median(shhs1$rdi4p)
rdi4p.sigma <- sd(shhs1$rdi4p)
alpha <- 0.95
n <- length(shhs1$rdi4p) #sample size
R <- 1000 #number of bootstraps replicantions

MED <- 0
SD <- 0
for (i in 1:R) {
  myBootstrap <- sample(shhs1$rdi4p, n, replace = T)
  MED[i] <- median(myBootstrap)
  SD[i] <- sd(myBootstrap)
}
MED <- sort(MED)
SD <- sort(SD)
CI_MED_left <- MED[floor(R*(1-alpha))/2]
CI_MED_right <- MED[floor(R*(1-(1-alpha)/2))]
CI_SD_left <- SD[floor(R*(1-alpha))/2]
CI_SD_right <- SD[floor(R*(1-(1-alpha)/2))]

boostrap_percentile_CI <- data.frame(CI_MED_left, CI_MED_right, CI_SD_left, CI_SD_right)
is_median <- rdi4p.med >= CI_MED_left & rdi4p.med <= CI_MED_right
is_sd <- rdi4p.sigma >= CI_SD_left & rdi4p.sigma <= CI_SD_right

#Now, we build the bootstrap accelerated bias-corrected confidence intervals 
#First for the median
rdi4p.median.rem <- shhs1$rdi4p [! shhs1$rdi4p %in% median(shhs1$rdi4p)] #we remove all values equal to the median, to avoid errors
A <- bcanon(rdi4p.median.rem, R, theta = median, alpha = c(0.025, 0.975))
z_MED <- A$z0
a.0_MED <- A$acc
CI_MED_left <- A$confpoints[1,2]
CI_MED_right <- A$confpoints[2,2]

is_median_A = rdi4p.med >= CI_MED_left & rdi4p.med <= CI_MED_right
#Then for the sd

A <- bcanon(shhs1$rdi4p, R, theta = sd, alpha = c(0.025, 0.975))
z_SD <- A$z0
a.0_SD <- A$acc
CI_SD_left <- A$confpoints[1,2]
CI_SD_right <- A$confpoints[2,2]

is_sd_A = rdi4p.sigma >= CI_SD_left & rdi4p.sigma <= CI_SD_right

BCa_CI <- data.frame(CI_MED_left, CI_MED_right, CI_SD_left, CI_SD_right, z_MED, a.0_MED, z_SD, a.0_SD)


tikz('Ex3plot.tex',width=3.5, height=3, sanitize=TRUE)
plot
dev.off()
