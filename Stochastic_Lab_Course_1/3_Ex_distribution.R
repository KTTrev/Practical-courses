setwd("~/Desktop/MScMaths/Summer2018/Stochastic_Lab_Course_1/CodeAndData")
library(dplyr)
library(ggplot2)
library(devtools)
library(qqplotr)
library(sfsmisc)
library("gridExtra") #to plot multiple plots in one page

#in the following code, "ww" stands for "white wine"
ww<- read.csv2("winequality-white.csv", dec = ".")
#add the column good. NB: change it to factor since it is representing two categories
ww$good<- as.factor(ifelse(ww$quality > 5, 1, 0))

#Create two samples subset: sample_white_wine.1 and sample_white_wine.0
sample_white_wine.1<- select(filter(ww, good == 1), c(volatile.acidity, residual.sugar, pH, quality))
sample_white_wine.0<- select(filter(ww, good == 0), c(volatile.acidity, residual.sugar, pH, quality))

#(a)lot a histogram of pH for all wines and add to the plot a normal density, estimating
##the parameters from the data
p1<-ggplot(ww, aes(x = pH, mean = mean(ww$pH), sd = sd(ww$pH))) +
  geom_histogram(aes(y=..density..),colour = "white") +
  stat_function(fun = function(x) dnorm(x, mean = mean(ww$pH), sd = sd(ww$pH)), color = "red", size = 1)+
  ggtitle("histogram of pH for all wines")

##same histograms with corresponding normal densities for good and bad wines separately
p2<-ggplot(sample_white_wine.1, aes(x = pH, mean = mean(sample_white_wine.1$pH), sd = sd(sample_white_wine.1$pH))) +
  geom_histogram(aes(y=..density..), colour = "white") +
  stat_function(fun = function(x) dnorm(x, mean = mean(sample_white_wine.1$pH), sd = sd(sample_white_wine.1$pH)), color = "red", size = 1) +
  ggtitle("histogram of pH for Good wines")

p3<-ggplot(sample_white_wine.0, aes(x = pH, mean = mean(sample_white_wine.0$pH), sd = sd(sample_white_wine.0$pH))) +
  geom_histogram(aes(y=..density..), colour = "white") +
  stat_function(fun = function(x) dnorm(x, mean = mean(sample_white_wine.0$pH), sd = sd(sample_white_wine.0$pH)), color = "red", size = 1) +
  ggtitle("histogram of pH for Bad wines")

grid.arrange(p1, p2, p3, nrow = 2)


#(b)
q1<-ggplot(data = ww, mapping = aes(sample = pH)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  ggtitle("QQ-plot of pH for all wines")

q2<-ggplot(data = sample_white_wine.1, mapping = aes(sample = pH)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  ggtitle("QQ-plot of pH for Good wines")

q3<-ggplot(data = sample_white_wine.0, mapping = aes(sample = pH)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  ggtitle("QQ-plot of pH for Bad wines")

grid.arrange(q1, q2, q3, nrow = 2)


#pp-plots
r1<-ggplot(data = ww, mapping = aes(sample = pH)) +
  stat_pp_band() +
  stat_pp_line() +
  stat_pp_point() +
  labs(x = "Probability Points", y = "Cumulative Probability") +
  ggtitle("PP-plot of pH for all wines")

r2<-ggplot(data = sample_white_wine.1, mapping = aes(sample = pH)) +
  stat_pp_band() +
  stat_pp_line() +
  stat_pp_point() +
  labs(x = "Probability Points", y = "Cumulative Probability") +
  ggtitle("PP-plot of pH for Good wines")

r3<-ggplot(data = sample_white_wine.0, mapping = aes(sample = pH)) +
  stat_pp_band() +
  stat_pp_line() +
  stat_pp_point() +
  labs(x = "Probability Points", y = "Cumulative Probability") +
  ggtitle("PP-plot of pH for Bad wines")

grid.arrange(r1, r2, r3, nrow = 2)


#(c)
#confidence intervals
ECDF.pH<-ecdf(ww$pH)(ww$pH)
s<-sd(ww$pH)
error<- qnorm(0.975)*sqrt(ECDF.pH*(1-ECDF.pH)/length(ww$pH))
upper.bound<- ECDF.pH + error
lower.bound<- ECDF.pH - error
#values in [0,1]
upper.bound<- pmax(upper.bound, 0)
lower.bound<- pmin(lower.bound, 1)
#plot

ggplot(ww, aes(pH)) + 
  stat_ecdf(geom = "point") + 
  labs(title="Empirical Distributions for pH") +
  geom_ribbon(aes(ymin = lower.bound , ymax = upper.bound), fill = "green", alpha = 0.5) +
  ggtitle("Empirical Distributions pH for all wines")

#(d)
#uniform confidence intervals
ECDF.pH<-ecdf(ww$pH)(ww$pH)
s<-sd(ww$pH)
upper.bound<- ECDF.pH + qunif(0.975)*1/length(ww$pH)
lower.bound<- ECDF.pH - qunif(0.975)*1/length(ww$pH)
#values in [0,1]
upper.bound<- pmax(upper.bound, 0)
lower.bound<- pmin(lower.bound, 1)
#plot

ggplot(ww, aes(pH)) + 
  stat_ecdf(geom = "point") + 
  labs(title="Empirical Distributions for pH") +
  geom_ribbon(aes(ymin = lower.bound , ymax = upper.bound), fill = "green", alpha = 0.5) +
  ggtitle("Empirical Distributions pH for all wines with ")


#(e)
#confidence intervals
ECDF.pH<-ecdf(sample_white_wine.1$pH)(sample_white_wine.1$pH)
s<-sd(sample_white_wine.1$pH)
error<- qnorm(0.975)*sqrt(ECDF.pH*(1-ECDF.pH)/length(sample_white_wine.1$pH))
upper.bound.1<- ECDF.pH + error
lower.bound.1<- ECDF.pH - error
#values in [0,1]
upper.bound.1<- pmax(upper.bound.1, 0)
lower.bound.1<- pmin(lower.bound.1, 1)
#plot
s1<-ggplot(sample_white_wine.1, aes(pH)) + 
  stat_ecdf(geom = "point") + 
  labs(title="Empirical Distributions for pH") +
  geom_ribbon(aes(ymin = lower.bound.1 , ymax = upper.bound.1), fill = "green", alpha = 0.5) +
  ggtitle("Empirical Distributions pH for Good wines")

#Same precedure
#confidence intervals
ECDF.pH<-ecdf(sample_white_wine.0$pH)(sample_white_wine.0$pH)
s<-sd(sample_white_wine.0$pH)
error<- qnorm(0.975)*sqrt(ECDF.pH*(1-ECDF.pH)/length(sample_white_wine.0$pH))
upper.bound.0<- ECDF.pH + error
lower.bound.0<- ECDF.pH - error
#values in [0,1]
upper.bound.0<- pmax(upper.bound.0, 0)
lower.bound.0<- pmin(lower.bound.0, 1)
#plot
s2<-ggplot(sample_white_wine.0, aes(pH)) + 
  stat_ecdf(geom = "point") + 
  labs(title="Empirical Distributions for pH") +
  geom_ribbon(aes(ymin = lower.bound.0 , ymax = upper.bound.0), fill = "green", alpha = 0.5) +
  ggtitle("Empirical Distributions pH for Good wines")

grid.arrange(s1, s2, nrow = 2)

