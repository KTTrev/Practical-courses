setwd("~/Desktop/MScMaths/Summer2018/Stochastic_Lab_Course_1/CodeAndData")
library(ggplot2)
library(dplyr)
library(tidyr)
library("gridExtra") #to plot multiple plots in one page

#in the following code, "ww" stands for "white wine"

#(a)
ww<- read.csv2("winequality-white.csv", dec = ".")
#add the column good. NB: change it to factor since it is representing two categories
ww$good<- as.factor(ifelse(ww$quality > 5, 1, 0))


#(b)
#Create two samples subset: subset_white_wine.1 and subset_white_wine.0
subset_white_wine.1<- select(filter(ww, good == 1), c(volatile.acidity, residual.sugar, pH, quality))
subset_white_wine.0<- select(filter(ww, good == 0), c(volatile.acidity, residual.sugar, pH, quality))

###Plots
##for subset_white_wine.1
p1<- ggplot(subset_white_wine.1, aes(residual.sugar))+
  geom_histogram(binwidth = nclass.Sturges(subset_white_wine.1$residual.sugar))+
  ggtitle(" Good white wine:\n Sturges ")

p2<- ggplot(subset_white_wine.1, aes(residual.sugar))+
  geom_histogram(binwidth = nclass.scott(subset_white_wine.1$residual.sugar))+
  ggtitle(" Good white wine:\n Scott")

p3<- ggplot(subset_white_wine.1, aes(residual.sugar))+
  geom_histogram(binwidth = nclass.FD(subset_white_wine.1$residual.sugar))+
  ggtitle(" Good white wine:\n Freadman-Diaconis")

p4<- ggplot(subset_white_wine.1, aes(residual.sugar))+
  geom_histogram()+
  ggtitle("Good white wine:\n Without any bin width")

grid.arrange(p1, p2, p3, p4, nrow = 2)


##for subset_white_wine.0
q1<-ggplot(subset_white_wine.0, aes(residual.sugar))+
  geom_histogram(binwidth = nclass.Sturges(subset_white_wine.0$residual.sugar))+
  ggtitle(" Bad white wine:\n Sturges ")

q2<-ggplot(subset_white_wine.0, aes(residual.sugar))+
  geom_histogram(binwidth = nclass.scott(subset_white_wine.0$residual.sugar))+
  ggtitle("Bad white wine:\n Scott")

q3<-ggplot(subset_white_wine.0, aes(residual.sugar))+
  geom_histogram(binwidth = nclass.FD(subset_white_wine.0$residual.sugar))+
  ggtitle("Bad white wine:\n Freadman-Diaconis")

q4<-ggplot(subset_white_wine.0, aes(residual.sugar))+
  geom_histogram()+
  ggtitle("Bad white wine:\n Without any bin width")

grid.arrange(q1, q2, q3, q4, nrow = 2)

##summary statistics for both wine groups
statistics_white_wine.1<-c(mean(subset_white_wine.1$residual.sugar), median(subset_white_wine.1$residual.sugar), sd(subset_white_wine.1$residual.sugar), IQR(subset_white_wine.1$residual.sugar), min(subset_white_wine.1$residual.sugar), max(subset_white_wine.1$residual.sugar))
statistics_white_wine.0<-c(mean(subset_white_wine.0$residual.sugar), median(subset_white_wine.0$residual.sugar), sd(subset_white_wine.0$residual.sugar), IQR(subset_white_wine.0$residual.sugar), min(subset_white_wine.0$residual.sugar), max(subset_white_wine.0$residual.sugar))


statistics_table<-rbind(statistics_white_wine.1, statistics_white_wine.0)

colnames(statistics_table)<- c("mean", "median", "sd", "IQR","min", "max")

##boxplot
ggplot(ww, aes(good ,residual.sugar, fill = good))+
  geom_boxplot()

###Generate a QQ-plot to compare the two groups
qqplot(x = subset_white_wine.1$residual.sugar, y = subset_white_wine.0$residual.sugar, xlim = range(subset_white_wine.0, ylim = range(subset_white_wine.0))) #can't find a way to do it with ggplot
# create (insertt) a diagonal line
abline(a = 0, b = 1)

###empirical distribution functions
# ecdf of white wine: Good and Bad
ggplot(ww, aes(x=residual.sugar, color = good)) + 
  stat_ecdf()+
  ggtitle("ecdf white whine for residual.sugar")



###(c) Now we consider volatile.acidity for good and bad wines, and then we repeat everthing. You can do that later on




#Create two samples subset: subset_white_wine.1 and subset_white_wine.0
subset_white_wine.1<- select(filter(ww, good == 1), c(volatile.acidity, residual.sugar, pH, quality))
subset_white_wine.0<- select(filter(ww, good == 0), c(volatile.acidity, residual.sugar, pH, quality))

###Plots
##for subset_white_wine.1
r1<- ggplot(subset_white_wine.1, aes(volatile.acidity))+
  geom_histogram(binwidth = nclass.Sturges(subset_white_wine.1$volatile.acidity))+
  ggtitle(" Good white wine:\n Sturges ")

r2<- ggplot(subset_white_wine.1, aes(volatile.acidity))+
  geom_histogram(binwidth = nclass.scott(subset_white_wine.1$volatile.acidity))+
  ggtitle(" Good white wine:\n Scott")

r3<- ggplot(subset_white_wine.1, aes(volatile.acidity))+
  geom_histogram(binwidth = nclass.FD(subset_white_wine.1$volatile.acidity))+
  ggtitle(" Good white wine:\n Freadman-Diaconis")

r4<- ggplot(subset_white_wine.1, aes(volatile.acidity))+
  geom_histogram()+
  ggtitle("Good white wine:\n Without any bin width")

grid.arrange(r1, r2, r3, r4, nrow = 2)


##for subset_white_wine.0
t1<-ggplot(subset_white_wine.0, aes(volatile.acidity))+
  geom_histogram(binwidth = nclass.Sturges(subset_white_wine.0$volatile.acidity))+
  ggtitle(" Bad white wine:\n Sturges ")

t2<-ggplot(subset_white_wine.0, aes(volatile.acidity))+
  geom_histogram(binwidth = nclass.scott(subset_white_wine.0$volatile.acidity))+
  ggtitle("Bad white wine:\n Scott")

t3<-ggplot(subset_white_wine.0, aes(volatile.acidity))+
  geom_histogram(binwidth = nclass.FD(subset_white_wine.0$volatile.acidity))+
  ggtitle("Bad white wine:\n Freadman-Diaconis")

t4<-ggplot(subset_white_wine.0, aes(volatile.acidity))+
  geom_histogram()+
  ggtitle("Bad white wine:\n Without any bin width")

grid.arrange(t1, t2, t3, t4, nrow = 2)

##summary statistics for both wine groups
statistics_white_wine.1<-c(mean(subset_white_wine.1$volatile.acidity), median(subset_white_wine.1$volatile.acidity), sd(subset_white_wine.1$volatile.acidity), IQR(subset_white_wine.1$volatile.acidity), min(subset_white_wine.1$volatile.acidity), max(subset_white_wine.1))
statistics_white_wine.0<-c(mean(subset_white_wine.0$residual.sugar), median(subset_white_wine.0$residual.sugar), sd(subset_white_wine.0$residual.sugar), IQR(subset_white_wine.0$residual.sugar), min(subset_white_wine.0$volatile.acidity), max(subset_white_wine.0$volatile.acidity))


statistics_table<-rbind(statistics_white_wine.1, statistics_white_wine.0)

colnames(statistics_table)<- c("mean", "median", "sd", "IQR","min", "max")

##boxplot
ggplot(ww, aes(good ,volatile.acidity, fill = good))+
  geom_boxplot()

###Generate a QQ-plot to compare the two groups
qqplot(x = subset_white_wine.1$volatile.acidity, y = subset_white_wine.0$volatile.acidity, xlim = c(0.1, 1), ylim = c(0.1, 1)) #can't find a way to do it with ggplot
# create (insertt) a diagonal line
abline(a = 0, b = 1)

###empirical distribution functions
# ecdf of white wine: Good and Bad
ggplot(ww, aes(x=volatile.acidity, color = good)) + 
  stat_ecdf()+
  ggtitle("ecdf white whine for residual.sugar")


