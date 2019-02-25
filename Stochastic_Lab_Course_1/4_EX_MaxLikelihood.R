set.seed(12547)
library("ggplot2")
library("rmutil") # contains the function rlaplace
library("gridExtra") #to plot multiple plots in one page

##(b)
#Generate n = 20 independent realisations of X with mean = 1 and sd = 1
x<- rlaplace(20, 1, 1)

#Determine the maximum likelihood estimator of mean based on this sample using R function quantile.
#In order to Experiment with the different 9 types of quantiles available on R, we use the vector MLx
#According to question (a), the maximum likelihood estimator for mean is the median of the sample x
ML.quantile.x<-rep(0,9)
for (i in 1:9){
  ML.quantile.x[i]<- quantile(x, 0.5, type = i) #ML.quantile.x stands for Maximum Likelihood for the sample x, using the quantiile function
}

#Another n = 1000 independent realisations of X with mean = 1 and sigma = 1
#and do the same estimations
y<-rlaplace(1000, 1, 1)

ML.quantile.y<-rep(0,9)
for (i in 1:9){
  ML.quantile.y[i]<- quantile(y, 0.5, type = i) #ML.quantile.y stands for Maximum Likelihood for the sample y using the quantiile function
}

##(c)
#Write a function that calculates the maximum likelihood estimator for a
#Laplace sample numerically using R function optimise.

Max.logL.num.x <- function(a){
  res<-a
  for(i in 1:length(res)){
    res[i]<- -sum(dlaplace(x, m = a[i], s = 1, log = T))
  }
  return(res)
}

optimize(Max.logL.num.x, interval = range(x))

#or
Max.logL.num.x <- function(a){
  -sum(dlaplace(x, m = a, s = 1, log = T))
}

optimize(Vectorize(Max.logL.num.x), interval = range(x))

#Generate n = 20 and n = 1000 independent realisations of X with mu = 1 and sigma = 1
z<-rlaplace(20, 1, 1)
y<-rlaplace(1000, 1, 1)
#Now we find the MLE of z, first with the previous function, then by using the quantile fucntion
Max.logL.num.z <- function(a){
  -sum(dlaplace(z, m = a, s = 1, log = T))
}
optimize(Max.logL.num.z, interval = range(z))

ML.quantile.z<-rep(0,9)
for (i in 1:9){
  ML.quantile.z[i]<- quantile(z, 0.5, type = i) #ML.quantile.z stands for Maximum Likelihood for the sample y using the quantiile function
}

#Then, we also find the MLE of y, first with the previous function, then by using the quantile fucntion
Max.logL.num.y <- function(a){
  -sum(dlaplace(y, m = a, s = 1, log = T))
}

optimize(Max.logL.num.y, interval = range(y))

ML.quantile.y<-rep(0,9)
for (i in 1:9){
  ML.quantile.y[i]<- quantile(y, 0.5, type = i) #ML.quantile.y stands for Maximum Likelihood for the sample y using the quantiile function
}


##(d)
#Let us now study the distribution of the maximum likelihood estimator.
M.20<-rep(0,5000)
for( i in 1:5000){
  z<-rlaplace(20, 1, 1)
  Max.logL.num.z <- function(a){
    -sum(dlaplace(z, m = a, s = 1, log = T))
  }
  a<- optimize(Max.logL.num.z, interval = range(z))
  M.20[i]<-a[[1]]
}

M.1000<-rep(0,5000)
for( i in 1:5000){
  y<-rlaplace(1000, 1, 1)
  Max.logL.num.y <- function(a){
    -sum(dlaplace(y, m = a, s = 1, log = T))
  }
  a<- optimize(Max.logL.num.y, interval = range(y))
  M.1000[i]<-a[[1]]
}

#
data.frame.1000<- as.data.frame(M.1000, stringsAsFactors=FALSE)
p1<-ggplot(data.frame.1000, aes(M.1000))+
  geom_histogram(aes(y = ..density..), color = 'white')+
  stat_function(fun = dnorm, args = list(mean = mean(data.frame.1000$M.1000), sd = sd(data.frame.1000$M.1000)), color = 'red')+
  ggtitle("Histogram: n = 1000")+
  theme(plot.title = element_text(hjust = 0.5)) #Title to the centre

p2<-ggplot(data.frame.1000, aes(sample = M.1000)) +
  stat_qq(distribution = stats::qnorm) +
  stat_qq_line()+
  ggtitle("Normal Q-Q Plot \n n = 1000")+
  theme(plot.title = element_text(hjust = 0.5)) #Title to the centre

#
data.frame.20<- as.data.frame(M.20, stringsAsFactors=FALSE)
p3<-ggplot(data.frame.20, aes(M.20))+
  geom_histogram(aes(y = ..density..), color = 'white')+
  stat_function(fun = dnorm, args = list(mean = mean(data.frame.20$M.20), sd = sd(data.frame.20$M.20)), color = 'red')+
  ggtitle("Histogram: n = 20")+
  theme(plot.title = element_text(hjust = 0.5)) #Title to the centre

p4<-ggplot(data.frame.20, aes(sample = M.20)) +
  stat_qq(distribution = stats::qnorm) +
  stat_qq_line()+
  ggtitle("Normal Q-Q Plot \n n = 20")+
  theme(plot.title = element_text(hjust = 0.5)) #Title to the centre

grid.arrange(p1, p2, p3, p4, nrow = 2)

#Mean and sd of both samples
Mean.sd.M.1000<-c(mean(M.1000), sd(M.1000))
Mean.sd.M.20<-c(mean(M.20), sd(M.20))





