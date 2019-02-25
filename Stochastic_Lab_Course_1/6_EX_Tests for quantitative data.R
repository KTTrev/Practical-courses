setwd("~/Desktop/MScMaths/Summer2018/Stochastic_Lab_Course_1/CodeAndData")
library("dplyr")
library("ggplot2")
library("gridExtra") #to plot multiple plots in one page

##(a)
#Read the data into R
elections<- read.csv("Presidential_race_2016.csv")

#add the variable vote
elections$vote<-ifelse(elections$Percent.Clinton > elections$Percent.Trump, 1, 0)

#Divide variable Diversity.Index into two groups: the one that corresponds
#to 1 in vote and another one that corresponds to 0 in vote. 
#We would like to compare diversity indices in both groups.
#Check if the diversity index in each group is normally distributed or not, with different graphs

elections_diversity_1<- select(filter(elections, vote == 1), Diversity.Index, Percent.Clinton, Percent.Trump)
elections_diversity_0<- select(filter(elections, vote == 0), Diversity.Index, Percent.Clinton, Percent.Trump)

# for elections_diversity_1: histogram and QQ-plot. The red line on the histogram indicates the normal 
#distribution
p1<-ggplot(elections_diversity_1, aes(Diversity.Index))+
  geom_histogram(aes(y = ..density..), color = 'white')+
  stat_function(fun = dnorm, args = list(mean = mean(elections_diversity_1$Diversity.Index), sd = sd(elections_diversity_1$Diversity.Index)), color = 'red')+
  ggtitle("Histogram \n Diversity.Index, vote = 1")+
  theme(plot.title = element_text(hjust = 0.5)) #Title to the centre

p2<-ggplot(elections_diversity_1, aes(sample = Diversity.Index)) +
  stat_qq(distribution = stats::qnorm) +
  stat_qq_line()+
  ggtitle("Normal Q-Q Plot \n Diversity.Index, vote = 1")+
  theme(plot.title = element_text(hjust = 0.5)) #Title to the centre

# for elections_diversity_0: histogram and QQ-plot. The red line on the higram indicates the normal 
#distribution
p3<-ggplot(elections_diversity_0, aes(Diversity.Index))+
  geom_histogram(aes(y = ..density..), color = 'white')+
  stat_function(fun = dnorm, args = list(mean = mean(elections_diversity_0$Diversity.Index), sd = sd(elections_diversity_0$Diversity.Index)), color = 'red')+
  ggtitle("Histogram \n Diversity.Index, vote = 0")+
  theme(plot.title = element_text(hjust = 0.5)) #Title to the centre

p4<-ggplot(elections_diversity_0, aes(sample = Diversity.Index)) +
  stat_qq(distribution = stats::qnorm) +
  stat_qq_line()+
  ggtitle("Normal Q-Q Plot \n Diversity.Index, vote = 0")+
  theme(plot.title = element_text(hjust = 0.5)) #Title to the centre


grid.arrange(p1, p2, p3, p4, nrow = 2)

#(b)
#testing the independence between the two groups
chisq.test(c(sum(elections_diversity_1$Diversity.Index), sum(elections_diversity_0$Diversity.Index)))

#t-test: According to question (a), since no normality has been assumed,
#one rather tests
#H0 : median(elections_diversity_1) = median(elections_diversity_0) vs
#H1 : median(elections_diversity_1) != median(elections_diversity_0)
t.test(elections_diversity_1$Diversity.Index, elections_diversity_0$Diversity.Index)

#Wilcoxon test for the same hypothesis with the same significance level.

wilcox.test(elections_diversity_1$Diversity.Index, elections_diversity_0$Diversity.Index, mu = 0, conf.int = T)


#(c)
r1<-ggplot(elections, aes(Diversity.Index))+
  geom_histogram(aes(y = ..density..), color = 'white')+
  stat_function(fun = dnorm, args = list(mean = mean(elections$Diversity.Index), sd = sd(elections$Diversity.Index)), color = 'red')

r2<-ggplot(elections, aes(sample = Diversity.Index)) +
  stat_qq(distribution = stats::qnorm) +
  stat_qq_line()

grid.arrange(r1, r2, nrow = 2)

#t-test
t.test(elections$Diversity.Index, elections$Diversity.Index, mu = 50)
#wilcox-test
wilcox.test(elections$Diversity.Index, elections$Diversity.Index, mu = 50, conf.int = T)



