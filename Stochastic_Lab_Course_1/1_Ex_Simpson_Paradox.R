setwd("~/Desktop/MScMaths/Summer2018/Stochastic_Lab_Course_1/CodeAndData")
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales) # for percentage scales
#Exercise 1: Simpson's paradox

#data frame that contains the data
#This can be done by creating a matrix, a list, and then using the function as.data.frame
M<- cbind(c(497, 221, 212, 503, 1841), c(62, 12, 20, 102, 305))
N<- cbind(c(694, 4840, 383, 320, 201), c(117, 415, 65, 129, 61))
#renaming rows and columns of M and N
rownames(M)<- c('Los Angeles', 'Phoenix', 'San Diego', 'San Francisco', 'Seattle')
colnames(M)<- c('On time', 'Delayed')
rownames(N)<- c('Los Angeles', 'Phoenix', 'San Diego', 'San Francisco', 'Seattle')
colnames(N)<- c('On time', 'Delayed')
#creating list
L<- list("Alaska Airlines" = M, "America West" = N)
#Finally the dataframe
df <- data.frame(L)

#
cities<- rep(c('Los Angeles', 'Phoenix', 'San Diego', 'San Francisco', 'Seattle'), 2)
airlines<- c(rep('Alaska Airlines', 5), rep('America West', 5))

k<- list("cities" = cities, "airlines" = airlines)
Df<- data.frame(k)

Df$delayed.Proportions<- c((df[1,2])/(df[1,2]+df[1,1]), 
                           (df[2,2])/(df[2,2]+df[2,1]),
                           (df[3,2])/(df[3,2]+df[3,1]),
                           (df[4,2])/(df[4,2]+df[4,1]), 
                           (df[5,2])/(df[5,2]+df[5,1]),
                           (df[1,4])/(df[1,4]+df[1,3]),
                           (df[2,4])/(df[2,4]+df[2,3]),
                           (df[3,4])/(df[3,4]+df[3,3]),
                           (df[4,4])/(df[4,4]+df[4,3]),
                           (df[5,4])/(df[5,4]+df[5,3]))

total.delayed.Proportions.Alaska<- c( sum(df$Alaska.Airlines.Delayed)/(sum(df$Alaska.Airlines.Delayed) + sum(df$Alaska.Airlines.On.time)))
total.delayed.Proportions.America<- c( sum(df$America.West.Delayed)/(sum(df$America.West.Delayed) + sum(df$America.West.On.time)))

Total.Delayed<-c(total.delayed.Proportions.Alaska, total.delayed.Proportions.America)
Df.Total.delayed<- data.frame(Total.Delayed)
Df.Total.delayed$airlines<- c("Alaska Airlines", "America West")

####barplots

ggplot(data=Df, aes(x=airlines, y=delayed.Proportions, fill = airlines)) +
  geom_bar(stat="identity", color = 'black') +
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x =element_blank(), axis.title.x=element_blank()) +
  theme(legend.position = "bottom") +
  facet_grid(~cities)

#all together
ggplot(data=Df.Total.delayed, aes(x=airlines, y=Total.Delayed, fill = airlines)) +
  geom_bar(stat="identity", color = 'black') +
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x =element_blank(), axis.title.x=element_blank()) +
  theme(legend.position = "bottom")


####Pie charts

ggplot(data=Df, aes(x="", y=delayed.Proportions, fill = airlines)) +
  geom_bar(stat="identity", position = position_fill(), color = 'black') +
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x =element_blank(), axis.title.x=element_blank()) +
  theme(legend.position = "bottom") +
  coord_polar("y") +
  facet_grid(~cities)


#all together
ggplot(data=Df.Total.delayed, aes(x="", y=Total.Delayed, fill = airlines)) +
  geom_bar(stat="identity", position = position_fill(), color = 'black') +
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x =element_blank(), axis.title.x=element_blank(), legend.position = "bottom") +
  coord_polar("y", start=0)


