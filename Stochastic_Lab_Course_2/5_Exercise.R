setwd("~/Stochastic_Lab/Stochastic_Lab_Course_2")
library("tidyverse")
library("survival")
library(ggfortify) #for the function autoplot for objects of type survfit


thor <- read.delim("Thoracic.txt", sep = " ")

thor <- thor %>%
  dplyr::select(c(T.2, X60, F.7))

colnames(thor) <- c("PRE30", "AGE", "Risk1Y")

#Question(a)
#Computing nonparametric estimators of the survivor function: Kaplan-Meier
km <- with(thor, Surv(AGE, Risk1Y))
#Computing nonparametric estimators of the survivor function: Flemming-Harrington
fh 

#Question(b)
#Proportion

#Kaplan-Meier estimators for each group

#plot on one plot together with the corresponding confidence bands
km_smoker_fit <- survfit(Surv(AGE, Risk1Y) ~ PRE30, data = thor)

autoplot(km_smoker_fit) +
  ggtitle("Smokers based Survival") +
  labs(x = "Time", y = "Survival Probability") +
  guides(fill=FALSE) +
  labs(colour = "Smoker")

