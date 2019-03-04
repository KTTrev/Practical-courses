setwd("~/Stochastic_Lab/Stochastic_Lab_Course_2")
library("tidyverse")
library("haven")

##Question(a)

children <- read_dta("childrenfinal.dta")
#remove all variables that start with “s“, “v” and “m”, followed by a number
children1<- children %>% 
  select(-matches("^[svm][0-9]"))



