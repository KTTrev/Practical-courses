setwd("~/Stochastic_Lab/Stochastic_Lab_Course_2")
library("tidyverse") 



#Question (a)
#Implement kernel density estimation in a function that depends on the sample, bandwidth and a kernel
estim.density <- function(s, h, K){
  density(x = s, bw = h, kernel = K)
}
#loading the data
a <- read.csv("StudentsPerformance.csv")
students <- a %>%
  dplyr::select(c(test.preparation.course, math.score, reading.score, writing.score))


ggplot(students, aes(x = math.score)) + 
  stat_density(aes(fill = "nrd0"), kernel = "epanechnikov", bw = "nrd0") + 
  stat_density(aes(fill = "nrd"), kernel = "epanechnikov", bw = "nrd") +
  stat_density(aes(fill = "SJ"), kernel = "epanechnikov", bw = "SJ")
  




plot( estim.density(s = students$math.score, h = "nrd", K = "epanechnikov"))


