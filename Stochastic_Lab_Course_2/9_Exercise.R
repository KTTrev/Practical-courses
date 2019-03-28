setwd("~/Practical-courses/Stochastic_Lab_Course_2")
library(JoSAE)
library(nlme)

data(landsat)

#Question(a)
#For the linear models on each county, we first create group objects
group.soy <- groupedData(HASoybeans ~ PixelsSoybeans | CountyName, data = landsat)
group.corn <- groupedData(HACorn  ~ PixelsCorn | CountyName, data = landsat)
#linear models
lm.model.soy <- lmList(group.soy)
lm.model.corn <- lmList(group.corn)

#Question(b)
fm1 <- lme(HACorn ~ HASoybeans, random=~1|CountyName, data = landsat)








