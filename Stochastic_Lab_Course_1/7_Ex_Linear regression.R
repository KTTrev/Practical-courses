setwd("~/Desktop/MScMaths/Summer2018/Stochastic_Lab_Course_1/CodeAndData")
library("dplyr")  
library("tidyr")   #used with ggplot2 to plot some variables against many others 
library("ggplot2")
library("gridExtra") #to plot multiple plots in one page

MyData<- read.csv("House_prices.csv")
#the folowing dataframe, house, contains the variables needed for this exercise
house<- select(MyData, c(price, bedrooms, bathrooms, sqft_living, floors, view, condition, grade, yr_built))

#(a)
# Estimate a linear model with the response variable price and all remaining variables as covariates.
model1<- lm(house$price ~ house$bedrooms + house$bathrooms + house$sqft_living + house$floors + house$view + house$condition + house$grade + house$yr_built)
summary(model1)

#Perform the residual analysis to validate model1.
par(mfrow = c(2,2))
plot(model1)

#(b)
#Produce a histogram and a QQ-plot of the response variable price
p1<-ggplot(house, aes(price))+
  geom_histogram(color = 'white')+
  ggtitle("Histogram: price")+
  theme(plot.title = element_text(hjust = 0.5)) #Title to the centre

p2<-ggplot(house, aes(sample = price)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle("Q-Q Plot: price")+
  theme(plot.title = element_text(hjust = 0.5)) #Title to the centre

# histogram and a QQ-plot of the response variable log(price)

house$logprice<-log(house$price) #adding the values of log(price) to "house" data set

p3<-ggplot(house, aes(logprice))+
  geom_histogram(color = 'white')+
  ggtitle("Histogram: log(price)")+
  theme(plot.title = element_text(hjust = 0.5)) #Title to the centre

p4<-ggplot(house, aes(sample = logprice)) +
  stat_qq() +
  stat_qq_line()+
  ggtitle("Q-Q Plot: log(price)")+
  theme(plot.title = element_text(hjust = 0.5)) #Title to the centre

grid.arrange(p1, p2, p3, p4, nrow = 2)

#linear model with the response variable log(price)
model2<- lm(house$logprice ~ house$bedrooms + house$bathrooms + house$sqft_living + house$floors + house$view + house$condition + house$grade + house$yr_built)
summary(model2)

# residual analysis for model2.
plot(model2)

#(c)
#Plot each covariate against log(price)
house %>%
  gather(-logprice, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = logprice)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

#Add to the model from (b), i.e. model1, squared terms for yr_built and sqft_living
house$sqr_yr_built<-(house$yr_built)**2
house$sqr_sqft_living<-(house$sqft_living)**2
model3<- lm(house$price ~ house$bedrooms + house$bathrooms + house$sqft_living + house$floors + house$view + house$condition + house$grade + house$yr_built + house$sqft_living +house$sqr_sqft_living)
summary(model3)

#(d)
#we would like to compare how well models from (b) and (c) make prediction
set.seed(1122)
training_set<- house[sample(nrow(house), 10806), ]
# to create the data set test_set, we use the command "anti_join" from the library diplyr
test_set<- anti_join(house, training_set, by = c("price", "bedrooms", "bathrooms", "sqft_living", "floors", "view", "condition", "grade", "yr_built", "logprice"))

#prediction
model2<- lm(logprice ~ bedrooms + bathrooms + sqft_living + floors + view + condition + grade + yr_built, data = training_set)
model3<- lm(logprice ~ bedrooms + bathrooms + sqft_living + floors + view + condition + grade + yr_built  + sqr_yr_built + sqr_sqft_living, data = training_set)

prediction_a<- predict(model2, newdata = test_set)
prediction_b<- predict(model3, newdata = test_set)

#mean squared difference (MSD) between predicted values and values of log(price) 
#from the test set for each model
mean((test_set$logprice - prediction_a)**2)
mean((test_set$logprice - prediction_b)**2)

########Trying to extend the model to improve the prediction: you can do that by adding cubes, squares,
########or what ever, to any covariates and compare the results.

house$sqr_yr_built<-(house$yr_built)**10
house$sqr_sqft_living<-(house$sqft_living)**2
model4<- lm(house$price ~ house$bedrooms + house$bathrooms + house$sqft_living + house$floors + house$view + house$condition + house$grade + house$yr_built + house$sqft_living +house$sqr_sqft_living)

training_set<- house[sample(nrow(house), 10806), ]
test_set<- anti_join(house, training_set, by = c("price", "bedrooms", "bathrooms", "sqft_living", "floors", "view", "condition", "grade", "yr_built", "logprice"))

#prediction
model4<- lm(logprice ~ bedrooms + bathrooms + sqft_living + floors + view + condition + grade + yr_built  + sqr_yr_built + sqr_sqft_living, data = training_set)

prediction_4<- predict(model3, newdata = test_set)

#mean squared difference (MSD) between predicted values and values of log(price) 
#from the test set for each model
mean((test_set$logprice - prediction_4)**2)



