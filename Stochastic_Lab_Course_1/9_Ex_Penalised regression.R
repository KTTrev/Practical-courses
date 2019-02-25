setwd("~/Desktop/MScMaths/Summer2018/Stochastic_Lab_Course_1/CodeAndData")
library("dplyr")
library("ggplot2")
library("glmnet")

#(a)
#Load the dataset into R and create a new dataset containing only those players for which all data is available.
dat<-read.csv("Hitters.csv", sep = " ")
hitters<- na.omit(dat) # to select all rows with non NA values

#(b) 
#Find the condition number 
df<- hitters[,-19]
x<-data.matrix(df) # obtain a matrix from the dataframe df
y<- hitters$Salary

kappa(t(x) %*% x)

kappa(t(scale((x))) %*% scale(x)) #the command scale is used to center the columns of the matrix t(x) %*% x
                         #the condion number here is less than the previous one, thus it helps to standardise.

#(c) 
#Fit a standard linear model (no regularisation) 
model_1<- lm( Salary ~ . , data = hitters)
coef(model_1)

#ridge regression with lamda = 70
model_ridge<- glmnet(x, y, alpha = 0, lambda = 70)
coef(model_ridge)

#(d)
#spliting the data: a diiferent and better method is beeing used here compared to what has been done for
#previous exercises
set.seed(1122)
ind<- sample(2, nrow(hitters), replace = T, prob = c(0.5, 0.5) )
training_set<-hitters[ind==1,]
test_set<- hitters[ind==2,]

#(e)
#a function that takes lambda as argument, fits a ridge regression on the training
#sets and calculates the squared prediction error on the test set
df1<- training_set[,-19]
df2<- test_set[,-19]
n<-data.matrix(df1)
m<- training_set$Salary
z<- data.matrix(df2)
#N.B: A for loop is used in the following function because we would like also to use lambda as
#vector. it is also possible avoid a for loop in the function, but when we will be using function
#with a vector, will use vectorise(MSE_find).
MSE_find<- function(lambda){
  MSPE<- numeric(length(lambda))
  for(i in 1:length(lambda)){
      model_ridge<- glmnet(n, m, alpha = 0, lambda = lambda[i])
      prediction<- predict(model_ridge, newx = z)
      MSPE[i]<- mean((test_set$Salary - prediction)**2)
    }
  return(MSPE)
}

#Run this function on a logarithmic grid (e.g., 10^seq(from = 10, to = -2, length = 100))
lambda <-10^seq(from = 10, to = -2, length = 100)
MSE_find(lambda)

#Plot the results against log(lambda) and graphically find the value lambda_opt that minimises the squared
#prediction error: graphically, we can say that lambda_opt is 10^2
#N.B: I have chosen to plot lambda against MSE_find(lambda), and then add a logarithmic scale on the x axis. 
#The result remains the same.
a<- as.data.frame(lambda)
b<- as.data.frame(MSE_find(lambda))
c<- cbind(a,b)
ggplot(c, aes(lambda,MSE_find(lambda)))+
  geom_point()+
  scale_x_log10()

#(f) 
#Fit a ridge regression with lambda_opt on all the data, and interpret some of the coefficients.
lambda_opt<- 100
model_ridge_opt<- glmnet(x, y, alpha = 0, lambda = lambda_opt)
#Interpret some of the coefficients.
coef(model_ridge_opt)

#(g) 
#Repeat (e), (f) for lasso

#(e')

df1<- training_set[,-19]
df2<- test_set[,-19]
n<-data.matrix(df1)
m<- training_set$Salary
z<- data.matrix(df2)


MSE_find<- function(lambda){
  MSPE<- numeric(length(lambda))
  for(i in 1:length(lambda)){
    model_lasso<- glmnet(n, m, alpha = 1, lambda = lambda[i])
    prediction<- predict(model_ridge, newx = z)
    MSPE[i]<- mean((test_set$Salary - prediction)**2)
  }
  return(MSPE)
}

a<- as.data.frame(lambda)
b<- as.data.frame(MSE_find(lambda))
c<- cbind(a,b)
ggplot(c, aes(lambda,MSE_find(lambda)))+
  geom_point()+
  scale_x_log10()

#(f')
lambda_opt<- 10**(1.4)
model_lasso_opt<- glmnet(x, y, alpha = 1, lambda = lambda_opt)
#Interpret some of the coefficients.
coef(model_lasso_opt)










