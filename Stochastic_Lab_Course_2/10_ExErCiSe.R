setwd("//ug-uyst-ba-cifs.student.uni-goettingen.de/home/users/t.khwamtabougua/Desktop/StochLab2")
library(Matrix)
library(pls)
library(tidyverse)

#Question(a)
users <- read.csv("users.csv")
likes <- read.csv("likes.csv")
users.likes <- read.csv("users-likes.csv")
#pos.in.users = position of userid (users.likes) in users
pos.in.users <- match(users.likes$userid, users$userid)
#pos.in.likes = position of likeid (users.likes) in like 
pos.in.likes <- match(users.likes$likeid, likes$likeid)
#Now update users.likeqs with new columns
users.likes <- cbind(users.likes, pos.in.users, pos.in.likes)

#building a matrix with 1 at position i; j, if user i made Like j
M <- sparseMatrix(i = users.likes$pos.in.users, j = users.likes$pos.in.likes, x = 1)
N <- M #will be used for question (d)
#Now names of the users-likes Matrix M are set to contain the IDs of the respective users,
#and M's column names areset to contain the names of the respective Likes.
rownames(M) <- users$userid
colnames(M) <- likes$name
#To remove some users, we first find the submatrix such that rowSums(M) >= 80 and colSums(M) >= 150
repeat {
  i <- sum(dim(M))
  M <- M[rowSums(M) >= 80, colSums(M) >= 150]
  if (sum(dim(M)) == i) break
}
#Next, the users deleted from M are removed from users dataframe
users <- users[match(rownames(M),users$userid), ]
#Same, removing Likes that have less than 150 users from likes dataframe
likes <- likes[match(colnames(M),likes$likeid), ]
#convert M into a matrix
users.likes.M <- as.matrix(M)

#Question(b)
#Spliting the data in train and test sets
set.seed(1122)
n <- nrow(users.likes.M)
train_indices <- sample(1:n, size = round((2/3)*n ))
train <- users.likes.M[train_indices, ]
test <- users.likes.M[-train_indices, ]

train.age <- users$age[train_indices]
test.age <- users$age[-train_indices]


#On the training set fit PLS regression models with
#age as a response variable and with up to 50 PLS components
model.1 <- plsr(train.age ~ train, ncomp = 50)

#prediction on the test set
age_predict.1 <- predict(model.1, newdata = test)
#compare it with the true age values from the test set,
#calculating the Pearson correlation coefficient

corr <- 0
for (i in 1:50) {
  corr[i] <- cor(age_predict.1[,1,i], test.age, method = "pearson")
}

#plot
plot(corr)
#d_opt
d_opt <- which(corr == max(corr))
d_opt
#plot
age.predicted <- age_predict.1[ , ,d_opt]
df <- data.frame(age.predicted, test.age)

ggplot(df, aes(x = age.predicted, y =  test.age)) +
  geom_point() +
  geom_abline( aes(intercept=0, slope=1, color = 'Identity \nline'), size = 1) +
  scale_colour_manual(name = " ", values = c("purple")) +
  theme_classic(base_size = 15) + #font size
  expand_limits(x = 0, y = 0) #To force the origin at 0

#Question (c)
a <- tail(sort(model.1$coefficients[ ,1 , d_opt]), 6)
b <- head(sort(model.1$coefficients[ ,1 , d_opt]), 6)

#Question (d) ##########################################repeat the analysis

users <- read.csv("users.csv")
likes <- read.csv("likes.csv")
users.likes <- read.csv("users-likes.csv")
#pos.in.users = position of userid (users.likes) in users
pos.in.users <- match(users.likes$userid, users$userid)
#pos.in.likes = position of likeid (users.likes) in like 
pos.in.likes <- match(users.likes$likeid, likes$likeid)
#Now update users.likeqs with new columns
users.likes <- cbind(users.likes, pos.in.users, pos.in.likes)

#building a matrix with 1 at position i; j, if user i made Like j
N<- sparseMatrix(i = users.likes$pos.in.users, j = users.likes$pos.in.likes, x = 1)
#Now names of the users-likes Matrix N are set to contain the IDs of the respective users,
#and N's column names areset to contain the names of the respective Likes.
rownames(N) <- users$userid
colnames(N) <- likes$name
#To remove some users, we first find the submatrix such that rowSums(N) >= 60 and colSums(N) >= 120
repeat {
  i <- sum(dim(N))
  N <- N[rowSums(N) >= 60, colSums(N) >= 120]
  if (sum(dim(N)) == i) break
}
#Next, the users deleted from N are removed from users dataframe
users <- users[match(rownames(N),users$userid), ]
#Same, removing Likes that have less than 120 users from likes dataframe
likes <- likes[match(colnames(N),likes$likeid), ]
#convert v into a matrix
users.likes.N <- as.matrix(N)

#Question(b)
#Spliting the data in train and test sets
set.seed(1122)
n <- nrow(users.likes.N)
train_indices <- sample(1:n, size = round((2/3)*n ))
train <- users.likes.N[train_indices, ]
test <- users.likes.N[-train_indices, ]

train.age <- users$age[train_indices]
test.age <- users$age[-train_indices]


#On the training set fit PLS regression models with
#age as a response variable and with up to 50 PLS components
model.2 <- plsr(train.age ~ train, ncomp = 50)

#prediction on the test set
age_predict.2 <- predict(model.2, newdata = test)
#compare it with the true age values from the test set,
#calculating the Pearson correlation coefficient

corr <- 0
for (i in 1:50) {
  corr[i] <- cor(age_predict.2[,1,i], test.age, method = "pearson")
}

#plot
plot(corr)
#d_opt
d_opt <- which(corr == max(corr))
d_opt
#plot
age.predicted <- age_predict.2[ , ,d_opt]
df <- data.frame(age.predicted, test.age)

ggplot(df, aes(x = age.predicted, y =  test.age)) +
  geom_point() +
  geom_abline( color = "red") +
  expand_limits(x = 0, y = 0) #To force the origin at 0

#Question (c)
c <- tail(sort(model.2$coefficients[ ,1 , d_opt]), 6)
d <- head(sort(model.2$coefficients[ ,1 , d_opt]), 6)
