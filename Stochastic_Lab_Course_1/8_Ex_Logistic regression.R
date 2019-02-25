setwd("~/Desktop/MScMaths/Summer2018/Stochastic_Lab_Course_1/CodeAndData")
library("dplyr")
library("tidyr")   #used with ggplot2 to plot some variables against many others 
library("ggplot2")

#(a) 
#Read the data into R 
Donors<- read.csv("Donors.csv")
colnames(Donors)<- c("recency", "frequency", "amount", "time", "donation")

#fit a generalised linear model with the binary response
#donation and covariate frequency using the canonical link function.
GLM.1<- glm(Donors$donation ~ Donors$frequency, family = binomial)
summary(GLM.1)

#Fit the same model by replacing the covarite by amount
GLM.2<- glm(Donors$donation ~ Donors$amount, family = binomial)
summary(GLM.2)

#Plot variable frequency against amount
ggplot(Donors, aes(frequency, amount))+
  geom_point()

#(b)
# Fit now the GLM model with the response donation and covariate recency using
#all link functions available in the glm function
GLM.3<- glm(Donors$donation ~ Donors$recency, family = binomial(link = "logit")) #logistic CDF
summary(GLM.3)

GLM.4<- glm(Donors$donation ~ Donors$recency, family = binomial(link = "probit")) #normal CDF
summary(GLM.4)

GLM.5<- glm(Donors$donation ~ Donors$recency, family = binomial(link = "cauchit")) #Cauchy CDF
summary(GLM.5)

GLM.6<- glm(Donors$donation ~ Donors$recency, family = binomial(link = "log")) 
summary(GLM.6)

GLM.7<- glm(Donors$donation ~ Donors$recency, family = binomial(link = "cloglog")) #complementary log-log
summary(GLM.7)

# Choose between all links: it is an open question, but the best way
# is to plot on the same graph, the reponse variable recency against fitted of each model, as follow:
ggplot(Donors)+
  geom_line(aes(x = recency, y = fitted(GLM.3), color = "fitted(GLM.3)"))+
  geom_line(aes(x = recency, y = fitted(GLM.4), color = "fitted(GLM.4)"))+
  geom_line(aes(x = recency, y = fitted(GLM.5), color = "fitted(GLM.5)"))+
  geom_line(aes(x = recency, y = fitted(GLM.6), color = "fitted(GLM.6)"))+
  geom_line(aes(x = recency, y = fitted(GLM.7), color = "fitted(GLM.7)"))+
  theme(axis.title.y = element_blank())+
  scale_colour_manual(values=c("red", "cyan", "purple", "pink", "green"))
#From this plot, we can choose cauchit as our best link function, because for the others, after a year,
#i.e.60 months, the functions tend to zero meaning that ther is no more blood donation after this period 
#which is weird.

#(c)
#Now we would like to build a model that makes the best prediction for the blood donations.
#training and a test set. Sample randomly 374 rows to include into 
#the training set and the rest will be the test set.
set.seed(1122)
Donors$indices<- 1:748 #This column is added to effectivelly return the complement of training_set
                       #in Donors, to obtain test_set, with the function anti_join. 
                       #Actually the function anti_join returns all rows from Donors where there not
                       #matching values with training_set. Unfortunately, some rows share the same

training_set<- Donors[sample(nrow(Donors), 374), ]
# to create the data set test_set, we use the command "anti_join" from the library diplyr
test_set<- anti_join(Donors, training_set, by = c("recency", "frequency", "amount", "time", "donation", "indices"))

#Fit a GLM model with the response donation and canonical link on the training set, choosing
#appropriate covariates

#In order to find the appropriate covariate, we plot each covariate against donation
training_set %>%
  gather(-donation, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = donation)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

#The model an the prediction
GLM.8<- glm(training_set$donation ~ training_set$frequency , family = binomial(link = "logit")) 
summary(GLM.8)

prediction<- predict.glm(GLM.8, newdata = test_set, type="response")

#classification: set the predicted ith value of donation to 0,
#if the corresponding ith predicted probability is less than 0.5 and to 1 otherwise.

for(i in 1:374){
  ifelse(prediction[i] <= 0.5, prediction[i]<- 0, prediction[i]<- 1)
}

#Assessing the goodness of your classification calculating the classification error

CE<- sum(test_set$donation - prediction)/374

####N.B: With the covariate frequency, the CE 0.2085561 has been beaten to 0.197861. We have
#### the same reult with amount. I have to admit that I have used recency as the covariate beforehand
#### and I had a CE of 0.2299465.







