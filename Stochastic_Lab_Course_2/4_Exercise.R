setwd("~/Practical-courses/Stochastic_Lab_Course_2")
library("tidyverse")
library(tikzDevice) 
student <- read.csv("student-mat.csv")

#Question(a)
#First we need to identify the distribution of each of G1, G2, G3
#We use Q-Q plots to do so. In order to use facet_wrap to plot into one panel, the dataset is refined as follow:

df1 <- data.frame(student$G1, rep('G1', nrow(student)))
colnames(df1) <- c("grades", "types")
df2 <- data.frame(student$G2, rep('G2', nrow(student)))
colnames(df2) <- c("grades", "types")
df3 <- data.frame(student$G3, rep('G3', nrow(student)))
colnames(df3) <- c("grades", "types")
df0 <- rbind(df1, df2, df3)

#Normal distributed?
plot1<- ggplot(data = df0, mapping = aes(sample = grades)) + 
  geom_density(aes(x = grades), fill = "chartreuse") +
  facet_wrap(. ~types)

plot2<- ggplot(data = df0, mapping = aes(sample = grades)) + 
  stat_qq(distribution = stats::qnorm, dparams = list(mean = mean(df0$grades), sd = sd(df0$grades))) +
  geom_abline(alpha = 0.25) +
  facet_wrap(. ~types)

#Poisson distributed?
plot3<- ggplot(data = df0, mapping = aes(sample = grades)) + 
  stat_qq(distribution = stats::qpois, dparams = list(lambda = mean(df0$grades))) +
  geom_abline(alpha = 0.25) +
  facet_wrap(. ~types)

# Are there signs for over-dispersion or any other anomalies in the
#distributions of any of G1, G2, G3? Just check the plots and use mean and var.

#For the next question, we'll need the Anscombe residuals. R doesn't have a formula to generate 
#these residuals, then we implement the anscombe residual formua for Poisson distribution
anscombe.residuals <- function(y, mu){
  (3*(y**(2/3)-mu**(2/3)))/2*(mu**(1/6))
}

#Question(b)
model.1 <- glm(formula = G1 ~. -G2 -G3, family = poisson, data = student) 
summary(model.1)

#Pearson residuals for model.1
pearson.resid <- residuals(model.1, "pearson")

df <- data.frame(pearson.resid)
ggplot(data = df, mapping = aes(sample = pearson.resid)) + 
  stat_qq(distribution = stats::qnorm, dparams = list(mean = mean(df$pearson.resid), sd = sd(df$pearson.resid))) +
  geom_abline(alpha = 0.25) +
  ggtitle("model 1 Pearson residuals Q-Q plot \n with normal theoretical distribution") +
  theme(plot.title = element_text(hjust = 0.5)) #to center the title on the plot

#Anscombe residuals for model.1
ans.resid <- anscombe.residuals(student$G1, model.1$fitted.values)

dt <- data.frame(ans.resid)
ggplot(data = dt, mapping = aes(sample = ans.resid)) + 
  stat_qq(distribution = stats::qnorm, dparams = list(mean = mean(dt$ans.resid), sd = sd(dt$ans.resid))) +
  geom_abline(alpha = 0.25) +
  ggtitle("model 2 Anscombe residuals Q-Q plot \n with normal theoretical distribution") +
  theme(plot.title = element_text(hjust = 0.5)) #to center the title on the plot

#residual analysis
plot(model.1)



#Question (c)
model.2 <- glm(formula = G1 ~ sex + Fedu + studytime + failures + schoolsup + famsup + goout , family = poisson, data = student) 
summary(model.2)

#Analysis of deviance
anova(model.2, model.1, test = "Chisq")

#model 3 and comparison with model 2
model.3 <- glm(formula = G1 ~ sex + Fedu + studytime + failures + schoolsup + famsup + Walc , family = poisson, data = student) 
summary(model.3)
plot(model.3)

#exporting plots
tikz('Ex4plot1.tex',width=3.5, height=3, sanitize=TRUE)
plot1
dev.off()

tikz('Ex4plot2.tex',width=3.5, height=3, sanitize=TRUE)
plot2
dev.off()

tikz('Ex4plot3.tex',width=3.5, height=3, sanitize=TRUE)
plot3
dev.off()



