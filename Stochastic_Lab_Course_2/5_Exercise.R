setwd("~/Practical-courses/Stochastic_Lab_Course_2")
library("tidyverse")
library("survival")
library(ggfortify) #for the function autoplot for objects of type survfit


thor <- read.delim("Thoracic.txt", sep = " ")

thor <- thor %>%
  dplyr::select(c(T.2, X60, F.7))

colnames(thor) <- c("PRE30", "AGE", "Risk1Y")

#Question(a)
#Computing nonparametric estimators of the survivor function: Kaplan-Meier
km <- survfit(Surv(AGE, Risk1Y) ~ 1, data = thor, type = "kaplan-meier")
#Computing nonparametric estimators of the survivor function: Flemming-Harrington
fh <- survfit(Surv(AGE, Risk1Y) ~ 1, data = thor, type = "fleming-harrington")
#plot of km and fh
plot(km, col = "blue")
lines(fh, col='red')
legend( par("usr")[2], par("usr")[4], yjust=3, xjust=1.6,
        c("Kaplan-Meier", "Fleming-Harrington"),
        lwd=c(1,1), lty=c(1,1),
        col=c("blue", 'red'))
title(main="nonparametric estimators of the survivor function")

#fit the exponential model to the data
exp_fit <- survreg(Surv(AGE, Risk1Y) ~ 1, data = thor, dist="exponential")
lam_hat_exp <- exp(-exp_fit$coefficients)
fit.dat.exp <- exp(-lam_hat_exp*c(0:90))
#fit the Weibull model to the data
wei_fit <- survreg(Surv(AGE, Risk1Y) ~ 1, data = thor, dist="weibull")
lam_hat_wei <- exp(-wei_fit$coefficients)
alpha <- 1/wei_fit$scale
fit.dat.wei <- exp(-(lam_hat_wei*c(0:90))^alpha)
#plot
plot(km, col = "blue")
lines(fh, col='red')
lines(fit.dat.exp, col = "green")
lines(fit.dat.wei, col = "black")
legend("bottomleft", inset=.02,
     legend =  c("Kaplan-Meier", "Fleming-Harrington", "Exponential", "Weibull"),
      col =  c("blue", "red", "green", "black"),
     lwd=c(1,1), lty=c(1,1),
     box.lty=0)
title(main="nonparametric & parametric estimators \n of the survivor function")

#Use appropriate graphical tools to check if the Weibull model is adequate for the data
plot(log(km$time) ,log(-log(km$surv)), col = "red")
abline(a = alpha*log(lam_hat_wei), b = alpha, col = "blue" )

#Question(b)
#How large is the proportion of smokers in the sample
sum(thor$PRE30)/nrow(thor)

#Kaplan-Meier estimators for each group
km_smoker_fit <- survfit(Surv(AGE, Risk1Y) ~ PRE30, data = thor)
#plot on one plot together with the corresponding confidence bands
autoplot(km_smoker_fit) +
  ggtitle("Smokers based Survival") +
  labs(x = "Time", y = "Survival Probability") +
  guides(fill=FALSE) +
  labs(colour = "Smoker")
#Test formally if the survival time depends on being a smoker using the log-rank test
survdiff(Surv(AGE, Risk1Y) ~ PRE30, data = thor, rho=0)

#Fit the Weibull model to both groups: we do it by spliting the data into smokers and non-smokers

thor.smokers <- thor %>%
  filter(PRE30 == "TRUE")

thor.nonsmokers <- thor %>%
  filter(PRE30 == "FALSE")

#fitting the Weibull model to smokers' group
wei_smokers_fit <- survreg(Surv(AGE, Risk1Y) ~ 1, data = thor.smokers, dist="weibull")
lam_hat_wei_smokers <- exp(-wei_smokers_fit$coefficients)
alpha_smokers <- 1/wei_smokers_fit$scale
fit.dat.wei.smokers <- exp(-(lam_hat_wei_smokers*c(0:90))^alpha_smokers)

#fitting the Weibull model to nonsmokers' group
wei_nonsmokers_fit <- survreg(Surv(AGE, Risk1Y) ~ 1, data = thor.nonsmokers, dist="weibull")
lam_hat_wei_nonsmokers <- exp(-wei_nonsmokers_fit$coefficients)
alpha_nonsmokers <- 1/wei_nonsmokers_fit$scale
fit.dat.wei.nonsmokers <- exp(-(lam_hat_wei_nonsmokers*c(0:90))^alpha_nonsmokers)

#Computing nonparametric estimators of the survivor function: Kaplan-Meier
km_smokers <- survfit(Surv(AGE, Risk1Y) ~ PRE30, data = thor.smokers, type = "kaplan-meier")
km_nonsmokers <- survfit(Surv(AGE, Risk1Y) ~ PRE30, data = thor.nonsmokers, type = "kaplan-meier")

#plot
plot(fit.dat.wei.smokers, col = "blue", type = "l")
lines(fit.dat.wei.nonsmokers, col='red')
lines(km_smokers, col = "green")
lines(km_nonsmokers, col = "black")
legend("bottomleft", inset=.0001,
       legend =  c("Weibull smokers", "Weibull nonsmokers", "km smokers", "km nonsmokers"),
       col =  c("blue", "red", "green", "black"),
       lwd=c(1,1), lty=c(1,1),
       box.lty=0)
title(main="nonparametric & parametric estimators \n of the survivor function by group")

#Is the Weibull model an appropriate assumption in both groups
plot(log(km_smokers$time) ,log(-log(km_smokers$surv)), col = "red")
abline(a = alpha_smokers*log(lam_hat_wei_smokers), b = alpha_smokers, col = "blue" )
title(main="Checking Weibull if weibull is appropriate \n for smokers' group")

plot(log(km_nonsmokers$time) ,log(-log(km_nonsmokers$surv)), col = "red")
abline(a = alpha_nonsmokers*log(lam_hat_wei_nonsmokers), b = alpha_nonsmokers, col = "blue" )
title(main="Checking Weibull if weibull is appropriate \n for nonsmokers' group")






