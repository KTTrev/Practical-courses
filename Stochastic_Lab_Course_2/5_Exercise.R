setwd("~/Practical-courses/Stochastic_Lab_Course_2")
library("tidyverse")
library("survival")
library(ggfortify) #for the function autoplot for objects of type survfit
library(tikzDevice)

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
tikz('Ex5plot1.tex',width=3.5, height=3)
plot(km, col = "blue", ylab ="S(t)", xlab="t")
lines(fh, col='red')
legend( par("usr")[2], par("usr")[4], yjust=3, xjust=1.4,
        c("Kaplan-Meier", "Fleming-Harrington"),
        lwd=c(1,1), lty=c(1,1),
        col=c("blue", 'red'))
                 #title(main="nonparametric estimators of the survivor function")
dev.off()

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
tikz('Ex5plot2.tex',width=3.5, height=3)
plot(km, col = "blue", ylab ="S(t)", xlab="t")
lines(fit.dat.exp, col = "red")
lines(fit.dat.wei, col = "green")
legend("bottomleft", inset=.1,
     legend =  c("Kaplan-Meier", "Exponential", "Weibull"),
      col =  c("blue", "red", "green"),
     lwd=c(1,1), lty=c(1,1),
     box.lty=0)
             #title(main="nonparametric & parametric estimators \n of the survivor function")
dev.off()

#Use appropriate graphical tools to check if the Weibull model is adequate for the data

df0 <- data.frame(A=log(km$time) ,B=log(-log(km$surv)))

tikz('Ex5plot3.tex',width=3.5, height=3)
ggplot(df0) +
  geom_point(aes(x=A, y=B), color = 'navy') +
  geom_abline(aes(intercept = alpha*log(lam_hat_wei), slope = alpha, color = 'D0'))+
  scale_colour_manual(name = " ", values = c("red")) +
  xlab("log(t)") +
  ylab("log[-log(s(t)]") +
  theme_classic(base_size = 10) + 
  theme(legend.position = c(0.2, 0.95),legend.justification = c("right", "top"), legend.key = element_rect(fill = "white", colour = "gray19"))
dev.off()

#Question(b)
#How large is the proportion of smokers in the sample
sum(thor$PRE30)/nrow(thor)

#Kaplan-Meier estimators for each group
km_smoker_fit <- survfit(Surv(AGE, Risk1Y) ~ PRE30, data = thor)
#plot on one plot together with the corresponding confidence bands

autoplot(km_smoker_fit) +
  #ggtitle("Smokers based Survival") +
  labs(x = "t", y = "S(t)") +
  guides(fill=FALSE) +
  theme_classic(base_size = 20) +
  theme(legend.position = c(0.4, 0.70),legend.justification = c("right", "top"), legend.key = element_rect(fill = "white", colour = "gray19"))+
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
tikz('Ex5plot5.tex',width=3.5, height=3)
plot(fit.dat.wei.smokers, col = "blue", type = "l", ylab ="S(t)", xlab="t")
lines(fit.dat.wei.nonsmokers, col='red')
lines(km_smokers, col = "green")
lines(km_nonsmokers, col = "black")
legend("bottomleft", inset=.0001,
       legend =  c("Weibull smokers", "Weibull nonsmokers", "km smokers", "km nonsmokers"),
       col =  c("blue", "red", "green", "black"),
       lwd=c(1,1), lty=c(1,1),
       box.lty=0)
     #title(main="nonparametric & parametric estimators \n of the survivor function by group")
dev.off()

#"Checking Weibull if weibull is appropriate \n for smokers' group"
df1 <- data.frame(A=log(km_smokers$time) ,B=log(-log(km_smokers$surv)))

tikz('Ex5plot6.tex',width=3.5, height=3)
ggplot(df1) +
  geom_point(aes(x=A, y=B), color = 'navy') +
  geom_abline(aes(intercept = alpha_smokers*log(lam_hat_wei_smokers), slope = alpha_smokers, color = 'D1'))+
  scale_colour_manual(name = " ", values = c("red")) +
  xlab("log(t)") +
  ylab("log[-log(s(t)]") +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.2, 0.95),legend.justification = c("right", "top"), legend.key = element_rect(fill = "white", colour = "gray19"))
dev.off()
#Checking Weibull if weibull is appropriate \n for nonsmokers' group
df2 <- data.frame(A=log(km_nonsmokers$time) ,B=log(-log(km_nonsmokers$surv)))

tikz('Ex5plot7.tex',width=3.5, height=3)
ggplot(df2) +
  geom_point(aes(x=A, y=B), color = 'navy') +
  geom_abline(aes(intercept = alpha_nonsmokers*log(lam_hat_wei_nonsmokers), slope = alpha_nonsmokers, color = 'D2'))+
  scale_colour_manual(name = " ", values = c("red")) +
  xlab("log(t)") +
  ylab("log[-log(s(t)]") +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.2, 0.95),legend.justification = c("right", "top"), legend.key = element_rect(fill = "white", colour = "gray19"))
dev.off()
