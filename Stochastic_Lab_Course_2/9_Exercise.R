setwd("~/Practical-courses/Stochastic_Lab_Course_2")
library(JoSAE)
library(nlme)
library(Matrix)
library(tidyverse)
library(mapdata)
library(maps)
library(maptools)
library(rgdal)
library(ggrepel)


#Question(a)
#Loading the data
data(landsat)
areasat <- dplyr::select(landsat, -outlier)

# Create a groupedData object
group.corn <- groupedData(HACorn ~ PixelsCorn | CountyName, data = areasat)
group.soy <- groupedData(HASoybeans ~ PixelsSoybeans | CountyName, data = areasat)

# Make individual lm fits according to the grouped data
lm.model.soy <- lmList(group.soy)
lm.model.soy
lm.model.corn <- lmList(group.corn)
lm.model.corn

#Question(b)

# Fit a linear mixed model for both crops such that segments share the same countywide random effect
lme.model.corn <- lme(HACorn ~ PixelsCorn, data = group.corn, random = ~ 1)
lme.model.corn
beta.corn <- lme.model.corn$coefficients$fixed

lme.model.soy <- lme(HASoybeans ~ PixelsSoybeans, data = group.soy, random = ~ 1)
lme.model.soy
beta.soy <- lme.model.soy$coefficients$fixed

#Question(c)
#Mean of the population for explanatory variables
pop.mean.corn <- unique(areasat$MeanPixelsCorn)
pop.mean.soy <- unique(areasat$MeanPixelsSoybeans)

#Mean over the observed segments
segments.mean <- aggregate(areasat[3:6], by = list(areasat$CountyName), mean)

#Number of observations in each county
number.observed.county <- plyr::count(areasat, "CountyName")
number.observed <- number.observed.county[, 2]
county.names <- number.observed.county[, 1]

#Model for corn and soy
var.estim.corn <- VarCorr(lme.model.corn) #estimated variance for corn
var.estim.soy <- VarCorr(lme.model.soy) #estimated variance for soy
sigma.eps.corn <- as.numeric(var.estim.corn[2])
sigma.eps.soy <- as.numeric(var.estim.soy[2])
sigma.rand.corn <- as.numeric(var.estim.corn[1])
sigma.rand.soy <- as.numeric(var.estim.soy[1])

#Calculate covariance matrix V.hat of beta.hat
M.corn <- list()
M.soy <- list()
for (i in 1:12){
  M.corn[[i]] = matrix(sigma.rand.corn, number.observed[i], number.observed[i])
  M.soy[[i]] = matrix(sigma.rand.soy, number.observed[i], number.observed[i])
}

I.n.corn <- diag(x = sigma.eps.corn, sum(number.observed), sum(number.observed))
I.n.soy <- diag(x = sigma.eps.soy, sum(number.observed), sum(number.observed))
block.M.corn <- bdiag(M.corn)
block.M.soy <- bdiag(M.soy)
V.corn <- I.n.corn + block.M.corn
V.soy <- I.n.soy + block.M.soy

a.corn <- cbind(1, areasat$PixelsCorn)
a.soy <- cbind(1, areasat$PixelsSoybeans)
V.hat.corn <- solve(t(a.corn) %*% solve(V.corn) %*% a.corn)
V.hat.soy <- solve(t(a.soy) %*% solve(V.soy) %*% a.soy)

gamma.corn <- sigma.rand.corn/(sigma.rand.corn + sigma.eps.corn/number.observed)
gamma.soy <- sigma.rand.soy/(sigma.rand.soy + sigma.eps.soy/number.observed)

#Calculate the different predictors for mu

reg.pred.corn <- cbind(1, pop.mean.corn) %*% beta.corn #Regression predictor for corn
reg.pred.soy <- cbind(1, pop.mean.soy) %*% beta.soy  #Regression predictor for soy

#Adjusted Survey Predictor for both groups
ASP.corn <- cbind(1, pop.mean.corn) %*% beta.corn + (segments.mean$HACorn - (cbind(1, segments.mean$PixelsCorn) %*% beta.corn))
ASP.soy <- cbind(1, pop.mean.soy) %*% beta.soy + (segments.mean$HASoybeans - (cbind(1, segments.mean$PixelsSoybeans) %*% beta.soy))

#Empirical BLUP for both groups
BLUP.pred.corn <- cbind(1, pop.mean.corn) %*% beta.corn + gamma.corn*(segments.mean$HACorn - (cbind(1, segments.mean$PixelsCorn) %*% beta.corn))
BLUP.pred.soy <- cbind(1, pop.mean.soy) %*% beta.soy + gamma.soy*(segments.mean$HASoybeans - (cbind(1, segments.mean$PixelsSoybeans) %*% beta.soy))

#Survey Predictor for both groups
SP.corn <- segments.mean$HACorn
SP.soy <- segments.mean$HASoybeans

#Save resulting predictors in a data frame
df.predictors <- data.frame(County = county.names, reg.pred.corn, reg.pred.soy, ASP.corn, ASP.soy,
                      BLUP.pred.corn, BLUP.pred.soy, SP.corn, SP.soy)
df.predictors

#Estimate MSE
MSE.hat <- function(d, crop){
  #crop: for "corn" or "soy"
  #d: characteristic number for predictor
  #d = 0:      Regression predictor
  #d = 1:      Adjusted survey predictor
  #d = gamma:  (Empirical) BLUP
  #d = 2:      Survey predictor
  if (length(d) == 1){
    d = rep(d, 12)
  }
  if (crop == "corn"){
    sigma_eps = sigma.eps.corn
    sigma_rand = sigma.rand.corn
    gamma = gamma.corn
    pop_mean = pop.mean.corn
    segments.mean = segments.mean$PixelsCorn
    V_hat = V.hat.corn
  } else {
    sigma_eps = sigma.eps.soy
    sigma_rand = sigma.rand.soy
    gamma = gamma.soy
    pop_mean = pop.mean.soy
    segments.mean = segments.mean$PixelsSoybeans
    V_hat = V.hat.soy
  }
  
  res = rep(0, 12)
  for (i in 1:12){
    if (d[1] == 2){
      aux = (cbind(1, pop_mean[i]) - cbind(1, segments.mean[i]))
      
      res[i] = sigma_eps/number.observed[i] + aux %*% V_hat %*% t(aux)
    } else {
      aux1 = (cbind(1, pop_mean[i]) - d[i]*cbind(1, segments.mean[i]))
      aux2 = cbind(1, segments.mean[i])
      
      term1 = (1 - d[i])**2*sigma_rand + d[i]**2*sigma_eps/number.observed[i]
      term2 = 2*(d[i] - gamma[i])*aux1 %*% V_hat %*% t(aux2)
      term3 = aux1 %*% V_hat %*% t(aux1)
      
      res[i] = term1 + term2 + term3
    }
  }
  return(res)
}

MSE_reg_pred_corn <- MSE.hat(0, crop = "corn")
MSE_reg_pred_soy <- MSE.hat(0, crop = "soy")
MSE_adj_surv_pred_corn <- MSE.hat(1, crop = "corn")
MSE_adj_surv_pred_soy <- MSE.hat(1, crop = "soy")
MSE_BLUP_pred_corn <- MSE.hat(gamma.corn, crop = "corn")
MSE_BLUP_pred_soy <- MSE.hat(gamma.soy, crop = "soy")
MSE_surv_pred_corn <- MSE.hat(2, crop = "corn")
MSE_surv_pred_soy <- MSE.hat(2, crop = "soy")
#saving all results in dataframes
df.MSE.corn <- data.frame(county.names,
                     reg.pred.corn = MSE_reg_pred_corn,
                     adj.surv.pred.corn = MSE_adj_surv_pred_corn,
                     BLUP.pred.corn = MSE_BLUP_pred_corn,
                     SP.corn = MSE_surv_pred_corn)
df.MSE.corn

df.MSE.soy <- data.frame(county.names,
                          reg.pred.soy = MSE_reg_pred_soy,
                          adj.surv.pred.soy = MSE_adj_surv_pred_soy,
                          BLUP.pred.soy = MSE_BLUP_pred_soy,
                          SP.soy = MSE_surv_pred_soy)
df.MSE.soy

#Question(d)
#Estimate the total county field size for both crops

county_num_segments_corn <- unique(areasat$MeanPixelsCorn)
county_num_segments_soy <- unique(areasat$MeanPixelsSoybeans)
#Estimates for total county field size
total_reg_pred_corn <- reg.pred.corn * county_num_segments_corn
total_reg_pred_soy <- reg.pred.soy * county_num_segments_soy
total_adj_surv_pred_corn <- ASP.corn * county_num_segments_corn
total_adj_surv_pred_soy <- ASP.soy * county_num_segments_soy
total_BLUP_pred_corn <- BLUP.pred.corn * county_num_segments_corn
total_BLUP_pred_soy <- BLUP.pred.soy * county_num_segments_soy
total_surv_pred_corn <- SP.corn * county_num_segments_corn
total_surv_pred_soy <- SP.soy * county_num_segments_soy

df_total_HA <- data.frame(county = county.names,
                          reg.pred.corn = total_reg_pred_corn,
                          reg.pred.soy = total_reg_pred_soy,
                          Adj_surv_pred_corn = total_adj_surv_pred_corn,
                          Adj_surv_pred_soy = total_adj_surv_pred_soy,
                          BLUP.pred.corn = total_BLUP_pred_corn,
                          BLUP.pred.soy = total_BLUP_pred_soy,
                          SP.corn = total_surv_pred_corn,
                          SP.soy = total_surv_pred_soy)
df_total_HA



#Plot the results by the BLUP from part (c) as well as the predictor
#only relying on the survey data in a table

df_total <- data.frame(County = county.names, 
                       BLUP_corn = total_BLUP_pred_corn,
                       Survey_corn = total_surv_pred_corn,
                       BLUP_soy = total_BLUP_pred_soy,
                       Survey_soy = total_surv_pred_soy)
df_total


##Plot the results onto a map of Iowa
states <- map_data("state")
iowa <- subset(states, region == "iowa")

counties <- map_data("county")
iowa_counties <- subset(counties, region == "iowa")

tl <- tolower(as.character(county.names))

iowa_counties_polygon <- select(iowa_counties, long, lat, subregion)
centroids <- aggregate(iowa_counties_polygon[,1:2], by=list(iowa_counties_polygon$subregion), FUN = mean)
centroids$County <- unique(iowa_counties_polygon[,3])
centroids <- filter(centroids, County %in% tl)

label_corn <- data.frame(total_BLUP = "BLUP= \n", BLUP = round(df_total$BLUP_corn, 0),
                         total_Survey = "Survey= \n", Survey = round(df_total$Survey_corn , 0))
label_soy <- data.frame(total_BLUP = "BLUP= \n", BLUP = round(df_total$BLUP_soy, 0),
                        total_Survey = "Survey= \n", Survey = round(df_total$Survey_soy, 0))
label_corn <- data.frame(BLUP = paste( "", label_corn$total_BLUP, "", label_corn$BLUP),
                         Survey = paste(label_corn$total_Survey, label_corn$Survey))
label_soy <- data.frame(BLUP = paste( "", label_soy$total_BLUP, "", label_soy$BLUP),
                        Survey = paste(label_soy$total_Survey, label_soy$Survey))
label_corn <- data.frame(Total_HA = paste("", label_corn$BLUP, "\n", label_corn$Survey))
label_soy <- data.frame(Total_HA = paste("", label_soy$BLUP, "\n", label_soy$Survey))

iowa_counties$fill_value <- 0
iowa_counties$fill_value[iowa_counties$subregion %in% tl] <- 1

#Map for corn
ggplot(data = iowa_counties, aes(x = long, y = lat), fill = ) + 
  geom_polygon(aes(x = long, y = lat, group = group), 
               fill = "palegreen", color = "black") +
  geom_polygon(data = iowa_counties, aes(x = long, y = lat, group = group, fill = factor(fill_value)), 
               color = "white", show.legend = FALSE) +
  geom_polygon(data = iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = NA) +
  geom_label(data = centroids, aes(x = long, y = lat, label = label_corn$Total_HA, group = NULL),
             fontface = "bold", alpha = 0.7, size = 3) +
  scale_fill_manual(values = c("cornflowerblue", "blueviolet")) +
  theme_void() 

#Map for soybeans
ggplot(data = iowa_counties, aes(x = long, y = lat), fill = ) + 
  geom_polygon(aes(x = long, y = lat, group = group)) +
  geom_polygon(data = iowa_counties, aes(x = long, y = lat, group = group, fill = factor(fill_value)), 
               color = "white", show.legend = FALSE) +
  geom_polygon(data = iowa, aes(x = long, y = lat, group = group), 
               color = "midnightblue", fill = NA) +
  geom_label(data = centroids, aes(x = long, y = lat, label = label_soy$Total_HA, group = NULL),
             fontface = "bold", alpha = 0.7, size = 3) +
  scale_fill_manual(values = c("cornflowerblue", "blueviolet")) +
  theme_void() 








