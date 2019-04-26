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

lme.model.soy <- lme(HASoybeans ~ PixelsSoybeans, data = group.soygroup.corn, random = ~ 1)
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

##################################################################################################
#Model for corn and soy
var_est_corn <- VarCorr(lme.model.corn)
var_est_soy <- VarCorr(lme.model.soy)
sigma_eps_corn <- as.numeric(var_est_corn[2])
sigma_eps_soy <- as.numeric(var_est_soy[2])
sigma_rand_corn <- as.numeric(var_est_corn[1])
sigma_rand_soy <- as.numeric(var_est_soy[1])

# Calculate covariance matrix V_hat of beta_hat
M_corn <- list()
M_soy <- list()
for (i in 1:12){
  M_corn[[i]] = matrix(sigma_rand_corn, number.observed[i], number.observed[i])
  M_soy[[i]] = matrix(sigma_rand_soy, number.observed[i], number.observed[i])
}

I_n_corn <- diag(x = sigma_eps_corn, sum(number.observed), sum(number.observed))
I_n_soy <- diag(x = sigma_eps_soy, sum(number.observed), sum(number.observed))
block_M_corn <- bdiag(M_corn)
block_M_soy <- bdiag(M_soy)
V_corn <- I_n_corn + block_M_corn
V_soy <- I_n_soy + block_M_soy

aux_corn = cbind(1, areasat$PixelsCorn)
aux_soy = cbind(1, areasat$PixelsSoybeans)
V_hat_corn <- solve(t(aux_corn) %*% solve(V_corn) %*% aux_corn)
V_hat_soy <- solve(t(aux_soy) %*% solve(V_corn) %*% aux_soy)

gamma_corn <- sigma_rand_corn/(sigma_rand_corn + sigma_eps_corn/number.observed)
gamma_soy <- sigma_rand_soy/(sigma_rand_soy + sigma_eps_soy/number.observed)

## Calculate the different predictors

# Regression predictor
reg_pred_corn <- cbind(1, pop.mean.corn) %*% beta.corn
reg_pred_soy <- cbind(1, pop.mean.soy) %*% beta.soy

# Adjusted survey predictor
adj_surv_pred_corn <- cbind(1, pop.mean.corn) %*% beta.corn +
  (segments.mean$HACorn - (cbind(1, segments.mean$PixelsCorn) %*% beta.corn))
adj_surv_pred_soy <- cbind(1, pop.mean.soy) %*% beta.soy +
  (segments.mean$HASoybeans - (cbind(1, segments.mean$PixelsSoybeans) %*% beta.soy))

# (Empirical) BLUP
BLUP_pred_corn <- cbind(1, pop.mean.corn) %*% beta.corn +
  gamma_corn*(segments.mean$HACorn - (cbind(1, segments.mean$PixelsCorn) %*% beta.corn))
BLUP_pred_soy <- cbind(1, pop.mean.soy) %*% beta.soy +
  gamma_soy*(segments.mean$HASoybeans - (cbind(1, segments.mean$PixelsSoybeans) %*% beta.soy))

# Survey predictor
surv_pred_corn <- segments.mean$HACorn
surv_pred_soy <- segments.mean$HASoybeans

# Save resulting predictors in a data frame
df_pred <- data.frame(County = county.names, 
                      Reg_pred_corn = reg_pred_corn,
                      Reg_pred_soy = reg_pred_soy,
                      Adj_surv_pred_corn = adj_surv_pred_corn,
                      Adj_surv_pred_soy = adj_surv_pred_soy,
                      BLUP_pred_corn = BLUP_pred_corn,
                      BLUP_pred_soy = BLUP_pred_soy,
                      Surv_pred_corn = surv_pred_corn, 
                      Surv_pred_soy = surv_pred_soy)
df_pred

# Estimate MSE
MSE_pred <- function(d, crop){
  # corn: whether we have the model for "corn" or "soy"
  # d: characteristic number for predictor
  # d = 0:      Regression predictor
  # d = 1:      Adjusted survey predictor
  # d = gamma:  (Empirical) BLUP
  # d = 2:      Survey predictor
  if (length(d) == 1){
    d = rep(d, 12)
  }
  if (crop == "corn"){
    sigma_eps = sigma_eps_corn
    sigma_rand = sigma_rand_corn
    gamma = gamma_corn
    pop_mean = pop.mean.corn
    segments.mean = segments.mean$PixelsCorn
    V_hat = V_hat_corn
  } else {
    sigma_eps = sigma_eps_soy
    sigma_rand = sigma_rand_soy
    gamma = gamma_soy
    pop_mean = pop.mean.soy
    segments.mean = segments.mean$PixelsSoybeans
    V_hat = V_hat_soy
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

MSE_reg_pred_corn <- MSE_pred(0, crop = "corn")
MSE_reg_pred_soy <- MSE_pred(0, crop = "soy")
MSE_adj_surv_pred_corn <- MSE_pred(1, crop = "corn")
MSE_adj_surv_pred_soy <- MSE_pred(1, crop = "soy")
MSE_BLUP_pred_corn <- MSE_pred(gamma_corn, crop = "corn")
MSE_BLUP_pred_soy <- MSE_pred(gamma_soy, crop = "soy")
MSE_surv_pred_corn <- MSE_pred(2, crop = "corn")
MSE_surv_pred_soy <- MSE_pred(2, crop = "soy")

df_MSE <- data.frame(County = county.names,
                     Reg_pred_corn = MSE_reg_pred_corn,
                     Reg_pred_soy = MSE_reg_pred_soy,
                     Adj_surv_pred_corn = MSE_adj_surv_pred_corn,
                     Adj_surv_pred_soy = MSE_adj_surv_pred_soy,
                     BLUP_pred_corn = MSE_BLUP_pred_corn,
                     BLUP_pred_soy = MSE_BLUP_pred_soy,
                     Surv_pred_corn = MSE_surv_pred_corn,
                     Surv_pred_soy = MSE_surv_pred_soy)
df_MSE


######
### d)

## Estimate the total county field size for both crops

county_num_segments <- unique(areasat$MeanPixelsCorn)

# Estimates for total county field size
total_reg_pred_corn <- reg_pred_corn * county_num_segments
total_reg_pred_soy <- reg_pred_soy * county_num_segments
total_adj_surv_pred_corn <- adj_surv_pred_corn * county_num_segments
total_adj_surv_pred_soy <- adj_surv_pred_soy * county_num_segments
total_BLUP_pred_corn <- BLUP_pred_corn * county_num_segments
total_BLUP_pred_soy <- BLUP_pred_soy * county_num_segments
total_surv_pred_corn <- surv_pred_corn * county_num_segments
total_surv_pred_soy <- surv_pred_soy * county_num_segments

df_total_HA <- data.frame(county = county.names,
                          Reg_pred_corn = total_reg_pred_corn,
                          Reg_pred_soy = total_reg_pred_soy,
                          Adj_surv_pred_corn = total_adj_surv_pred_corn,
                          Adj_surv_pred_soy = total_adj_surv_pred_soy,
                          BLUP_pred_corn = total_BLUP_pred_corn,
                          BLUP_pred_soy = total_BLUP_pred_soy,
                          Surv_pred_corn = total_surv_pred_corn,
                          Surv_pred_soy = total_surv_pred_soy)
df_total_HA



## Plot the results by the BLUP from part (c) as well as the predictor
## only relying on the survey data in a table

df_total <- data.frame(County = county.names, 
                       BLUP_corn = total_BLUP_pred_corn,
                       BLUP_soy = total_BLUP_pred_soy,
                       Survey_corn = total_surv_pred_corn,
                       Survey_soy = total_surv_pred_soy)
df_total


## Plot the results onto a map of Iowa
states <- map_data("state")
iowa <- subset(states, region == "iowa")

counties <- map_data("county")
iowa_counties <- subset(counties, region == "iowa")

tl <- tolower(as.character(county.names))

iowa_counties_polygon <- select(iowa_counties, long, lat, subregion)
centroids <- aggregate(iowa_counties_polygon[,1:2], by=list(iowa_counties_polygon$subregion), FUN = mean)
centroids$County <- unique(iowa_counties_polygon[,3])
centroids <- filter(centroids, County %in% tl)

label_corn <- data.frame(total_BLUP = "BLUP:", BLUP = round(df_total$BLUP_corn, 0),
                         total_Survey = "Survey:", Survey = round(df_total$Survey_corn , 0))
label_soy <- data.frame(total_BLUP = "BLUP:", BLUP = round(df_total$BLUP_soy, 0),
                        total_Survey = "Survey:", Survey = round(df_total$Survey_soy, 0))
label_corn <- data.frame(BLUP = paste( "", label_corn$total_BLUP, "", label_corn$BLUP),
                         Survey = paste(label_corn$total_Survey, label_corn$Survey))
label_soy <- data.frame(BLUP = paste( "", label_soy$total_BLUP, "", label_soy$BLUP),
                        Survey = paste(label_soy$total_Survey, label_soy$Survey))
label_corn <- data.frame(Total_HA = paste("", label_corn$BLUP, "\n", label_corn$Survey))
label_soy <- data.frame(Total_HA = paste("", label_soy$BLUP, "\n", label_soy$Survey))

iowa_counties$fill_value <- 0
iowa_counties$fill_value[iowa_counties$subregion %in% tl] <- 1

# Map for corn
ggplot(data = iowa_counties, aes(x = long, y = lat), fill = ) + 
  geom_polygon(aes(x = long, y = lat, group = group), 
               fill = "palegreen", color = "black") +
  geom_polygon(data = iowa_counties, aes(x = long, y = lat, group = group, fill = factor(fill_value)), 
               color = "white", show.legend = FALSE) +
  geom_polygon(data = iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = NA) +
  geom_label_repel(data = centroids, aes(long, lat, label = label_corn$Total_HA),
                   size = 3.5, alpha = 0.7, point.padding = 1.5,
                   min.segment.length = 0, segment.size = 0.6) +
  scale_fill_manual(values = c("grey80", "turquoise")) +
  theme_void()

# Map for soybeans
ggplot(data = iowa_counties, aes(x = long, y = lat), fill = ) + 
  geom_polygon(aes(x = long, y = lat, group = group), 
               fill = "palegreen", color = "black") +
  geom_polygon(data = iowa_counties, aes(x = long, y = lat, group = group, fill = factor(fill_value)), 
               color = "white", show.legend = FALSE) +
  geom_polygon(data = iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = NA) +
  geom_label_repel(data = centroids, aes(long, lat, label = label_soy$Total_HA),
                   size = 3.5, alpha = 0.7, point.padding = 1.5,
                   min.segment.length = 0, segment.size = 0.6) +
  scale_fill_manual(values = c("grey80", "khaki")) +
  theme_void()










