setwd("~/Practical-courses/Stochastic_Lab_Course_2")
library("tidyverse")
library("haven")
library(maptools)
library(raster)
library(rgdal)
library(tikzDevice) # To export plots as .tex files
##Question(a)

children <- read_dta("childrenfinal.dta")
#remove all variables that start with “s“, “v” and “m”, followed by a number
children1<- children %>% 
  dplyr::select(-matches("^[svm][0-9]"))

#convert labelled double into double variables
children2<- children1 %>%
  mutate_if(is.double, as.double)

##Question(b)

#(b) Make a smaller tibble that contains variables hypage, ruralfacto, female, zstunt, zweight, zwast, adm2.
children3<- children2 %>%
  dplyr::select(c(hypage, ruralfacto, female, zstunt, zweight, zwast, adm2))

#Make a scatter plot of zstunt against hypage, Add a smooth line to the plot

plot1 <- ggplot(children3, aes(x = hypage, y = zstunt)) +
  geom_point() +
  geom_smooth(se = F, color = "purple")

#smooth plots of zstunt against age for females and males on one plot
plot2 <- ggplot(children3, aes(x = hypage, y = zstunt,  colour = factor(female))) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = F) +
  scale_colour_manual(labels = c("male", "female"), values = c("blue2", "green3")) +
  guides(colour = guide_legend(title="Gender"))

#plot zstunt against age for urban and rural children
plot3 <- ggplot(children3, aes(x = hypage, y = zstunt,  colour = factor(ruralfacto))) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = F) +
  scale_colour_manual(labels = c("urban", "rural"), values = c("blue2", "green3")) +
  guides(colour = guide_legend(title="Area"))

#Experiment with different aesthetics, themes and font sizes for the plots, report your favourite


##Question(c)
#help link https://rpubs.com/spoonerf/countrymapggplot2
Kenya1<-getData("GADM", country="KE", level=1) #Download Kenya shapefile data

#WARNING for linux users: Do not capitalize epsg in the following code to avoid errors.
Kenya1_UTM<-spTransform(Kenya1, CRS("+init=epsg:32537")) # setting an appropriate projection 
colnames(children3)[7] <- "NAME_1" #rename adm2 into NAME_1

#sort in alphabetic order with respect to NAME_1
children3<- children3[order(children3$NAME_1),]
Kenya1_UTM@data<- Kenya1_UTM@data[order(Kenya1_UTM@data$NAME_1),]

#summarising children3 data by the mean of zstunt in the corresponding county
children4 <- children3 %>%
  group_by(NAME_1) %>%
  summarise(mean = mean(zstunt), n = n())

#Adding the missing county Isiolo
children4[nrow(children4) + 1,] <- NA
children4$NAME_1[47] <- "Isiolo"
#reordering 
children4<- children4[order(children4$NAME_1),]
#Prepare the dataframe for ggplot
Kenya1_UTM@data$id <- rownames(Kenya1_UTM@data)
Kenya1_UTM@data <- mutate(Kenya1_UTM@data, zstunt.mean= children4$mean)
Kenya1_df <- fortify(Kenya1_UTM)
Kenya1_df <- full_join(Kenya1_df,Kenya1_UTM@data, by="id")

#In order to add names to map, we need another dataframe with all the conunties' centroids
# "coordinates" extracts centroids of the polygons, in the order listed at Kenya1_UTM@data
centroids_df <- as.data.frame(coordinates(Kenya1_UTM))
names(centroids_df) <- c("long", "lat")
children4<- children4[order(children4$NAME_1),]
centroids_df$NAME_1 <- Kenya1_UTM@data$NAME_1
centroids_df$zstunt.mean <- children4$mean

#Generating the map
ggplot(data = Kenya1_df, aes(x = long, y = lat, group = group, fill = zstunt.mean)) + 
  geom_polygon(color = "black", size = 0.25) +
  geom_text(data = centroids_df, aes(x = long, y = lat, label = NAME_1, group = NULL), size = 3) +
  scale_fill_distiller(name="Zstunt mean for \n each county", palette = "Spectral") +
  theme(aspect.ratio = 1)

##(d)write the tibble from (b) into a text file
write.table(children3,"children3.txt")

#exports plots as .tex files
tikz('Ex1plot1.tex',width=3.5, height=3)
plot1
dev.off()

tikz('Ex1plot2.tex',width=3.5, height=3)
plot2
dev.off()

tikz('Ex1plot3.tex',width=3.5, height=3)
plot3
dev.off()







