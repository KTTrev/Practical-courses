library(ggfortify)

#(a)
#Create a reduced dataset discarding the species
df<-iris
df$Species<-NULL

#principal componets
pca_cov = prcomp(df, scale=FALSE)
summary(pca_cov)
pca_cov

#(b)
pca_cov$rotation
pca_cor = prcomp(df, scale=TRUE)

summary(pca_cor)
pca_cor$rotation

#(c)
#dataset in millimeters
df.milli = df
df.milli$Petal.Length <- df.milli$Petal.Length * 10

pca_cov_milli = prcomp(df.milli, scale=FALSE)
summary(pca_cov_milli)

pca_cor_milli = prcomp(df.milli, scale=TRUE)
summary(pca_cor_milli)

#(d)
autoplot(pca_cov, data = iris, colour = 'Species', loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

#(e)
#KMEANS ON 1-2PC
set.seed(47)
comp <- data.frame(pca_cov$x[,1:2])
k.means <- kmeans(comp, 3)

## Rename columns
iris_sample = iris
iris_sample$new_species <- NA
# 
iris_sample <- iris_sample %>% mutate(new_species = replace(new_species, which(Species == "setosa"), 2))
iris_sample <- iris_sample %>% mutate(new_species = replace(new_species, which(Species == "versicolor"), 1))
iris_sample <- iris_sample %>% mutate(new_species = replace(new_species, which(Species == "virginica"), 3))
# 
iris_sample$new_species <- as.integer(iris_sample$new_species)

#KMEANS ON entire dataset
k.means.all <- kmeans(df, 3)
table(k.means$cluster, iris$Species)
table(k.means.all$cluster, iris$Species)

