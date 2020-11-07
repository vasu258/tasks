#Read data into R using .csv file
iris=read.csv(file.choose())
library(mlr)
library(explore)
library(dplyr)

iris %>%  explore_all()
iris %>%  explore_all(target=Species)

summary(iris)

iris %>% explain_tree(target=Species)

str(iris)

iris$Species=as.factor(iris$Species)

library(ggplot2)
ggplot(iris,aes(x =SepalLengthCm, y =SepalWidthCm, col= Species)) + geom_point()
colnames(iris)
ggplot(iris,aes(x =PetalLengthCm, y = PetalWidthCm, col= Species)) + geom_point()

#missing value graph
library(Amelia)
missmap(iris)

library(factoextra) # # Required for choosing optimum K

# function to compute total within-cluster sum of squares
iris=iris[,-1]
fviz_nbclust(iris[,2:4], kmeans, method ="wss", k.max =25) +theme_minimal() +ggtitle("The Elbow Method")
fviz_nbclust(iris[, 2:4], kmeans, method ="silhouette", k.max =25) +theme_minimal() +ggtitle("The Silhouette Plot")


# function to perform k-means clustering
library(NbClust)
library(fpc)
nc<-NbClust(data =iris[, 2:4], distance="euclidean", min.nc =2, max.nc =20, method ="kmeans")
nc

#cluster 2
kmeans.clus2 =kmeans(iris[, 2:4], centers=2, nstart =25)
kmeans.clus2
plotcluster( iris[, 2:4], kmeans.clus2$cluster)

kmeans.clus2$withinss
kmeans.clus2$tot.withinss
kmeans.clus2$betweenss

#cluster 3
kmeans.clus3=kmeans(iris[, 2:4], centers=3, nstart =25)
kmeans.clus3
plotcluster( iris[, 2:4], kmeans.clus3$cluster)

kmeans.clus3$withinss
kmeans.clus3$tot.withinss
kmeans.clus3$betweenss
# All within cluster sums of squares are small compared to the between cluster sum of squares.


# Cluster Profile
#various mean values of the attributes in different clusters
round(kmeans.clus3$centers, 2)
round(kmeans.clus2$centers, 2)

table(kmeans.clus3$cluster, iris$Species)
