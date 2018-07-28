#Kindly Install packages : "ggplot2", "NbClust"

library(factoextra)
library(cluster)
library(NbClust)

# Importing the dataset
dataset = read.csv('SnPcompanies.csv')

#Cleaning and transforming the dataset
x = scale(dataset[2:4])
gowerData = cbind(dataset,x)
gowerData <- gowerData[,-(2:4)] 

#Computing gowers distance between each data point with more weights to numerical variables
gower_dist <- daisy(gowerData[,],
                    metric = "gower",
                    weights=c(0,0,1,2,2,2))
summary(gower_dist)
#Creating dissimilarity matrix based on gowers distances between each data point
gower_mat <- as.matrix(gower_dist)

#plot using fviz
fviz_nbclust(gower_mat,pam,method="silhouette",diss=gower_dist,k.max=20)+theme_classic()

#visualize heamap gower matrix
heatmap(gower_mat,symm = TRUE,distfun=function(x) as.dist(x))

#Visualize distance matrix
fviz_dist(gower_dist,
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#lower size of gower matrix and visualise the dissimilarity matrix for 30 companies
gower_mat1 = gower_mat
gower_mat1 <- gower_mat1[-c(1:475), -c(1:475)]
gower_list1 = as.dist(gower_mat1)
fviz_dist(gower_list1,show_labels = TRUE,
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#pam fit
pam_fit <- pam(gower_dist, diss = TRUE, k = 2)
print(pam_fit)
print(pam_fit$medoids)
head(pam_fit$clustering)
print(typeof(pam_fit))
print(dim(pam_fit))

names(gowerData)[1] = "Name"
pam_fit$clusinfo #gives the number of observations in each cluster

gowerData = cbind(gowerData,pam_fit$clustering)
names(gowerData)[7] = "Cluster"

#Export the GowerData into csv for futher analysis on Python 
#write.csv(gowerData, "/filePath")