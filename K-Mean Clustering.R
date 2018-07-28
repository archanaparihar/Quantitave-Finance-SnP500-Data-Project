# K-Means Clustering

# Importing the dataset
dataset = read.csv('SnPcompanies.csv')
x = scale(dataset[2:4])
dataset = cbind(dataset,x)
dataset <- dataset[,-(2:4)] 

# Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss = vector() #initialize an empty vector
for (i in 1:10) wcss[i] = sum(kmeans(x, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

#optimal number of clusters is 5-6 clusters clearly
# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x, centers = 5,iter.max=300, nstart=10)
y_kmeans = kmeans$cluster
matrix = kmeans$centers
summary(kmeans)

#assign cluster value to dataset
clusteredData = cbind(dataset, y_kmeans)
names(clusteredData)[7] = "Cluster"

library(factoextra)
library(cluster)
library(NbClust)

fviz_cluster(kmeans, data = x)
# Visualising the clusters

clusplot(x,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of companies'))

#Export the data into csv for futher analysis on Python 
write.csv(clusteredData, "/filepath")
