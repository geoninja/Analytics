#MIT class week 6 - Clustering

#setwd("~/Dropbox/Analytics_Edge") #change this accordingly

#Hierarchical Clustering of Movie Genres (for Recommendation Engines)

movies = read.table("movieLens.txt", header = F, sep = "|", quote = "\"") #1682 x 24

#R automatically names the variables since there was no header. Let's label them.
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", 
                     "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", 
                     "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical",
                     "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

movies$ID = NULL #this effectively removes the data from the set
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
movies = unique(movies) #remove duplicates: now 1664 x 20

table(movies$Comedy)
table(movies$Western)
table(movies$Romance, movies$Drama)


distances = dist(movies[2:20], method = "euclidian")
clusterMovies = hclust(distances, method = "ward.D") #ward.D uses centroids and min variance
plot(clusterMovies)
#we select k = 10 clusters, based on the dendogram, and the fact that we want a certain
#minimum number of clusters to make recommendations (use of expert knowledge of problem)
clusterGroups = cutree(clusterMovies, k = 10)
tapply(movies$Action, clusterGroups, mean) #computes % of Action genre in each cluster

spl = split(movies[2:20], clusterGroups)
lapply(spl, colMeans) #returns % of each genre in all 10 clusters

subset(movies, Title == "Men in Black (1997)") #index 257
#let's check in which cluster the above movie falls on:
clusterGroups[257] #cluster 2: action adventure sci-fi (thriller) cluster
cluster2 = subset(movies, clusterGroups == 2)
cluster2$Title[1:10] #these could be good recommendations for a person whose preferences (based
#on content and/or collaborative filtering) fall into cluster 2.


#Recitation

#Flower grey scale image: hierarchical clustering
flower = read.csv("flower.csv", header = F)
flowerMatrix = as.matrix(flower) #50 x 50 pixels
flowerVector = as.vector(flowerMatrix) #1 x 2500 we MUST convert from matrix to vector

distance = dist(flowerVector, method = "euclidian") #compute all parwise distances b/w pixels
#total pairwise distances = n*(n-1)/2

#let's first use hierarchical clustering to cluster pixels intensities:
clusterIntensity = hclust(distance, method = "ward.D") #minimum variance method (aims for
#compact, spherical clusters): minimize variance within each cluster, and
#the distance among clusters 

plot(clusterIntensity)
rect.hclust(clusterIntensity, k = 3, border = "red") #draws rectangles around choosen clusters

flowerClusters = cutree(clusterIntensity, k = 3) #create vector w/ assigned cluster for each val
tapply(flowerVector, flowerClusters, mean) #cluster 1 is darkest, cluster 3 is whitest

dim(flowerClusters) = c(50,50) #creater a matrix representation for plotting clustered image
image(flowerClusters, axes = F) #voila!
image(flowerMatrix, axes = F, col = grey(seq(0, 1, length = 256))) #original image


#MRI images of a healthy subject versus with cancer tumor: K-means clustering
healthy = read.csv("healthy.csv", header = F) #566 x 646
healthyMatrix = as.matrix(healthy)
image(healthyMatrix, axes = F, col = grey(seq(0, 1, length = 256)))

healthyVector = as.vector(healthyMatrix)
#distance = dist(healthyVector, method = "euclidian") #too big to allocate in memory!
#n = 365636, n*(n-1)/2

#Scree Plot (to select k)
KMC2 = kmeans(healthyVector, centers = 2, iter.max = 1000) #K-means with 2 clusters
KMC2$withinss #within clusters vector of the sum of sq dist b/w each point and cluster centroid
#note: the vector return the sum for each k cluster, thus vector size = k

NumClusters = seq(2,10,1)
#Repeat K-means for k from 3 to 10
SumWithinss = sapply(2:10, function(x) sum(kmeans(healthyVector, centers = x, iter.max = 1000)$withinss))
# SumWithinss = c(sum(KMC2$withinss), sum(KMC3$withinss), sum(KMC4$withinss), sum(KMC5$withinss),
#                 sum(KMC6$withinss), sum(KMC7$withinss), sum(KMC8$withinss), sum(KMC9$withinss),
#                 sum(KMC10$withinss))
plot(NumClusters, SumWithinss, type="b") #scree plot: elbow at about k = 4 or 5

#Run K-means with chosen k = 5
k = 5
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
healthyClusters = KMC$cluster #results from clustering
KMC$centers[2] #mean intensity value of the cluster 2
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))#convert result vec to matrix 
image(healthyClusters, axes = F, col = rainbow(k))#gray matter in purple, white matter in yellow

#Let's repeat process to check if tumor can be identified in MRI
tumor = read.csv("tumor.csv", header = F) #571 x 512
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

#We will use the healthy K-means model as the training model, and the tumor data as test set:
library(flexclust) #contains function kcca: k centroid cluster analysis
KMC.kcca = as.kcca(KMC, healthyVector) #convert KCM to object of the class kcca for prediction
tumorClusters = predict(KMC.kcca, newdata = tumorVector)
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes = F, col = rainbow(k))

#Notes: modified clustering K-means are more advanced but produce better results for MRI images
#packages in R specialized in medical imaging: MedicalImaging.html at CRAN repo

