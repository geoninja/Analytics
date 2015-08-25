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


###############################################################################
#        HOMEWORK - WEEK 6 CLUSTERING
###############################################################################

#DailyKos documents clustering
dailykos = read.csv("dailykos.csv") #3430 x 1545

#Part 1: Hierarchical Clustering
distances = dist(dailykos, method = "euclidian")
clusterKos = hclust(distances, method = "ward.D")
plot(clusterKos)
clusterGroups = cutree(clusterKos, k = 7) #3430
table(clusterGroups)

#Create a subset for each cluster
group1 = dailykos[clusterGroups == 1, ]
group2 = dailykos[clusterGroups == 2, ]
group3 = dailykos[clusterGroups == 3, ]
group4 = dailykos[clusterGroups == 4, ]
group5 = dailykos[clusterGroups == 5, ]
group6 = dailykos[clusterGroups == 6, ]
group7 = dailykos[clusterGroups == 7, ]

tail(sort(colMeans(group1)))
head(sort(colMeans(group2), decreasing = T)) #same as above
head(sort(colMeans(group3), decreasing = T))
head(sort(colMeans(group4), decreasing = T))
head(sort(colMeans(group5), decreasing = T))
head(sort(colMeans(group6), decreasing = T))
head(sort(colMeans(group7), decreasing = T))

#Part 2: K-Means Clustering
#kosVector = as.vector(as.matrix(dailykos))
set.seed(1000)
kmclust7 = kmeans(dailykos, centers = 7) #K-means with 7 clusters
kmclust7$size
table(kmclust7$cluster) #same as above

kcluster = split(dailykos, kmclust7$cluster)
nrow(kcluster[[3]])

head(sort(colMeans(kcluster[[1]]), decreasing = T))
head(sort(colMeans(kcluster[[2]]), decreasing = T))
head(sort(colMeans(kcluster[[3]]), decreasing = T))
head(sort(colMeans(kcluster[[4]]), decreasing = T))
head(sort(colMeans(kcluster[[5]]), decreasing = T))
head(sort(colMeans(kcluster[[6]]), decreasing = T))
head(sort(colMeans(kcluster[[7]]), decreasing = T))

table(clusterGroups, kmclust7$cluster)
#116 obs from kmeans cluster 2 (~80%) also fall into hclust 7



#Airlines Market Segmentation
airlines = read.csv("data/AirlinesCluster.csv")

#Part 1
summary(airlines)
#the data needs to be normalized, and we will do this by using the preProcess fun from caret pkg
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

#Part 2: perform hierarchical clustering in the data
distances = dist(airlinesNorm, method = "euclidian")
air.clusters = hclust(distances, method = "ward.D")
plot(air.clusters)
air.groups = cutree(air.clusters, k = 5)
table(air.groups)

#analyzing results
df = data.frame()
for(n in 1:7){
    x = as.vector(tapply(airlines[, n], air.groups, mean))
    df = rbind(df, x)
}
colnames(df) = c(1:5)
rownames(df) = names(airlines)
df

#Part 3: K-Means clustering
set.seed(88)
kmc.air = kmeans(airlines, centers = 5, iter.max = 1000)
table(kmc.air$cluster)
tapply(airlines$Balance, kmc.air$cluster, mean)



#Stock returns: cluster then predict
stocks = read.csv("StocksCluster.csv")
attach(stocks)

#Part 1
table(stocks$PositiveDec)
6324/11580 #0.546114

m = matrix(999, ncol = 11, nrow = 11)
for(i in 1:11){
    for(j in 1:11){
        m[i, j] = cor(stocks[, i], stocks[, j])
    }
}

diag(m) = 0; m
max(abs(m)) #0.1916728

which.max(apply(stocks[, 1:11], 2, mean)) #Apr
which.min(apply(stocks[, 1:11], 2, mean)) #Sep
#summary(stocks) #another way by visually checking means

#Part 2 - Logistic Regression
library(caTools)
set.seed(144)
spl = sample.split(PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == T)
stocksTest = subset(stocks, spl == F)

logfit = glm(PositiveDec ~., data = stocksTrain, family = binomial)
summary(logfit)
predTrain = predict(logfit, type = "response") #"response" returns probabilities
#tapply(predTrain, stocksTrain$PositiveDec, mean)
table(stocksTrain$PositiveDec, predTrain >= 0.5)
(3640+990)/nrow(stocksTrain) #0.5711818

predTest = predict(logfit, newdata = stocksTest, type = "response")
table(stocksTest$PositiveDec, predTest >= 0.5)
(417+1553)/nrow(stocksTest) #0.5670697

table(stocksTest$PositiveDec)
1897/nrow(stocksTest) #0.5460564

#Part 3 - Clustering
#let's remove the dependent variable, since we will make predicitions for each cluster
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#In cases where we have a training and testing set, we'll want to normalize by the mean 
#and standard deviation of the variables in the training set. We can do this by passing
#just the training set to the preProcess function.
library(caret)
preproc = preProcess(limitedTrain) #normalize data based on training set only
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
mean(normTrain$ReturnJan) #mean is zero, as it should
mean(normTest$ReturnJan) #mean is zero, as it should
par(mfrow=c(1, 2))
hist(normTrain$ReturnJan)
hist(normTest$ReturnJan)

set.seed(144)
km = kmeans(normTrain, centers = 3)
km$size

#we can use the flexclust package to obtain training set and testing set cluster 
#assignments for our observations (computation is intensive)
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata = normTest)
table(clusterTest) #2080 obs in cluster 2

#Part 4 - Cluster specific predictions
stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

summary(stocksTrain1$PositiveDec)
summary(stocksTrain2$PositiveDec)
summary(stocksTrain3$PositiveDec)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

stocksModel1 = glm(PositiveDec ~., data = stocksTrain1, family = binomial)
stocksModel2 = glm(PositiveDec ~., data = stocksTrain2, family = binomial)
stocksModel3 = glm(PositiveDec ~., data = stocksTrain3, family = binomial)

stocksModel1$coefficients
stocksModel2$coefficients
stocksModel3$coefficients

predTest1 = predict(stocksModel1, newdata = stocksTest1, type = "response")
tab1 = table(stocksTest1$PositiveDec, predTest1 >= 0.5)
(tab1[1, 1] + tab1[2, 2])/ nrow(stocksTest1) #accuracy = 0.6194145

predTest2 = predict(stocksModel2, newdata = stocksTest2, type = "response")
tab2 = table(stocksTest2$PositiveDec, predTest2 >= 0.5)
(tab2[1, 1] + tab2[2, 2])/ nrow(stocksTest2) #accuracy = 0.5504808

predTest3 = predict(stocksModel3, newdata = stocksTest3, type = "response")
tab3 = table(stocksTest3$PositiveDec, predTest3 >= 0.5)
(tab3[1, 1] + tab3[2, 2])/ nrow(stocksTest3) #accuracy = 0.6458333

AllPredictions = c(predTest1, predTest2, predTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions >= 0.5)
(467+1544)/(467+1110+353+1544) #accuracy = 0.5788716


