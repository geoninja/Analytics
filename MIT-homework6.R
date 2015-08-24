#MIT Homework week 6  - Clustering

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
airlines = read.csv("AirlinesCluster.csv")

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


