#KAGGLE MODEL BUILDING SCRIPT

#Load Data and Packages
library(ROCR)
library(plyr) #use mapvalues function
library(tm)

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)



#PROCESS DATA (after data exploration):

# Convert date/time to a format R will understand:
NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")
range(NewsTrain$PubDate) # SEP to NOV
range(NewsTest$PubDate) #DEC

#Create Weekday variable: weekend is ~ twice more popular than weekdays
NewsTrain$Weekday = NewsTrain$PubDate$wday #extract weekdays (0 to 6)
NewsTest$Weekday = NewsTest$PubDate$wday

#Convert 0 - 6 into actual weekday names:
wd = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
NewsTrain$Weekday = as.factor(mapvalues(NewsTrain$Weekday, from = 0:6, to = wd))
NewsTest$Weekday = as.factor(mapvalues(NewsTest$Weekday, from = 0:6, to = wd))

#blogTrain$Weekend[blogTrain$Weekday == "Saturday" | blogTrain$Weekday == "Sunday"] = 1
#blogTrain$Weekend[!(blogTrain$Weekday == "Saturday" | blogTrain$Weekday == "Sunday")] = 0

#Create Hour variable: pub hours with popularity above 18% mean are 18-23(33%), 15(21%), 10-12(18%)
NewsTrain$Hour = NewsTrain$PubDate$hour #extract publishing hour
NewsTest$Hour = NewsTest$PubDate$hour

#blogTrain$PopHour[blogTrain$Hour >= 18] = 1
#blogTrain$PopHour[blogTrain$Hour < 18] = 0



#FIRST MODEL: logistic regression

mod1 = glm(Popular ~ WordCount + Hour + SectionName + Weekday, data = NewsTrain, family = binomial)
summary(mod1) #AIC = 3887

pred.train1 = predict(mod1, type="response") #predictions on training set
summary(pred.train1)
table(NewsTrain$Popular, pred.train1 > 0.5) #(5158+640)/6532 0.8876301
ROC.mod1 = prediction(pred.train1, NewsTrain$Popular)
auc = as.numeric(performance(ROC.mod1, "auc")@y.values) #0.8939753

pred.mod1 = predict(mod1, newdata = NewsTest, type="response") #prediction on test set
summary(pred.mod1)
tapply(pred.mod1, NewsTest$SectionName, mean)
tapply(pred.train1, NewsTrain$SectionName, mean)

#Make submission file for Kaggle:
Submission1 = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = pred.mod1)
write.csv(Submission1, "Submission_mod1.csv", row.names=FALSE) #AUC 0.86780


#Model 1 using log of WordCount to make it more linear:
NewsTrain$LogWordCount = log(NewsTrain$WordCount + 1) #+1 is added to avoid Inf values
NewsTest$LogWordCount = log(NewsTest$WordCount + 1)

mod1log = glm(Popular ~ LogWordCount + Hour + SectionName + Weekday, data = NewsTrain, 
              family = binomial)
summary(mod1log) #AIC = 3728

pred.train1log = predict(mod1log, type="response") #predictions on training set
summary(pred.train1log)
table(NewsTrain$Popular, pred.train1log > 0.5) #(5204+594)/6532 0.8876301
ROC.mod1log = prediction(pred.train1log, NewsTrain$Popular)
auc = as.numeric(performance(ROC.mod1log, "auc")@y.values) #0.9075345 better!

pred.mod1log = predict(mod1log, newdata = NewsTest, type="response") #prediction on test set
summary(pred.mod1log)


#Make submission file for Kaggle:
Submission1log = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = pred.mod1log)
write.csv(Submission1log, "Submission_mod1log.csv", row.names=FALSE) #AUC 0.87985 better than mod1



#Model 2: same as mod1, but include NewsDesk
mod2 = glm(Popular ~ WordCount + Hour + SectionName + Weekday + NewsDesk, data = NewsTrain, 
           family = binomial)
summary(mod2) #AIC = 3498

pred.train2 = predict(mod2, type="response") #predictions on training set
summary(pred.train2)
table(NewsTrain$Popular, pred.train2 > 0.5) #(5205+655)/6532 0.8971219
ROC.mod2 = prediction(pred.train2, NewsTrain$Popular)
auc = as.numeric(performance(ROC.mod2, "auc")@y.values) #0.9116301 overfitting?

pred.mod2 = predict(mod2, newdata = NewsTest, type="response") #prediction on test set
summary(pred.mod2)

#Make submission file for Kaggle:
Submission2 = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = pred.mod2)
write.csv(Submission2, "Submission_mod2.csv", row.names=FALSE) #AUC 0.88735


#PROCESS TEXT VARIABLES: Headline and Abstract
#Headline
CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

dtm.hl = DocumentTermMatrix(CorpusHeadline)
findFreqTerms(dtm.hl, lowfreq = 50)
sparse.hl = removeSparseTerms(dtm.hl, 0.985) #42 terms at 99%, 16 terms at 98.5%
HeadlineWords = as.data.frame(as.matrix(sparse.hl))
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

HlTrainSet = head(HeadlineWords, nrow(NewsTrain))
HlTestSet = tail(HeadlineWords, nrow(NewsTest))


#Abstract
CorpusAbstract = Corpus(VectorSource(c(NewsTrain$Abstract, NewsTest$Abstract)))
CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)

dtm.abs = DocumentTermMatrix(CorpusAbstract)
findFreqTerms(dtm.abs, lowfreq = 50)
sparse.abs = removeSparseTerms(dtm.abs, 0.985) #101 terms
AbstractWords = as.data.frame(as.matrix(sparse.abs))
colnames(AbstractWords) = make.names(colnames(AbstractWords))

AbsTrainSet = head(AbstractWords, nrow(NewsTrain))
AbsTestSet = tail(AbstractWords, nrow(NewsTest))

#Analyze text processing results
abs = findFreqTerms(dtm.abs, lowfreq = 130) #101 terms
hl = findFreqTerms(dtm.hl, lowfreq = 85) #42 terms (used 85 to approximate to 99% sparsity)

diff = hl[!hl %in% abs] #only 14 words in headlines are not in abstract (don't seem relevant)
diff = abs[!abs %in% hl] #73 words in abstract are not in headlines (230-193)

#Conclusion: work with headlines, switch to abstract if model results do not seem satisfcatory.

#Create datasets with either headline or abstract text to be used in modeling:
#Headline:
HlTrainSet$Popular = NewsTrain$Popular
#HlTrainSet$WordCount = log(NewsTrain$WordCount + 1)
HlTrainSet$WordCount = NewsTrain$WordCount
HlTrainSet$Weekday = NewsTrain$Weekday
HlTrainSet$Hour = NewsTrain$Hour
HlTrainSet$SectionName = NewsTrain$SectionName
HlTrainSet$NewsDesk = NewsTrain$NewsDesk
HlTrainSet$UniqueID = NewsTrain$UniqueID
dim(HlTrainSet) #6532 x 49 (23)
#names(HlTrainSet)[42:49]
names(HlTrainSet)[16:23]

#HlTestSet$WordCount = log(NewsTest$WordCount + 1)
HlTestSet$WordCount = NewsTest$WordCount
HlTestSet$Weekday = NewsTest$Weekday
HlTestSet$Hour = NewsTest$Hour
HlTestSet$SectionName = NewsTest$SectionName
HlTestSet$NewsDesk = NewsTest$NewsDesk
HlTestSet$UniqueID = NewsTest$UniqueID
#names(HlTestSet)[42:48]
names(HlTestSet)[16:22]

#Abstract:
AbsTrainSet$Popular = NewsTrain$Popular
#AbsTrainSet$WordCount = log(NewsTrain$WordCount + 1)
AbsTrainSet$WordCount = NewsTrain$WordCount
AbsTrainSet$Weekday = NewsTrain$Weekday
AbsTrainSet$Hour = NewsTrain$Hour
AbsTrainSet$SectionName = NewsTrain$SectionName
AbsTrainSet$NewsDesk = NewsTrain$NewsDesk
AbsTrainSet$UniqueID = NewsTrain$UniqueID
dim(AbsTrainSet) #6532 x 108

#AbsTestSet$WordCount = log(NewsTest$WordCount + 1)
AbsTestSet$WordCount = NewsTest$WordCount
AbsTestSet$Weekday = NewsTest$Weekday
AbsTestSet$Hour = NewsTest$Hour
AbsTestSet$SectionName = NewsTest$SectionName
AbsTestSet$NewsDesk = NewsTest$NewsDesk
AbsTestSet$UniqueID = NewsTest$UniqueID
dim(AbsTestSet) #1870 x 107

# dtmTitle = as.data.frame(as.matrix(dtmTitle))
# dtmAbstract = as.data.frame(as.matrix(dtmAbstract))
# colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
# colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
# dtm = cbind(dtmTitle, dtmAbstract)



#MODELING WITH TEXT DATA: logistic regression

#Model 3: using processed Headline and removing NewsDesk
mod3 = glm(Popular ~. -NewsDesk, data = HlTrainSet, family = binomial)
summary(mod3) #AIC = 3657 overfit and worse than mod2

pred.train3 = predict(mod3, type="response") #predictions on training set
summary(pred.train3)
table(NewsTrain$Popular, pred.train3 > 0.5) #(5215+603)/6532 0.890692
ROC.mod3 = prediction(pred.train3, NewsTrain$Popular)
auc = as.numeric(performance(ROC.mod3, "auc")@y.values) #0.9171627 better than mod2


#Model 4: using processed Headline and keeping NewsDesk
mod4 = glm(Popular ~. , data = HlTrainSet, family = binomial)
summary(mod4) #AIC = 3283, the best so far but overfit

pred.train4 = predict(mod4, type="response") #predictions on training set
summary(pred.train4)
table(NewsTrain$Popular, pred.train4 > 0.5) #(5238+658)/6532 0.9026332
ROC.mod4 = prediction(pred.train4, NewsTrain$Popular)
auc = as.numeric(performance(ROC.mod4, "auc")@y.values) #0.9320218 the best so far, overfit


#Model 5: using processed Abstract and removing NewsDesk
mod5 = glm(Popular ~. -NewsDesk, data = AbsTrainSet, family = binomial)
summary(mod5) #AIC = 3734 (3409 with NewsDesk), overfit

pred.train5 = predict(mod5, type="response") #predictions on training set
summary(pred.train5)
table(NewsTrain$Popular, pred.train5 > 0.5) #(5220+654)/6532 0.899265 small increase over headline
ROC.mod5 = prediction(pred.train5, NewsTrain$Popular)
auc = as.numeric(performance(ROC.mod5, "auc")@y.values) #0.9276059
#auc 0.9393006 with NewsDesk: higher... but is less more?



#CART MODELING
library(rpart)
library(rpart.plot)

#remove log terms for WordCount:
HlTrainSet$WordCount = NewsTrain$WordCount
HlTestSet$WordCount = NewsTest$WordCount

#Model 6: CART with default minbucket and cp, Headline text including NewsDesk
# HlTrainSet$NewsDesk = NewsTrain$NewsDesk
# HlTestSet$NewsDesk = NewsTest$NewsDesk

mod6 = rpart(Popular ~., data = HlTrainSet, method = "class") #default minbucket and cp
prp(mod6)
pred.train6 = predict(mod6) #predictions on training set
prob.train6 = pred.train6[,2]
table(HlTrainSet$Popular, prob.train6 >= 0.5) #actual vs predicted
(5291 + 608)/nrow(HlTrainSet) #accuracy: 0.9030925

ROC.mod6 = prediction(prob.train6, HlTrainSet$Popular)
auc = as.numeric(performance(ROC.mod6, "auc")@y.values) #0.7781507


#Model 7: same as model 6, without NewsDesk
HlTrainSet$NewsDesk = NULL
HlTestSet$NewsDesk = NULL

mod7 = rpart(Popular ~., data = HlTrainSet, method = "class") #default minbucket and cp
prp(mod7)
pred.train7 = predict(mod7)
prob.train7 = pred.train7[,2]
table(HlTrainSet$Popular, prob.train7 >= 0.5)
(5267 + 604)/nrow(HlTrainSet) #accuracy: 0.8988059 lower than mod6 but more sensible?

ROC.mod7 = prediction(prob.train7, HlTrainSet$Popular)
auc = as.numeric(performance(ROC.mod7, "auc")@y.values) #0.7765372 lowest so far

pred.mod7 = predict(mod7, newdata = HlTestSet, type = "class") #prediction on test set
summary(pred.mod7) #176/nrow(HlTestSet) 9.4% Popular items


#Model 8: simple CART, without text data
mod8 = rpart(Popular ~ WordCount + Hour + SectionName + Weekday, data = NewsTrain, 
             method = "class")
prp(mod8) #in conclusion, adding text data didn't seem to be relevant to CART with std params.


#MODELING WITH RANDOM FORESTS
library(randomForest)
set.seed(1010)

#Data Pre-processing
NewsPop = rbind(NewsTrain[, c(1:8, 10:12)], NewsTest) #this needs to be done due to bug in pred RF
y.train = as.factor(NewsTrain$Popular)
x.train = head(NewsPop, nrow(NewsTrain))
x.test = tail(NewsPop, nrow(NewsTest))

#Model 9: RF without text data, including NewsDesk
mod9 = randomForest(y.train ~ WordCount + Hour + SectionName + Weekday + NewsDesk, 
                    data = x.train, importance = T)
varImpPlot(mod9)
pred.train9 = predict(mod9, type = "prob")[,2]
summary(pred.train9)

table(NewsTrain$Popular, pred.train9 >= 0.5)
(5230+762)/nrow(NewsTrain) #accuracy: 0.9173301
ROC.mod9 = prediction(pred.train9, y.train)
auc = as.numeric(performance(ROC.mod9, "auc")@y.values) #0.9352087

pred.mod9 = predict(mod9, newdata = x.test, type = "prob")[,2]
summary(pred.mod9)
 
#Make submission file for Kaggle:
Submission9 = data.frame(UniqueID = x.test$UniqueID, Probability1 = pred.mod9)
write.csv(Submission9, "Submission_mod9.csv", row.names=FALSE) #AUC 0.92024


#Model 10: RF without text data, excluding NewsDesk
set.seed(1010)
mod10 = randomForest(y.train ~ WordCount + Hour + SectionName + Weekday, data = x.train, 
                     importance = T)
varImpPlot(mod10)
pred.train10 = predict(mod10, type = "prob")[,2]
summary(pred.train10)
table(y.train, pred.train10 >= 0.5)
(5205+724)/nrow(NewsTrain) #accuracy: 0.9076852 slightly lower without NewsDesk
ROC.mod10 = prediction(pred.train10, y.train)
auc = as.numeric(performance(ROC.mod10, "auc")@y.values) #0.923021


#RANDOM FOREST INCLUDING TEXT DATA
#standard parameters (classification): mtry (sqrt(p)), nodesize = 1, ntree = 500
# Data Pre-processing
#HlPop = rbind(HlTrainSet[, c(1:42, 44:49)], HlTestSet) #this needs to be done due to bug in pred RF
HlPop = rbind(HlTrainSet[, c(1:16, 18:23)], HlTestSet)
y.hltrain = as.factor(HlTrainSet$Popular)
x.hltrain = head(HlPop, nrow(HlTrainSet))
x.hltest = tail(HlPop, nrow(HlTestSet))

AbsPop = rbind(AbsTrainSet[, c(1:101, 103:108)], AbsTestSet) #this needs to be done due to bug in pred RF
y.abstrain = as.factor(AbsTrainSet$Popular)
x.abstrain = head(AbsPop, nrow(AbsTrainSet))
x.abstest = tail(AbsPop, nrow(AbsTestSet))

#Model 11: RF with headline text at 99% sparsity, including NewsDesk
set.seed(1010)
mod11 = randomForest(y.hltrain ~. -UniqueID, data = x.hltrain, importance = T)
varImpPlot(mod11)
pred.train11 = predict(mod11, type = "prob")[,2]
summary(pred.train11)
table(y.hltrain, pred.train11 >= 0.5)
(5243+728)/nrow(HlTrainSet) #accuracy: 0.9141151
ROC.mod11 = prediction(pred.train11, y.hltrain)
auc = as.numeric(performance(ROC.mod11, "auc")@y.values) #0.9333806

pred.mod11 = predict(mod11, newdata = x.hltest, type = "prob")[,2]
summary(pred.mod11)

#Make submission file for Kaggle:
Submission11 = data.frame(UniqueID = x.hltest$UniqueID, Probability1 = pred.mod11)
write.csv(Submission11, "Submission_mod11.csv", row.names=FALSE) #AUC 0.92126 best so far!


#Model 12: RF with abstract text at 98.5% sparsity, including NewsData
set.seed(1010)
mod12 = randomForest(y.abstrain ~. -UniqueID, data = x.abstrain, importance = T)
varImpPlot(mod12)
pred.train12 = predict(mod12, type = "prob")[,2]
summary(pred.train12)
table(y.abstrain, pred.train12 >= 0.5)
(5262+711)/nrow(AbsTrainSet) #accuracy: 0.9144213
ROC.mod12 = prediction(pred.train12, y.abstrain)
auc = as.numeric(performance(ROC.mod12, "auc")@y.values) #0.9323759

pred.mod12 = predict(mod12, newdata = x.abstest, type = "prob")[,2]
summary(pred.mod12)

#Make submission file for Kaggle:
Submission12 = data.frame(UniqueID = x.abstest$UniqueID, Probability1 = pred.mod12)
write.csv(Submission12, "Submission_mod12.csv", row.names=FALSE) #AUC 0.92023 slightly worse!


#Model 13: same as model 12, but ntree = 701
set.seed(1010)
mod13 = randomForest(y.abstrain ~. -UniqueID, data = x.abstrain, importance = T, ntree = 701)
varImpPlot(mod13)
pred.train13 = predict(mod13, type = "prob")[,2]
summary(pred.train13)
table(y.abstrain, pred.train13 >= 0.5)
(5267+714)/nrow(AbsTrainSet) #accuracy: 0.9156461
ROC.mod13 = prediction(pred.train13, y.abstrain)
auc = as.numeric(performance(ROC.mod13, "auc")@y.values) #0.9324862

pred.mod13 = predict(mod13, newdata = x.abstest, type = "prob")[,2]
summary(pred.mod13) #lowest OOB error

#Make submission file for Kaggle:
Submission13 = data.frame(UniqueID = x.abstest$UniqueID, Probability1 = pred.mod13)
write.csv(Submission13, "Submission_mod13.csv", row.names=FALSE) #AUC 0.91961


#Model 14: RF with headline text at 98.5% sparsity, including NewsData, ntree = 701
set.seed(1010)
mod14 = randomForest(y.hltrain ~. -UniqueID, data = x.hltrain, importance = T, ntree = 701)
varImpPlot(mod14)
pred.train14 = predict(mod14, type = "prob")[,2]
summary(pred.train14)
table(y.hltrain, pred.train14 >= 0.5)
(5241+731)/nrow(HlTrainSet) #accuracy: 0.9142682
ROC.mod14 = prediction(pred.train14, y.hltrain)
auc = as.numeric(performance(ROC.mod14, "auc")@y.values) #0.9339284

pred.mod14 = predict(mod14, newdata = x.hltest, type = "prob")[,2]
summary(pred.mod14)

#Make submission file for Kaggle:
Submission14 = data.frame(UniqueID = x.hltest$UniqueID, Probability1 = pred.mod14)
write.csv(Submission14, "Submission_mod14.csv", row.names=FALSE) #AUC ?


#BOOSTING
library(gbm)
#parameters: d (depth) 1, 2, 4; lambda (shrinkage) 0.001 (default), 0.01, 0.1; 
#n.trees - requires cross-validation to optimize (start with 10000), it can overfit

#Model 15 - using Headline text at 98.5% sparsity
#Boosting with d = 1, lambda = 0.001, n.tree = 10000, bernoulli distrib for classification
mod15 = gbm(Popular ~. -UniqueID, data = HlTrainSet, distribution = "bernoulli", 
            n.trees = 10000, interaction.depth = 1)
summary(mod15)

pred.train15 = predict(mod15, n.trees = 10000, type = "response")
summary(pred.train15)
table(HlTrainSet$Popular, pred.train15 >= 0.5)
(5246+665)/nrow(HlTrainSet) #accuracy: 0.9049296
ROC.mod15 = prediction(pred.train15, HlTrainSet$Popular)
auc = as.numeric(performance(ROC.mod15, "auc")@y.values) #0.9221406



#Model 16 - same as 15 but changed d = 4 and ntree = 5000
mod16 = gbm(Popular ~. -UniqueID, data = HlTrainSet, distribution = "bernoulli", 
            n.trees = 5000, interaction.depth = 4)
summary(mod16)

pred.train16 = predict(mod16, n.trees = 5000, type = "response")
summary(pred.train16)
table(HlTrainSet$Popular, pred.train16 >= 0.5)
(5249+696)/nrow(HlTrainSet) #accuracy: 0.9101347
ROC.mod16 = prediction(pred.train16, HlTrainSet$Popular)
auc = as.numeric(performance(ROC.mod16, "auc")@y.values) #0.9424268

pred.mod16 = predict(mod16, newdata = HlTestSet, n.trees = 5000, type = "response")
summary(pred.mod16)

#Make submission file for Kaggle:
Submission16 = data.frame(UniqueID = HlTestSet$UniqueID, Probability1 = pred.mod16)
write.csv(Submission16, "Submission_mod16.csv", row.names=FALSE) #AUC



#Model 17 - same as 16 but changed shrinkage to 0.01 from 0.001
mod17 = gbm(Popular ~. -UniqueID, data = HlTrainSet, distribution = "bernoulli", 
            n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
summary(mod17)

pred.train17 = predict(mod17, n.trees = 5000, type = "response")
summary(pred.train17)
table(HlTrainSet$Popular, pred.train17 >= 0.5)
(5305+835)/nrow(HlTrainSet) #accuracy: 0.9399878
ROC.mod17 = prediction(pred.train17, HlTrainSet$Popular)
auc = as.numeric(performance(ROC.mod17, "auc")@y.values) #0.9709868

pred.mod17 = predict(mod17, newdata = HlTestSet, n.trees = 5000, type = "response")
summary(pred.mod17)

#Make submission file for Kaggle:
Submission17 = data.frame(UniqueID = HlTestSet$UniqueID, Probability1 = pred.mod17)
write.csv(Submission17, "Submission_mod17.csv", row.names=FALSE) #AUC 0.92034


#Model 18 - same as 17, but shrinkage now is 0.1
mod18 = gbm(Popular ~. -UniqueID, data = HlTrainSet, distribution = "bernoulli", 
            n.trees = 5000, interaction.depth = 4, shrinkage = 0.1)
summary(mod18)

pred.train18 = predict(mod18, n.trees = 5000, type = "response")
summary(pred.train18)
table(HlTrainSet$Popular, pred.train18 >= 0.5)
(5432+1048)/nrow(HlTrainSet) #accuracy: 0.9920392 overfit?
ROC.mod18 = prediction(pred.train18, HlTrainSet$Popular)
auc = as.numeric(performance(ROC.mod18, "auc")@y.values) #0.9989951

pred.mod18 = predict(mod18, newdata = HlTestSet, n.trees = 5000, type = "response")
summary(pred.mod18)

#Make submission file for Kaggle:
Submission18 = data.frame(UniqueID = HlTestSet$UniqueID, Probability1 = pred.mod18)
write.csv(Submission18, "Submission_mod18.csv", row.names=FALSE) #AUC


#Model 19 - same as 17 but changed depth to 1
mod19 = gbm(Popular ~. -UniqueID, data = HlTrainSet, distribution = "bernoulli", 
            n.trees = 5000, interaction.depth = 1, shrinkage = 0.01)
summary(mod19)

pred.train19 = predict(mod19, n.trees = 5000, type = "response")
summary(pred.train19)
table(HlTrainSet$Popular, pred.train19 >= 0.5)
(5254+683)/nrow(HlTrainSet) #accuracy: 0.90891
ROC.mod19 = prediction(pred.train19, HlTrainSet$Popular)
auc = as.numeric(performance(ROC.mod19, "auc")@y.values) #0.9367003


#Model 20 - same parameters as mod17, but excluding text data
mod20 = gbm(Popular ~ -UniqueID + WordCount + Hour + SectionName + Weekday + NewsDesk, 
            data = HlTrainSet,
            distribution = "bernoulli", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
summary(mod20)
pred.train20 = predict(mod20, n.trees = 5000, type = "response")
summary(pred.train20)
table(HlTrainSet$Popular, pred.train20 >= 0.5)
(5306+834)/nrow(HlTrainSet) #accuracy: 0.9399878
ROC.mod20 = prediction(pred.train20, HlTrainSet$Popular)
auc = as.numeric(performance(ROC.mod20, "auc")@y.values) #0.970011

pred.mod20 = predict(mod20, newdata = HlTestSet, n.trees = 5000, type = "response")
summary(pred.mod20)

#Make submission file for Kaggle:
Submission20 = data.frame(UniqueID = HlTestSet$UniqueID, Probability1 = pred.mod20)
write.csv(Submission20, "Submission_mod20.csv", row.names=FALSE) #AUC


#Submit models 14 (line 417), 16, 18 and 20.
#Did not try logistic regression with regularization (glmnet)
#Did not try optimizing boosting parameters (caret)
#However, in general, it seems text adds more noise than accuracy.
#Thus, I select models 11 and 9 (both RF) as best models (followed by 17, boosting)

# gbmGrid <-  expand.grid(interaction.depth = 13, n.trees = 10000, shrinkage = 0.001)
# nf  <- trainControl(method="cv", number=10, classProbs = TRUE, summaryFunction = twoClassSummary)
# gbmtr <- train(as.factor(Popular) ~. ,data = Train, method = "gbm",trControl = nf, 
#                tuneGrid=gbmGrid, metric ="ROC",verbose = T)

# install.packages('doParallel')
# library(doParallel)
# cl = makeCluster(detectCores())
# registerDoParallel(cl)
#For Mac you might want to look at doMC: http://topepo.github.io/caret/parallel.html
