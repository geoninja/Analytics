#MIT Homework week 5  - Text Analytics

#Detecting Vandalism on Wikipedia
#Part1
wiki = read.csv("wiki.csv", stringsAsFactors = F)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)


library(tm)
library(SnowballC)
#pre-process Added words
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded #3876 docs x 6675 terms

sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

#pre-process Removed words
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved #3876 docs x 6675 terms

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
ncol(wordsRemoved)

#merge data, then split into training and test sets
wikiWords = cbind(wordsAdded, wordsRemoved) #3876 docs x 328 (166 + 162) terms
wikiWords$Vandal = wiki$Vandal #3876 x 329

set.seed(123)
library(caTools)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiTrain = subset(wikiWords, split == T)
wikiTest = subset(wikiWords, split == F)

table(wikiTest$Vandal)
618/(618+545) # baseline accuracy (not vandal): 0.5313844

#CART model
library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal ~., data = wikiTrain, method = "class") #default minbucket and cp
predictCART = predict(wikiCART, newdata = wikiTest, type = "class") #default threshold 0.5
table(wikiTest$Vandal, predictCART) #actual vs predicted
(618+11)/(618+11+534) #CART accuracy: 0.5408426

prp(wikiCART) #plot sentiment analysis tree

#Part 2
wikiWords2 = wikiWords #create a copy of the dataset
#since CART didn;t work very well (words were not predictive of vandalism), let's be more
#specific by aplying some domain knowledge. Let's check whether an added link to a
#promotional or irrelevant website (identified by the presence of the word "http") is
#a significant indicator of vandalism.

#Add column with the result of grep search "http" (True if found in doc).
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed = TRUE), 1, 0)
table(wikiWords2$HTTP)

set.seed(123)
wikiTrain2 = subset(wikiWords2, split == TRUE) #note we did NOT recompute split
wikiTest2 = subset(wikiWords2, split == FALSE)

#let's try CART again
wikiCART2 = rpart(Vandal ~., data = wikiTrain2, method = "class") #default minbucket and cp
prp(wikiCART2)
predictCART2 = predict(wikiCART2, newdata = wikiTest2, type = "class") #default threshold 0.5
table(wikiTest2$Vandal, predictCART2) #actual vs predicted
(609+57)/(609+9+488+57) #accuracy: 0.5726569 thus http seems to be a better indicator

#Another idea is that perhaps the number of words added or removed, rather than the words
#themselves, may be good indicators of vandalism. Let's check this by adding columns to
#the dataframe (dtm matrix) with the sum of the words added and removed:
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
summary(wikiWords2$NumWordsAdded) #mean = 4.05 (from 0 to 259)

#let's try CART once again
set.seed(123)
wikiTrain3 = subset(wikiWords2, split == TRUE) #note we did NOT recompute split
wikiTest3 = subset(wikiWords2, split == FALSE)

wikiCART3 = rpart(Vandal ~., data = wikiTrain3, method = "class") #default minbucket and cp
prp(wikiCART3)
predictCART3 = predict(wikiCART3, newdata = wikiTest3, type = "class") #default threshold 0.5
table(wikiTest3$Vandal, predictCART3) #actual vs predicted
(514+248)/(514+248+297+104) #accuracy: 0.6552021

#Part 3 - using metadata (Minor and Loggedin variables)
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

#let's build a CART model for the dataset with the metadata
set.seed(123)
wikiTrain4 = subset(wikiWords3, split == TRUE) #note we did NOT recompute split
wikiTest4 = subset(wikiWords3, split == FALSE)

wikiCART4 = rpart(Vandal ~., data = wikiTrain4, method = "class") #default minbucket and cp
prp(wikiCART4)
predictCART4 = predict(wikiCART4, newdata = wikiTest4, type = "class") #default threshold 0.5
table(wikiTest4$Vandal, predictCART4)
(595+241)/(595+23+304+241) #accuracy = 0.7188306 Loggedin is a significant predictor


#Automating Publication Reviews in Medicine
#Part1
trials = read.csv("clinical_trial.csv", stringsAsFactors = F)
summary(trials)

which.max(nchar(trials$abstract)) #664
nchar(trials$abstract)[664] #3708
max(nchar(trials$abstract)) #same as above, 3708, one liner

length(trials$abstract[nchar(trials$abstract) == 0]) #112
table(nchar(trials$abstract) == 0) #gets same result as above

which.min(nchar(trials$title)) #1258
trials$title[1258]

#Part 2
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

corpusTitle = tm_map(corpusTitle, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmTitle #1860 x 2833
dtmAbstract #1860 x 12224

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
dtmTitle #1860 x 31
dtmAbstract #1860 x 335

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

which.max(colSums(dtmAbstract)) #patient (212)

#Part 3
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial
dim(dtm) #1860 x 367

set.seed(144)
library(caTools)
split = sample.split(dtm$trial, SplitRatio = 0.7)
train = subset(dtm, split == T)
test = subset(dtm, split == F)

table(train$trial)
730/(730+572) #accuracy: 0.5606759

library(rpart)
library(rpart.plot)

trialCART = rpart(trial ~., data = train, method = "class")
prp(trialCART)
predTrain = predict(trialCART)
max(predTrain[,2]) #0.8718861
probTrain = predTrain[,2]
table(train$trial, probTrain >= 0.5)
(631+441)/(99+131+441+631) #0.8233487 training model accuracy at 50% threshold
# Sensitivity: ratio of True Pos (Actual & Predicted = 1 / sum(Actual = 1)) i.e. TP/(TP+FN)
441/(441+131) # 0.770979
# Specificity: ratio of True Neg (Actual & Predicted = 0 / sum(Actual = 0)) i.e. TN/(TN+FP) 
631/(631+99) # 0.8643836

#Part 4
predTest = predict(trialCART, newdata = test)
probTest = predTest[,2]
table(test$trial, probTest >= 0.5) #a lower threshold is better since FN are more costly
(261+162)/nrow(test) #accuracy: 0.7580645 50% threshold, predicting that result is a trial  

library(ROCR)
ROCpredTest = prediction(probTest, test$trial)
auc = as.numeric(performance(ROCpredTest, "auc")@y.values) 
auc #0.8371063


#Filtering Spam Emails
#Part 1
emails = read.csv("emails.csv", stringsAsFactors = F)
table(emails$spam)
head(emails$text)
max(nchar(emails$text)) #43952
which.min(nchar(emails$text)) #13, row 1992

#Part 2
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, 0.95)
spdtm

emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))

ham = subset(emailsSparse, emailsSparse$spam == 0)
hfreq = which(colSums(ham) >= 5000)
names(ham[,hfreq]) #Q2.4

spam = subset(emailsSparse, emailsSparse$spam == 1)
sfreq = which(colSums(spam) >= 1000)
names(spam[,sfreq]) #"spam" is not a word but the name of the last column (col 331)

#Part 3
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split == T)
test = subset(emailsSparse, split == F)


spamLog = glm(spam ~., data = train, family = "binomial") #overfit messages
summary(spamLog)
predTrainLog = predict(spamLog, newdata = train, type = "response")
summary(predTrainLog)
table(train$spam, predTrainLog >= 0.5)
(3052+954)/(3052+954+4) #accuracy: 0.9990025
length(subset(predTrainLog, predTrainLog <= 0.00001)) #3046
length(subset(predTrainLog, predTrainLog >= 0.99999)) #954
table(predTrainLog >= 0.00001 & predTrainLog <= 0.99999) #10
library(ROCR)
ROCpredTrain = prediction(predTrainLog, train$spam)
auc = as.numeric(performance(ROCpredTrain, "auc")@y.values) #0.9999959


library(rpart)
library(rpart.plot)
spamCART = rpart(spam ~., data = train, method = "class")
predTrainCART = predict(spamCART)[,2] #prediction on the training set
summary(predTrainCART)
prp(spamCART)
table(train$spam, predTrainCART >= 0.5)
(2885+894)/nrow(train) #accuracy: 0.942394
ROCpredCART = prediction(predTrainCART, train$spam)
auc = as.numeric(performance(ROCpredCART, "auc")@y.values) #0.9696044


library(randomForest)
set.seed(123)
spamRF = randomForest(spam ~., data = train)
predTrainRF = predict(spamRF, type = "prob")[,2]
summary(predTrainRF)
table(train$spam, predTrainRF >= 0.5)
(3013+914)/nrow(train) #accuracy: 0.9793017
ROCpredRF = prediction(predTrainRF, train$spam)
auc = as.numeric(performance(ROCpredRF, "auc")@y.values) #0.9979116

#Part 4
predTestLog = predict(spamLog, newdata = test, type = "response")
table(test$spam, predTestLog >= 0.5)
(1257+376)/nrow(test) #accuracy: 0.9505239
ROCpredTest = prediction(predTestLog, test$spam)
auc = as.numeric(performance(ROCpredTest, "auc")@y.values) #0.9627517

predTestCART = predict(spamCART, newdata = test)
table(test$spam, predTestCART[,2] >= 0.5)
(1228+386)/nrow(test) #accuracy: 0.9394645
ROCpredCART = prediction(predTestCART[,2], test$spam)
auc = as.numeric(performance(ROCpredCART, "auc")@y.values) #0.963176

predTestRF = predict(spamRF, newdata = test, type = "prob")
table(test$spam, predTestRF[,2] >= 0.5)
(1290+386)/nrow(test) #accuracy: 0.975553
ROCpredRF = prediction(predTestRF[,2], test$spam)
auc = as.numeric(performance(ROCpredRF, "auc")@y.values) #0.9975656

#Part 6
#Document term matrix has documents (in this case, emails) as its rows, terms (in this case,
#word stems) as its columns, and frequencies as its values

wordCount = rowSums(as.matrix(dtm)) #compute number of words in each (5728) email
hist(wordCount, breaks = 50)
hist(log(wordCount))

emailsSparse$logWordCount = log(wordCount)
boxplot(emailsSparse$logWordCount ~ emailsSparse$spam, names = c("ham", "spam"))
#boxplot(y ~ groupby)
tapply(emailsSparse$logWordCount, emailsSparse$spam, sum)
tapply(emailsSparse$logWordCount, emailsSparse$spam, mean)

train2 = subset(emailsSparse, split == T)
test2 = subset(emailsSparse, split == F)

spam2CART = rpart(spam ~., data = train2, method = "class")
prp(spam2CART)
pred2CART = predict(spam2CART, newdata = test2)
table(test2$spam, pred2CART[,2] >= 0.5)
(1214+384)/nrow(test2) #accuracy = 0.9301513
ROCpred2CART = prediction(pred2CART[,2], test2$spam)
auc = as.numeric(performance(ROCpred2CART, "auc")@y.values) #0.9582438

set.seed(123)
spam2RF = randomForest(spam ~., data = train2)
predTest2RF = predict(spam2RF, newdata = test2, type = "prob")
table(test2$spam, predTest2RF[,2] >= 0.5) 
(1296+383)/nrow(test2) #accuracy: 0.9772992
ROCpred2RF = prediction(predTest2RF[,2], test2$spam)
auc = as.numeric(performance(ROCpred2RF, "auc")@y.values) #0.9980905

#"RTextTools", "tau", "RWeka", and "textcat" packages are good tools for n-gram text analysis.


