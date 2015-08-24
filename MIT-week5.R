#MIT class week 5 - Text Analytics

#Analyzing tweets

tweets = read.csv("tweets.csv", stringsAsFactors = F) #1181 x 2
#additional arg is needed for text analysis, so the text is read properly.
#Avg is the tagged sentiment for each tweet made by humans (Amazon Mechanical Turk)

tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)
library(tm)
library(SnowballC)

#Let's turn the Tweet variable of the tweets dataset into a corpus:
corpus = Corpus(VectorSource(tweets$Tweet))
corpus[[1]] #shows first tweet in the corpus

#Now let's turn the entire corpus to lower case, then remove all punctuation:
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument) #additional line due to changes in tm package

corpus = tm_map(corpus, removePunctuation)
corpus[[1]]

#Let's now remove stopwords
stopwords("english")[1:10] #shows some of the words in the database
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]

#Finally, we do stemming:
corpus = tm_map(corpus, stemDocument)
corpus[[1]]

#Let's now create a bag of words
frequencies = DocumentTermMatrix(corpus) #create Docum x Terms matrix
frequencies #matrix with 1181 docs and 3289 words

inspect(frequencies[1000:1005, 505:515]) #visualize matrix for given range of docs and words
#we see the word cheer appears once in one document (1005)

findFreqTerms(frequencies, lowfreq = 20) #find terms (words) with freq >= 20
#56 words fall nto this category out of 3289, this means that only a sparse number of terms
#are considered meaningful, so we create a sparse matrix to facilitate computation, by
#removing the sparse terms using a threshold of 0.995, i.e. keeping only the terms
#that appear in about 0.5% or more of the tweets, or in about 6 or more tweets.

sparse = removeSparseTerms(frequencies, 0.995)
sparse #there are only 309 terms in the sparse matrix

#Now, let's convert our sparse matrix in a dataframe
tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse)) 
#fix colnames that start with numbers so R can process it. Always do this with text analytics!

tweetsSparse$Negative = tweets$Negative #adds variable to df

#Let's split the data:
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split == T)
testSparse = subset(tweetsSparse, split == F)

#Now the data is ready for modeling!
library(rpart) #let's build a CART model
library(rpart.plot)

tweetCART = rpart(Negative ~., data = trainSparse, method = "class")
#uses default minbucket and cp
prp(tweetCART) #plot our sentment analysis tree

predictCART = predict(tweetCART, newdata = testSparse, type = "class")
table(testSparse$Negative, predictCART) #actual vs predicted
(294+18)/(294+6+37+18) #accuracy: 0.8788732

table(testSparse$Negative) #baseline where all predicitions are negative
300/355 #baseline accuracy: 0.8450704


#Let's now see how Random Forests do to predict negative response to tweets:
library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~., data = trainSparse) #uses default parameters
#it takes a while because data has over 300 independent variables (words)

predictRF = predict(tweetRF, newdata = testSparse)
table(testSparse$Negative, predictRF) #actual vs predicted
(293+21)/355 #accuracy: 0.884507 slightly better than CART. Using optimal cp for CART would
#make CART's accuracy closer to RF, and preferred due to better interpretability of CART.


#Finally, let's build a logistic regression model for comparison
tweetLog = glm(Negative ~., data = trainSparse, family = binomial)
predictLog = predict(tweetLog, newdata = testSparse, type = "response")
table(testSparse$Negative, predictLog >= 0.5)
(253+32)/(253+47+23+32) #accuracy 0.8028169 much lower than baseline!!



#Recitation: Enron emails (energy bid and schedule)

emails = read.csv("energy_bids.csv", stringsAsFactors = F)

#variable responsive refers to a legal parlance in eDiscovery (positive search result)
emails$email[1]
emails$responsive[1] #0

emails$email[2]
emails$responsive[2] #1

table(emails$responsive) #small portion (~16%) is responsive (typical)

#Let's proceed to pre-process the data using the tm package
#library(tm)
corpus = Corpus(VectorSource(emails$email))
corpus[[1]]

corpus = tm_map(corpus, tolower) #makes everything lower case
corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation) #remove punctuation
corpus = tm_map(corpus, removeWords, stopwords("english")) #remove stopwords
corpus = tm_map(corpus, stemDocument) #stems the document
corpus[[1]]

#Let's create the dataframe
dtm = DocumentTermMatrix(corpus)
dtm #21735 terms

dtm = removeSparseTerms(dtm, 0.97)
dtm #down to 788 terms

labeledTerms = as.data.frame(as.matrix(dtm))
labeledTerms$responsive = emails$responsive #855 x 789 sparse matrix

#Let's now use the data to build models
#library(caTools)
set.seed(144)
spl = sample.split(labeledTerms$responsive, SplitRatio = 0.7)
train = subset(labeledTerms, spl == T)
test = subset(labeledTerms, spl == F)

#library(rpart)
#library(rpart.plot)
emailCART = rpart(responsive ~., data = train, method = "class")
prp(emailCART)

pred = predict(emailCART, newdata = test)
pred[1:10, ]
pred.prob = pred[, 2]
table(test$responsive, pred.prob >= 0.5)
(195+25)/(195+20+17+25) #accuracy = 0.8560311

table(test$responsive)
215/(215+42) #baseline accuracy = 0.8365759
#small improvements in accuracy are typical for unbalanced (skewed) datasets.

#let's use ROC to choose threshold and analyze cost of TP vs FP
library(ROCR)
predROCR = prediction(pred.prob, test$responsive)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = T) #probably reasonable threshold at ~ 0.15
performance(predROCR, "auc")@y.values #auc = 0.7936323


