#Analytics Edge Week 3 - Logistic Regression

#Here is a suggestion on how to split the data into training and test sets:
spl = sample(1:nrow(data), size=0.7 * nrow(data)) #puts 70% of obs into training set
train = data[spl,]
test = data[-spl,]

#P(y=1) if logit = -1:
1/(1+exp(1))

#PREDICTING POOR QUALITY OF HEALTH CARE

quality = read.csv("quality.csv")
names(quality)

library(caTools) #contains sample.split function which is useful for classification problems
#because it can split the data randomly but also maintaining the original proportion of each
#binary class
table(quality$PoorCare) #1 = poor care
#Baseline accuracy:
98/(98+33) #0.748 (poor care = 0)

set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75) #baseline accuracy
qualityTrain = subset(quality, split == T)
qualityTest = subset(quality, split == F)

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, 
                 family = binomial)

summary(QualityLog) #The coefficient value of StartedOnCombinationTRUE is positive, meaning 
#that positive values of the variable make the outcome of 1 (Poor Care) more likely.

# Make and analyze predictions on training set
predictTrain = predict(QualityLog, type="response") #"response" returns probabilities
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)

# Confusion matrix for threshold of 0.5
table(qualityTrain$PoorCare, predictTrain > 0.5)

# Sensitivity (inversely proportional to threshold value), and Specificity, respectively:
10/25 #Ratio of True Positive (Actual & Predicted = 1 / sum(Actual = 1)) i.e. TP/(TP+FN)
70/74 #Ratio of True Negative (Actual & Predicted = 0 / sum(Actual = 0)) i.e. TN/(TN+FP)
#Overall accuracy: (TP+TN)/nobs
#Overall error rate: (FP+FN)/nobs


library(ROCR) #create ROC curves
#A threshold needs to be determined to predict the binary response, so we use ROC curve to
#visualize the FP vs TP rates for each threshold value (specificity in x vs sensitivity in y).
#Choose threshold based on cost of failing to detect positives, versus costs of raising false 
#alarms.

# Prediction function
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
#prediction function is from the RORC package!

# Performance function (various measures, check ?performance)
ROCRperf = performance(ROCRpred, measure = "tpr", x.measure = "fpr")

# Plot ROC curve, add colors and threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
#note that threshold values go from 1 to zero. Cutoffs are the thresholds to print.

#Prediction on the test data
predictTest = predict(QualityLog, type="response", newdata = qualityTest)
ROCpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCpredTest, "auc")@y.values) #@ denotes OOP
#The AUC has the following interpretation: given a random patient from the dataset who 
#actually received poor care, and a random patient from the dataset who actually received 
#good care, the AUC is the perecentage of time that our model will classify which is which 
#correctly, or the probability that the classifier will score a randomly drawn positive sample
#higher than a randomly drawn negative sample. 

ROCperf = performance(ROCpredTest, "auc") #to understand the above syntax
str(ROCperf) #class @y.value is returned as a list


#PREDICTING THE RISK OF CORONARY HEART DISEASE - FRAMINGHAM STUDY

framingham = read.csv("framingham.csv")
# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

train = subset(framingham, split==TRUE)
test = subset(framingham, split==FALSE)

# Logistic Regression Model
framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)
summary(framinghamLog)

# Predictions on the test set
predictTest = predict(framinghamLog, type="response", newdata=test)

ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values) #0.742

# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)
(1069+11)/(1069+6+187+11) #Model accuracy: 0.8484
(1069+6)/(1069+6+187+11) #Baseline accuracy: 0.8444

#Try a model with less predictors (based on results from complete model)
mod_rev1 = glm(TenYearCHD ~ male + age + prevalentStroke + totChol + sysBP + glucose, 
               data = train, family=binomial)
summary(mod_rev1)

predictTest_1 = predict(mod_rev1, type="response", newdata=test)
ROC_rev1 = prediction(predictTest_1, test$TenYearCHD)
as.numeric(performance(ROC_rev1, "auc")@y.values) #0.743
table(test$TenYearCHD, predictTest_1 > 0.5)
(1118+11)/(1118+9+187+9) #Model accuracy: 0.8533
(1118+9)/(1118+9+197+9) #Baseline accuracy: 0.8455

ROCperf_1 = performance(ROC_rev1, measure = "tpr", x.measure = "fpr")
#attributes(ROCperf1)$y.values
plot(ROCperf_1, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
table(test$TenYearCHD, predictTest_1 > 0.2)


#PREDICTING THE WINNER OF US PRESIDENTIAL ELECTION

polling = read.csv("PollingData.csv")
summary(polling)
table(polling$Year) #data from 2004, 2008 and 2012 elections
#since nobs is small, and we want to predict the outcome for all states, we should not
#just remove the NA's but instead create multiple imputations for these missing values.
#We use the mice package to do so.

library(mice)
#let's first create a small dataframe, without the response variable "Republican", which
#will receive imputation values (all, including those without NA's)
simple = polling[c("Rasmussen", "SurveyUSA", "DiffCount", "PropR")]
summary(simple)

set.seed(144) #imputation generates random values
imputed = complete(mice(simple)) #5 rounds of imputation have been run
summary(imputed)
polling$Rasmussen = imputed$Rasmussen #replace NA's with imputed values
polling$SurveyUSA = imputed$SurveyUSA
summary(polling) #there are no longer missing values!

Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)
table(Train$Republican) #baseline predicts Republican even when Democrat wins (weak baseline)

#Let's use sign() to create a better baseline model:
table(sign(Train$Rasmussen)) #1 for Rep, -1 for Dem, 0 for inconclusive
table(Train$Republican, sign(Train$Rasmussen)) #check smart baseline model (only 4% mistakes)

#Let's check for multicollinearity (expected as predictors measure similar data)
cor(Train[c("Rasmussen", "SurveyUSA", "DiffCount", "PropR", "Republican")])
#since the predictors are highly correlated with the response, we choose only one, 
#with the highest correlation with the response (PropR), to do a good job in the model.

mod1 = glm(Republican ~ PropR, data = Train, family = binomial)
summary(mod1)
pred1 = predict(mod1, type = "response")
table(Train$Republican, pred1 >= 0.5) #check our model in relation to the baseline
#model is just as good as baseline (4% error)

#Let's see if we can improve our model by adding another variable. We look back into the
#correlation matrix and choose a pair of variables with the least correlation 
mod2 = glm(Republican ~ SurveyUSA + DiffCount, data = Train, family = binomial)
summary(mod2) #AIC has slightly higher value (weaker model)
pred2 = predict(mod2, type = "response")
table(Train$Republican, pred2 >= 0.5) #still 4% error

mod3 = glm(Republican ~ Rasmussen + DiffCount, data = Train, family = binomial)
summary(mod3) #lowest AIC (better)
pred3 = predict(mod3, type = "response")
table(Train$Republican, pred3 >= 0.5) #error is down to 3% (not impressive but better)

mod4 = glm(Republican ~ . - State - Year, data = Train, family = binomial)
summary(mod4) #highest AIC
pred4 = predict(mod4, type = "response")
table(Train$Republican, pred4 >= 0.5) #full model error is 3%, no significant variable

#Let's work with mod2. Let's check the baseline model on the test data:
table(Test$Republican, sign(Test$Rasmussen))

#Let's now make predictions:
TestPrediction = predict(mod2, newdata = Test, type = "response")
#for this case (predicting the election winner), a cutoff of 0.5 is adequate.
table(Test$Republican, TestPrediction >= 0.5) #only 1 mistake out of 45 observations.

#Let's check the data for the prediction mistake:
subset(Test, TestPrediction >= 0.5 & Republican == 0) #state of Florida


###############################################################################
#        HOMEWORK - WEEK 3 LOGISTIC REGRESSION
###############################################################################

#Predicting Music Popularity
#Part 1
songs = read.csv("songs.csv")
summary(songs)
nrow(subset(songs, songs$year == 2010))
nrow(subset(songs, songs$artistname == "Michael Jackson"))
length(grep("^Michael Jackson$", songs$artistname)) #using anchors
length(grep("\\bMichael Jackson\\b", songs$artistname)) #same as above
subset(songs, songs$artistname == "Michael Jackson" & songs$Top10 == 1)
sort(unique(songs$timesignature))
table(songs$timesignature)
songs$songtitle[which.max(songs$tempo)]

#Part 2
SongsTrain = subset(songs, songs$year <= 2009)
SongsTest = subset(songs, songs$year > 2009)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data = SongsTrain, family = binomial)
summary(SongsLog1)

#Part 3
cor(SongsTrain$loudness, SongsTrain$energy )

SongsLog2 = glm(Top10 ~ . - loudness, data = SongsTrain, family = binomial)
#note: subtracting the variable from the model formula only works if removing numeric variables
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data = SongsTrain, family = binomial)
summary(SongsLog3)

#Part 4
pred3 = predict(SongsLog3, type = "response", newdata = SongsTest)
table(SongsTest$Top10, pred3 > 0.45)
(309+19)/(nrow(SongsTest)) #model accuracy: 0.8793566

table(SongsTest$Top10) #baseline accuracy: 314/(314+59)=0.8418231 for most frequent outcome (0)
#It seems model 3 gives a small improvement over baseline model, but does it create an edge?

table(SongsTest$Top10, pred3 > 0.45)

#Sensitivity: Ratio of True Positive (Actual & Predicted = 1 / sum(Actual = 1)) i.e. TP/(TP+FN)
19/(19+40) #0.3220339

#Specificity: Ratio of True Negative (Actual & Predicted = 0 / sum(Actual = 0)) i.e. TN/(TN+FP)
309/(309+5) #0.9840764


#Predicting Parole Outcomes
#Part 1
parole = read.csv("parole.csv")
summary(parole)
table(parole$violator)

#Part 2
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

#Part 3
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

#Part 4
parole.mod1 = glm(violator ~ ., data = train, family = binomial)
summary(parole.mod1)
#Concerning the multiple.offenses variable coefficient:
#model predicts that a parolee who committed multiple offenses has 5.01 (e ^ coef) times 
#higher odds of being a violator than a parolee who did not commit multiple offenses 
#but is otherwise identical.

#ln(odds of A) = ln(odds of B) + 1.61
#exp(ln(odds of A)) = exp(ln(odds of B) + 1.61)
#exp(ln(odds of A)) = exp(ln(odds of B)) * exp(1.61)
#odds of A = exp(1.61) * odds of B
#odds of A= 5.01 * odds of B

#Question 4.3: predictions for male individual, of white race, aged 50 years at prison release, 
#from the state of Maryland, served 3 months, had a maximum sentence of 12 months, did not 
#commit multiple offenses, and committed a larceny:

#1. odds that above individual is a violator:
odds = exp(-4.2411574 + 0.3869904 + 0.8867192 + (-0.0001756*50) + (-0.1238867*3) + (0.0802954*12) +
               0.6837143) #0.1825687

0.1543832/(1-0.1543832) #0.1825687

#2. probability that above individual is a violator:
1/(1 + exp(-(-4.2411574 + 0.3869904 + 0.8867192 + (-0.0001756*50) + (-0.1238867*3) + 
                 (0.0802954*12) + 0.6837143))) #0.1543832

#Part 5
pred = predict(parole.mod1, type = "response", newdata = test)
max(pred)

table(test$violator, pred > 0.5)

#Sensitivity: TP/(TP+FN)
12/(12+11) #0.5217391

#Specificity: TN/(TN+FP)
167/(167+12) #0.9329609

#Accuracy:
(167+12)/202 #0.8861386

table(test$violator)
179/(179+23) #0.8861386: baseline has same accuracy as the model built

#The model at cutoff 0.5 has 12 false positives and 11 false negatives, while the baseline 
#model has 0 false positives and 23 false negatives. Because a parole board is likely to 
#assign more cost to a false negative, the model at cutoff 0.5 is likely of value to the 
#board. The parole board would likely benefit from decreasing the logistic regression 
#cutoffs, which decreases the false negative rate while increasing the false positive rate.

table(test$violator, pred > 0.3)
14/(14+9) #sensitivity = 0.6086957
160/(160+14) #specificity = 0.9195402
174/202 #accuracy = 0.8613861

library(ROCR)
ROCpred = prediction(pred, test$violator)
auc = as.numeric(performance(ROCpred, "auc")@y.values)
auc #0.8945834


#Part 6 - BIAS IN OBSERVATIONAL DATA
#The dataset contains all individuals released from parole in 2004, either due to completing 
#their parole term or violating the terms of their parole. However, it does not contain 
#parolees who neither violated their parole nor completed their term in 2004, causing 
#non-violators to be underrepresented. This is called "selection bias" or "selecting on the 
#dependent variable," because only a subset of all relevant parolees were included in our 
#analysis, based on our dependent variable in this analysis (parole violation). 
#How could we improve our dataset to best address selection bias?
#We should use a dataset tracking a group of parolees from the start of their parole until 
#either they violated parole or they completed their term.


#Predicting loan repayment
#Part 1
loan = read.csv("loans.csv")
table(loan$not.fully.paid)
1533/(8045+1533)

summary(loan)

#Code for imputation (however, will use provided file)
# library(mice)
# set.seed(144)
# vars.for.imputation = setdiff(names(loan), "not.fully.paid")
# imputed = complete(mice(loan[vars.for.imputation]))
# loan[vars.for.imputation] = imputed

loans_imputed = read.csv("loans_imputed.csv")

#Part 2
set.seed(144)
split = sample.split(loans_imputed$not.fully.paid, SplitRatio = 0.7)
train = subset(loans_imputed, split == TRUE)
test = subset(loans_imputed, split == FALSE)

loan.mod = glm(not.fully.paid ~ ., data = train, family = binomial)
summary(loan.mod)

num = sum(as.vector(coef(loan.mod))[-13])
(num + -9.317e-03 * 700) - (num + -9.317e-03 * 710) #same as -9.317e-03 * (700 - 710)
oddsA = exp(num + -9.317e-03 * 700)
oddsB = exp(num + -9.317e-03 * 710)
oddsA/oddsB #1.097648, same as exp(-9.317e-03 * 700)/exp(-9.317e-03 * 710)

predicted.risk = predict(loan.mod, type = "response", newdata = test)
table(test$not.fully.paid, predicted.risk > 0.5)
2403/2873 #accuracy: 0.8364079

table(test$not.fully.paid)
2413/2873 #baseline accuracy: 0.8398886

ROCpred = prediction(predicted.risk, test$not.fully.paid)
auc = as.numeric(performance(ROCpred, "auc")@y.values) #0.6720995

#Part 3
loan.mod2 = glm(not.fully.paid ~ int.rate, data = train, family = binomial)
summary(loan.mod2)
cor(test$int.rate, test$fico)

pred.risk = predict(loan.mod2, type = "response", newdata = test)
max(pred.risk) #0.426624
table(test$not.fully.paid, pred.risk > 0.5)
ROCpred2 = prediction(pred.risk, test$not.fully.paid)
auc = as.numeric(performance(ROCpred2, "auc")@y.values) #0.6239081

#Part 4
10*exp(0.06 * 3)

#Part 5
test$profit = exp(test$int.rate*3) - 1 #assumes investment is of $1, all data is for 3 years
test$profit[test$not.fully.paid == 1] = -1 #turn non-paid loans into worst case, $-1 profit
max(test$profit) * 10

#Part 6
highInterest = subset(test, test$int.rate >= 0.15)
mean(highInterest$profit)
table(highInterest$not.fully.paid)
110/(327+110) #0.2517162

pred.risk = predict(loan.mod, type = "response", newdata = highInterest)
highInterest$pred.risk = pred.risk
cutoff = sort(highInterest$pred.risk, decreasing=FALSE)[100] #0.1763305

selectedLoans = subset(highInterest, highInterest$pred.risk <= cutoff)
dim(selectedLoans) #100 x 16
sum(selectedLoans$profit) #31.27825
table(selectedLoans$not.fully.paid) #19




