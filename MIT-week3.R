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





