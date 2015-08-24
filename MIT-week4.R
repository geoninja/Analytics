#MIT class week 4 - Trees

#setwd("C:/Users/pa120604/Downloads") #change this accordingly

#Part 1: SCOTUS decisions predictions
stevens = read.csv("stevens.csv")
summary(stevens)
table(stevens$Reverse)
309/(309+257) #0.546 (baseline)

#Let's randomly split the data into training and test sets
library(caTools)
set.seed(3000)
split = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)

#Let's create CART model using rpart (recursive partitioning and regressions trees) library
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + 
                        Unconst, data = Train, method="class", minbucket = 25)

prp(StevensTree) #plot tree *lower minbucket(number of leaves in the last node) = more splits

# Let's now make predictions
PredictCART = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART)
(41+71)/(41+36+22+71) #0.659 much better than baseline

# ROC curve
library(ROCR)

PredictROC = predict(StevensTree, newdata = Test)
head(PredictROC, 10) #column 2 is for pred = 1

pred = prediction(PredictROC[ ,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)

as.numeric(performance(pred, "auc")@y.values) #AUC = 0.693 (not very high)

#Let's now use random forests instead of CART
library(randomForest)

set.seed(100)
set.seed(200)

Train$Reverse = as.factor(Train$Reverse) # Convert outcome to factor
Test$Reverse = as.factor(Test$Reverse)

StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + 
                                 LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Finally, let's make predictions
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(40+74)/(40+37+19+74) #0.67 (no seed set), slightly better accuracy than CART
(43+74)/170 #seed 100: 0.6882353
(44+76)/170 #seed 200: 0.7058824


#Let's use cross validation to choose cp, the complexity parameter for the CART model in rpart
library(caret)
library(e1071)

# Define cross-validation experiment
numFolds = trainControl(method = "cv", number = 10)
#note: trainControl is a caret function that allows more detailed parameters for the
#computation of the train function

cpGrid = expand.grid(cp = seq(0.01,0.5,0.01))

# Perform the cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
      data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid ) #caret package

# Create a new CART model
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + 
                          Unconst, data = Train, method = "class", cp = 0.18)

# Make predictions
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")
table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64) #0.7235294 optimum accuracy with 10 fold cv.

prp(StevensTreeCV) #only one split in the optimized model!


#Part 2: predicting health care costs
Claims = read.csv("ClaimsData.csv") #458005 x 16, contains response for 2008 and 2009

# Percentage of patients in each cost bucket
table(Claims$bucket2009)/nrow(Claims) #67% in bucket 1, only 0.5% in bucket 5 (like me!)

# Let's split the data
library(caTools)
set.seed(88)
spl = sample.split(Claims$bucket2009, SplitRatio = 0.6) #arbitrary ratio, large data
ClaimsTrain = subset(Claims, spl==TRUE)
ClaimsTest = subset(Claims, spl==FALSE)

tbl = table(ClaimsTest$bucket2009, ClaimsTest$bucket2008) #5x5 matrices
sum(diag(tbl))/nrow(ClaimsTest) #baseline accuracy: 0.6838135

table(ClaimsTest$bucket2009)
122978/nrow(ClaimsTest) #0.67127 accuracy
tbl.base = as.matrix((table(ClaimsTest$bucket2009)))
#tbl.base[1,1]/sum(tbl.base) #0.67127 accuracy

mean(ClaimsTrain$age) #72.63773 - Medicare data!
table(ClaimsTrain$diabetes)
104672/nrow(ClaimsTrain) #0.3808983 percentage of patients with diabetes in training set

#Let's build the penalty matrix (weights to penalize FN for actual high buckets with low 
#bucket predicted)
PenaltyMatrix = matrix(c(0:4,2,0:3,4,2,0:2,6,4,2,0,1,8,6,4,2,0), byrow=T, nrow=5)
PenaltyMatrix

# Penalty Error of Baseline Method
m = as.matrix(tbl)*PenaltyMatrix #element-wise matrix multiplication
sum(m)/nrow(ClaimsTest) #penalized baseline accuracy: 0.7386055
#the above is the same as (110138 + 10721 + 2774 + 1539 + 104)/nrow(ClaimsTest)

new.penalty = matrix(c(0, 2, 4, 6, 8), ncol = 1)
sum(tbl.base * new.penalty)/nrow(ClaimsTest) #1.044301


# CART model
#library(rpart)
#library(rpart.plot)

ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + 
                       diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + 
                       bucket2008 + reimbursement2008, data = ClaimsTrain, method = "class", 
                   cp = 0.00005) #optimum cp previously checked

#prp(ClaimsTree) #very bushy tree that takes a while to load

# Let's make predictions
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")

table(ClaimsTest$bucket2009, PredictTest)
(114141 + 16102 + 118 + 201 + 0)/nrow(ClaimsTest) #0.7126669 accuracy (higher than baseline)

table(PredictTest)/nrow(ClaimsTest) #predictions per bucket

# Penalty Error
tbl.pe = as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix
sum(tbl.pe)/nrow(ClaimsTest) #0.7578902 penalty error (slightly higher than baseline)


# New CART model with loss matrix
ClaimsTree2 = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + 
                       diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + 
                       bucket2008 + reimbursement2008, data = ClaimsTrain, method = "class", 
                   cp = 0.00005, parms = list(loss = PenaltyMatrix))

# Redo predictions and penalty error
PredictTest2 = predict(ClaimsTree2, newdata = ClaimsTest, type = "class")

table(ClaimsTest$bucket2009, PredictTest2)
(94310 + 18942 + 4692 + 636 + 2)/nrow(ClaimsTest) #0.6472746 accuracy (lower than baseline)

table(PredictTest2)/nrow(ClaimsTest) #predictions per bucket

sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest2))*PenaltyMatrix)/nrow(ClaimsTest)
#0.6418161 penalty error (much lower than baseline)

