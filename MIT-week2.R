#Week 2 lecture - LINEAR REGRESSION

# Wine Data

#Wine Quality and Price Prediction
wine = read.csv("wine.csv")
names(wine)
model1 = lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(model1)
cor(wine)
cor(wine$HarvestRain, wine$WinterRain)

model4 = lm(Price ~ HarvestRain + WinterRain + AGST + Age, data = wine)
wine_test = read.csv("Wine_test.csv")
str(wine_test)
predict_test = predict(model4, newdata = wine_test)
#manually calculate the R squared:
#Sum of Squared Errors
SSE = sum((wine_test$Price - predict_test) ^ 2)
#Total Sum of Squares
SST = sum((wine_test$Price - mean(wine$Price)) ^ 2)
Rsq = 1 - SSE/SST


# Baseball Data

#Moneyball - Baseball Predictions
baseball = read.csv("baseball.csv")
moneyball = subset(baseball, Year < 2002)
moneyball$RD = moneyball$RS - moneyball$RA #run difference b/w runs scored and runs allowed
plot(moneyball$RD, moneyball$W) #runs difference vs wins
WinsReg = lm(W ~ RD, data = moneyball)
summary(WinsReg)

RunsReg = lm(RS ~ OBP + SLG + BA, data = moneyball) #on base percentage, slugging percentage,
#and batting average to predict runs scored. All 3 variables are collinear.
summary(RunsReg) #Runs in the regular season (not the playoffs)
RunsReg = lm(RS ~ OBP + SLG, data = moneyball) #removed batting average as not significant
RunsAllow = lm(RA ~ OOBP + OSLG, data = moneyball)
summary(RunsAllow)

teamRank = c(1, 2, 3, 3, 4, 4, 4, 4, 5, 5)
wins2012 = c(94, 88, 95, 88, 93, 94, 98, 97, 93, 94)
wins2013 = c(97, 97, 92, 93, 92, 96, 94, 96, 92, 90)
cor(teamRank, wins2012)
cor(teamRank, wins2013)


###############################################################################
#        HOMEWORK - WEEK 2 LINEAR REGRESSION
###############################################################################

#PROBLEM SET 1: CLIMATE CHANGE
clima = read.csv("climate_change.csv")
summary(clima)
train = subset(clima, clima$Year <= 2006) #284 obs
test = subset(clima, clima$Year > 2006) #24 obs

clima_mod1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, 
                data = train)
summary(clima_mod1)
pairs(~ Temp + MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,data = train)

x1 = train[, 6]
y1 = train[-x1]
N2O_cor = cor(x1, y1) #correlation of N2O with all other variables

x2 = train[, 7]
y2 = train[-x2]
CFC11_cor = cor(x2, y2) #correlation of CFC.11 with all other variables

clima_mod2 = lm(Temp ~ MEI + N2O + TSI + Aerosols, data = train)
summary(clima_mod2)

clima_mod3 = step(clima_mod1) #attempts to simplify model using AIC criteria (default is both
#backward then forward stepwise search, and chooses the most parsimonious)
summary(clima_mod3)

pred3 = predict(clima_mod3, newdata = test)
SSE = sum((test$Temp - pred3) ^ 2) #Sum of Squared Errors
SST = sum((test$Temp - mean(train$Temp)) ^ 2) #Total Sum of Squares
Rsq = 1 - SSE/SST
Rsq #0.6286


#PROBLEM SET 2: READING TEST SCORES
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

tapply(pisaTrain$readingScore, pisaTrain$male, mean)
summary(pisaTrain)
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White") #reset the reference level of variable 
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")


lmScore = lm(readingScore ~., data = pisaTrain)
summary(lmScore)$r.sq
summary(lmScore)$sigma

#The training-set RMSE can be computed by first computing the SSE:
#SSE = sum(lmScore$residuals ^ 2)
#and then dividing by the number of observations and taking the square root:
#RMSE = sqrt(SSE / nrow(pisaTrain))
#A alternative way of getting this answer would be with the following command:
RMSE = sqrt(mean(lmScore$residuals ^ 2)) #formula for training set
RMSE

lmScore$xlevels #assigned order of categorical variable levels

predTest = predict(lmScore, pisaTest)
range(predTest)
SSE = sum((pisaTest$readingScore - predTest) ^ 2)
RMSE = sqrt(mean((pisaTest$readingScore - predTest) ^ 2)) #formula for test set

baseline = mean(pisaTrain$readingScore)
SST = sum((pisaTest$readingScore - baseline) ^ 2)
Rsq = 1 - SSE/SST


#PROBLEM SET 3: FLU EPIDEMICS
FluTrain = read.csv("FluTrain.csv")
summary(FluTrain)
tail(FluTrain$Week)

#Which week corresponds to the highest percentage of ILI-related physician visits?
which.max(FluTrain$ILI) #303
FluTrain$Week[303]

#Which week corresponds to the highest percentage of ILI-related query fraction?
which.max(FluTrain$Queries) #303, same as before!

hist(FluTrain$ILI)
plot(log(FluTrain$ILI), FluTrain$Queries)

FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)
cor(log(FluTrain$ILI), FluTrain$Queries) ^ 2

FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest)) #uses exp because y was log transformed
class(FluTest$Week)
FluTest[grep("2012-03-11", FluTest$Week), ] #return search for partial string value
est = PredTest1[11] #index returned from search
obs = FluTest$ILI[11]
error = (obs - est) / obs
sqrt(mean((FluTest$ILI - PredTest1) ^ 2)) #RMSE for test data

sqrt(mean((FluTrain$ILI - exp(FluTrend1$fitted.values)) ^ 2)) #train RMSE (smaller than test)

#Rsq = 1 - SSE/SST,
#SST is calculated using the average value of the dependent variable on the training set.
#Since SSE and SST are the sums of squared terms, we know that both will be positive. 
#Thus SSE/SST must be greater than or equal to zero. 
#This means it is not possible to have an out-of-sample Rsq value of 2.4.
#However, all other values are valid (even the negative ones!), since SSE can be 
#more or less than SST, due to the fact that this is an out-of-sample Rsq, NOT a model Rsq.

library(zoo) #zero ordered onservations, used for time series data
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad = TRUE) 
#create variable that contains the ILI value from 2 weeks before (-2) the current observation
#na.pad = TRUE add missing values (NA) for the first two weeks of our dataset, 
#where we can't compute the data from 2 weeks earlier

FluTrain$ILILag2 = coredata(ILILag2) #strip off time attributes and return only observations
head(FluTrain$ILI); head(FluTrain$ILILag2)

plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad = TRUE) #careful! using same variable name
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest$ILILag2) #first two obs turned NA

#There is no gap in time between data from training set to test set (sequential), so
#we must manually add the correspoding values from the training set to the test set
nrow(FluTrain) #417
FluTest$ILILag2[1] = FluTrain$ILI[416] #second-to-last obs
FluTest$ILILag2[2] = FluTrain$ILI[417] #last obs
head(FluTest$ILILag2)

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
sqrt(mean((FluTest$ILI - PredTest2) ^ 2)) #much lower (better) RMSE than FluTrend1!
#same as: SSE = sum((PredTest2-FluTest$ILI)^2); RMSE = sqrt(SSE / nrow(FluTest))

?arima #to learn more of time series analysis


