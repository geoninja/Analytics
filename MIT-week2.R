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
