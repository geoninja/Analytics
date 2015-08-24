#MIT class week 1 - Introduction to Analytics

#setwd("C:/Users/pa120604/Downloads") #change the path accordingly

# USDA Data

USDA = read.csv("USDA.csv")
dim(USDA) #7058 x 16
names(USDA)
summary(USDA)

which.max(USDA$Sodium)
USDA$Description[265]
HighSodium = subset(USDA, Sodium > 10000)
nrow(HighSodium)
HighSodium$Description

match("CAVIAR", USDA$Description) #returns index 4154
USDA$Sodium[4154]
#Likewise:
USDA$Sodium[match("CAVIAR", USDA$Description)]
summary(USDA$Sodium)
sd(USDA$Sodium, na.rm = T)

attach(USDA)
plot(Protein, TotalFat, xlab = "Protein", ylab = "Fat", main = "Protein vs Fat", col = "red")
hist(VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C Levels", xlim = c(0,100), 
     breaks = 2000)
boxplot(Sugar, main = "Boxplot of Sugar Levels", ylab = "Sugar (g)")

USDA$HighSodium = as.numeric(Sodium > mean(Sodium, na.rm = T))
str(HighSodium) #structure was originally logic, nto numeric

USDA$HighProtein = as.numeric(Protein > mean(Protein, na.rm = T))
USDA$HighFat = as.numeric(TotalFat > mean(TotalFat, na.rm = T))
USDA$HighCarbs = as.numeric(Carbohydrate > mean(Carbohydrate, na.rm = T))

str(USDA) #show added variables
table(USDA$HighSodium)
table(USDA$HighSodium, USDA$HighFat) #show count for association b/w both variables; HFat on top
tapply(Iron, USDA$HighProtein, mean, na.rm = T) #mean for Iron, sorted by HighProtein
tapply(VitaminC, USDA$HighCarbs, max, na.rm = T)
tapply(VitaminC, USDA$HighCarbs, summary, na.rm = T)


# WHO Data

Country = c("Brazil", "China", "India", "USA")
LifeExpectancy = c(74, 76, 65, 79)
CountryData = data.frame(Country, LifeExpectancy)
CountryData$Population = c(199, 1390, 1240, 318)
CountryData

WHO = read.csv("WHO.csv")
str(WHO)
WHO_Europe = subset(WHO, Region == "Europe")
str(WHO_Europe)
write.csv(WHO_Europe, "WHO_Europe.csv")
ls()
rm(WHO_Europe)
summary(WHO$Under15)
which.min(WHO$Under15) #returns [86]
WHO$Country[86] #returns Japan

plot(WHO$GNI, WHO$FertilityRate)
Outliers = subset(WHO, GNI > 10000 & FertilityRate > 2.5)
nrow(Outliers) #returns 7
Outliers[c("Country", "GNI", "FertilityRate")] #list selected variables

summary(WHO$Over60)
which.min(WHO$Over60)
WHO$Country[183]
which.max(WHO$LiteracyRate)
WHO$Country[44]

boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab = "", ylab = "Life Expectancy", 
        main = "Life Expectancy of Countries by Rgeion")
tapply(WHO$Over60, WHO$Region, mean)
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm = T)
tapply(WHO$ChildMortality, WHO$Region, mean)
