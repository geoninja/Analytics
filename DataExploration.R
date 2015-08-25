#Kaggle Data Exploration

setwd("~/Dropbox/Analytics_Edge/Kaggle")
blogTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors = F)
summary(blogTrain)
attach(blogTrain)

summary(WordCount) #0 to 10910, median 374, mean 524
hist(WordCount, breaks = 20) #right skewed
hist(log(WordCount), breaks = 10) #less skewed (a bit to the left)

table(Popular)
1093/nrow(blogTrain) #0.16733
tapply(WordCount, Popular, mean) #1: 838, 0: 461
blogTrain$PopWCount[blogTrain$WordCount >= 500 & blogTrain$WordCount <=850] = 1
blogTrain$PopWCount[!(blogTrain$WordCount >= 500 & blogTrain$WordCount <=850)] = 0

library(plyr) #use mapvalues function

#Abstract and Snippet are very similar, but abstract may have up to 336 charaters more
unique(NewsDesk) #12 + "", check Styles and TStyle
table(Popular, NewsDesk)
PopNewsDesk = table(Popular, NewsDesk)[2,] / table(NewsDesk)
sort(PopNewsDesk, decreasing = T) #OpEd*, Science*, Styles, Business (Top 4)
WordsNewsDesk = sort(tapply(WordCount, NewsDesk, mean), decreasing = T)
mean(WordsNewsDesk[2:5]) #Top 4
#Travel: shortest, Sports: longest, Top4 average: 644 (594 - 701)
length(NewsDesk[NewsDesk == ""]) #1846 ~ 28%

#Create PopDesk Variable
pop2 = c("OpEd", "Science")
pop1 = c("Styles", "Business")
pop0 = c("Culture", "Foreign", "TStyle", "Magazine", "Travel", "Metro", "National", "Sports")
blogTrain$PopDesk[blogTrain$NewsDesk %in% pop2] = "high"
blogTrain$PopDesk[blogTrain$NewsDesk %in% pop1] = "medium"
blogTrain$PopDesk[blogTrain$NewsDesk %in% pop0] = "low"
blogTrain$PopDesk = as.factor(blogTrain$PopDesk)
table(blogTrain$PopDesk) #15.3% high pop


unique(SectionName) #15 + ""
table(Popular, SectionName)
PopSectionName = table(Popular, SectionName)[2,] / table(SectionName)
sort(PopSectionName, decreasing = T) #Crosswords*, Opinion*, Health*, US, Technology (top 5)
WordsSectionName = sort(tapply(WordCount, SectionName, mean), decreasing = T)
range(WordsSectionName[c(2, 4, 6, 8, 11)]) #mean: 640 (480 - 857)
length(SectionName[SectionName == ""]) #2300 ~ 35%


#Create PopSection Variable
poph = c("Crosswords/Games", "Opinion", "Health")
#popm = c("U.S.", "Technology")
#popl = c("Arts", "Business Day", "Style", "World", "Magazine", "Multimedia", "Travel", "Open", 
         "Sports", "N.Y. / Region")
blogTrain$PopSection[blogTrain$SectionName %in% poph] = 1
blogTrain$PopSection[!blogTrain$SectionName %in% poph] = 0
#blogTrain$PopSection[blogTrain$SectionName %in% popm] = "medium"
#blogTrain$PopSection[blogTrain$SectionName %in% popl] = "low"
blogTrain$PopSection = as.factor(blogTrain$PopSection)
table(blogTrain$PopSection) #21.8% high pop


unique(SubsectionName) #8 + ""
table(Popular, SubsectionName)
PopSubsectionName = table(Popular, SubsectionName)[2,] / table(SubsectionName)
sort(PopSubsectionName, decreasing = T) #The Public Editor*, Dealbook
length(SubsectionName[SubsectionName == ""]) #4826 ~ 74%
#Conclusion: subsection name probably not important, the other two may be.


# Convert date/time to a format R will understand:
blogTrain$PubDate = strptime(blogTrain$PubDate, "%Y-%m-%d %H:%M:%S")
range(blogTrain$PubDate) #SEP, OCT, NOV 2014

blogTrain$Weekday = blogTrain$PubDate$wday #extract weekdays (0 to 6)
tw = table(blogTrain$Weekday, blogTrain$Popular)
wd = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
blogTrain$Weekday = as.factor(mapvalues(blogTrain$Weekday, from = 0:6, to = wd))
unique(blogTrain$Weekday)
tw[,2]/apply(tw, 1, sum) #weekend is ~ twice more popular than weekdays
blogTrain$Weekend[blogTrain$Weekday == "Saturday" | blogTrain$Weekday == "Sunday"] = 1
blogTrain$Weekend[!(blogTrain$Weekday == "Saturday" | blogTrain$Weekday == "Sunday")] = 0

blogTrain$Hour = blogTrain$PubDate$hour #extract publishing hour
t = table(blogTrain$Hour, blogTrain$Popular)
h = sort((t/apply(t, 1, sum))[, 2], decreasing = T)
mean((t/apply(t, 1, sum))[, 2]) #17.86%
h[1:10] #pub hours with popularity above mean 18-23, 15(21.2%), 10-12
mean(h[c(1:5, 8)]) #32.6% 18-23h prime pub time
mean(h[c(7, 9, 10)]) #18.1% 10-12h
blogTrain$PopHour[blogTrain$Hour >= 18] = 1
blogTrain$PopHour[blogTrain$Hour < 18] = 0


#Is there anything else about the headline to explore?

#FIRST MODEL: LOGISTIC REGRESSION
mod1 = glm(Popular ~ WordCount + Hour + SectionName, data = blogTrain, family = binomial)
summary(mod1)
#Prediction on training data
pred.mod1 = predict(mod1, type="response") #"response" returns probabilities
summary(pred.mod1)
table(blogTrain$Popular, pred.mod1 > 0.5) #(5158+637)/6532 = 0.8871709
table(blogTrain$Popular, pred.mod1 > 0.2) #(4989+767)/6532 = 0.8812002

library(ROCR)
ROC.mod1 = prediction(pred.mod1, blogTrain$Popular)
ROCperf1 = performance(ROC.mod1, measure = "tpr", x.measure = "fpr")
# Plot ROC curve, add colors and threshold labels (useful to choose threshold)
plot(ROCperf1, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
auc = as.numeric(performance(ROC.mod1, "auc")@y.values)
auc #0.8921399

#Prediction on the test data
# predTest1 = predict(pred.mod1, type="response", newdata = blogTest)
# ROCpred1 = prediction(predTesta, blogTest$Popular)
# auc = as.numeric(performance(ROCpred1, "auc")@y.values)


mod2 = glm(Popular ~ WordCount + Hour + SectionName + Weekday, data = blogTrain, family = binomial)
summary(mod2)
pred.mod2 = predict(mod2, type="response")
summary(pred.mod2)
table(blogTrain$Popular, pred.mod2 > 0.5) #(5158+640)/6532 0.8876301
ROC.mod2 = prediction(pred.mod2, blogTrain$Popular)
auc = as.numeric(performance(ROC.mod2, "auc")@y.values) #0.8939753


mod3 = glm(Popular ~ PopWCount + PopHour + PopSection + Weekend, data = blogTrain, 
           family = binomial)
summary(mod3)
pred.mod3 = predict(mod3, type="response")
summary(pred.mod3)
table(blogTrain$Popular, pred.mod3 > 0.5) #(5163+648)/6532 0.8896203
ROC.mod3 = prediction(pred.mod3, blogTrain$Popular)
auc = as.numeric(performance(ROC.mod3, "auc")@y.values) #0.8147658


mod4 = glm(Popular ~ WordCount + Hour + SectionName + Weekday + NewsDesk, data = blogTrain, 
           family = binomial)
summary(mod4)
pred.mod4 = predict(mod4, type="response")
summary(pred.mod4) #overfitting?
table(blogTrain$Popular, pred.mod4 > 0.5) #(5205+655)/6532 0.8971219
ROC.mod4 = prediction(pred.mod4, blogTrain$Popular)
auc = as.numeric(performance(ROC.mod4, "auc")@y.values) #0.9116301 not better model, despite auc

#Among mods 1-4, mod2 is chosen as the best so far, based on training data fit alone.

#Try: Ensemble methods, logistic regression with regularization, forward stepwise model selection
#and k-fold cross-validation (k=5)

#words from headline and snippet with trees (or regularized logit)
