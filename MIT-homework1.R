#Homework week 1 - Introduction to Analytics

#PROBLEM SET 1 - Chicago crime data (car thefts)
#Part 1
mvt = read.csv("mvtWeek1.csv")
attach(mvt)
names(mvt)
which.max(ID)
ID[18134]
which.min(Beat)
Beat[4756]
table(Arrest)
table(LocationDescription == "ALLEY")

#Part 2
head(mvt$Date)
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M")) #converts character vector to POSIXlt
summary(DateConvert)
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
which.min(table(mvt$Month))
which.max(table(mvt$Weekday))
table(mvt$Month, mvt$Arrest)
which.max(table((subset(mvt, Arrest == T))$Month)) #January

#Another example of working with date and time data:
## read in date/time info in format 'm/d/y h:m:s'
dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
times <- c("23:03:20", "22:29:56", "01:03:30", "18:21:03", "16:56:26")
x <- paste(dates, times)
strptime(x, "%m/%d/%y %H:%M:%S")

#Part 3
hist(mvt$Date, breaks = 100)
boxplot(mvt$Date ~ mvt$Arrest, xlab = "Arrests", ylab = "Date")
hist((subset(mvt, Arrest == T))$Date, breaks = 100)
#Q:For what proportion of motor vehicle thefts in 2001 was an arrest made?
arrests_year = table(mvt$Year, mvt$Arrest)
arrests_year
arrests_year[1, 2]/sum(arrests_year[1,]) #0.104

#Q:For what proportion of motor vehicle thefts in 2007 was an arrest made?
arrests_year[7, 2]/sum(arrests_year[7,]) #0.085

#Q:For what proportion of motor vehicle thefts in 2012 was an arrest made
arrests_year[12, 2]/sum(arrests_year[12,]) #0.039


#Part 4
theft_loc = sort(table(mvt$LocationDescription))
length(theft_loc) #78 different locations in the dataset
answer = names(tail(theft_loc))[-4]
answer

Top5 = subset(mvt, LocationDescription %in% answer)
summary(Top5)
str(Top5)
unique(Top5$LocationDescription)
sum(theft_loc[73:78])-theft_loc[76] #177510, double-checking

Top5$LocationDescription = factor(Top5$LocationDescription) #redefine as factor eliminating zero values
unique(Top5$LocationDescription) #now there are only 5 instead of 78

arrests_loc = table(Top5$LocationDescription, Top5$Arrest)
alley = arrests_loc[1, 2] / sum(arrests_loc[1,])
driveway = arrests_loc[2, 2] / sum(arrests_loc[2,])
gas_station = arrests_loc[3, 2] / sum(arrests_loc[3,])
parking_lot = arrests_loc[4, 2] / sum(arrests_loc[4,])
street = arrests_loc[5, 2] / sum(arrests_loc[5,])

ans = c("alley" = alley, "driveway" = driveway, "gas_station" = gas_station, 
        "parking_lot" = parking_lot, "street" = street)
which.max(ans)

table(Top5$Weekday, Top5$LocationDescription)


#PROBLEM SET 2 - stocks
#Part 1
IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")

summary(IBM)
head(IBM)
IBM$Date = as.Date(IBM$Date, "%m/%d/%y") #converts from factor to date object
GE$Date = as.Date(GE$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
which.min(IBM$Date)
which.max(IBM$Date)
summary(GE)
summary(CocaCola)
summary(Boeing)
summary(ProcterGamble)
sd(ProcterGamble$StockPrice)

#Part 2
plot(CocaCola$StockPrice ~ CocaCola$Date, col = "red", type = "l")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")
abline(v=as.Date(c("2000-03-01")), lwd = 2, lty = 3)

#Part 3
which(CocaCola$Date == "1995-01-01")
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", 
     ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col = "blue")
lines(GE$Date[301:432], GE$StockPrice[301:432], col = "orange")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col = "darkgreen")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], lty = 2)
abline(v=as.Date(c("2000-03-01")), lwd = 2, lty = 3)
abline(v=as.Date(c("1997-09-01")), lwd = 2, lty = 3, col = "purple")
abline(v=as.Date(c("1997-11-01")), lwd = 2, lty = 3, col = "purple")
abline(v=as.Date(c("2004-01-01")), lwd = 2, lty = 3, col = "purple")
abline(v=as.Date(c("2005-01-01")), lwd = 2, lty = 3, col = "purple")

#Part 4
mean(IBM$StockPrice)
mon = tapply(IBM$StockPrice, months(IBM$Date), mean)
which(mon > mean(IBM$StockPrice))

mean_GE = mean(GE$StockPrice)
tapply(GE$StockPrice, months(GE$Date), mean)

mean_Coke = mean(CocaCola$StockPrice)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)

mean_Boeing = mean(Boeing$StockPrice)
tapply(Boeing$StockPrice, months(Boeing$Date), mean)

mean_PG = mean(ProcterGamble$StockPrice)
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)


#PROBLEM SET 3 - census
#Part 1
CPS = read.csv("CPSData.csv")
summary(CPS)
names(CPS)
sort(table(CPS$State))[1] #New Mexico
table(CPS$Citizenship)
cit = (table(CPS$Citizenship)[1] + table(CPS$Citizenship)[2])/sum(table(CPS$Citizenship))
#For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity?
head(CPS)
hisp = table(subset(CPS$Race, CPS$Hispanic == 1))
which(hisp > 250)

#Part 2
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

table(CPS$State, is.na(CPS$MetroAreaCode))
table(CPS$Region, is.na(CPS$MetroAreaCode))

sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

#Part 3
MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
#The first two arguments determine the data frames to be merged (they are called "x" and "y", 
#respectively, in the subsequent parameters to the merge function). by.x="MetroAreaCode" means 
#we're matching on the MetroAreaCode variable from the "x" data frame (CPS), while by.y="Code" 
#means we're matching on the Code variable from the "y" data frame (MetroAreaMap). Finally, 
#all.x=TRUE means we want to keep all rows from the "x" data frame (CPS), even if some of the 
#rows' MetroAreaCode doesn't match any codes in MetroAreaMap (for those familiar with database 
#terminology, this parameter makes the operation a left outer join instead of an inner join).

names(CPS)
#How many interviewees have a missing value for the new metropolitan area variable? 
#Note that all of these interviewees would have been removed from the merged data frame if 
#we did not include the all.x=TRUE parameter.
table(is.na(CPS$MetroArea))
sort(table(CPS$MetroArea))
#Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity?
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean)) #Laredo, TX
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm = T))[1]

#Part 4
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

#What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" 
#metropolitan area have a country of birth that is not the United States? 
sub = subset(CPS, CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA")
summary(sub)
#another way of doing the same:
table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", 
      CPS$Country != "United States") #1668/5404

#Which metropolitan area has the largest number of interviewees with a country of birth in India?
sort(tapply(CPS$Country == "India", CPS$MetroArea, mean, na.rm = T))
sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, mean, na.rm = T))
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, mean, na.rm = T))


#OPTIONAL PROBLEM SET - Internet Privacy Poll
poll = read.csv("AnonymityPoll.csv")
names(poll)
summary(poll$Smartphone)
table(poll$Smartphone)
table(poll$State, poll$Region == "South")
summary(poll$Internet.Use)
NA %in% poll$Internet.Use #TRUE
anyNA(poll$Internet.Use) #same as above
table(poll$Internet.Use, poll$Smartphone)
limited = subset(poll, poll$Internet.Use == 1 | poll$Smartphone == 1)
nrow(limited) #792 as it includes 20 NA's
summary(limited)
mean(table(limited$Info.On.Internet))
table(limited$Worry.About.Info)
table(limited$Anonymity.Possible)
table(limited$Tried.Masking.Identity)
table(limited$Privacy.Laws.Effective)
hist(limited$Age)
max(table(limited$Age, limited$Info.On.Internet))
jitter(c(1,2,3)) #run twice to compare results
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))
tapply(limited$Info.On.Internet, limited$Smartphone, summary)
table(limited$Tried.Masking.Identity, limited$Smartphone)





