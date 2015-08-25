#Week 7: Visualization

# VIDEO 4 - A BASIC SCATTERPLOT (WHO data)

WHO = read.csv("WHO.csv")
plot(WHO$GNI, WHO$FertilityRate) #ugly plot!

library(ggplot2) 
# Create the ggplot object with the data and the aesthetic mapping:
scatterplot = ggplot(WHO, aes(x = GNI, y = FertilityRate)) #doesn't plot anything yet
names(scatterplot)

# Make a line graph:
scatterplot + geom_line()

# Switch to points geometry:
scatterplot + geom_point() #much nicer!

# Redo the plot with blue triangles instead of circles:
scatterplot + geom_point(color = "blue", size = 3, shape = 17)

# Another option:
scatterplot + geom_point(color = "darkred", size = 3, shape = 8) 

# Add a title to the plot:
scatterplot + geom_point(color = "blue", size = 3, shape = 17) + 
    ggtitle("Fertility Rate vs. Gross National Income")

# Save our plot:
fertilityGNIplot = scatterplot + geom_point(color = "blue", size = 3, shape = 17) + 
    ggtitle("Fertility Rate vs. Gross National Income")

pdf("MyPlot.pdf")
print(fertilityGNIplot) #print plot to file (write)
dev.off() #close file


# VIDEO 5 - MORE ADVANCED SCATTERPLOTS (WHO Data)

# Color the points by region (factor type gives different colors)
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point()

# Color the points according to life expectancy (numeric type gives shades of same color):
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()

# Is the fertility rate of a country a good predictor of the percentage of the 
#population under 15?
ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point()

# Let's try a log transformation:
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point()

# Simple linear regression model to predict the percentage of the population under 15, 
#using the log of the fertility rate:
mod = lm(Under15 ~ log(FertilityRate), data = WHO)
summary(mod)

# Add this regression line to our plot:
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + 
    stat_smooth(method = "lm")

# 99% confidence interval
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + 
    stat_smooth(method = "lm", level = 0.99)

# No confidence interval in the plot
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + 
    stat_smooth(method = "lm", se = F)

# Change the color of the regression line:
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + 
    stat_smooth(method = "lm", color = "orange")

#Last question:
ggplot(WHO, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point()

#Color blind people option:
ggplot(WHO, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point() + 
    scale_color_brewer(palette = "Dark2")



#Predictive Policing - video 3 (mvt data)
mvt = read.csv("mvt.csv", stringsAsFactors=FALSE) #load date as character, not factor

# Convert the Date variable to a format that R will recognize:
mvt$Date = strptime(mvt$Date, format = "%m/ %d/ %y %H:%M") #date now is POSIXlt

# Extract the hour and the day of the week:
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour

# Create a simple line plot - need the total number of crimes on each day of the week. 
# We can get this information by creating a table:
table(mvt$Weekday) #total number of thefts per weekday

# Save this table as a data frame:
WeekdayCounts = as.data.frame(table(mvt$Weekday))

# Create plot
library(ggplot2)
ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1))

# Make the "Var1" variable an ORDERED factor variable
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, 
                            levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                       "Thursday", "Friday","Saturday"))

# Try again:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) #groups data into 1 line

# Change x and y labels:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) + 
    xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

# Make the line dashed:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), linetype = 2)

# Make the line lighter in color:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), alpha = 0.3)


# VIDEO 4 - HEATMAPS (mvt data)

# Create a counts table for the weekday and hour:
table(mvt$Weekday, mvt$Hour) #thefts per hour per weekday

# Save this to a data frame:
DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))

# Convert the second variable, Var2, to numbers and call it Hour:
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2)) #note: factor to char to num

# Create plot:
ggplot(DayHourCounts, aes(x = Hour, y = Freq)) + geom_line(aes(group = Var1))

# Change colors and make lines thicker:
ggplot(DayHourCounts, aes(x = Hour, y = Freq)) + geom_line(aes(group = Var1, color = Var1), 
                                                           size = 2)

# Separate the weekends from the weekdays:
DayHourCounts$Type = ifelse((DayHourCounts$Var1== "Sunday") | (DayHourCounts$Var1== "Saturday"), 
                            "Weekend", "Weekday")

# Redo our plot, this time coloring by Type:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size = 2)

# Make the lines a little transparent:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size = 2, 
                                                       alpha = 0.5)

# Fix the order of the days:
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered = TRUE, 
                            levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                     "Friday", "Saturday", "Sunday"))

# Make a heatmap:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq))

# Change the label on the legend, and get rid of the y-label:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) +
    scale_fill_gradient(name = "Total MV Thefts") + 
    theme(axis.title.y = element_blank())

# Change the color scheme
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) +
    scale_fill_gradient(name = "Total MV Thefts", low = "white", high = "black") + 
    theme(axis.title.y = element_blank())


# VIDEO 5 - Maps (mvt data)
library(maps)
library(ggmap)

# Load a map of Chicago into R:
chicago = get_map(location = "chicago", zoom = 11)
ggmap(chicago) #check the map

#sf = get_map(location = "sanfrancisco", zoom = 13)

# Round our latitude and longitude to 2 digits of accuracy, and create a crime counts 
# data frame for each area:
LatLonCounts = as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude, 2)))
# Convert our Longitude and Latitude variable to numbers:
LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1)) #note: factor to char to num
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))

# Plot these points on our map:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, 
                                                     color = Freq, size = Freq)) + 
    scale_color_gradient(low = "yellow", high = "red")


LatLonCounts2 = subset(LatLonCounts, LatLonCounts$Freq > 0) #remove Freq = 0

# We can also use the geom_tile geometry:
ggmap(chicago) + geom_tile(data = LatLonCounts2, aes(x = Long, y = Lat, 
                                                     alpha = Freq), fill = "red")


# VIDEO 6 - Geographical Map on US (murders data)

murders = read.csv("murders.csv")

# Load the map of the US
statesMap = map_data("state")

# Plot the map:
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", 
                                                                        color = "black")
# Create a new variable called region with the lowercase names to match the statesMap:
murders$region = tolower(murders$State)

# Join the statesMap data and the murders data into one dataframe:
#repeated murders data for same region in map
murderMap = merge(statesMap, murders, by = "region") 

# Plot the number of murder on our map of the United States:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + 
    geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", 
                                                        guide = "legend")

# Plot a map of the population:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + 
    geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", 
                                                        guide = "legend")

# Create a new variable that is the number of murders per 100,000 population:
murderMap$MurderRate = murderMap$Murders / murderMap$Population * 100000

# Redo plot with murder rate:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + 
    geom_polygon(color = "black") + scale_fill_gradient(low = "red", high = "white", 
                                                        guide = "legend")

# Redo the plot, removing any states with murder rates above 10:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + 
    geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", 
                                                        guide = "legend", limits = c(0, 10))


# Finally, redo plot with % of gun ownership:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = GunOwnership)) + 
    geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", 
                                                        guide = "legend")


#Recitation

# VIDEO 3 - Bar Charts (intl data)

# Load our data, which lives in intl.csv
intl = read.csv("intl.csv")

# We want to make a bar plot with region on the X axis
# and Percentage on the y-axis. stat = identity means height of the bar is the y value as is.
ggplot(intl, aes(x = Region, y = PercentOfIntl)) + geom_bar(stat = "identity") + 
    geom_text(aes(label = PercentOfIntl))

# Make Region an ordered factor
# We can do this with the re-order command and transform command. 
intl = transform(intl, Region = reorder(Region, -PercentOfIntl)) 
#reorder Region based on decreasing order (- signal) of PercentageOfIntl.

# Make the percentages out of 100 instead of fractions
intl$PercentOfIntl = intl$PercentOfIntl * 100

# Remake the plot (vjust vertically adjusts label):
ggplot(intl, aes(x = Region, y = PercentOfIntl)) + 
    geom_bar(stat = "identity", fill = "darkblue") + 
    geom_text(aes(label = PercentOfIntl), vjust = -0.4) +
    ylab("Percent of International Students") +
    theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))


# VIDEO 5 - World map (intlall data)

# Load the ggmap package
library(ggmap)

# Load in the international student data
intlall = read.csv("intlall.csv", stringsAsFactors = FALSE)
head(intlall)

# Those NAs are really 0s, and we can replace them easily
intlall[is.na(intlall)] = 0

# Load the world map
world_map = map_data("world")

# Lets merge intlall into world_map using the merge command
world_map = merge(world_map, intlall, by.x = "region", by.y = "Citizenship")

# Plot the map (some countries are missing, may have different names before merging)
# group is a number by country, points not in order
ggplot(world_map, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = "white", color = "black") + 
    coord_map("mercator")

# Reorder the data (group and order of points (lat/long))
world_map = world_map[order(world_map$group, world_map$order),]

# Lets look for China
table(intlall$Citizenship)

# Lets "fix" that in the intlall dataset
intlall$Citizenship[intlall$Citizenship == "China (People's Republic Of)"] = "China"

# We'll repeat same merge and order (be careful of str changes when re-merging!):
world_map = merge(world_map, intlall, by.x = "region", by.y = "Citizenship")
world_map = world_map[order(world_map$group, world_map$order),]

ggplot(world_map, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = Total), color = "black") + 
    coord_map("mercator")

# We can try other projections - this one is visually interesting
ggplot(world_map, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = Total), color = "black") + 
    coord_map("ortho", orientation = c(20, 30, 0))

ggplot(world_map, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = Total), color = "black") + 
    coord_map("ortho", orientation = c(-37, 175, 0))


# VIDEO 7 - Line Charts (households data)

households = read.csv("households.csv")
library(reshape2)

# Lets look at the first two columns of our households dataframe
households[,1:2]

# First few rows of our melted households dataframe
head(melt(households, id = "Year"))
households[ ,1:3]

melt(households, id = "Year")[1:10, 3]
melt(households, id = "Year")[1:10, ]

#Plot it
ggplot(melt(households, id = "Year"), aes(x = Year, y = value, color = variable)) +
    geom_line(size = 2) + geom_point(size = 5) + ylab("Percentage of Households")


###############################################################################
#        HOMEWORK - WEEK 7 VISUALIZATION
###############################################################################

#Predicting Election Results (PollingImputed data)

#Part 1
library(ggplot2)
library(maps)
library(ggmap)

statesMap = map_data("state")
unique(statesMap$group)
ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
    geom_polygon(fill = "white", color = "black")

#Part 2
polling = read.csv("PollingImputed.csv")

Train = subset(polling, polling$Year == 2004 | polling$Year == 2008 )
Test = subset(polling, polling$Year == 2012)

length(unique(Train$State)) #50
length(unique(Test$State)) #45 (some states will not be colored in the map)

mod2 = glm(Republican ~ SurveyUSA + DiffCount, data = Train, family = "binomial")
summary(mod2)

TestPrediction = predict(mod2, newdata = Test, type = "response")

#Let's also create a vector of Republican/Democrat predictions:
TestPredictionBinary = as.numeric(TestPrediction > 0.5)

#Let's put the predictions and state labels into a DF so we can plot it
predictionDF = data.frame(TestPrediction, TestPredictionBinary, Test$State)

subset(Test$State, TestPredictionBinary == 1)
mean(TestPrediction)

#Convert the Test.State variable to lower case, then merge DF and map:
predictionDF$region = tolower(predictionDF$Test.State)
predictionMap = merge(statesMap, predictionDF, by = "region")

#Let's now reorder the merged points
predictionMap = predictionMap[order(predictionMap$order), ]

#Let's plot!
ggplot(predictionMap, aes(x = long, y = lat, group = group, 
                          fill = TestPredictionBinary)) +
    geom_polygon(color = "black")

#Let's change the color and remove gradient since fill is discrete:
ggplot(predictionMap, aes(x = long, y = lat, group = group, 
                          fill = TestPredictionBinary)) +
    geom_polygon(color = "black") + 
    scale_fill_gradient(low = "blue", high = "red", guide = "legend", 
                        breaks = c(0,1), labels = c("Democrat", "Republican"),
                        name = "Prediction 2012")

#Let's change the fill to TestPrediction, the predicted probabilities (purple Iowa!)
ggplot(predictionMap, aes(x = long, y = lat, group = group, 
                          fill = TestPrediction)) +
    geom_polygon(color = "black") + 
    scale_fill_gradient(low = "blue", high = "red", guide = "legend", 
                        breaks = c(0,1), labels = c("Democrat", "Republican"),
                        name = "Prediction 2012")

#Part 3
mean(subset(predictionMap, predictionMap$region == "florida")$TestPrediction)

#Part 4
ggplot(predictionMap, aes(x = long, y = lat, group = group, 
                          fill = TestPrediction)) +
    geom_polygon(color = "black", alpha = 0.3, linetype = 3, size = 3) + 
    scale_fill_gradient(low = "blue", high = "red", guide = "legend", 
                        breaks = c(0,1), labels = c("Democrat", "Republican"),
                        name = "Prediction 2012")


#VISUALIZING NETWORK DATA (edges and users data)

#Part 1
edges = read.csv("edges.csv") #146 pairs
users = read.csv("users.csv")

(2 * nrow(edges)) / nrow(users) # 4.95 average friends per user

table(users$locale)
table(users$gender, users$school)

#Part 2
library(igraph)

g = graph.data.frame(edges, F, users)
plot(g, vertex.size = 5, vertex.label = NA)

degree(g) #number of friends, or degree of each node (vertices)
table(degree(g) >= 10)

#make size attribute of the vertices to be an increasing function of their degree:
V(g)$size = degree(g)/2+2 #V(g) are the vertices of the graph g
plot(g, vertex.label = NA) #no longer need vertex.size parameter

max(V(g)$size); min(V(g)$size)

#Part 3
V(g)$color = "black" #change the color of the vertices to black
V(g)$color[V(g)$gender == "A"] = "red" #reset color based on gender
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label = NA) #color is black if gender is missing

#Color vertices based on school/s attended
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "yellow"
plot(g, vertex.label = NA)

#Color vertices based on locale of the users
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "yellow"
plot(g, vertex.label = NA)

plot(g, edge.width = 2, vertex.label=NA)


#VISUALIZING TEXT DATA USING WORD CLOUDS (tweets data)

#Part 1
tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
library(wordcloud)
library(RColorBrewer)
library(tm)

corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))

dtm = DocumentTermMatrix(corpus); dtm
findFreqTerms(dtm, lowfreq = 20)

allTweets = as.data.frame(as.matrix(dtm))
(colnames(allTweets))[100:110]
which.max(colSums(allTweets))

#Part 2
wordcloud(colnames(allTweets), colSums(allTweets), scale = c(4, 0.5), min.freq = 6)

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
dtm = DocumentTermMatrix(corpus); dtm
allTweets = as.data.frame(as.matrix(dtm))
wordcloud(colnames(allTweets), colSums(allTweets), scale = c(3, 0.33), min.freq = 7)

#Part 3
negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets), scale = c(4, 0.5), min.freq = 5)


wordcloud(colnames(allTweets), colSums(allTweets), scale = c(3, 0.33), min.freq = 7, 
          random.order = F, rot.per = 0.2)

#Part 4
wordcloud(colnames(negativeTweets), colSums(negativeTweets), scale = c(4, 0.5), min.freq = 5, 
          colors = brewer.pal(9, "Blues")[c(-1, -2, -3, -4)])

#display.brewer.all() will show all available palletes in the RColorBrewer package



#EXTRA: Visualizing random forests variable importance:

## Train a model across all the training data and plot the variable importance
# rf <- randomForest(extractFeatures(train), train$count, ntree=100, importance=TRUE)
# imp <- importance(rf, type=1)
# featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
# 
# p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
#   geom_bar(stat="identity", fill="#53cfff") +
#   coord_flip() + 
#   theme_light(base_size=20) +
#   xlab("Importance") +
#   ylab("") + 
#   ggtitle("Random Forest Feature Importance\n") +
#   theme(plot.title=element_text(size=18))
# 
# ggsave("2_feature_importance.png", p)


#EXTRA MAP:
library(maps)
east_asia <- map_data("world", region=c("Japan", "China", "North Korea","South Korea"))

# Map region to fill color
ggplot(east_asia, aes(x=long, y=lat, group=group, fill=region)) +
    geom_polygon(colour="black") +
    scale_fill_brewer(palette="Set2")


#CHOOSING A FORMAT TO SAVE:

# Windows programs such as Microsoft Word and PowerPoint have poor support for
# importing PDF files, but they natively support WMF. One drawback is that WMF files
# do not support transparency (alpha).

win.metafile("myplot.wmf", width=4, height=4)
plot(...)
dev.off()

# With ggsave()
ggsave("myplot.wmf", width=8, height=8, units="cm")



# When it comes to importing images, some programs may handle SVG files better than
# PDFs, and vice versa. For example, web browsers tend to have better SVG support, while
# document-creation programs like LaTeX tend to have better PDF support.

svg("myplot.svg", width=4, height=4)
plot(...)
dev.off()

# With ggsave()
ggsave("myplot.svg", width=8, height=8, units="cm")
