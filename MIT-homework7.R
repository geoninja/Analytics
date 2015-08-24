#Week 7 Homeowrk: Visualizations

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











