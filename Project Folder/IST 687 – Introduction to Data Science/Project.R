#Read in CSV files and assign to data frames
beersdf <- read.csv("C:/Users/Noah/Desktop/Syracuse/IST 687/Project/beers.csv")
breweriesdf <- read.csv("C:/Users/Noah/Desktop/Syracuse/IST 687/Project/breweries.csv")
breweriesdf

#Merge beersdf and breweriesdf on breweryid and X
combineddf <- merge(beersdf, breweriesdf, by.x = "brewery_id", by.y = "X" )
str(combineddf)

#Begin processing data
#Remove column for X(not needed)
combineddf$X <- NULL
combineddf

#Change column names for id=beer_id, name.x=beer_name, name.y = brewery_name and state=state_abb
colnames(combineddf)[which(names(combineddf) == "id")] <- "beer_id"
colnames(combineddf)[which(names(combineddf) == "name.x")] <- "beer_name"
colnames(combineddf)[which(names(combineddf) == "name.y")] <- "brewery_name"
colnames(combineddf)[which(names(combineddf) == "state")] <- "state_abv"

#Remove blank spaces from state_abv column
combineddf$state_abv <- gsub(" ","", combineddf$state_abv)

#Add column for full state name 
combineddf$state_name <- tolower(state.name[match(combineddf$state, state.abb)])

#Check data types are correct
str(combineddf)
#Confirmed that data types are correct

#Check Summary of data frame
summary(combineddf)

#Create histograms for IBU and ABV
hist(combineddf$abv)
hist(combineddf$ibu)

#Begin analyzing data and answering questions
#What styles of beer are produced the most?
library(dplyr)
#Find the top ten beer styles
beerstyles <- combineddf %>% count(style, sort=TRUE) %>% top_n(10)
colnames(beerstyles)[which(names(beerstyles) == "n")] <- "beer_count"
beerstyles

#Create bar plot
library(ggplot2)
gstyle <-ggplot(beerstyles, aes(x = reorder(style, -beer_count), beer_count)) + geom_bar(stat="identity")+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
gstyle <- gstyle + theme(axis.text=element_text(size=12), axis.title.x = element_blank()) + ggtitle("Number of Craft Beers by Beer Style")
gstyle

#What cities and states have the most beers?
#Find the top ten cities
beercities <- combineddf %>% count(city, sort=TRUE) %>% top_n(10)
colnames(beercities)[which(names(beercities) == "n")] <- "beer_count"
beercities
#Create bar plot
gcities <-ggplot(beercities, aes(x = reorder(city, -beer_count), beer_count)) + geom_bar(stat="identity")+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
gcities <- gcities + theme(axis.text=element_text(size=12), axis.title.x = element_blank()) + ggtitle("Cities with the Highest Number of Craft Beers")
gcities 

#Order states by beer count for all states
beerstatesall <- combineddf %>% count(state_name, sort=TRUE)
colnames(beerstatesall)[which(names(beerstatesall) == "n")] <- "beer_count"
beerstatesall

#Find the top ten states by number of beers
beerstates <- combineddf %>% count(state_name, sort=TRUE) %>% top_n(10)
colnames(beerstates)[which(names(beerstates) == "n")] <- "beer_count"
beerstates
#Create bar plot
gstates <-ggplot(beerstates, aes(x = reorder(state_name, -beer_count), beer_count)) + geom_bar(stat="identity")+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
gstates <- gstates + theme(axis.text=element_text(size=12), axis.title.x = element_blank()) + ggtitle("States with the Highest Number of Craft Beers")
gstates 

#Create map for each state with color representing beer count
library(ggmap)
us <- map_data("state")

map.beer <- ggplot(beerstatesall, aes(map_id = state_name))
map.beer <- map.beer + geom_map(map = us, aes(fill=beer_count))
map.beer <- map.beer + expand_limits(x = us$long, y = us$lat)
map.beer <- map.beer + coord_map() + ggtitle("Beer Count Per State")
map.beer

#Which states and cities are producing beers with the highest IBU(bitterness)?
#Remove blanks for IBU column
ibudf <- combineddf[!is.na(combineddf$ibu),]
ibudf

#Find the top ten cities and aggregate by mean ibu
ibucities <- aggregate(x = ibudf$ibu,  
          by = list(ibudf$city),      
          FUN = mean) %>% top_n(10)
colnames(ibucities)[which(names(ibucities) == "Group.1")] <- "city"
colnames(ibucities)[which(names(ibucities) == "x")] <- "ibu"
ibucities <- ibucities[order(-ibucities$ibu),]
ibucities
#Create bar plot
gibucities <-ggplot(ibucities, aes(x = reorder(city, -ibu), ibu)) + geom_bar(stat="identity")+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
gibucities <- gibucities + theme(axis.text=element_text(size=12), axis.title.x = element_blank()) + ggtitle("Cities with the Highest Average IBU Beers")
gibucities 

#Find the top ten states
ibustates <- aggregate(x = ibudf$ibu,  
                       by = list(ibudf$state_name),      
                       FUN = mean)
colnames(ibustates)[which(names(ibustates) == "Group.1")] <- "state_name"
colnames(ibustates)[which(names(ibustates) == "x")] <- "ibu"
ibustates

#Narrow down to top 10 states
ibustatestop10 <- ibustates %>% top_n(10)
ibustatestop10 <- ibustatestop10[order(-ibustatestop10$ibu),]
ibustatestop10

#Create bar plot
gibustates <-ggplot(ibustatestop10, aes(x = reorder(state_name, -ibu), ibu)) + geom_bar(stat="identity")+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
gibustates <- gibustates + theme(axis.text=element_text(size=12), axis.title.x = element_blank()) + ggtitle("States with the Highest Average IBU Beers")
gibustates 

#Create map for each state with color representing mean ibu
us <- map_data("state")
map.ibu <- ggplot(ibustates, aes(map_id = state_name))
map.ibu <- map.ibu + geom_map(map = us, aes(fill=ibu))
map.ibu <- map.ibu + expand_limits(x = us$long, y = us$lat)
map.ibu <- map.ibu + coord_map() + ggtitle("Average IBU Per State")
map.ibu

#Which states and cities are producing beers with the highest alcohol content?
#Remove blanks for abv column
abvdf <- combineddf[!is.na(combineddf$abv),]
abvdf

#Find the top ten cities and aggregate cites by mean abv
abvcities <- aggregate(x = abvdf$abv,  
                       by = list(abvdf$city),      
                       FUN = mean) %>% top_n(10)
colnames(abvcities)[which(names(abvcities) == "Group.1")] <- "city"
colnames(abvcities)[which(names(abvcities) == "x")] <- "abv"
abvcities <- abvcities[order(-abvcities$abv),]
abvcities

#Create bar plot
gabvcities <-ggplot(abvcities, aes(x = reorder(city, -abv), abv)) + geom_bar(stat="identity")+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
gabvcities <- gabvcities + theme(axis.text=element_text(size=12), axis.title.x = element_blank()) + ggtitle("Cites with the Highest Average ABV Beers")
gabvcities 

#Aggregate by mean abv for all states
abvstates <- aggregate(x = abvdf$abv,  
                       by = list(abvdf$state_name),      
                       FUN = mean) 
colnames(abvstates)[which(names(abvstates) == "Group.1")] <- "state_name"
colnames(abvstates)[which(names(abvstates) == "x")] <- "abv"
abvstates

#Find the top ten states
abvstatestop10 <- abvstates %>% top_n(10)
abvstatestop10 <- abvstatestop10[order(-abvstatestop10$abv),]
abvstatestop10

#Create bar plot for top 10
gabvstatestop10 <-ggplot(abvstatestop10, aes(x = reorder(state_name, -abv), abv)) + geom_bar(stat="identity")+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
gabvstatestop10 <- gabvstatestop10 + theme(axis.text=element_text(size=12), axis.title.x = element_blank()) + ggtitle("States with the Highest Average ABV Beers")
gabvstatestop10 

#Create map for each state with color representing mean abv
us <- map_data("state")
map.abv <- ggplot(abvstates, aes(map_id = state_name))
map.abv <- map.abv + geom_map(map = us, aes(fill=abv))
map.abv <- map.abv + expand_limits(x = us$long, y = us$lat)
map.abv <- map.abv + coord_map() + ggtitle("Average ABV Per State")
map.abv

#Predict ABV value as a function of IBU value using linear modeling
#Import Libraries
library(kernlab)
library(e1071)

#Create new data frame for modeling with only the IBU and ABV columns from beersdf
modeldf <- data.frame(beersdf$abv, beersdf$ibu)
#Change column names
colnames(modeldf)[which(names(modeldf) == "beersdf.abv")] <- "abv"
colnames(modeldf)[which(names(modeldf) == "beersdf.ibu")] <- "ibu"

#Omit NAs
modeldf <- na.omit(modeldf)
str(modeldf)

#Plot abv vs. ibu
gIbuAbv <-ggplot(modeldf, aes(ibu, abv)) + geom_point()
gIbuAbv <- gIbuAbv + ggtitle("ABV vs. IBU")
gIbuAbv 

#Create train and test data sets
#Create randindex
randIndex <- sample(1:dim(modeldf)[1])
#Create cut point at 2/3rds of data
cutpoint2_3 <- floor(2 * dim(modeldf)[1]/3)
#Create trainData set
trainData <- modeldf[randIndex[1:cutpoint2_3],]

#Create testData set
testData <- modeldf[randIndex[(cutpoint2_3 + 1):dim(modeldf[1])],]
#Look at structure of testdata and traindata
str(testData)
str(trainData)

#Compute model for lm with trainData set
regressionlm <- lm(formula = abv~ibu, data=trainData)
summary(regressionlm)
regressionlm

#Test the lm model on	the	testData set
lmPredicted <- 0.0452188 + 0.0003428*testData$ibu
lmPredicted

#Create comparison table for actual and predicted 
lmActual = testData[,1]
lmCompTable <- data.frame(lmActual,lmPredicted)
lmCompTable

#Compute the Root	Mean Squared Error for the model
lmRMSE = sqrt(mean((lmCompTable$lmActual - lmCompTable$lmPredicted)^2))
lmRMSE

# Calculate error and add to table
lmCompTable$error <- lmActual - lmPredicted
#Create Plot
lmScat <- ggplot(testData, aes(x=testData$ibu, y=testData$abv, color=abs(lmCompTable$error), size=abs(lmCompTable$error))) + geom_point()
lmScat <- lmScat + ggtitle("IBU vs ABV (LM Prediction)")
lmScat

#Compute model ksvm with trainData set
ksvmOutput	<-	ksvm(abv ~	ibu,data=trainData, kernel="rbfdot",kpar="automatic",
                  C=5,cross=3,prob.model=TRUE)
ksvmOutput

#run new model with c=50
ksvmOutput2	<-	ksvm(abv ~	ibu,data=trainData, kernel="rbfdot",kpar="automatic",
                   C=50,cross=3,prob.model=TRUE)

ksvmOutput2

#Test the model on the testData set
ksvmPred <- predict(ksvmOutput, testData, type="votes")

#Create comparison table for actual and predicted 
ksvmActual = testData[,1]
ksvmPredicted = ksvmPred[,1]
ksvmCompTable <- data.frame(ksvmActual,ksvmPredicted)

#Compute the Root	Mean Squared Error for the model
ksvmRMSE = sqrt(mean((ksvmCompTable$ksvmActual - ksvmCompTable$ksvmPredicted)^2))
ksvmRMSE

# Calculate error and add to table
ksvmCompTable$error <- ksvmActual - ksvmPredicted
#Create Plot
ksvmScat <- ggplot(testData, aes(x=testData$ibu, y=testData$abv, color=abs(ksvmCompTable$error), size=abs(ksvmCompTable$error))) + geom_point()
ksvmScat <- ksvmScat + ggtitle("IBU vs ABV (ksvm Prediction)")
ksvmScat

