#Read CSV
chefMozCuisine <- read.csv("data/chefmozcuisine.csv")
View(chefMozCuisine)

geoPlaces <- read.csv("data/geoplaces2.csv")
View(geoPlaces)

ratingFinal <- read.csv("data/rating_final.csv")
View(ratingFinal)

#PIE 
#a. Show cuisines distribution where the cuisines
#are provided by more than 5 restaurants.

dataCuisine <- merge(chefMozCuisine, geoPlaces,
                     by.x = "placeID", by.y = "placeID")
View(dataCuisine)

countCuisine <- table(dataCuisine$Rcuisine)
countCuisine <- countCuisine[countCuisine > 5]
View(countCuisine)

#install.packages("lessR")
library(lessR)

PieChart(countCuisine,
    main = "Cuisines distribution provided by more than 5 restaurants",
    hole = 0,
    hole_fill = rainbow(length(countCuisine)),
    values = "input",
    )

#HISTOGRAM
#b.	Show the frequency of restaurant cuisines count.

dataRestaurant <- table(dataCuisine$placeID)
View(dataRestaurant)

hist(dataRestaurant,
        main = "Cuisines Count Frequency based on Restaurant",
        ylab = "Frequency",
        xlab = "Cuisines Count",
        col = "lightblue"
    )

# BARPLOT
# c. Show restaurant state distribution that the average rating is above 1.2

dataRating <- merge(ratingFinal, geoPlaces,
                    by.x = "placeID", by.y = "placeID")
dataRating["avg_rating"] <- (dataRating$rating + dataRating$food_rating
                          + dataRating$service_rating) / 3
dataRating <- dataRating[dataRating$avg_rating > 1.2,]
View(dataRating)

dataRating$state <- tolower(dataRating$state)
dataRating$state <- ifelse(dataRating$state=="s.l.p.", "slp",
                      ifelse(dataRating$state=="san luis potos", "slp",
                          ifelse(dataRating$state=="san luis potosi", "slp",
                                  dataRating$state)))

avgRating <- table(dataRating$avg_rating, dataRating$state)
View(avgRating)

barplot(avgRating,
        main = "Average Ratings Distribution based on the State of Restaurant",
        beside = TRUE,
        col = c("red","yellow","green"),
        ylab = "Frequency",
        xlab = "State")

numberDetail = c(rownames(avgRating))
legend("topright", legend = rownames(avgRating),
       text.width = strwidth(numberDetail)[1]/3,
       fill = c("red","yellow","green"), cex = 0.8)

#Data Preprocessing
preprocessData <- dataCuisine[dataCuisine$franchise == "f",]
preprocessData <- preprocessData[preprocessData$other_services == "none",]
preprocessData <- preprocessData[preprocessData$country != "?",]
preprocessData$Rcuisine <- gsub("_"," ",preprocessData$Rcuisine)
View(preprocessData)

#Data Transformation
aprioriData <- split(preprocessData$Rcuisine, preprocessData$placeID)
View(aprioriData)

#Data Mining
#install.packages("arules")
library(arules)

#Frequent Item Set
freqItemSets <- apriori(aprioriData, parameter = 
          list(
            support = 0.008,
            target = "frequent itemsets"
          ))

inspect(freqItemSets)

#Association Rules
assocRule <- ruleInduction(freqItemSets, confidence = 0.8)

inspect(assocRule)