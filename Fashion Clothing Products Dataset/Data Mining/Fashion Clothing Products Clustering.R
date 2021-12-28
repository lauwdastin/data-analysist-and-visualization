clothing_data <- read.csv("myntra_products_catalog.csv")
View(clothing_data)

#Preprocessing Data
preprocess_clothing_data <- clothing_data[clothing_data$PrimaryColor != "",]
View(preprocess_clothing_data)

#Column Gender
preprocess_clothing_data$Gender <- gsub("Boys", "1",
                                        preprocess_clothing_data$Gender)
preprocess_clothing_data$Gender <- gsub("Girls", "2",
                                        preprocess_clothing_data$Gender)
preprocess_clothing_data$Gender <- gsub("Men", "3",
                                        preprocess_clothing_data$Gender)
preprocess_clothing_data$Gender <- gsub("Women", "4",
                                        preprocess_clothing_data$Gender)
preprocess_clothing_data$Gender <- gsub("Unisex Kids", "5",
                                        preprocess_clothing_data$Gender)
preprocess_clothing_data$Gender <- gsub("Unisex", "6",
                                        preprocess_clothing_data$Gender)
preprocess_clothing_data$Gender <- as.numeric(
  preprocess_clothing_data$Gender)

preprocess_clothing_data$Gender <- scale(
  preprocess_clothing_data$Gender) #Normal Scaling

#Column Price
preprocess_clothing_data$Price..INR. <- scale(
  preprocess_clothing_data$Price..INR.) #Normal Scaling

#Column Primary Color
preprocess_clothing_data$PrimaryColor <- gsub("Beige", "1",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Black", "2",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Blue", "3",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Bronze", "4",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Brown", "5",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Burgundy", "6",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Charcoal", "7",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Gold", "8",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Green", "9",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Grey", "10",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Khaki", "11",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Lavender", "12",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Magenta", "13",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Maroon", "14",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Matte", "15",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Mustard", "16",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Navy", "17",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Orange", "18",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Peach", "19",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Pink", "20",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Platinum", "21",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Purple", "22",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Red", "23",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Rose", "24",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Silver", "25",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("White", "26",
                                        preprocess_clothing_data$PrimaryColor)
preprocess_clothing_data$PrimaryColor <- gsub("Yellow", "27",
                                        preprocess_clothing_data$PrimaryColor)

preprocess_clothing_data$PrimaryColor <- as.numeric(
  preprocess_clothing_data$PrimaryColor)

preprocess_clothing_data$PrimaryColor <- scale(
  preprocess_clothing_data$PrimaryColor) #Normal Scaling

#table(preprocess_clothing_data$ProductBrand)

#Clustering
library(cluster)
clustering_data <- preprocess_clothing_data[,c(4,8)]
View(clustering_data)

model = kmeans(clustering_data, 6)
clusplot(clustering_data,model$cluster,color=T,shade=T)
legend("topleft", c("Boys","Girls","Men","Women","Unisex Kids","Unisex"))