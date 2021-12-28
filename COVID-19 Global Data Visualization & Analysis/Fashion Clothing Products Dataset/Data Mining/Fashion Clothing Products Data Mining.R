#Read Data Source CSV
clothing_data <- read.csv("myntra_products_catalog.csv")
View(clothing_data)

#Checking datatype
class(clothing_data$ProductName)
class(clothing_data$ProductBrand)
class(clothing_data$Gender)
class(clothing_data$PrimaryColor)

#Checking missing data value
which(is.na(clothing_data))

#Removing primary color that have empty value
clothing_data <- clothing_data[clothing_data$PrimaryColor != "",]

#Data Transformation
apriori_Data <- split(clothing_data$PrimaryColor, clothing_data$ProductBrand)
View(apriori_Data)

#Data Mining
library(arules)

#Frequent Pattern
freq_item_sets <- apriori(apriori_Data, parameter = 
                          list(
                            support = 0.2,
                            target = "frequent itemsets"
                          ))
inspect(freq_item_sets)

#Association Rules
association <- ruleInduction(freq_item_sets, confidence = .8)
inspect(association)

#Spearman Correlation
productID_price_correlation = cor(clothing_data$PrimaryColor, clothing_data$ProductBrand)
productID_price_correlation

#FP between product ID and price
apriori_Data2 <- split(clothing_data$Price..INR.,clothing_data$ProductID)
View(apriori_Data2)

#Frequent Pattern
freq_item_sets2 <- apriori(apriori_Data2, parameter = 
                            list(
                              support = 0.00008,
                              target = "frequent itemsets"
                            ))
inspect(freq_item_sets2)

#Association Rules
association <- ruleInduction(freq_item_sets2, confidence = .8)
inspect(association)

#Converting datatype character into numeric datatype
clothing_data$PrimaryColor <- as.numeric(clothing_data$PrimaryColor)
clothing_data$ProductBrand <- as.numeric(clothing_data$ProductBrand)