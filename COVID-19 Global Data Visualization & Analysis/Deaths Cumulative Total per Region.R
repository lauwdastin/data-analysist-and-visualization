#Author   : Lauw Dastin Falentino Laurianto
#Email    : lauwdastin@gmail.com
#Linkedin : https://www.linkedin.com/in/lauw-dastin-falentino-laurianto

globalData <- read.csv("WHO_COVID-19_Global_Tabel_Data_October_11th_2021.csv", sep = ",", fileEncoding = "UTF-8-BOM", stringsAsFactors = TRUE)
View(globalData)
summary(globalData)

#DCT -> Death Cumulative Total
globalDCT_SUM <- aggregate(Deaths_Cumulative_Total ~ WHORegion, globalData, FUN = sum)
globalDCT_SUM
globalDCT_SUM <- globalDCT_SUM[order(globalDCT_SUM$Deaths_Cumulative_Total, decreasing = TRUE), ]

globalDCT_AVG <- aggregate(Deaths_Cumulative_Total ~ WHORegion, globalData, FUN = mean)
globalDCT_AVG
globalDCT_AVG <- globalDCT_AVG[order(globalDCT_AVG$Deaths_Cumulative_Total, decreasing = TRUE), ]

#Data Analysis
length(globalData$Deaths_Cumulative_Total)
sum(globalData$Deaths_Cumulative_Total)
summary(globalData$Deaths_Cumulative_Total)
quantile(globalData$Deaths_Cumulative_Total)
sd(globalData$Deaths_Cumulative_Total)

#PieChart Visualization
piePercent <- paste(round(globalDCT_SUM$Deaths_Cumulative_Total*100/sum(globalDCT_SUM$Deaths_Cumulative_Total), 5), "%")
pie(globalDCT_SUM$Deaths_Cumulative_Total,
    main = "Global Cumulative Deaths per Region",
    col = rainbow(length(globalDCT_SUM$Deaths_Cumulative_Total)),
    label = piePercent)
legend("topleft", c("Americas","Europe","South-East Asia","Eastern Mediterranean
","Africa","Western Pacific","Other"), fill = rainbow(length(globalDCT_SUM$Deaths_Cumulative_Total)), cex = 0.63, bty = "n", title = "Region")

#Bar Chart Visualization
barplot(globalDCT_SUM$Deaths_Cumulative_Total,
        main = "Global Cumulative Deaths per Region",
        names.arg = globalDCT_SUM$WHORegion,
        ylab = "Total Deaths Cases",
        ylim = c(0,100000000),
        xlab = "Region",
        width = 1,
        col = rainbow(length(globalDCT_SUM$Deaths_Cumulative_Total)))