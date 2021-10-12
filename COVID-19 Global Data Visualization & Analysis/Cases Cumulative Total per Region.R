#Author   : Lauw Dastin Falentino Laurianto
#Email    : lauwdastin@gmail.com
#Linkedin : https://www.linkedin.com/in/lauw-dastin-falentino-laurianto

globalData <- read.csv("WHO_COVID-19_Global_Tabel_Data_October_11th_2021.csv", sep = ",", fileEncoding = "UTF-8-BOM", stringsAsFactors = TRUE)
View(globalData)
summary(globalData)

#CCT -> Cases Cumulative Total
globalCCT_SUM <- aggregate(Cases_Cumulative_Total ~ WHORegion, globalData, FUN = sum)
globalCCT_SUM
globalCCT_SUM <- globalCCT_SUM[order(globalCCT_SUM$Cases_Cumulative_Total, decreasing = TRUE), ]

globalCCT_AVG <- aggregate(Cases_Cumulative_Total ~ WHORegion, globalData, FUN = mean)
globalCCT_AVG
globalCCT_AVG <- globalCCT_AVG[order(globalCCT_AVG$Cases_Cumulative_Total, decreasing = TRUE), ]

#Data Analysis
length(globalData$Cases_Cumulative_Total)
sum(globalData$Cases_Cumulative_Total)
summary(globalData$Cases_Cumulative_Total)
quantile(globalData$Cases_Cumulative_Total)
sd(globalData$Cases_Cumulative_Total)

#Pie Chart Visualization
piePercent <- paste(round(globalCCT_SUM$Cases_Cumulative_Total*100/sum(globalCCT_SUM$Cases_Cumulative_Total), 5), "%")
pie(globalCCT_SUM$Cases_Cumulative_Total,
  main = "Global Cumulative Cases per Region",
  col = rainbow(length(globalCCT_SUM$Cases_Cumulative_Total)),
  label = piePercent)
legend("topleft", c("Americas","Europe","South-East Asia","Eastern Mediterranean
","Western Pacific", "Africa","Other"), fill = rainbow(length(globalCCT_SUM$Cases_Cumulative_Total)), cex = 0.63, bty = "n", title = "Region")

#Bar Chart Visualization
barplot(globalCCT_SUM$Cases_Cumulative_Total,
        main = "Global Cumulative Cases per Region",
        names.arg = globalCCT_SUM$WHORegion,
        ylab = "Total Deaths Cases",
        ylim = c(0,100000000),
        xlab = "Region",
        width = 1,
        col = rainbow(length(globalCCT_SUM$Cases_Cumulative_Total)))