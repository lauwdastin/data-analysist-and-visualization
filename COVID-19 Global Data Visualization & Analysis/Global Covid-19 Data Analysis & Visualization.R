#Author   : Lauw Dastin Falentino Laurianto
#Email    : lauwdastin@gmail.com
#Linkedin : https://www.linkedin.com/in/lauw-dastin-falentino-laurianto

globalData <- read.csv("WHO_COVID-19_Global_Tabel_Data_October_11th_2021_at_5.43.15_PM.csv", sep = ",", fileEncoding = "UTF-8-BOM", stringsAsFactors = TRUE)
View(globalData)
summary(globalData)

#CCT -> Cases Cumulative Total
globalCCT_SUM <- aggregate(Cases_Cumulative_Total ~ WHORegion, globalData, FUN = sum)
globalCCT_SUM
globalCCT_SUM <- globalCCT_SUM[order(globalCCT_SUM$Cases_Cumulative_Total, decreasing = TRUE), ]

#PieChart Visualization
piePercent <- paste(round(globalCCT_SUM$Cases_Cumulative_Total*100/sum(globalCCT_SUM$Cases_Cumulative_Total), 5), "%")
pie(globalCCT_SUM$Cases_Cumulative_Total,
  main = "Global Cumulative Cases per Region",
  col = rainbow(length(globalCCT_SUM$Cases_Cumulative_Total)),
  clockwise = FALSE,
  label = piePercent)
legend("topleft", c("Americas","Europe","South-East Asia","Eastern Mediterranean
","Western Pacific", "Africa","Other"), fill = rainbow(length(globalCCT_SUM$Cases_Cumulative_Total)), cex = 0.63, bty = "n", title = "Region")

#DR -> Death Report
#Per 100000 Population
global7DaysperPopulationDR_AVG <- aggregate(Deaths_Newly_Reported_in_Last_7_Days_per_100000_Population ~ WHORegion, globalData, mean)
global7DaysperPopulationDR_AVG

global24HoursDR_AVG <- aggregate(Deaths_Newly_Reported_in_Last_24_Hours ~ WHORegion, globalData, mean)
global24HoursDR_AVG