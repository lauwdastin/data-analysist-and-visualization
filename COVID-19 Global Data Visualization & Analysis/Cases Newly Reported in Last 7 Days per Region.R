#Author   : Lauw Dastin Falentino Laurianto
#Email    : lauwdastin@gmail.com
#Linkedin : https://www.linkedin.com/in/lauw-dastin-falentino-laurianto

globalData <- read.csv("WHO_COVID-19_Global_Tabel_Data_October_11th_2021.csv", sep = ",", fileEncoding = "UTF-8-BOM", stringsAsFactors = TRUE)
View(globalData)
summary(globalData)

#CNR -> Cases Newly Reported
globalCNR_SUM <- aggregate(Cases_Newly_Reported_in_Last_7_Days ~ WHORegion, globalData, FUN = sum)
globalCNR_SUM
globalCNR_SUM <- globalCNR_SUM[order(globalCNR_SUM$Cases_Newly_Reported_in_Last_7_Days, decreasing = TRUE), ]

globalCNR_AVG <- aggregate(Cases_Newly_Reported_in_Last_7_Days ~ WHORegion, globalData, FUN = mean)
globalCNR_AVG
globalCNR_AVG <- globalCNR_AVG[order(globalCNR_AVG$Cases_Newly_Reported_in_Last_7_Days, decreasing = TRUE), ]

#Data Analysis
length(globalData$Cases_Newly_Reported_in_Last_7_Days)
sum(globalData$Cases_Newly_Reported_in_Last_7_Days)
summary(globalData$Cases_Newly_Reported_in_Last_7_Days)
quantile(globalData$Cases_Newly_Reported_in_Last_7_Days)
sd(globalData$Cases_Newly_Reported_in_Last_7_Days)

#Pie Chart Visualization
piePercent <- paste(round(globalCNR_SUM$Cases_Newly_Reported_in_Last_7_Days*100/sum(globalCNR_SUM$Cases_Newly_Reported_in_Last_7_Days), 5), "%")
pie(globalCNR_SUM$Cases_Newly_Reported_in_Last_7_Days,
    main = "Global Newly Reported Cases in 7 Days per Region",
    col = rainbow(length(globalCNR_SUM$Cases_Newly_Reported_in_Last_7_Days)),
    label = piePercent)
legend("topleft", c("Europe","Americas","Western Pacific","South-East Asia","Eastern Mediterranean
","Africa","Other"), fill = rainbow(length(globalCNR_SUM$Cases_Newly_Reported_in_Last_7_Days)), cex = 0.63, bty = "n", title = "Region")

#Bar Chart Visualization
barplot(globalCNR_SUM$Cases_Newly_Reported_in_Last_7_Days,
        main = "Global Newly Reported Cases in 7 Days per Region",
        names.arg = globalCNR_SUM$WHORegion,
        ylab = "Total Deaths Cases",
        ylim = c(0,1600000),
        xlab = "Region",
        width = 1,
        col = rainbow(length(globalCNR_SUM$Cases_Newly_Reported_in_Last_7_Days)))