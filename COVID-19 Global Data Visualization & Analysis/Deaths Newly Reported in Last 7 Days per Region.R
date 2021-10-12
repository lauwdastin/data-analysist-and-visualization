#Author   : Lauw Dastin Falentino Laurianto
#Email    : lauwdastin@gmail.com
#Linkedin : https://www.linkedin.com/in/lauw-dastin-falentino-laurianto

globalData <- read.csv("WHO_COVID-19_Global_Tabel_Data_October_11th_2021.csv", sep = ",", fileEncoding = "UTF-8-BOM", stringsAsFactors = TRUE)
View(globalData)
summary(globalData)

#DNR -> Deaths Newly Reported
globalDNR_SUM <- aggregate(Deaths_Newly_Reported_in_Last_7_Days ~ WHORegion, globalData, FUN = sum)
globalDNR_SUM
globalDNR_SUM <- globalDNR_SUM[order(globalDNR_SUM$Deaths_Newly_Reported_in_Last_7_Days, decreasing = TRUE), ]

globalDNR_AVG <- aggregate(Deaths_Newly_Reported_in_Last_7_Days ~ WHORegion, globalData, FUN = mean)
globalDNR_AVG
globalDNR_AVG <- globalDNR_AVG[order(globalDNR_AVG$Deaths_Newly_Reported_in_Last_7_Days, decreasing = TRUE), ]

#Data Analysis
length(globalData$Deaths_Newly_Reported_in_Last_7_Days)
sum(globalData$Deaths_Newly_Reported_in_Last_7_Days)
summary(globalData$Deaths_Newly_Reported_in_Last_7_Days)
quantile(globalData$Deaths_Newly_Reported_in_Last_7_Days)
sd(globalData$Deaths_Newly_Reported_in_Last_7_Days)

#Pie Chart Visualization
piePercent <- paste(round(globalDNR_SUM$Deaths_Newly_Reported_in_Last_7_Days*100/sum(globalDNR_SUM$Deaths_Newly_Reported_in_Last_7_Days), 5), "%")
pie(globalDNR_SUM$Deaths_Newly_Reported_in_Last_7_Days,
    main = "Global Newly Reported Deaths in 7 Days per Region",
    col = rainbow(length(globalDNR_SUM$Deaths_Newly_Reported_in_Last_7_Days)),
    label = piePercent)
legend("topleft", c("Americas","Europe","South-East Asia","Western Pacific","Eastern Mediterranean
","Africa","Other"), fill = rainbow(length(globalDNR_SUM$Deaths_Newly_Reported_in_Last_7_Days)), cex = 0.63, bty = "n", title = "Region")

#Bar Chart Visualization
barplot(globalDNR_SUM$Deaths_Newly_Reported_in_Last_7_Days,
        main = "Global Newly Reported Deaths in 7 Days per Region",
        names.arg = globalDNR_SUM$WHORegion,
        ylab = "Total Deaths Cases",
        ylim = c(0,25000),
        xlab = "Region",
        width = 1,
        col = rainbow(length(globalDNR_SUM$Deaths_Newly_Reported_in_Last_7_Days)))
