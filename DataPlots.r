
library(ggplot2)
library(rlang)
library(dplyr)

getwd()
setwd("C:/Users/rawan/Documents/CS424/Project1/CTA-Ridership")

#read in UIC data
uicData <- read.csv(file = "UIC_Data.csv", header = TRUE)

#Re-format date field
library(lubridate)
uicData$newDate <- mdy(uicData$date)
uicData$date <- NULL

# Add up entries at UIC-Halsted for each year 
byYearUIC <- setNames(aggregate(uicData$rides, by=list(format(uicData$newDate, "%Y")), sum), c("Year", "Entries"))

# Entries at UIC-Halsted for Each Year
ggplot(byYearUIC, aes(x=Year, y=Entries)) + 
  geom_bar(stat="identity", width=0.7, fill="steelblue") + 
  labs(title="Entries at UIC-Halsted for Each Year", x="Year", y="Total Entries")

# Data for one year
entriesInOneYear <- subset(uicData, year(uicData$newDate) == 2021)
entriesInOneYear$weekday <- wday(entriesInOneYear$newDate, label=TRUE)

#Entries at UIC-Halsted Each Day of 2021
ggplot(entriesInOneYear, aes(x=newDate, y=rides)) + 
  geom_bar(stat="identity", width=0.7, fill="steelblue") + 
  labs(title="Entries at UIC-Halsted Each Day of 2021", x="Day in 2021", y="Total Entries")

# Add up entries at UIC-Halsted for each Month of 2021
byMonthUIC <- setNames(aggregate(entriesInOneYear$rides, by=list(format(entriesInOneYear$newDate, "%b")), sum), c("Month", "Entries"))
byMonthUIC$Month <- factor(x=byMonthUIC$Month, levels=month.abb, ordered=TRUE)

#Entries at UIC-Halsted Each Month of 2021
ggplot(byMonthUIC, aes(x=Month, y=Entries)) + 
  geom_bar(stat="identity", width=0.7, fill="steelblue") + 
  labs(title="Entries at UIC-Halsted Each Month of 2021", x="Month", y="Total Entries")

# Add up entries at UIC-Halsted for each Day of the Week of 2021
byDayofWeekUIC <- setNames(aggregate(entriesInOneYear$rides, by=list(entriesInOneYear$weekday), sum), c("Day", "Entries"))

#Entries at UIC-Halsted Each Day of the Week of 2021
ggplot(byDayofWeekUIC, aes(x=Day, y=Entries)) + 
  geom_bar(stat="identity", width=0.7, fill="steelblue") + 
  labs(title="Entries at UIC-Halsted Each Day of the Week of 2021", x="Day of the Week", y="Total Entries")
