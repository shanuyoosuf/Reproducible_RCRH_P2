---
title: "Strom Event - Public Health & Economic impact analysis"
---


## SYNOPSIS
# Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.The analysis shows the impact of weather events were flash floods and thunderstorm winds caused billions of dollars in property damages between 1950 and 2011. The largest damage to crops were caused by droughts, followed by floods and hailing.


## DATA PROCESSING
#Loading raw data

library("ggplot2")
library("R.utils")
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, "StormData.csv.bz2")
library(R.utils)
bunzip2("StormData.csv.bz2", "StormData.csv")

main_data <- read.csv("StormData.csv")

Dim (main_data)
head (main_data)

## Evaluating health impact
data <- main_data[,c("STATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP","CROPDMG", "CROPDMGEXP")]

sum (is.na (data))

## RESULT

# Fatalities

fatal <- aggregate (FATALITIES~EVTYPE, data, sum)
fatal <- fatal [order(fatal$FATALITIES, decreasing=TRUE),]
par(mar=c(12, 6, 1, 1))
barplot (height = fatal$FATALITIES[1:30], names.arg = fatal$EVTYPE[1:30], las = 2, cex.names= 0.8,
         col = rainbow (30, start=0, end=0.5))
title (main = "Fatalities: Top 30 Events", line=-5)
title (ylab = "Totla number of Fatalities", line=4)

# Injuries

injur <- aggregate (INJURIES~EVTYPE, data, sum)
injur <- injur [order(injur$INJURIES, decreasing=TRUE),]
par(mar=c(12, 6, 1, 1))
barplot (height = injur$INJURIES[1:30], names.arg = injur$EVTYPE[1:30], las = 2, cex.names = 0.8,
         col = rainbow (30, start=0, end=0.5))
title (main = "Injuries: Top 30 Events", line=-5)
title (ylab = "Total number of Injuries", line=4)

# Economical impact

symbol <- c("", "+", "-", "?", 0:9, "h", "H", "k", "K", "m", "M", "b", "B");
factor <- c(rep(0,4), 0:9, 2, 2, 3, 3, 6, 6, 9, 9)
multiplier <- data.frame (symbol, factor)

data$damage.prop <- data$PROPDMG*10^multiplier[match(data$PROPDMGEXP,multiplier$symbol),2]
data$damage.crop <- data$CROPDMG*10^multiplier[match(data$CROPDMGEXP,multiplier$symbol),2]
data$damage <- data$damage.prop + data$damage.crop

damage <- aggregate (damage~EVTYPE, data, sum);
damage$bilion <- damage$damage / 1e9;
damage <- damage [order(damage$bilion, decreasing=TRUE),]

par(mar=c(12, 6, 1, 1))
barplot (height = damage$bilion[1:30], names.arg = damage$EVTYPE[1:30], las = 2, cex.names = 0.8,
         col = rainbow (30, start=0, end=0.5))
title ("Damage: Top 30 Events", line=-5)
title (ylab = "Total damage (bilion USD)")

## SUMMARY 

#Tornado and flood are contributed in damaging human health interms of fatalites and injuries and floor directly affected the human life with huge economical impact.



