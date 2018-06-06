---
title: "Strom Event - Public Health & Economic impact analysis"
output: word_document
---

## Synopsis : Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

## Data Processing - The below steps are unboxing the data and prepare to process.
## download the raw data

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, "StormData.csv.bz2")
library(R.utils)
bunzip2("StormData.csv.bz2", "StormData.csv")

data_fl <- read.csv("StormData.csv")

## Evaluating health impact
library(dplyr)
data_fl.fatalities <- data_fl %>% select(EVTYPE, FATALITIES) %>% group_by(EVTYPE) %>% summarise(total.fatalities = sum(FATALITIES)) %>% arrange(-total.fatalities)
head(data_fl.fatalities, 10)

data_fl.injuries <- data_fl %>% select(EVTYPE, INJURIES) %>% group_by(EVTYPE) %>% summarise(total.injuries = sum(INJURIES)) %>% arrange(-total.injuries)
head(data_fl.injuries, 10)

## Evaluating Economic Impact 
data_fl.damage <- data_fl %>% select(EVTYPE, PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)

Symbol <- sort(unique(as.character(data_fl.damage$PROPDMGEXP)))
Multiplier <- c(0,0,0,1,10,10,10,10,10,10,10,10,10,10^9,10^2,10^2,10^3,10^6,10^6)
convert.Multiplier <- data.frame(Symbol, Multiplier)

data_fl.damage$Prop.Multiplier <- convert.Multiplier$Multiplier[match(data_fl.damage$PROPDMGEXP, convert.Multiplier$Symbol)]
data_fl.damage$Crop.Multiplier <- convert.Multiplier$Multiplier[match(data_fl.damage$CROPDMGEXP, convert.Multiplier$Symbol)]

data_fl.damage <- data_fl.damage %>% mutate(PROPDMG = PROPDMG*Prop.Multiplier) %>% mutate(CROPDMG = CROPDMG*Crop.Multiplier) %>% mutate(TOTAL.DMG = PROPDMG+CROPDMG)

data_fl.damage.total <- data_fl.damage %>% group_by(EVTYPE) %>% summarize(TOTAL.DMG.EVTYPE = sum(TOTAL.DMG))%>% arrange(-TOTAL.DMG.EVTYPE) 

head(data_fl.damage.total,10)


## RESULTS - Both Health and economic impact formation.

## Health Impact

library(ggplot2)
g <- ggplot(data_fl.fatalities[1:10,], aes(x=reorder(EVTYPE, -total.fatalities), y=total.fatalities))+geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))+ggtitle("Top 10 Events with Highest Total Fatalities") +labs(x="EVENT TYPE", y="Total Fatalities")
g

## High total injuries

g <- ggplot(data_fl.injuries[1:10,], aes(x=reorder(EVTYPE, -total.injuries), y=total.injuries))+geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))+ggtitle("Top 10 Events with Highest Total Injuries") +labs(x="EVENT TYPE", y="Total Injuries")
g

## Economic impact

g <- ggplot(data_fl.damage.total[1:10,], aes(x=reorder(EVTYPE, -TOTAL.DMG.EVTYPE), y=TOTAL.DMG.EVTYPE))+geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))+ggtitle("Top 10 Events with Highest Economic Impact") +labs(x="EVENT TYPE", y="Total Economic Impact ($USD)")

g

## END