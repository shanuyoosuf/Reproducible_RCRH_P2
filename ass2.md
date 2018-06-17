---
title: "Strom Event - Public Health & Economic impact analysis"
---

## SYNOPSIS
 # Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.The analysis shows the impact of weather events were flash floods and thunderstorm winds caused billions of dollars in property damages between 1950 and 2011. The largest damage to crops were caused by droughts, followed by floods and hailing.

## DATA PROCESSING

 #Loading raw data

library("ggplot2")
library("R.utils")
library(reshape2)
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, "StormData.csv.bz2")
bunzip2("StormData.csv.bz2", "StormData.csv")

main_data <- read.csv("StormData.csv")

Dim (main_data)
head (main_data)

    colnames(main_data) <- tolower(colnames(main_data))

    
    data <- subset(x=main_data, 
                   subset=(evtype != "?" & 
                               (injuries > 0 | fatalities > 0 | propdmg > 0 | cropdmg > 0)),
                   select=c("evtype", 
                            "fatalities", 
                            "injuries", 
                            "propdmg", 
                            "propdmgexp", 
                            "cropdmg", 
                            "cropdmgexp"))   
 # Health Data aggregation
 
 data$propdmgexp <- toupper(data$propdmgexp)
    data$cropdmgexp <- toupper(data$cropdmgexp)

    # Map property damage alphanumeric exponents to numeric values.
    propDmgKey <-  c("\"\"" = 10^0,
                     "-" = 10^0, 
                     "+" = 10^0,
                     "0" = 10^0,
                     "1" = 10^1,
                     "2" = 10^2,
                     "3" = 10^3,
                     "4" = 10^4,
                     "5" = 10^5,
                     "6" = 10^6,
                     "7" = 10^7,
                     "8" = 10^8,
                     "9" = 10^9,
                     "H" = 10^2,
                     "K" = 10^3,
                     "M" = 10^6,
                     "B" = 10^9)
    data$propdmgexp <- propDmgKey[as.character(data$propdmgexp)]
    data$propdmgexp[is.na(data$propdmgexp)] <- 10^0
    
    # Map crop damage alphanumeric exponents to numeric values
    cropDmgKey <-  c("\"\"" = 10^0,
                     "?" = 10^0, 
                     "0" = 10^0,
                     "K" = 10^3,
                     "M" = 10^6,
                     "B" = 10^9)
    data$cropdmgexp <- cropDmgKey[as.character(data$cropdmgexp)]
    data$cropdmgexp[is.na(data$cropdmgexp)] <- 10^0
 
 # Aggregate number of fatalities and injuries per evtype into healthData dataframe
    healthData <- aggregate(cbind(fatalities, injuries) ~ evtype, data=data, FUN=sum)
    # Add total column to healthData
    healthData$total <- healthData$fatalities + healthData$injuries
    
 # Remove rows with zero health impact
    healthData <- healthData[healthData$total > 0, ]
    # Sort health data in descending order
    healthData <- healthData[order(healthData$total, decreasing=TRUE), ]
    # Re-label the rows
    rownames(healthData) <- 1:nrow(healthData)
    # Create dataframe of highest health impacting event types and append an "other" event type as a catchall 
    # for everything else
    healthDataTop <- healthData[1:10, ]
    
    # Economic data aggregation
    data$propertyloss <- data$propdmg * data$propdmgexp
    data$croploss <- data$cropdmg * data$cropdmgexp
    economicData <- aggregate(cbind(propertyloss, croploss) ~ evtype, data=data, FUN=sum)
    economicData$total <- economicData$propertyloss + economicData$croploss
    
  # Remove rows with zero economic impact
    economicData <- economicData[economicData$total > 0, ]
    # Sort the economy data in descending order
    economicData <- economicData[order(economicData$total, decreasing=TRUE), ]
    # Re-label the rows
    rownames(economicData) <- tolower(rownames(economicData))
    # Create dataframe of highest economy impacting event types
    economicDataTop <- economicData[1:10, ]    

## RESULTS

 #Ready to plot data
  healthDataTopMelt <- melt(healthDataTop, id.vars="evtype")
  # Plot
  
  healthChart <- ggplot(healthDataTopMelt, aes(x=reorder(evtype, -value), y=value))
        healthChart = healthChart + geom_bar(stat="identity", aes(fill=variable), position="dodge")
        healthChart = healthChart + scale_y_sqrt("Frequency Count") 
        healthChart = healthChart + xlab("Event Type") 
     
    healthChart = healthChart + theme(axis.text.x = element_text(angle=45, hjust=1))
  
    healthChart = healthChart + ggtitle("Pareto Chart of Top 10 US Storm Health Impacts")
  
    plot(healthChart)
    
 # Top 10 Economic impact
 
   # Prepare plot data
   economicDataTopMelt <- melt(economicDataTop, id.vars="evtype")

    economicChart <- ggplot(economicDataTopMelt, aes(x=reorder(evtype, -value), y=value))
    # Add bars                            
    economicChart <- economicChart + geom_bar(stat="identity", aes(fill=variable), position="dodge")
    # Format y-axis scale and set y-axis label
    economicChart <- economicChart + scale_y_sqrt("Damage Impact [$]") 
    # Set x-axis label
    economicChart <- economicChart + xlab("Event Type") 
    # Rotate x-axis tick labels 
    economicChart <- economicChart + theme(axis.text.x = element_text(angle=45, hjust=1))
    # Set chart title
    economicChart <- economicChart + ggtitle("Pareto Chart of Top 10 US Storm Economic Impacts")
    # Display the chart
    print(economicChart)
    
    

## SUMMARY 

 #Tornado and flood are contributed in damaging human health interms of fatalites and injuries and floor directly affected the human life with huge economical impact.



