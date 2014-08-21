---
title: "PA2_template"
author: "carlos rios"
date: "Wednesday, August 20, 2014"
output: html_document
---
##Synopsis

##Data Processing
blabla

### load libraries

```r
library("knitr")
library("plyr")
library("ggplot2")
library("lubridate")
library("stringr")
source("R code/dataProcessing.R")
```

### load data set


```r
stormData <- read.csv("Data/repdata-data-StormData.csv")
```

### Processing data set

```r
stormData <- dataProcessing(stormData)
```


##Results 
blabla


```r
totalFatalities <- ddply(stormData, .(EVTYPE), summarise, totalFatalities = sum(FATALITIES))
totalFatalities <- totalFatalities[!(totalFatalities$totalFatalities == 0), 
    ]
totalFatalities$EVTYPE <- as.factor(totalFatalities$EVTYPE)
i <- order(totalFatalities$totalFatalities, decreasing = T)
totalFatalities <- totalFatalities[i, ]
```



```r
top20 <- totalFatalities[1:20, ]
top20$EVTYPE <- reorder(top20$EVTYPE, top20$totalFatalities, desc)
bar <- ggplot(top20, aes(y = totalFatalities))
bar + geom_bar(aes(x = EVTYPE), stat = "identity", binwidth = 1) + labs(title = "20 first Event type and injuries", 
    y = "total injuries", x = "") + theme(axis.text.x = element_text(hjust = 1, 
    angle = 45))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

