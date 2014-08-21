##################################################
##### In this file 
##### load libraries and functions ##############
library("knitr")
library("plyr")
library("ggplot2")
library("lubridate")
library("stringr")

library("timeDate")
library("stringdist")
source("R code/dataProcessing.R")
######### load data set ##############
stormData<-read.csv("Data/repdata-data-StormData.csv")
######### data processing ##############
stormData<-dataProcessing(stormData)
########  Fatalities
totalFatalities<-ddply(stormData,.(EVTYPE),summarise,totalFatalities=sum(FATALITIES))
totalFatalities<-totalFatalities[!(totalFatalities$totalFatalities==0),]
totalFatalities$EVTYPE<-as.factor(totalFatalities$EVTYPE)
i<-order(totalFatalities$totalFatalities,decreasing = T )
totalFatalities<-totalFatalities[i,]
##### grafico los primeros 20 eventos con mas  fatalities
top20<-totalFatalities[1:20,]
top20$EVTYPE<- reorder(top20$EVTYPE, top20$totalFatalities,desc)
bar <- ggplot(top20, aes(y=totalFatalities)) 
bar + geom_bar(aes(x=EVTYPE),stat ="identity",binwidth=1 ) +labs(title="20 first Event type and fatalities",y="total fatalities",x="")+
theme(axis.text.x = element_text(hjust=1,angle = 45))
##### INJURIES
totalInjuries<-ddply(stormData,.(EVTYPE),summarise,totalInjuries=sum(INJURIES))
totalInjuries<-totalInjuries[!(totalInjuries$totalInjuries==0),]
totalInjuries$EVTYPE<-as.factor(totalInjuries$EVTYPE)
i<-order(totalInjuries$totalInjuries,decreasing = T )
totalInjuries<-totalInjuries[i,]
##### grafico los primeros 20 eventos con mas  Injuries
top20Injuries<-totalInjuries[1:20,]
top20Injuries$EVTYPE<- reorder(top20Injuries$EVTYPE, top20Injuries$totalInjuries,desc)
bar <- ggplot(top20Injuries, aes(y=totalInjuries)) 
bar + geom_bar(aes(x=EVTYPE),stat ="identity",binwidth=1 ) +labs(title="20 first Event type and injuries",y="total injuries",x="")+
theme(axis.text.x = element_text(hjust=1,angle = 45))

##### PROPDMG 
totalPropDmg<-ddply(stormData,.(EVTYPE),summarise,totalPropDmg=sum(PROPDMG*PropExp))
totalPropDmg<-totalPropDmg[!(totalPropDmg$totalPropDmg==0),]
totalPropDmg$EVTYPE<-as.factor(totalPropDmg$EVTYPE)
i<-order(totalPropDmg$totalPropDmg,decreasing = T )
totalPropDmg<-totalPropDmg[i,]

##### grafico los primeros 20 eventos con mas daño en propiedades
top20PropDmg<-totalPropDmg[1:20,]
top20PropDmg$EVTYPE<- reorder(top20PropDmg$EVTYPE, top20PropDmg$totalPropDmg,desc)
bar <- ggplot(top20PropDmg, aes(y=totalPropDmg)) 
bar + geom_bar(aes(x=EVTYPE),stat ="identity",binwidth=1 ) +labs(title="20 first Event type and Property damage",y="total property damage ",x="")+
    theme(axis.text.x = element_text(hjust=1,angle = 45))

##### CROPDMG 
totalCropDmg<-ddply(stormData,.(EVTYPE),summarise,totalCropDmg=sum(CROPDMG*CropExp))
totalCropDmg<-totalCropDmg[!(totalCropDmg$totalCropDmg==0),]
totalCropDmg$EVTYPE<-as.factor(totalCropDmg$EVTYPE)
i<-order(totalCropDmg$totalCropDmg,decreasing = T )
totalCropDmg<-totalCropDmg[i,]

##### grafico los primeros 20 eventos con mas daño en cultivo
top20CropDmg<-totalCropDmg[1:20,]
top20CropDmg$EVTYPE<- reorder(top20CropDmg$EVTYPE, top20CropDmg$totalCropDmg,desc)
bar <- ggplot(top20CropDmg, aes(y=totalCropDmg)) 
bar + geom_bar(aes(x=EVTYPE),stat ="identity",binwidth=1 ) +labs(title="20 first Event type and Crop damage",y="total crop damage ",x="")+
    theme(axis.text.x = element_text(hjust=1,angle = 45))

stormDataHE<-ddply(stormData,.(EVTYPE),summarise,
                   fatalities=sum(FATALITIES),
                   injuries=sum(INJURIES),
                   propDmg=sum(PROPDMG*PropExp),
                   cropDmg=sum(CROPDMG*CropExp)
                   )
stormDataHE$totalHarmfulHealth<-(stormDataHE$fatalities+stormDataHE$injuries)
stormDataHE$totalDmgEconomic<-(stormDataHE$propDmg+stormDataHE$cropDmg)
stormDataHE<-stormDataHE[!(stormDataHE$totalHarmfulHealth==0&stormDataHE$totalDmgEconomic==0),]
i<-order(stormDataHE$totalHarmfulHealth,decreasing = T )
stormDataHE<-stormDataHE[i,]

ggplot(stormDataHE, aes(x=fatalities, y=injuries, label=EVTYPE),guide=FALSE)+
        geom_point(aes(colour  = cropDmg,size=propDmg))+scale_size_area() +geom_text(size=4)+ theme_bw()

crime <-read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv", header=TRUE, sep="\t")
ggplot(crime, aes(x=murder, y=burglary, size=population, label=state),guide=FALSE)+
        geom_point(colour="white", fill="red", shape=21)+ scale_size_area()+
        scale_x_continuous(name="Murders per 1,000 population", limits=c(0,12))+
        scale_y_continuous(name="Burglaries per 1,000 population", limits=c(0,1250))+
        geom_text(size=4)+ theme_bw()

knit("PA2_template.Rmd")
