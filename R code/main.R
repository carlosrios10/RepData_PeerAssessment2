##################################################
##### In this file 
##### load libraries and functions ##############
setwd("D:/coursera-reproducible-research/RepData_PeerAssessment2")
rm(list = ls())
library("knitr")
library("plyr")
library("ggplot2")
library("lubridate")
library("stringr")
library("reshape2")
sessionInfo()
library("timeDate")
library("stringdist")
source("R code/dataProcessing.R")
source("R code/loadRawData.R")
######### load data set ##############
stormDataRaw<-loadRawData()
dim(stormDataRaw)
head(stormDataRaw)
str(stormDataRaw)
######### data processing ##############
stormData<-dataProcessing(stormDataRaw)
head(stormData[,c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PropExp","CROPDMG","CropExp")])
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
###############################################################
stormDataHE<-ddply(stormData,.(EVTYPE),summarise,
                   fatalities=sum(FATALITIES),
                   injuries=sum(INJURIES),
                   propDmg=sum(PROPDMG*PropExp),
                   cropDmg=sum(CROPDMG*CropExp)
                   )
stormDataHE$totalHarmfulHealth<-(stormDataHE$fatalities+stormDataHE$injuries)
stormDataHE$totalDmgEconomic<-(stormDataHE$propDmg+stormDataHE$cropDmg)
stormDataHE<-stormDataHE[!(stormDataHE$totalHarmfulHealth==0&stormDataHE$totalDmgEconomic==0),]
stormDataHE$perFatalities<-prop.table(stormDataHE$fatalities)
stormDataHE$perInjuries<-prop.table(stormDataHE$injuries)
stormDataHE$perPropDmg<-prop.table(stormDataHE$propDmg)
stormDataHE$perCropDmg<-prop.table(stormDataHE$cropDmg)


########## total damage to helth population
i<-order(stormDataHE$totalHarmfulHealth,decreasing = T )
stormDataHE<-stormDataHE[i,]
top20<-stormDataHE[1:20,]
top20$EVTYPE<- reorder(top20$EVTYPE, top20$totalHarmfulHealth,desc)
top20 <- melt(top20[,1:3], id.vars = c("EVTYPE"))
bar <- ggplot(top20, aes(y=value,fill=variable)) 
bar + geom_bar(aes(x=EVTYPE),position="dodge",stat ="identity",binwidth=1 ) + 
        scale_y_continuous(breaks = seq(from=0,to=10^5,by=10^4))+
        labs(title="First twenty events and total damage to population health",y="number of victims",x="")+
        theme(axis.text.x = element_text(hjust=1,angle = 45))+
        theme(legend.title=element_blank())

########## total damage to economy
i<-order(stormDataHE$totalDmgEconomic,decreasing = T )
stormDataHE<-stormDataHE[i,]
top20<-stormDataHE[1:20,]
top20$EVTYPE<- reorder(top20$EVTYPE, top20$totalDmgEconomic,desc)
top20 <- melt(top20[c("EVTYPE","propDmg","cropDmg")], id.vars = c("EVTYPE"))
bar <- ggplot(top20, aes(y=value,fill=variable)) 
bar + geom_bar(aes(x=EVTYPE),position="dodge",stat ="identity",binwidth=1 ) +
        labs(title="First twenty events and total damage to economy",y="dollar amounts",x="")+
        theme(axis.text.x = element_text(hjust=1,angle = 45))+
        theme(legend.title=element_blank())


########## 
i<-order(stormDataHE$totalDmgEconomic,decreasing = T )
stormDataHE<-stormDataHE[i,]
TFirstEventsDmgEconomic<-stormDataHE[1:20,1]
i<-order(stormDataHE$totalHarmfulHealth,decreasing = T )
stormDataHE<-stormDataHE[i,]
TFirstEventsHealth<-stormDataHE[1:20,1]
unionEvents<-union(TFirstEventsDmgEconomic,TFirstEventsHealth)
selectStormData<-stormDataHE[stormDataHE$EVTYPE %in% unionEvents,]
ggplot(melt(selectStormData[c("EVTYPE","perFatalities","perInjuries","perPropDmg","perCropDmg")], id.vars = c("EVTYPE")), aes(x=variable, y=EVTYPE)) +
        geom_tile(aes(fill=value))+
        labs(title="Relationship between damage to economy and \n damage to population health",y="",x="")+
        scale_fill_gradient2( name="%Damage",low = "green", high = "red", breaks=c(0,0.65),labels=c("Minimum","Maximum")) +
        scale_x_discrete(breaks=c("perFatalities", "perInjuries", "perPropDmg","perCropDmg"), 
                         labels=c("fatalities", "injuries", "property-damage","crop-damage"))+
        theme(axis.text.x = element_text(hjust=1,angle = 45))

knit("PA2_template.Rmd")
require(markdown)
markdownToHTML('PA2_template.md', 'PA2_template.html', options=c("use_xhml"))
pandoc('PA2_template.Rmd', format='latex')
knit2pdf('PA2_template.Rmd')
Sys.which("pdflatex")
Sys.which('texi2dvi')

bar <- ggplot(cars, aes(y=speed,x=dist))
bar + geom_point()