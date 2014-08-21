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
stormDataRaw<-read.csv("Data/repdata-data-StormData.csv")
######### data processing ##############
stormData<-dataProcessing(stormDataRaw)
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
i<-order(stormDataHE$totalHarmfulHealth,decreasing = T )
stormDataHE<-stormDataHE[i,]

stormDataHE2<-stormDataHE[1:50,]
str(stormDataHE2)
factor(stormDataHE2$id)
ggplot(stormDataHE, aes(x=id2, y=id),guide=FALSE)+
        geom_point(aes(colour=totalDmgEconomic,size=totalHarmfulHealth))+scale_size_area(min=5,max_size=25) +geom_text(size=2)+ theme_bw()

ggplot(stormDataHE, aes(x=id2, y=id3, label=EVTYPE),guide=FALSE) +geom_point(aes(size=sc2,colour=sc1))+
    scale_size_continuous(range = c(3,15))+scale_colour_gradientn(colours=rainbow(2))+geom_text(size=3, vjust = 1.9,hjust = 0.5)+ theme_bw()

var(stormDataHE$totalHarmfulHealth)
sd(stormDataHE$totalHarmfulHealth)

ggplot(melt(C)) +
    geom_point(aes(id2,id3)) +
    geom_rect(aes(xmin=as.numeric(Var1)-0.5*abs(value),xmax=as.numeric(Var1)+0.5*abs(value),ymin=as.numeric(Var2)-0.5*abs(value),ymax=as.numeric(Var2)+0.5*abs(value),fill=as.factor(sign(value)))) +
    theme_bw()

d <- ggplot(stormDataHE, aes(uni, uni2)) 
    d+stat_bin2d(bins = 25, colour="grey50")

crime <-read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv", header=TRUE, sep="\t")
ggplot(crime, aes(x=murder, y=burglary, size=population, label=state),guide=FALSE)+
        geom_point(colour="white", fill="red", shape=21)+ scale_size_area()+
        scale_x_continuous(name="Murders per 1,000 population", limits=c(0,12))+
        scale_y_continuous(name="Burglaries per 1,000 population", limits=c(0,1250))+
        geom_text(size=4)+ theme_bw()

stormData$EVTYPE[grep(pattern = "^WATERS", stormDataHE$EVTYPE)]
length(unique(stormData$EVTYPE))
length(unique(stormDataRaw$EVTYPE))
setwd("C:/Users/Usuarioç/Desktop/carlos/reproducible research/RepData_PeerAssessment2")
getwd()
knit("PA2_template.Rmd")
dunif(x, min = 1, max = 250, log = FALSE)
v<-(floor(100*runif(250)))
v2<-(floor(100*runif(250)))
stormDataHE$uni2<-v2
stormDataHE$id<-c(1:250)
stormDataHE$id3<-seq(from=10, to=250, by=10)
stormDataHE$id2<-rep(1:10, each=25)

stormDataHE$sc1<-scale(stormDataHE$totalHarmfulHealth,center = T)
stormDataHE$sc2<-scale(stormDataHE$totalDmgEconomic,center = T)

x <- c("abcd hola", "efgh", "abce")
abbreviate(stormDataHE$EVTYPE,method = "both",dot = T)
abbreviate(x, 2, strict = TRUE) # >> 1st and 3rd are == "ab"
st.ab2 <- abbreviate(state.name, 2, method = "both")
