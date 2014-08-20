setwd("C:/Users/Usuarioç/Desktop/carlos/reproducible research/RepData_PeerAssessment2")
getwd()
rm(list = ls())
sessionInfo()
library("knitr")
library("plyr")
library("ggplot2")
library("timeDate")
library("downloader")
library("stringr")
library("stringdist")
library("lubridate")
##Loading and Processing the Raw Data
stormData<-read.csv("data/repdata-data-StormData.csv")
event<-read.table("data/Storm_Data_Event.txt",sep="\t")
#vemos la cantidad de filas y columnas
dim(stormData)
str(stormData)
#las variables que me interesan
head(stormData[,c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")])
summary(stormData[,c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")])
#valores perdidos
sum(is.na(stormData[,c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]))
#date
stormData$YEAR<-year(mdy_hms(as.character(stormData$BGN_DATE)))
#variable EVTYPE
stormData$EVTYPE<-toupper(stormData$EVTYPE)
stormData$EVTYPE<-str_trim(stormData$EVTYPE, side = "both")
stormData$EVTYPE<-gsub(pattern = "TSTM",replacement ="THUNDERSTORM",x =stormData$EVTYPE ) 
stormData$EVTYPE<-gsub(pattern = "\\(.+?\\)",replacement = "", x = stormData$EVTYPE)
stormData$EVTYPE<-gsub(pattern = "[0-9]",replacement = "", x = stormData$EVTYPE)
stormData$EVTYPE<-gsub(pattern = ".",replacement = "", x = stormData$EVTYPE,fixed = T)
stormData$EVTYPE<-gsub(pattern = ":",replacement = "", x = stormData$EVTYPE,fixed = T)
stormData$EVTYPE<-gsub(pattern = "-",replacement = "", x = stormData$EVTYPE,fixed = T)
stormData$EVTYPE<-gsub(pattern = "\\(|\\)",replacement = "", x = stormData$EVTYPE)
stormData$EVTYPE[grep(pattern = "^THUNDERSTORM WIND", stormData$EVTYPE)]<-"THUNDERSTORM WIND"
stormData$EVTYPE[grep(pattern = "^HURRICANE", stormData$EVTYPE)]<-"HURRICANE"
stormData$EVTYPE[grep(pattern = "^FLASH FLO", stormData$EVTYPE)]<-"FLASH FLOOD"
stormData$EVTYPE[grep(pattern = "^FLOOD", stormData$EVTYPE)]<-"FLOOD"
stormData$EVTYPE[grep(pattern = "^TORN", stormData$EVTYPE)]<-"TORNADO"
stormData$EVTYPE[grep(pattern = "^HEAVY RA", stormData$EVTYPE)]<-"HEAVY RAIN"
stormData$EVTYPE[grep(pattern = "^HEAVY SN", stormData$EVTYPE)]<-"HEAVY SNOW"
stormData$EVTYPE[grep(pattern = "^HIGH WI", stormData$EVTYPE)]<-"HIGH WIND"
stormData$EVTYPE[grep(pattern = "^BEACH ERO", stormData$EVTYPE)]<-"BEACH EROSION"
stormData$EVTYPE[grep(pattern = "^AVALAN", stormData$EVTYPE)]<-"AVALANCHE"
stormData$EVTYPE[grep(pattern = "^BITTER WIND CHILL", stormData$EVTYPE)]<-"BITTER WIND CHILL"
stormData$EVTYPE[grep(pattern = "^BLIZZARD", stormData$EVTYPE)]<-"BLIZZARD"
stormData$EVTYPE[grep(pattern = "^EXTREME WIND", stormData$EVTYPE)]<-"EXTREME WIND"
stormData$EVTYPE[grep(pattern = "^FREEZING RAIN", stormData$EVTYPE)]<-"FREEZING RAIN"
stormData$EVTYPE[grep(pattern = "^FUNNEL", stormData$EVTYPE)]<-"FUNNEL CLOUD"
stormData$EVTYPE[grep(pattern = "^BLOWING SNOW", stormData$EVTYPE)]<-"BLOWING SNOW"
stormData$EVTYPE[grep(pattern = "^LIGHTNING", stormData$EVTYPE)]<-"LIGHTNING"


unique(stormData$EVTYPE)[order(unique(stormData$EVTYPE))]
i <- amatch(stormData$EVTYPE, t(event),maxDist=3)
stormData$EVTYPECOD<-t(event)[i]
stormData$EVTYPECOD[is.na(stormData$EVTYPECOD)]<-stormData$EVTYPE[is.na(stormData$EVTYPECOD)]
sum(is.na(stormData$EVTYPECOD))
unique(stormData$EVTYPECOD)
dd<-data.frame(v=stormData$EVTYPE[grep(pattern = "^LIGHTNING", stormData$EVTYPE)])
table(dd$v)
class(stormData$EVTYPE)
pp<-data.frame(v=unique(stormData$EVTYPECOD)[order(unique(stormData$EVTYPECOD))])

pp[grep(pattern = "^THUNDERSTORM WIND", pp)]

event$V1<-toupper(event$V1)
event$V1<-str_trim(event$V1, side = "both")

j <- amatch(pp, t(event),maxDist=3)
pp[j]<-t(event)[j]
ev<-data.frame(rawtext = pp, coded = t(event)[j],stringsAsFactors = F)
ev$coded[is.na(ev$coded)]<-ev$rawtext[is.na(ev$coded)]

#variable FATALITIES
sum(is.na(stormData$FATALITIES))
sum(stormData$FATALITIES=="")
#variable INJURIES
sum(is.na(stormData$INJURIES))
sum(stormData$INJURIES=="")
#variable PROPDMG
sum(is.na(stormData$PROPDMG))
sum(stormData$PROPDMG=="")
#variable PROPDMGEXP
sum(is.na(stormData$PROPDMGEXP))
sum(stormData$PROPDMGEXP=="")
levels(stormData$PROPDMGEXP)
table(stormData$PROPDMGEXP)
#variable CROPDMG
sum(is.na(stormData$CROPDMG))
sum(stormData$CROPDMG=="")
#variable CROPDMGEXP
sum(is.na(stormData$CROPDMGEXP))
sum(stormData$CROPDMGEXP=="")
levels(stormData$CROPDMGEXP)
table(stormData$CROPDMGEXP)
########
totalFatalities<-ddply(stormData,.(EVTYPECOD),summarise,totalFatalities=sum(FATALITIES))
i<-order(totalFatalities$EVTYPECOD,decreasing = F )
totalFatalities<-totalFatalities[i,]
totalFatalities<-totalFatalities[!(totalFatalities$totalFatalities==0),]
totalFatalities$EVTYPECOD<-as.factor(totalFatalities$EVTYPECOD)
##grafica de barras de las 10 primeras
topTen<-totalFatalities[1:95,]
rownames(topTen)<-NULL
topTen$EVTYPECOD <- reorder(topTen$EVTYPECOD, topTen$totalFatalities,desc)
bar <- ggplot(topTen, aes(y=totalFatalities)) 
bar + geom_bar(aes(x=EVTYPECOD),stat ="identity",binwidth=1 ) +labs(title="10 first Event type and fatalities",y="total fatalities",x="")+
    theme(axis.text.x = element_text(angle = 45))
##grafica de barras para todas las categorias
totalFatalities$EVTYPECOD <- reorder(totalFatalities$EVTYPECOD, totalFatalities$totalFatalities,desc)
bar <- ggplot(totalFatalities, aes(x=EVTYPECOD,y=totalFatalities)) 
bar + geom_bar(stat ="identity" ) +labs(title="",y="total fatalities",x="")+ coord_flip()+
    theme(axis.text.y = element_text(size = rel(0.8)))
table(totalFatalities$EVTYPECOD )
p <- ggplot(totalFatalities, aes(x=factor(1), y=EVTYPECOD)) 
p + geom_tile(aes(fill=totalFatalities))  + theme(axis.text.y = element_text(size = rel(0.8)))

p + scale_fill_gradient(low = "white",high = "steelblue")

qplot(x=Var1, y=Var2, data=totalFatalities[1:48,], fill=value, geom="tile")

unique(totalFatalities$EVTYPE)[order((unique(totalFatalities$EVTYPE)))]

sum(grepl(pattern ="TSTM",x = stormData$EVTYPE ))
table(stormData$EVTYPE[grepl(pattern ="TSTM ",x = stormData$EVTYPE )])
unique(stormData$PROPDMGEXP)
table(stormData$PROPDMGEXP)
sum(is.na(stormData$PROPDMGEXP))
sum(stormData$PROPDMGEXP=="")
stormData[stormData$PROPDMGEXP=="","PROPDMG"][((stormData[stormData$PROPDMGEXP=="","PROPDMG"]!=0))]
dat$PropExp[dat$PROPDMGEXP=="" | dat$PROPDMGEXP=="?" |dat$PROPDMGEXP=="-"|dat$PROPDMGEXP=="+"|dat$PROPDMGEXP=="0" ]=10^0
dat$PropExp[dat$PROPDMGEXP=="1" ]=10^1
dat$PropExp[dat$PROPDMGEXP=="2" |dat$PROPDMGEXP=="h"|dat$PROPDMGEXP=="H"]=10^2
dat$PropExp[dat$PROPDMGEXP=="3"|dat$PROPDMGEXP=="K" ]=10^3
dat$PropExp[dat$PROPDMGEXP=="4" ]=10^4
dat$PropExp[dat$PROPDMGEXP=="5" ]=10^5
dat$PropExp[dat$PROPDMGEXP=="6"|dat$PROPDMGEXP=="m"|dat$PROPDMGEXP=="M" ]=10^6
dat$PropExp[dat$PROPDMGEXP=="7" ]=10^7
dat$PropExp[dat$PROPDMGEXP=="8" ]=10^8
dat$PropExp[dat$PROPDMGEXP=="B" ]=10^9
table(stormData$PROPDMGEXP)
table(stormData$CROPDMGEXP)

D <- adist(unique(stormData$EVTYPEClean),unique(stormData$EVTYPEClean))
colnames(D) <- unique(stormData$EVTYPEClean)
rownames(D) <- unique(stormData$EVTYPEClean)
stringdist
sum(D==0)
diag(D)=Inf
i <- apply(D, 1, which.min)
min<-apply(D, 1, min)

which.min(x = D[1,])
min(D[1,])

evun<-data.frame(v=unique(stormData$EVTYPE))
evun$v<-as.character(evun$v)

evun$v<-gsub(pattern = "\\(.+?\\)",replacement = "", x = evun$v)
evun$v<-gsub(pattern = "[0-9]",replacement = "", x = evun$v)
evun$v<-gsub(pattern = ".",replacement = "", x = evun$v,fixed = T)
evun$v<-gsub(pattern = ":",replacement = "", x = evun$v,fixed = T)
evun$v<-gsub(pattern = "-",replacement = "", x = evun$v,fixed = T)
evun$v<-gsub(pattern = "\\(|\\)",replacement = "", x = evun$v)

ain(x =event$V1,gg$v )
agrep("lasy", "1 lazy 2")
agrep("lasy", c(" 1 lazy 2", "1 lasy 2"), max = list(sub = 0))
j<-grep("^THUNDERSTORM WIND",gg$v)
gg$v[j]<-"THUNDERSTORM WIND"
gg$v<-
    gsub(pattern = "^THUNDERSTORM WIND",replacement = "", x = "THUNDERSTORM WIND DAMAGE")

unique(evun$)
i <- amatch(gg$v, t(event),maxDist=3)
ev<-data.frame(rawtext = evun$v, coded = t(event)[i],stringsAsFactors = F)
pp<-ev[order(ev$rawtext),]
sum(is.na(pp$coded))
pp$coded[is.na(pp$coded)]<-pp$rawtext[is.na(pp$coded)]
gg<-data.frame(v=unique(pp$coded))
str(pp)
agrep("lasy", c(" 1 lazy 2", "1 lasy 2"), max = list(sub = 0))
pmatch(c("THUNDERSTORM WIND", "mod"), c("THUNDERSTORM WINDS HAIL", "median", "mode"))
pmatch("THUNDERSTORM WIND",gg$v)
gg$v<-as.character(gg$v)
matches=partialMatch(gg$v,event$V1)
str(gg)

table(ev$rawtext)
table(ev$coded)
ev[2001:3000,]
stormData$EVTYPEClean[1:2000]
table(stormData$EVTYPEClean)
stormData$EVTYPEClean<-toupper(stormData$EVTYPE)
stormData$EVTYPEClean<-str_trim(stormData$EVTYPEClean, side = "both")
length(unique(stormData$EVTYPEClean))
length(unique(stormData$EVTYPE))

##Across the United States, 
#which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
table(stormData$EVTYPECOD)
summary(storm)
sum(is.na(storm$INJURIES))
totalFat<-ddply(storm,.(EVTYPE),summarise,totalFatalities=sum(FATALITIES))

table(storm$STATE__)

totalFat$EVTYPE

