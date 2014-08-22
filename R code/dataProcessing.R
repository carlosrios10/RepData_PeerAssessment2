###################################################################################################
## EVTYPE,PROPDMGEXP, CROPDMGEXP
dataProcessing<-function(stormData){
    #Cleaning the EVTYPE variable.
    stormData$EVTYPE<-toupper(stormData$EVTYPE)
    stormData$EVTYPE<-str_trim(stormData$EVTYPE, side = "both")
    stormData$EVTYPE<-gsub(pattern = "TSTM",replacement ="THUNDERSTORM",x =stormData$EVTYPE ) 
    stormData$EVTYPE<-gsub(pattern = "\\(.+?\\)",replacement = "", x = stormData$EVTYPE)
    stormData$EVTYPE<-gsub(pattern = "[0-9]",replacement = "", x = stormData$EVTYPE)
    stormData$EVTYPE<-gsub(pattern = ".",replacement = "", x = stormData$EVTYPE,fixed = T)
    stormData$EVTYPE<-gsub(pattern = ":",replacement = "", x = stormData$EVTYPE,fixed = T)
    stormData$EVTYPE<-gsub(pattern = "-",replacement = "", x = stormData$EVTYPE,fixed = T)
    stormData$EVTYPE<-gsub(pattern = "\\(|\\)",replacement = "", x = stormData$EVTYPE)
    stormData$EVTYPE[grep(pattern = "^THUN", stormData$EVTYPE)]<-"THUNDERSTORM WIND"
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
    stormData$EVTYPE[grep(pattern = "^RIP CURRENT", stormData$EVTYPE)]<-"RIP CURRENT"
    stormData$EVTYPE[grep(pattern = "^EXTREME COLD", stormData$EVTYPE)]<-"EXTREME COLD/WIND CHILL"
    stormData$EVTYPE[grep(pattern = "^EXTREME HEAT", stormData$EVTYPE)]<-"EXCESSIVE HEAT"
    stormData$EVTYPE[grep(pattern = "^WILD/FOREST FIRE", stormData$EVTYPE)]<-"WILDFIRE"
    stormData$EVTYPE[grep(pattern = "^COASTAL FL", stormData$EVTYPE)]<-"COASTAL FLOOD"
    stormData$EVTYPE[grep(pattern = "^COLD/WIND", stormData$EVTYPE)]<-"COLD/WIND CHILL"
    stormData$EVTYPE[grep(pattern = "^DRY MI", stormData$EVTYPE)]<-"DRY MICROBURST"
    stormData$EVTYPE[grep(pattern = "^DUST STORM", stormData$EVTYPE)]<-"DUST STORM"
    stormData$EVTYPE[grep(pattern = "^EXTREME WIND", stormData$EVTYPE)]<-"EXTREME COLD/WIND CHILL"
    stormData$EVTYPE[grep(pattern = "^FOREST FIRES", stormData$EVTYPE)]<-"WILDFIRE"
    stormData$EVTYPE[grep(pattern = "^WINTER STO", stormData$EVTYPE)]<-"WINTER STORM"
    stormData$EVTYPE[grep(pattern = "^WINTER WEAT", stormData$EVTYPE)]<-"WINTER WEATHER"
    stormData$EVTYPE[grep(pattern = "^TROPICAL STORM", stormData$EVTYPE)]<-"TROPICAL STORM"
    stormData$EVTYPE[grep(pattern = "^WATERS", stormData$EVTYPE)]<-"WATERSPOUT"
    stormData$EVTYPE[grep(pattern = "^GUSTY WI", stormData$EVTYPE)]<-"GUSTY WIND"
    stormData$EVTYPE[grep(pattern = "^WILD", stormData$EVTYPE)]<-"WILDFIRE"
 
    #Creation the new PropExp variable.
    stormData$PropExp[stormData$PROPDMGEXP=="" | stormData$PROPDMGEXP=="?" |stormData$PROPDMGEXP=="-"|stormData$PROPDMGEXP=="+"|stormData$PROPDMGEXP=="0" ]=10^0
    stormData$PropExp[stormData$PROPDMGEXP=="1" ]=10^1
    stormData$PropExp[stormData$PROPDMGEXP=="2" |stormData$PROPDMGEXP=="h"|stormData$PROPDMGEXP=="H"]=10^2
    stormData$PropExp[stormData$PROPDMGEXP=="3"|stormData$PROPDMGEXP=="K" ]=10^3
    stormData$PropExp[stormData$PROPDMGEXP=="4" ]=10^4
    stormData$PropExp[stormData$PROPDMGEXP=="5" ]=10^5
    stormData$PropExp[stormData$PROPDMGEXP=="6"|stormData$PROPDMGEXP=="m"|stormData$PROPDMGEXP=="M" ]=10^6
    stormData$PropExp[stormData$PROPDMGEXP=="7" ]=10^7
    stormData$PropExp[stormData$PROPDMGEXP=="8" ]=10^8
    stormData$PropExp[stormData$PROPDMGEXP=="B" ]=10^9
    
    #Creation the new CropExp variable.
    stormData$CropExp[stormData$CROPDMGEXP=="" | stormData$CROPDMGEXP=="?" |stormData$CROPDMGEXP=="0" ]=10^0
    stormData$CropExp[stormData$CROPDMGEXP=="2"]=10^2
    stormData$CropExp[stormData$CROPDMGEXP=="k"|stormData$CROPDMGEXP=="K" ]=10^3
    stormData$CropExp[stormData$CROPDMGEXP=="m"|stormData$CROPDMGEXP=="M" ]=10^6
    stormData$CropExp[stormData$CROPDMGEXP=="B" ]=10^9
    
    
    stormData$YEAR<-year(mdy_hms(as.character(stormData$BGN_DATE)))
    return (stormData[,c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PropExp","CROPDMG","CropExp")])
    
}