loadRawData<-function(){
        file_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        zip_name <- "Data/repdata-data-StormData.csv.bz2"
        # Download zip file if not found
        if(file.exists(zip_name) == FALSE) {
                download.file(file_url,zip_name)  
        }
        stormDf <-read.csv(bzfile(zip_name))
        return(stormDf)
}