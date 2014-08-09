## plot1
library(data.table) 

# Set english locale. Needed to show the days with english name. 
# Needed only if the machine uses another locale e.g "Italian".
Sys.setlocale("LC_TIME", "English")

## Downloads the .zip file and extracts only interesting data from it.
## Function saves the .zip file on disk and if it exists 
## avoids to download it again.
loadData <- function(){
    filePath <- "./household_power_consumption.zip"
    # donwload the .zip file if there isn't.
    if(!file.exists(filePath)){
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", 
                      filePath, method="curl")    
    }
    # get the .txt filename.
    dataName <- unzip(filePath, list=TRUE)$Name
    # unizp the file.
    dataFile <- unzip(filePath)
    # open .txt file for reading.
    tmp <- file(dataName, "r")
    # find the lines with dates 1/2/2007 through 2/2/2007 and save them on file.
    cat(grep("(^Date)|(^[1|2]/2/2007)",readLines(tmp), value=TRUE), sep="\n", 
        file="dataToPlot.txt")
    close(tmp)
    # read the selected data into memory.
    dataToPlot <- fread("dataToPlot.txt", sep=";", header=TRUE, na.strings="?")
    dataToPlot
}

## Converts date format contained in the data.
convertDate <- function(data){
    # convert dates. NB capital "Y" because year=yyyy
    data$Date <- as.Date(data$Date, "%d/%m/%Y")
    #convert to "date time" string to be later converted as.POSIXct
    data <- paste(as.character(data$Date), data$Time, sep=" ")
    data
}

## Plots the data on a .png file.
plot1 <- function(dataToPlot){
    # open png device
    png("plot1.png", width = 480, height = 480, bg="transparent")
    hist(dataToPlot$Global_active_power, col="red", main="Global Active Power",
         xlab="Global Acitve Power (kilowatts)", ylab="Frequency")
    #close device
    dev.off()    
}

data <-loadData()
plot1(data)
