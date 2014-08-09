## plot4
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
plot4 <- function(dataToPlot){
    #concat date and time columns in one unique value.
    datetime <- as.POSIXct(convertDate(dataToPlot))
    # open png device
    png("plot4.png", width = 480, height = 480, bg="transparent")
    par(mfrow=c(2,2))
    with(dataToPlot, {
        # plot top-left
        plot(x=datetime, y=Global_active_power, type='l', 
             xlab='', ylab='Global Active Power')
        # plot top-right
        plot(x=datetime, y=Voltage, type='l', xlab='datetime', ylab='Voltage')
        # plot bottom-left
        plot(x=datetime, y=Sub_metering_1, type='l', xlab='', 
             ylab='Energy sub metering')
        lines(x=datetime, y=Sub_metering_2, type='l', col='red')
        lines(x=datetime, y=Sub_metering_3, type='l', col='blue')
        legend(x='topright', col=c('black', 'red', 'blue'), lty=c(1, 1, 1), 
               legend=c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'),
               lwd=1, bty='n')
        # plot bottom-right
        plot(x=datetime, y=Global_reactive_power, type='l', 
             xlab='datetime', ylab='Global_reactive_power')
    })
    #close device
    dev.off()    
}

data <-loadData()
plot4(data)
