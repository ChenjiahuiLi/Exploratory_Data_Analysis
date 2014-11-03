library(data.table)
library(parallel)
library(lubridate)
######################################################################
## Loading the data
dataFN <- fread("household_power_consumption.txt",
                colClasses="character",skip = 66637, nrows = 2880)
ColNames <- c("Date", "Time", "Global_active_power" ,
              "Global_reactive_power", "Voltage", "Global_intensity",
              "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
Consump <- dataFN
setnames(Consump,ColNames)
######################################################################
## Converting the data to easy handle

## Pasting date and time
Consump$myDate <- paste(Consump$Date,Consump$Time)
Consump$myDate <- dmy_hms(Consump$myDate)
## OR
Consump$myDate <- striptime(Consump$myDate,"%d/%m/%Y %H:%M:%S")

## Change the class of data
Consump$Global_active_power <-
    sapply(Consump$Global_active_power, as.numeric)

Consump$Global_reactive_power <-
    sapply(Consump$Global_reactive_power, as.numeric)

Consump$Voltage <-
    sapply(Consump$Voltage, as.numeric)

Consump$Global_intensity <-
    sapply(Consump$Global_intensity, as.numeric)

Consump$Sub_metering_1 <- sapply(Consump$Sub_metering_1,
                                 as.numeric)

Consump$Sub_metering_2 <- sapply(Consump$Sub_metering_2,
                                 as.numeric)

Consump$Sub_metering_3 <- sapply(Consump$Sub_metering_3,
                                 as.numeric)

######################################################################
## Plot code:
png("plot3.png",width=480,height=480)

plot(Consump$myDate, Consump$Sub_metering_1, type = "l",
     xlab = "", ylab = "Energy sub metering")

axis(1,c(Consump$myDate[1],mean(Consump$myDate),Consump$myDate[2880]),
     labels=c("Thu","Fri","Sat"))

lines(Consump$myDate, Consump$Sub_metering_2, col="red")
lines(Consump$myDate, Consump$Sub_metering_3, col="blue")

legendText <- c("Sub_metering_1","Sub_metering_2","Sub_metering_3")

legend("topright",
       legendText, # puts text in the legend
       lty = c(1,1), # gives the legend appropriate symbols (lines)
       lwd = c(2.5, 2.5), col=c("black","red", "blue"), # gives the legend lines
       # the correct color and width 
       cex = 0.75)                      # Character expansion factor)


graphics.off()












