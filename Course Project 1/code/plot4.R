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
## Plot Code
png("plot4.png",width=600,height=600)

opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))

## (0, 0) Plot
plot(Consump$myDate, Consump$Global_active_power,
     type="l", ylab = "Global Active Power", xlab = "")

axis(1,c(Consump$myDate[1],mean(Consump$myDate),Consump$myDate[2880]),
     labels=c("Thu","Fri","Sat"))

## (0, 1) Plot
plot(Consump$myDate, Consump$Voltage, type = "l", 
     ylab ="Voltage", xlab = "datetime")

axis(1,c(Consump$myDate[1],mean(Consump$myDate),Consump$myDate[2880]),
     labels=c("Thu","Fri","Sat"))


## (1, 0) Plot
plot(Consump$myDate, Consump$Sub_metering_1,
     type="l", ylab = "Energy sub metering", xlab = "")

axis(1,c(Consump$myDate[1],mean(Consump$myDate),Consump$myDate[2880]),
     labels=c("Thu","Fri","Sat"))


lines(Consump$myDate, Consump$Sub_metering_2, col="red")
lines(Consump$myDate, Consump$Sub_metering_3, col = "blue")

legendText <- c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")

legend("topright", # places a legend at the appropriate place
       legendText, # puts text in the legend
       lty = c(1,1), # gives the legend appropriate symbols (lines)
       lwd = c(2.5, 2.5), col=c("black","red", "blue"), # gives the legend lines
       # the correct color and width 
       cex = 0.75,                      # Character expansion factor
       bty = "n")                       # Remove border

## (1, 1) Plot
plot(Consump$myDate, Consump$Global_reactive_power, type =
         "l", xlab = "datetime", ylab = "Global_reactive_power")

axis(1,c(Consump$myDate[1],mean(Consump$myDate),Consump$myDate[2880]),
     lables = c("Thu","Fri","Sat"))

par(opar)
graphics.off()





