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
png("plot2.png",width=600,height=480)

plot(Consump$myDate, Consump$Global_active_power,
     type="l", 
     ylab = "Global Active Power (kilowatts)", xlab = "",
     xlim=c(min(Consump$myDate),max(Consump$myDate)))

axis(1,c(Consump$myDate[1],mean(Consump$myDate),Consump$myDate[2880]),
     labels=c("Thu","Fri","Sat"))

graphics.off()
## Or use the ggplot2 package
library(ggplot2)
qplot(myDate, Global_active_power, data = Consump, geom = "line")
