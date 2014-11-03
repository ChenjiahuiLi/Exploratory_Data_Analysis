library(data.table)
library(parallel)
library(lubridate)
dataFN <- fread("household_power_consumption.txt",
                colClasses="character",skip = 66637, nrows = 2880)
ColNames <- c("Date", "Time", "Global_active_power" ,
              "Global_reactive_power", "Voltage", "Global_intensity",
              "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
Consump <- dataFN
setnames(Consump,ColNames)
######################################################################
## Converting the data to easy handle

## Change the type of data and time into POSIXct & POSIXt
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
png("plot1.png",width=480,height=480,unit="px",pointsize=12,bg="transparent")
hist(Consump$Global_active_power, col = "red",
     main="Global Active Power",
     xlab="Global Active Power (kilowatts)")
dev.off()
## Or use the ggplot2 package
library(ggplot2)
qplot(Global_active_power, data = Consump, geom = "histogram",
      binwidth = 0.5, fill = "red", colour = "black")

