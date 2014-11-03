setwd("/Users/submarine/Git/ExData_Plotting1/project2")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Question 1
# compute the total amount of emission of each year
q1 <- tapply(NEI$Emissions,NEI$year,sum)
# use the data above to describe the trend
png("plot1.png",width=300,height=300)
barplot(q1,width=0.5,main="Total PM2.5 of each year")
graphics.off()

## Question 2
# subset the data of fips=="24510" from NEI
BC <- NEI[NEI$fips=="24510",]
# compute the total amount of emission of each year
q2 <- tapply(BC$Emissions,BC$year,sum)
# use the data above to describe the trend
png("plot2.png",width=300,height=300)
barplot(q2,width=0.5,main="Total PM2.5 in Maryland")
graphics.off()

## Question 3
library(data.table)
library(ggplot2)
dt <- data.table(NEI)
dtSCC <- data.table(SCC)
setkey(dt,fips,year,SCC,type)
setkey(dtSCC,EI.Sector)

dtBT <- dt[fips=="24510",list(emissions = sum(Emissions)),
           by=list(year,type)]

dtBT <- dtBT[order(dtBT$year)]
p <- ggplot(dtBT,aes(x=year,y=emissions,col=type))+geom_line()
p <- p + ggtitle("PM2.5 Emission by Baltimore")
p <- p + scale_color_discrete(name="Source Type")
p <- p + xlab("Year") + ylab("Emission")

## Question 4
names(SCC)<-gsub("\\.","", names(SCC))
SCCcombustion<-grepl(pattern = "comb", SCC$SCCLevelOne, ignore.case = TRUE)
SCCCoal<-grepl(pattern = "coal", SCC$SCCLevelFour, ignore.case = TRUE)
SCCCoalCombustionSCC<-SCC[SCCcombustion & SCCCoal,]$SCC
NIECoalCombustionValues<-NEI[NEI$SCC %in% SCCCoalCombustionSCC,]
NIECoalCombustionTotalEm<-aggregate(Emissions~year, NIECoalCombustionValues, sum)
#g <- ggplot(aes(year, Emissions/10^5), data=NIECoalCombustionTotalEm) +
#      geom_bar(stat="identity",fill="grey",width=0.75) +
#      guides(fill=FALSE) +
#      labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) +
#      labs(title=expression("PM"[2.5]*" Coal Combustion Source Emissions Across US from 1999-2008"))
png("plot4.png")
ggplot(data = NIECoalCombustionTotalEm, aes(year, Emissions/10^5)) +
     geom_line(aes(group=1, col=Emissions)) +
     geom_point(aes(size=2, col=Emissions)) +
     labs( title = 'Total Emission from Coal combustion')+
     ylab(expression( paste( 'PM' , ''[2.5] , ' in kilotons' ) ) ) +
     geom_text( aes( label=round( Emissions / 1000 , digits=2 ) , size=2 , hjust=1.5 , vjust=1.5 ) ) +
     theme( legend.position='none' ) 
dev.off()

## Question 5
# solotion 1
NEI$year <- factor( NEI$year , levels = c( '1999' , '2002' , '2005' , '2008' ) )
# Baltimore City, Maryland == fips
MD.onroad <- subset( NEI , fips==24510 & type=='ON-ROAD' )
# Aggregate
MD.df <- aggregate( MD.onroad[ , 'Emissions' ] , by=list( MD.onroad$year ) , sum )
colnames( MD.df ) <- c( 'year' , 'Emissions' )
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
## GRAPH
# Generate graph
png("plot5.png")
ggplot( data=MD.df , aes( x=year , y=Emissions ) ) +
    geom_bar( stat="identity" ) +
    guides( fill=F ) +
    ggtitle( 'Total Emissions of Motor Vehicle Sources in Baltimore City, Maryland' ) +
    ylab( expression( 'PM'[2.5] ) ) +
    xlab( 'Year' ) +
    theme( legend.position='none' ) +
    geom_text( aes( label=round( Emissions , 0 ) , size=1 , hjust=0.5 , vjust=2 ) )
dev.off()
 
# solution 2
SCCvehicle <- grepl(pattern = "vehicle", SCC$EISector, ignore.case = TRUE)
SCCvehicleSCC <- SCC[SCCvehicle,]$SCC
NEIvehicleSSC <- NEI[NEI$SCC %in% SCCvehicleSCC, ]
# Baltimore City, Maryland == fips
NEIvehicleBaltimore <- subset(NEIvehicleSSC, fips == "24510")
# Aggregate
NIEvehicleBaltimoreTotEm<-aggregate(Emissions~year, NEIvehicleBaltimore, sum)
png("plot5_1.png")
ggplot(aes(year, Emissions), data=NIEvehicleBaltimoreTotEm) +
    geom_bar(stat="identity",fill="grey",width=0.75) +
    guides(fill=FALSE) +
    labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
    labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore from 1999-2008"))
dev.off()

## Question 6
NEIvehicleBalti<-subset(NEIvehicleSSC, fips == "24510")
NEIvehicleBalti$city <- "Baltimore City"
NEIvehiclela <- subset(NEIvehicleSSC, fips == "06037")
NEIvehiclela$city <- "Los Angeles County"
NEIBothCity <- rbind(NEIvehicleBalti, NEIvehiclela)
png("plot6.png")
ggplot(NEIBothCity, aes(x=year, y=Emissions, fill=city)) +
    geom_bar(aes(fill=year),stat="identity") +
    facet_grid(.~city) +
    guides(fill=FALSE) + theme_bw() +
    labs(x="year", y=expression("Total PM"[2.5]*" Emission (Kilo-Tons)")) + 
    labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008"))
dev.off()











