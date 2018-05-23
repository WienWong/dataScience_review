# http://ocw.jhsph.edu/index.cfm/go/viewCourse/course/exdata/coursePage/projects/
# ExData Project 2
# fips: A five-digit number (represented as a string) indicating the U.S. county
# SCC: The name of the source as indicated by a digit string (see source code classification table)
# Pollutant: A string indicating the pollutant
# Emissions: Amount of PM2.5 emitted, in tons
# type: The type of source (point, non-point, on-road, or non-road)
# year: The year of emissions recorded
library(ggplot2)

fpath = './R/data/exdata-data-NEI_data/'
NEI <- readRDS(paste(fpath,"summarySCC_PM25.rds",sep="")) 
SCC <- readRDS(paste(fpath,"Source_Classification_Code.rds",sep="")) 
str(NEI)
str(SCC)

direct <- paste(fpath, "/plot1.png", sep="", collapse=NULL)
png(filename=direct, width=500, height=500, units="px")

# ?aggregate
# Splits the data into subsets, computes summary statistics for each, 
# and returns the result in a convenient form.
# formula -- such as y~x or cbind(y1,y2)~x1+x2, where the y variables are
# numeric data to be split into groups according to the grouping x variables.
# FUN -- a function to compute the summary statistics which can be applied 
# to all data subsets.
totEm <- aggregate(formula=Emissions~year, data=NEI, FUN=sum, na.action=na.omit)
str(totEm)
barplot(totEm$Emissions/1e6, names.arg=c(1999,2002,2005,2008), xlab='Year', 
        ylab=expression('Total PM'[2.5]*" Emissions (Million Tons)"), col='blue',
        main="Total Emissions of US from 1999 to 2008")

# hist(totEm$Emissions, xlab='Year', col='blue',
#      ylab=expression('Total PM'[2.5]*" Emissions (Million Tons)"),
#      main="Total Emissions of US from 1999 to 2008")

dev.off()

# Alternative code

totEmi <- aggregate(NEI$Emissions, by=list(NEI$year), FUN="sum", na.rm=TRUE)
names(totEmi) <- c("year","emissions")
with(totEmi, {barplot(emissions/1e6, names.arg=year, xlab='Year',
                     ylab=expression('Total PM'[2.5]*" Emissions (Million Tons)"),
                     col='blue', main="Total Emissions of US from 1999 to 2008" )
    })


# Baltimore City, Maryland (fips == "24510")
# subset into dataset of Baltimore City
BalDt <- NEI[ NEI$fips %in% "24510", ]
# alternative ways:
balDt <- NEI[NEI$fips=="24510", ]
baldt <- subset(NEI, NEI$fips=="24510")

BalEm <- aggregate(formula=Emissions~year, data=BalDt, FUN=sum, na.action=na.omit)
head(BalEm)
barplot(BalEm$Emissions, names.arg=BalEm$year, xlab="Year", col='blue',
        ylab=expression('Total PM'[2.5]*" Emissions (Million Tons)"),
        main='Total Emissions for Baltimore City')

# 
BalEmType <- aggregate(formula=Emissions~type+year, data=BalDt, FUN=sum, na.action=na.omit)
par(mfrow=c(2,2))
barplot(BalEmType$Emissions[which(BalEmType$type=="NON-ROAD")], xlab='Year', 
        ylab=expression('PM'[2.5]*'Emissions (Tons)'), col='blue',
        names.arg=c(1999,2002,2005,2008), main='NON-ROAD Emissions for Baltimore City') 
barplot(BalEmType$Emissions[which(BalEmType$type=="ON-ROAD")], xlab='Year', 
        ylab=expression('PM'[2.5]*'Emissions (Tons)'), col='blue',
        names.arg=c(1999,2002,2005,2008), main='ON-ROAD Emissions for Baltimore City') 
barplot(BalEmType$Emissions[which(BalEmType$type=="POINT")], xlab='Year', 
        ylab=expression('PM'[2.5]*'Emissions (Tons)'), col='blue',
        names.arg=c(1999,2002,2005,2008), main='POINT Emissions for Baltimore City') 
barplot(BalEmType$Emissions[which(BalEmType$type=="NONPOINT")], xlab='Year', 
        ylab=expression('PM'[2.5]*'Emissions (Tons)'), col='blue',
        names.arg=c(1999,2002,2005,2008), main='NONPOINT Emissions for Baltimore City') 
par(mfrow=c(1,1))

# Or ggplot
g <- ggplot(BalDt, aes(factor(year), Emissions, fill = type)) 

g + geom_bar(stat = "identity") + theme_bw() + 
facet_grid(.~ type, scales = "free", space = "free") + 
labs(x = "Year", y = expression("Total PM"[2.5]*" Emissions (Tons)")) +
    labs(title = expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))

# coal combustion-related sources ?grep
# grep: search for matches to argument pattern within each element of a character vector:
# grep(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE,
#                   fixed = FALSE, useBytes = FALSE, invert = FALSE)
USCoal<- grep(pattern='coal', SCC$Short.Name, ignore.case=TRUE)
# ignore.case: if FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching.

# "%in%": To find the indexes of the specific elements inside a larger vector 
# (notice the order): length(NEI$SCC)=6497651, length(SCC[USCoal,]$SCC)=239
dtnew <- NEI[(NEI$SCC %in% SCC[USCoal,]$SCC), ]
dtcoal <- aggregate(formula=Emissions~year, data=dtnew , FUN=sum, na.action=na.omit)
barplot(dtcoal$Emissions/1e3, names.arg=c(1999,2002,2005,2008), xlab='Year', col='blue',
        main='Coal Combustion-related Emissions in US',
        ylab=expression('PM'[2.5]*'Emissions (Kilo Tons)') )

# Motor vehicle sources changed in Baltimore City
USvehicle <- grep(pattern="vehicle", SCC$Short.Name, ignore.case=TRUE)
Balvehicle <- BalDt[ (BalDt$SCC %in% SCC[USvehicle, ]$SCC), ]
Balnew <- aggregate(formula=Emissions~year, data=Balvehicle, FUN=sum, na.action=na.omit)
barplot(Balnew$Emissions, names.arg=c(1999,2002,2005,2008), xlab="Year", col="blue",
        main="Motor Vehicle-related Emissions in Baltimore City",
        ylab=expression("PM"[2.5]*"Emissions (Tons)"))

# Los Angeles County, California (fips == "06037").
LosDt <- NEI[NEI$fips=="06037", ]
Losvehicle <- LosDt[ (LosDt$SCC %in% SCC[USvehicle, ]$SCC), ]
Losnew <- aggregate(formula=Emissions~year, data=Losvehicle, FUN=sum, na.action=na.omit)
barplot(Losnew$Emissions, names.arg=c(1999,2002,2005,2008), xlab="Year", col="blue",
        main="Motor Vehicle-related Emissions in Los Angeles City",
        ylab=expression("PM"[2.5]*"Emissions (Tons)"))
par(mfrow=c(1,2))
barplot(Balnew$Emissions, names.arg=c(1999,2002,2005,2008), xlab="Year", col="blue",
        main="Motor Vehicle-related Emissions in Baltimore City",
        ylab=expression("PM"[2.5]*"Emissions (Tons)"))
barplot(Losnew$Emissions, names.arg=c(1999,2002,2005,2008), xlab="Year", col="blue",
        main="Motor Vehicle-related Emissions in Los Angeles City",
        ylab=expression("PM"[2.5]*"Emissions (Tons)"))
par(mfrow=c(1,1))

# 
Balnew$city <- "Baltimore City"
Losnew$city <- "Los Angeles"
motorNew <- rbind(Losnew,Balnew)

# Or ggplot, minor modification needed.
g <- ggplot(motorNew, aes(x = factor(year), y = Emissions, fill = city))
g + geom_bar(stat = "identity") + theme_bw() + 
    labs(x = "Year", y = expression("Total PM"[2.5]*" Emissions (Kilo-Tons)")) +
    labs(title = expression("Emissions from Motor Vehicle Sources in Two Cities"))  


# gc()
# used  (Mb) gc trigger   (Mb)  max used   (Mb)
# Ncells  1587787  84.8   16708522  892.4  20885653 1115.5
# Vcells 19813031 151.2  184930022 1411.0 283592205 2163.7
