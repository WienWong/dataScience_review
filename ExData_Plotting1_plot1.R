# http://ocw.jhsph.edu/index.cfm/go/viewCourse/course/exdata/coursePage/projects/
# ExData Project 1
# start from 16/12/2006 17:24:00, thus skip number chooses 31+15
?read.table  # 24*2*60
namevec=c("Date","Time","Global_active_power","Global_reactive_power",
          "Voltage","Global_intensity","Sub_metering_1",
          "Sub_metering_2","Sub_metering_3")
filepath="./R/data/household_power_consumption/household_power_consumption.txt"
dt <- read.table(file=filepath, na.strings="?", sep=";", col.names=namevec,
                 stringsAsFactors=FALSE, skip=24*60*(31+15) )
head(dt,5)
str(dt)      
# library(lubridate)
# dmy("01/2/2007")

# ?subset 
dt.subset <- subset(dt, dt$Date=="1/2/2007" | dt$Date=="2/2/2007")
hist(dt.subset$Global_active_power, main="Global Active Power", 
     xlab="Global active power (kilowatts)", ylab="Frequency", col="Red")

# which() returns a vector of column or row indices that satisfies the condition. 
dt.subst <- dt[which(dt$Date=="1/2/2007" | dt$Date=="2/2/2007"),]
hist(dt.subst$Global_active_power, main="Global Active Power", 
     xlab="Global active power (kilowatts)", ylab="Frequency", col="Red")

# ?"%in%" : match {base}
dt.sub <- dt[dt$Date %in% c("1/2/2007","2/2/2007"), ]
str(dt.sub)
head(dt.sub,5)
tail(dt.sub,5)

hist(dt.sub$Global_active_power, main="Global Active Power", 
     xlab="Global active power (kilowatts)", ylab="Frequency", col="Red")

# save img
dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()

# ?as.POSIXct # Date-time Conversion Functions
dt.sub$DateTime <- as.POSIXct(paste(dt.sub$Date, dt.sub$Time), format="%d/%m/%Y %H:%M:%S") 
# format="%Y-%m-%d %H:%M:%S" will result in NAs
# as.POSIXct(strptime("2010-10-31 01:30:00", "%Y-%m-%d %H:%M:%S"))
plot(dt.sub$DateTime,dt.sub$Global_active_power,type='l',
     xlab="", ylab="Global active power (kilowatts)")

# Save img
dev.copy(png, file="plot2.png", height=480, width=480)
dev.off()

#
plot(dt.sub$DateTime, dt.sub$Sub_metering_1, type='l', col='black',
     xlab='', ylab='Energy sub meeting')
lines(dt.sub$DateTime, dt.sub$Sub_metering_2, type='l', col='red')
lines(dt.sub$DateTime, dt.sub$Sub_metering_3, type='l', col='blue')
legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col=c("black","red","blue"), lty=1, lwd=2)

# ?with  Evaluate an R expression in an environment constructed from data
# with(data, expr, ...)
with(dt.sub, {
    plot(Sub_metering_1~DateTime, type="l",
         ylab="Energy sub metering", xlab="")
    lines(Sub_metering_2~DateTime,col='Red')
    lines(Sub_metering_3~DateTime,col='Blue')
})
legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col=c("black","red","blue"), lty=1, lwd=2)

# Save img
dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()

# 
par(mfrow=c(2,2))
plot(dt.sub$DateTime,dt.sub$Global_active_power,type='l',
     xlab="", ylab="Global active power (kilowatts)")
plot(dt.sub$DateTime, dt.sub$Voltage, xlab='datetime', ylab='Voltage',
     type='l', col='black')
plot(dt.sub$DateTime, dt.sub$Sub_metering_1, type='l', col='black',
     xlab='', ylab='Energy sub meeting')
lines(dt.sub$DateTime, dt.sub$Sub_metering_2, type='l', col='red')
lines(dt.sub$DateTime, dt.sub$Sub_metering_3, type='l', col='blue')
#legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       #col=c("black","red","blue"), lty=1, lwd=2)
with(dt.sub, {plot(Global_reactive_power~DateTime, type='b', pch=20, 
                   xlab='datetime', ylab='Global_reactive_power')})
par(mfrow=c(1,1))

# Or
par(mfrow=c(2,2))
with(dt.sub, {
    plot(Global_active_power ~ DateTime, type = "l", 
         ylab = "Global Active Power", xlab = "")
    
    plot(Voltage ~ DateTime, type = "l", 
         ylab = "Voltage", xlab = "datetime")
    
    plot(Sub_metering_1 ~ DateTime, type = "l", 
         ylab = "Energy sub metering", xlab = "")
    lines(Sub_metering_2 ~ DateTime, col = 'Red')
    lines(Sub_metering_3 ~ DateTime, col = 'Blue')
    #legend("topright", col = c("black", "red", "blue"), lty = 1, lwd = 2, bty = "n",
           #legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    plot(Global_reactive_power ~ DateTime, type = "b", pch=20,
         ylab = "Global_rective_power", xlab = "datetime")
})
par(mfrow=c(1,1))

