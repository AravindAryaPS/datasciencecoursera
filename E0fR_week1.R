df <- read.table(file = "household_power_consumption.txt", header = TRUE,sep = ";", na.strings = "?")
df$Date <- as.Date(df$Date, "%d/%m/%Y")
Data <- subset(df, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))
dateTime <- paste(Data$Date,Data$Time)
dateTime <- setNames(teTime,"DateTime")
Data1 <- Data[,!(names(Data) %in% c("Date","Time"))]
Data2 <- cbind(dateTime, Data1)
Data2$dateTime <- as.POSIXct(dateTime)
hist(Data2$Global_active_power,col = "red", xlab = "Global Active Power (kilowatts)",ylab = "Frequency",main = "Global Active Power")
dev.copy(png,"plot1.png",width = 480,height = 480)
dev.off()


plot(Data2$Global_active_power ~ Data2$dateTime,type = "l",ylab="Global Active Power (kilowatts)", xlab="")
dev.copy(png,"plot2.png",width = 480,height = 480)
dev.off()
with(Data2, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.copy(png,"plot3.png",width = 480,height = 480)
dev.off()

par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(Data2, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})
dev.copy(png,"plot4.png",width = 480,height = 480)
dev.off()