#Exploratory Data Analysis  Course Project 1 due July 12
#Author: Priyantha Perera
#Date: July 9, 2015
#plot1


library("data.table", lib.loc="~/R/win-library/3.1")

data <- read.table("C:/0/Coursera/DataScience/Exploratory Data Analysis/household_power_consumption.txt", sep=";", header=TRUE, stringsAsFactors = FALSE )



data <- as.data.table(data)
subdata <-data[data$Date == "1/2/2007" | data$Date == "2/2/2007"]
rm(data)

dates <- subdata$Date
times <- subdata$Time
x <- paste(dates, times)
datetime <- strptime(x, "%d/%m/%Y %H:%M:%S")
rm(dates, times)
subdata <- na.omit(subdata)

subdata <- cbind(subdata, datetime)

names(subdata)

setwd("C:/0/Coursera/DataScience/Exploratory Data Analysis/Week1")

#plot3


png(filename = "plot3.png",
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))

with(subdata, plot(subdata$datetime,subdata$Sub_metering_1 , xlab = "", ylab = "Energy sub metering",  type = "n"))
with(subdata, points(subdata$datetime, subdata$Sub_metering_1, col = "black", type = "l"))
with(subdata, points(subdata$datetime, subdata$Sub_metering_2, col = "red", type = "l"))
with(subdata, points(subdata$datetime, subdata$Sub_metering_3, col = "blue", type = "l"))
legend("topright", lty =1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

dev.off()
