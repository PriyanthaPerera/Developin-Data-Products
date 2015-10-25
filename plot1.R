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

#plot1

png(filename = "plot1.png",
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))


hist(as.numeric(subdata$Global_active_power),  main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")

dev.off()


