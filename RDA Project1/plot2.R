data <- read.table("./data/RDA/household_power_consumption.txt", header=TRUE, sep=";", stringsAsFactors=FALSE, dec=".")
setwd("./data/RDA")

datetime <- strptime(paste(data$Date, data$Time, sep=" "), "%d/%m/%Y %H:%M:%S")
png("plot2.png")
plot(datetime,data$Global_active_power,xlab="",
     ylab="Global Active Power (kilowatts)")
dev.off()