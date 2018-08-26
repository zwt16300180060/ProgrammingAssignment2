data <- read.table("./data/RDA/household_power_consumption.txt", header=TRUE, sep=";", stringsAsFactors=FALSE, dec=".")
setwd("./data/RDA")

png("polt1.png")
hist(data$Global_active_power,col="red",main="Global Active Power",xlab="Global Active Power(kilowatts)")
dev.off()