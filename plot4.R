## I checked to ensure there were no missing values to worry about
## in any of the data frame's fields
## for the dates about which we are concerned.
## However I had already written the helper function below,
## so I just left it alone because its other function is still useful.

setwd('~/Dropbox/coursera/R_data_science_4_eda/week1/ExData_Plotting1')
library(lubridate)

fix_na_make_numeric <- function (ls) {
  temp <- ls
  for(i in seq_along(temp)){
    if (temp[[i]] == '?')
      temp[[i]] <- NA
  }
  temp <- as.numeric(temp)
}

dat0 <- read.csv('~/Dropbox/coursera/R_data_science_4_eda/week1/household_power_consumption.txt',
                header = TRUE,
                sep = ";",
                stringsAsFactors = FALSE)

temp1 <- with(dat0, paste(Date, Time))
temp2 <- strptime(temp1, "%d/%m/%Y %H:%M:%S")

dat0[['datetime']] <- temp2

## Filter to the correct year, month, and day
dat1 <- dat0[year(dat0$datetime)==2007 & month(dat0$datetime)==2,]
dat2 <- dat1[day(dat1$datetime)==1 | day(dat1$datetime)==2,]
unique(year(dat2$datetime)); unique(month(dat2$datetime)); unique(day(dat2$datetime));

## Now dat will be the set containing the desired subset of
## the provided data.
dat <- dat2

## dat$Date <- NULL
## dat$Time <- NULL

## Convert column to numeric and convert question marks to proper NA values
global_active_power <- fix_na_make_numeric(dat$Global_active_power)
## Add to data frame, to replace original column
dat[['global_active_power']] <- global_active_power
dat$Global_active_power <- NULL

## These are similar to the above, for the other fields used
global_reactive_power <- fix_na_make_numeric(dat$Global_reactive_power)
dat[['global_reactive_power']] <- global_reactive_power
dat$Global_reactive_power <- NULL

voltage <- fix_na_make_numeric(dat$Voltage)
dat[['voltage']] <- voltage
dat$Voltage <- NULL

global_intensity <- fix_na_make_numeric(dat$Global_intensity)
dat[['global_intensity']] <- global_intensity
dat$Global_intensity <- NULL

sub_metering_1 <- fix_na_make_numeric(dat$Sub_metering_1)
dat[['sub_metering_1']] <- sub_metering_1
dat$Sub_metering_1 <- NULL

sub_metering_2 <- fix_na_make_numeric(dat$Sub_metering_2)
dat[['sub_metering_2']] <- sub_metering_2
dat$Sub_metering_2 <- NULL

sub_metering_3 <- fix_na_make_numeric(dat$Sub_metering_3)
dat[['sub_metering_3']] <- sub_metering_3
dat$Sub_metering_3 <- NULL


## Plot 4

png(filename = "plot4.png", width = 480, height = 480)

par(mfrow = c(2,2))

## Upper Left Plot: extremely similar to Plot 2
## The only difference I saw was that it didn't say (kilowatts) on the y-axis
with(dat, plot(datetime, global_active_power,
               lwd = 1,
               type = "l",
               lty = 1,
               xlab = "",
               ylab = "Global Active Power"))

## Upper Right Plot: new
## datetime vs. voltage
with(dat, plot(datetime, voltage,
               lwd = 1,
               type = "l",
               lty = 1,
               xlab = "datetime",
               ylab = "Voltage"))

## Lower Left Plot: seems the same as Plot 3
with(dat, plot(datetime, sub_metering_1,
               lwd = 1,
               type = "l",
               lty = 1,
               col = "black",
               cex = 2,
               xlab = "",
               ylab = "Energy sub metering"))

with(dat, lines(datetime, sub_metering_2,
               lwd = 1,
               type = "l",
               lty = 1,
               cex = 2,
               col = "red"))

with(dat, lines(datetime, sub_metering_3,
               lwd = 1,
               type = "l",
               lty = 1,
               cex = 2,
               col = "blue"))

legend("topright",
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       fill = c("black","red","blue"))

## Lower Right Plot: datetime vs global reactive power
with(dat, plot(datetime, global_reactive_power,
               lwd = 1,
               type = "l",
               lty = 1,
               xlab = "datetime",
               ylab = "Global_reactive_power"))

dev.off()

