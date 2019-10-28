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
## Check
sum(is.na(global_active_power))
sum(dat$Global_active_power == '?')
## Add to data frame, to replace original column
dat[['global_active_power']] <- global_active_power
dat$Global_active_power <- NULL

## Plot 1

png(filename = "plot1.png", width = 480, height = 480)

hist(dat$global_active_power,
     col = "red",
     main = "Global Active Power",
     xlab = "Global Active Power (kilowatts)")

dev.off()

## This code shows that there were no question mark or missing files
## on the days we are looking at. That simplifies things.

## sum(dat$Global_reactive_power == '?')
## sum(dat$Global_active_power == '?')
## sum(dat$Voltage == '?')
## sum(dat$Global_intensity == '?')
## sum(dat$Sub_metering_1 == '?')
## sum(dat$Sub_metering_2 == '?')
## sum(dat$Sub_metering_3 == '?')

## sum(is.na(dat$Global_reactive_power))
## sum(is.na(dat$Global_active_power))
## sum(is.na(dat$Voltage))
## sum(is.na(dat$Global_intensity))
## sum(is.na(dat$Sub_metering_1))
## sum(is.na(dat$Sub_metering_2))
## sum(is.na(dat$Sub_metering_3))

