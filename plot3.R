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

## Plot 3

png(filename = "plot3.png", width = 480, height = 480)

## Convert column to numeric and convert question marks to proper NA values
sub_metering_1 <- fix_na_make_numeric(dat$Sub_metering_1)
## Check
sum(is.na(sub_metering_1))
sum(dat$Sub_metering_1 == '?')
## Add to data frame, to replace original column
dat[['sub_metering_1']] <- sub_metering_1
dat$Sub_metering_1 <- NULL

## Convert column to numeric and convert question marks to proper NA values
sub_metering_2 <- fix_na_make_numeric(dat$Sub_metering_2)
## Check
sum(is.na(sub_metering_2))
sum(dat$Sub_metering_2 == '?')
## Add to data frame, to replace original column
dat[['sub_metering_2']] <- sub_metering_2
dat$Sub_metering_2 <- NULL

## Convert column to numeric and convert question marks to proper NA values
sub_metering_3 <- fix_na_make_numeric(dat$Sub_metering_3)
## Check
sum(is.na(sub_metering_3))
sum(dat$Sub_metering_3 == '?')
## Add to data frame, to replace original column
dat[['sub_metering_3']] <- sub_metering_3
dat$Sub_metering_3 <- NULL

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

dev.off()

