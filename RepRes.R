
# Create a new folder named RepRes under your default working directory and set 
# your workspace into this folder

if (!file.exists("RepRes")) {
    dir.create("RepRes")
}
setwd("RepRes")

# Check whether the required raw dataset has been downloaded. If not, download 
# the .zip file

if (!file.exists("data.zip")) {
    url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(url, destfile = "data.zip", mode = "wb")
}

# Unzip the dataset and read the activity.csv file to create myDT dataframe

unzip("data.zip")
myDT <- read.csv("activity.csv", stringsAsFactors = F)

# Transform the format of variable date to class "POSIXct" by  package 
# lubridate

if(!is.element("lubridate", installed.packages()[,1])) {
    print("Installing packages")
    install.packages("lubridate")
}
library(lubridate)
myDT$date <- ymd(myDT$date)

# Use aggregate() FUN to collapse the data and get a dataframe named aggDT1  
# showing the total number of steps taken per day

aggDT1 <- with(myDT, aggregate(steps ~ date, FUN = sum, na.rm = T))

# Make a histogram of the total number of steps taken each day with hist() FUN  
# in base plotting system

opar <- par(no.readonly = T)
par(mar = c(5, 5, 4, 1), cex = .85, las = 1)
with(aggDT1, hist(steps, breaks = 20, col = "steelblue", 
                  xlab = "Total steps", 
                  main = "Histogram of the total number of steps by day")
)
par(opar)

# Build the function stat() which calculates mean and median of a numeric 
# vector and returns a dataframe

stat <- function(x){
    mean <- mean(x, na.rm = T)
    median <- median(x, na.rm = T)
    DT <- cbind(mean, median)
    return(DT)
}

# Calculate the mean and median of the total number of steps taken per day

stat(aggDT1$steps)

# Create a character vector which formats the interval variable in myDT to 
# width 4, then construct the variable int_scale showing the decimal hours

int_char <- formatC(myDT$interval, width = 4, flag = "0")
myDT$int_scale <- as.numeric(substr(int_char, 1, 2)) + 
    as.numeric(substr(int_char, 3, 4)) / 60

# Use aggregate() FUN to collapse the data and get a dataframe named aggDT2  
# showing the average number of steps taken by 5-minute interval

aggDT2 <- with(myDT, aggregate(steps ~ interval + int_scale, FUN = mean, 
                               na.rm = T))

# Make a time series plot of the 5-minute interval and the average number of   
# steps taken using base plotting system

opar <- par(no.readonly = T)
par(mar = c(5, 5, 4, 1), cex = .85, las = 1)
with(aggDT2, plot(int_scale, steps, type = "l",
                  xlab = "5-minute interval (by Hour)", 
                  ylab = "Average steps", 
                  main = "Average number of steps taken vs. 5-minute intervals")
)
par(opar)

# Give the 5-minute interval containing the maximum number of steps

subset(aggDT2, aggDT2$steps == max(aggDT2$steps))[, c(1, 3)]

# Calculate the total number of missing values in the dataset 

sum(!complete.cases(myDT))

# Fill in all of the missing values with the corresponding average steps of  
# 5-minute interval to generate a new complete dataframe impDT

int_unique <- unique(myDT$interval)
impDT <- myDT
for (i in 1:288) {
    impDT[which(impDT$interval == int_unique[i] & 
                    !complete.cases(impDT)), 1] <- aggDT2$steps[i]
}

# Check whether the new dataset have incomplete observations or not

sum(!complete.cases(impDT))

# Use aggregate() FUN to collapse the new dataset impDT and get a dataframe  
# named aggDT3 showing the total number of steps taken per day

aggDT3 <- with(impDT, aggregate(steps ~ date, FUN = sum))

# Make a histogram of the total number of steps taken each day generated from  
# NAs filled-in data 

opar <- par(no.readonly = T)
par(mar = c(5, 5, 4, 1), cex = .85, las = 1)
with(aggDT3, hist(
    steps, breaks = 20, col = "steelblue", 
    xlab = "Total steps",
    main = " Histogram of the total number of steps by day with NAs filled-in")
)
par(opar)

# Calculate the mean and median of the total number of steps taken per day with 
# the new data

stat(aggDT3$steps)

# Let's look back to the mean and median of the original data

stat(aggDT1$steps)

# Create a new factor variable weekcat in impDT with two levels -- ¡°weekday¡± 
# and ¡°weekend¡± indicating whether a given date is a weekday or weekend day

impDT <- within(impDT, {
    weekcat <- NA
    weekcat[wday(date)>= 2 & wday(date) <= 6] <- "weekday"
    weekcat[wday(date) == 1 | wday(date) == 7] <- "weekend"
})
impDT$weekcat <- factor(impDT$weekcat, levels = c("weekend", "weekday"))

# Use aggregate() FUN to collapse impDT and get aggDT4 dataset showing the  
# average number of steps taken by 5-minute interval and weekcat

aggDT4 <- with(impDT, aggregate(steps ~ int_scale + weekcat, FUN = mean))

# Make a panel plot containing a time series plot of the 5-minute interval and 
# the average number of steps taken across all weekday days or weekend days, 
# using lattice plotting system

if(!is.element("lattice", installed.packages()[,1])) {
    print("Installing packages")
    install.packages("lattice")
}
library(lattice)
xyplot(steps ~ int_scale | weekcat, data = aggDT4, type = "l", 
       col = "steelblue", layout = c(1, 2), xlim = c(0, 24), 
       xlab = "5-minute interval (by Hour)", 
       ylab = "Average steps", 
       main = "Average number of steps taken vs. 5-minute
       intervals across weekdays or weekends")
