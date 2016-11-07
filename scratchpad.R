library(dplyr)
library(ggplot2)


## Loading and preprocessing the data

rm(list = ls()) # clear vars
setwd("C:\\dev\\RepData_PeerAssessment1\\")

activity.data <- read.csv(unz("activity.zip", "activity.csv"), colClasses = c("integer", "Date", "integer"))
activity.data$date <- as.POSIXct(activity.data$date)

## What is mean total number of steps taken per day?

#Calculate the total number of steps taken per day
total.steps <- tapply(activity.data$steps, activity.data$date, FUN = sum, na.rm = TRUE)
daily.steps <- activity.data %>% group_by(date) %>% summarise(Steps = sum(steps))

names(daily.steps) <- c("Date", "steps")

#Calculate and report the mean and median of the total number of steps taken per day
mean.daily.steps <- mean(total.steps, na.rm = TRUE)
median.daily.steps <- median(total.steps, na.rm = TRUE)

#Make a histogram of the total number of steps taken each day
hist(daily.steps$steps, breaks = 11,
     xlab = "number of steps per day",
     main = "Histogram of total steps per day")
abline(v = mean.daily.steps, col = "red", lwd = 1)
abline(v = median.daily.steps, col = "blue", lwd = 1)
legend(x = "topright", legend = c("mean", "median"), col = c("red", "blue"), bty = "n", lwd = 3)


## What is the average daily activity pattern?
activity.interval <- activity.data %>% group_by(interval) %>% summarise(mean_steps = mean(steps, na.rm = TRUE))

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

activity.interval.plot <- ggplot(data = activity.interval, mapping = aes(x = interval, y = mean_steps)) +
    geom_line() + scale_x_continuous("Day Interval", breaks = seq(min(activity.interval$interval),
    max(activity.interval$interval), 100)) + scale_y_continuous("Average Number of Steps") +
    ggtitle("Average Number of Steps Taken by Interval")
activity.interval.plot

## Imputing missing values


## Are there differences in activity patterns between weekdays and weekends?

