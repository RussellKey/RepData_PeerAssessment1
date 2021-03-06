# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
setwd("C:/Users/Russell Key/RepData/RepData_PeerAssessment1")

# Read Data File after unzipping
Activity <- read.csv("activity.csv", header=T, sep=",")
Activity$date = as.Date(Activity$date, format="%Y-%m-%d")


## What is mean total number of steps taken per day?

steps_per_day = aggregate(steps ~ date, data=Activity, FUN=sum)

hist(steps_per_day$steps, main="Distribution of number of steps taken in a day", xlab="Steps")
mean(steps_per_day$steps)
median(steps_per_day$steps)

## What is the average daily activity pattern?

average_steps_per_interval = aggregate(steps ~ interval, data=Activity, FUN=mean)

plot(average_steps_per_interval$interval, average_steps_per_interval$steps, type="l", main="Average number of steps taken per 5 minute interval"
     , xlab="Interval", ylab="Average Steps")

max_steps_row = which.max(average_steps_per_interval$steps)
average_steps_per_interval[max_steps_row, ]


## Imputing missing values

library(dplyr)

# total number of missing values in the dataset
nrow(Activity[is.na(Activity$steps), ])

# filling in all of the missing values in the dataset
average_steps_per_interval = aggregate(steps ~ interval, data=Activity, FUN=mean)
missing <- Activity[is.na(Activity$steps), ]
missing <- inner_join(missing, average_steps_per_interval, by = c("interval"))

Activity2 <- left_join(Activity, missing, by = c("date", "interval"))
Activity2$steps <- ifelse(is.na(Activity2$steps), Activity2$steps.y, Activity2$steps)
Activity2 <- Activity2[, -c(4,5)]

steps_per_day2 = aggregate(steps ~ date, data=Activity2, FUN=sum)

hist(steps_per_day2$steps, main="Distribution of number of steps taken in a day", xlab="Steps")
mean(steps_per_day2$steps)
median(steps_per_day2$steps)


## Are there differences in activity patterns between weekdays and weekends?

library(lattice)

Activity2$weekday = weekdays(Activity2$date)
Activity2$dayType = ifelse(Activity2$weekday == "Saturday" | Activity2$weekday == "Sunday", "Weekend", "Weekday")

Activity2$dayType = factor(Activity2$dayType)
table(Activity2$dayType, Activity2$weekday)

steps_per_day3 = aggregate(steps ~ interval + dayType, data=Activity2, FUN=mean)

xyplot(steps ~ interval | dayType, steps_per_day3, type = "l", layout = c(1, 2), main="Average number of steps taken per 5 minute interval", 
      xlab = "Interval", ylab = "Average steps")
