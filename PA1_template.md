# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
setwd("C:/Users/Russell Key/RepData/RepData_PeerAssessment1")

# Read Data File after unzipping
Activity <- read.csv("activity.csv", header=T, sep=",")
Activity$date = as.Date(Activity$date, format="%Y-%m-%d")

head(Activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
tail(Activity)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

## What is mean total number of steps taken per day?

```r
steps_per_day = aggregate(steps ~ date, data=Activity, FUN=sum)

hist(steps_per_day$steps, main="Distribution of number of steps taken in a day", xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
average_steps_per_interval = aggregate(steps ~ interval, data=Activity, FUN=mean)

plot(average_steps_per_interval$interval, average_steps_per_interval$steps, type="l", main="Average number of steps taken per 5 minute interval"
     , xlab="Interval", ylab="Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
max_steps_row = which.max(average_steps_per_interval$steps)
average_steps_per_interval[max_steps_row, ]
```

```
##     interval    steps
## 104      835 206.1698
```

**The 5-minute interval that contains the maximum number of steps on average is 835 with 206.2 steps.**

## Imputing missing values

```r
library(dplyr)

# total number of missing values in the dataset
nrow(Activity[is.na(Activity$steps), ])
```

```
## [1] 2304
```

**There are 2304 rows with missing data for steps (coded as NA)**


```r
# filling in all of the missing values in the dataset based upon mean of interval
average_steps_per_interval = aggregate(steps ~ interval, data=Activity, FUN=mean)
missing <- Activity[is.na(Activity$steps), ]
missing <- inner_join(missing, average_steps_per_interval, by = c("interval"))

# Join missing values back into main datatable
Activity2 <- left_join(Activity, missing, by = c("date", "interval"))
Activity2$steps <- ifelse(is.na(Activity2$steps), Activity2$steps.y, Activity2$steps)
Activity2 <- Activity2[, -c(4,5)]

steps_per_day2 = aggregate(steps ~ date, data=Activity2, FUN=sum)

hist(steps_per_day2$steps, main="Distribution of number of steps taken in a day", xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
mean(steps_per_day2$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day2$steps)
```

```
## [1] 10766.19
```

**The mean remained the same at 10766.2 with the median increasing slightly to 10766.2 from 10765**

## Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice)

Activity2$weekday = weekdays(Activity2$date)
Activity2$dayType = ifelse(Activity2$weekday == "Saturday" | Activity2$weekday == "Sunday", "Weekend", "Weekday")

Activity2$dayType = factor(Activity2$dayType)
table(Activity2$dayType, Activity2$weekday)
```

```
##          
##           Friday Monday Saturday Sunday Thursday Tuesday Wednesday
##   Weekday   2592   2592        0      0     2592    2592      2592
##   Weekend      0      0     2304   2304        0       0         0
```

```r
steps_per_day3 = aggregate(steps ~ interval + dayType, data=Activity2, FUN=mean)

xyplot(steps ~ interval | dayType, steps_per_day3, type = "l", layout = c(1, 2), main="Average number of steps taken per 5 minute interval", 
      xlab = "Interval", ylab = "Average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

**The weekday activity shows a more pronounced spike in activity in the morning over the weekend.  This is probably do to people being more activity in the moring before going to work.**
