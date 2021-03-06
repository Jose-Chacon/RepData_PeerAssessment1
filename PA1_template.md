---
title: "PA1_template"
author: "Jose Chacon"
date: "Sunday, June 14, 2015"
output: html_document
---
#Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

#Data

The data for this assignment can be downloaded from the course web site:

###Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

####steps:
Number of steps taking in a 5-minute interval (missing values are coded as NA)

####date: 
The date on which the measurement was taken in YYYY-MM-DD format

####interval: 
Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

#### Next operations will detail steps need to answer each question in the Assignment 1 from Coursera�s course Reproducible Research, part of Data Science Specialization.


#### Part 1: Loading and preprocessing the data

#####Step 1: Data will be loaded into data frame keeping headers


```r
# Load data from csv file keeping headers
df <- read.csv("./activity.csv", header = TRUE, stringsAsFactors = FALSE)

# Transform text date into Date type class
df$date <- as.Date(df$date)
```
#### Part 2: What is mean total number of steps taken per day?
#####Step 1: Calculate the total number of steps taken per day
#####Step 2: Make an histogram of the total number of steps taken each day


```r
# Total steps per day ommiting missing values
tot.steps.day <- aggregate(steps  ~ date, data = df, FUN = "sum")
hist(tot.steps.day$steps, main = "Total steps per day", xlab = "Steps", ylab = "Frequency")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

#####Step 3: Calculate and report the mean and median of the total number of steps taken per day


```r
# Mean total number of steps
print(mean(tot.steps.day$steps))
```

```
## [1] 10766.19
```

```r
# Median total numbre of steps
print(median(tot.steps.day$steps))
```

```
## [1] 10765
```

#### Part 3: What is the average daily activity pattern?

#####Step 1: Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
# Average steps per interval
steps.interval.average <- aggregate(steps  ~ interval, data = df, FUN = "mean")

# Plot intervals and average steps
plot(steps.interval.average$steps,steps.interval.average$inteval, type = "l", xlab = "Steps" , ylab = "Interval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

#####Step 2: Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
# Maximun number of steps
steps.interval.average[which.max( steps.interval.average[,2] ),]
```

```
##     interval    steps
## 104      835 206.1698
```
####Part 4: Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.


#####Step 1: Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
# Missing values quantity
length(which(is.na(df)))
```

```
## [1] 2304
```

#####Step 2: Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#####Step 3: Create a new dataset that is equal to the original dataset but with the missing data filled in.

Fill missing values with mean from average observations per interval

```r
# Fill missing values with mean from all observations
df1 <- df
df1$steps[is.na(df1$steps)] <- steps.interval.average$steps[match(df1$interval,steps.interval.average$interval)]
```

```
## Warning in df1$steps[is.na(df1$steps)] <- steps.interval.average
## $steps[match(df1$interval, : number of items to replace is not a multiple
## of replacement length
```
#####Step 4: Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.  


```r
# Plot histogram by daily total steps
tot.steps.day <- aggregate(steps  ~ date, data = df1, FUN = "sum")
hist(tot.steps.day$steps, main = "Total steps per day (without NA)", xlab = "Steps", ylab = "Frequency")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

####Do these values differ from the estimates from the first part of the assignment? 
Yes, it differs.


####What is the impact of imputing missing data on the estimates of the total daily number of steps
Mean and Median were increased and now distribution seens more like bell curve.


```r
# Mean from data with replaced missing values
print(mean(tot.steps.day$steps))
```

```
## [1] 10766.19
```


```r
# Median from data with replaced missing values
print(median(tot.steps.day$steps))
```

```
## [1] 10766.19
```


####Part 5: Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

#####Step 1: Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
# Indentify weekdays and weekends
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
df1$weekdays <- c('weekend', 'weekday')[(weekdays(df1$date) %in% weekdays1)+1L]

# Observations during weekdays
weekday <- subset (df1, weekdays == "weekday" )

# Observations during weekends
weekend <- subset (df1, weekdays == "weekend" )
```

#####Step 2: Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
# Plot by intervals, average steps by weekday
weekday <- aggregate(steps  ~ interval, data = weekday, FUN = "mean")
weekend <- aggregate(steps  ~ interval, data = weekend, FUN = "mean")

par(mfrow = c(2, 1), mar = c(0, 4, 2, 1))
plot(weekend, type = "l", main = "Weekend" ,col = "blue3")
plot(weekday, type = "l", main = "Weekday", col = "blue3")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

