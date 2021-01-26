---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



- Author : J. Ivan Avalos
- date : 26 ene. 2021

## Loading and preprocessing the data
* Check if the zip file has been unzipped
* Read activity.csv file

```r
if(!file.exists("activity.csv")){
    print("Unzipping the file")
    unzip(zipfile = "activity.zip")}

DFactivity <- read.csv("activity.csv",header = TRUE,na.strings = c("NA"))
head(DFactivity)
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


## What is mean total number of steps taken per day?
* Use aggregate function to compute total steps by day
* Histogram of total steps
* Use aggregate function to compute mean and median steps by day
* Compute mean and median of the total number of steps per day

```r
no.steps <- aggregate(steps ~ date, DFactivity, sum, na.rm = TRUE)
```

```r
hist(no.steps$steps, xlab = "Total number of steps per day", ylab = "Days",
     main = "Histogram of steps taken each day", col = "green",
     breaks = seq(0,25000,2000), ylim = c(0,20), xlim = c(0,25000))
```

![](PA1_template_files/figure-html/plots1-1.png)<!-- -->

Mean and median number of steps taken each day

```r
mean_Steps <- mean(no.steps$steps)
mean_Steps
```

```
## [1] 10766.19
```

```r
median_Steps <- median(no.steps$steps)
median_Steps
```

```
## [1] 10765
```

## What is the average daily activity pattern?
* Use aggregate function to compute the average number of steps taken every 5-minute interval
* use plot to plot time series with type = "l"
* use which.max function to get the index of the maximum average number of steps 

```r
interval.steps <- aggregate(steps ~ interval, DFactivity, mean, na.rm = TRUE)
maxIndex.steps <- with(interval.steps, which.max(steps))
```

```r
with(interval.steps, plot(interval, steps, type = "l", xlab = "interval - 5 minutes",
                          ylab = "average across all days",
                          main = "Average number of steps per intervals"))
```

![](PA1_template_files/figure-html/plot2-1.png)<!-- -->

Maximum number of steps of 5-minute interval, on average across all the days

```r
maxInterSteps <- interval.steps[maxIndex.steps, "interval"]
maxInterSteps
```

```
## [1] 835
```

## Imputing missing values
* colSums to obtain how many missing values are
* Fill missing data with mean of total number of steps per day 
* Histograms of total steps with new filled data

```r
missing.values <- colSums(is.na(DFactivity))
missing.values
```

```
##    steps     date interval 
##     2304        0        0
```

```r
index.matched <- match(DFactivity$interval, interval.steps$interval)
fillWithInterval <- interval.steps$steps[index.matched]
DFactivity.filled <- transform(DFactivity, steps = ifelse(
  is.na(DFactivity$steps), yes = fillWithInterval, no = DFactivity$steps))
missing.values.filled <- colSums(is.na(DFactivity.filled))
missing.values.filled
```

```
##    steps     date interval 
##        0        0        0
```

```r
no.steps.filled <- aggregate(steps ~ date, DFactivity.filled, sum)
hist(no.steps.filled$steps, xlab = "Total number of steps per day", ylab = "Days",
     main = "Histogram of steps taken each day", col = "green",
     breaks = seq(0,25000,2000), ylim = c(0,25), xlim = c(0,25000))
```

![](PA1_template_files/figure-html/plot3-1.png)<!-- -->
Mean and median number of steps taken each day

```r
mean_Steps <- mean(no.steps.filled$steps)
mean_Steps
```

```
## [1] 10766.19
```

```r
median_Steps <- median(no.steps.filled$steps)
median_Steps
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
* Create a factor with is.Weekedn function, import chron library
* Use aggregate function to compute the average number of steps taken every 5-minute interval for weeday and weekends
* Make a panel plot

```r
library(chron)
DFactivity.filled <- transform(DFactivity.filled, date = as.Date(date))
DFactivity.filled$isWeekend <- factor(is.weekend(DFactivity.filled$date))
interval.steps_w <- aggregate(steps ~ interval + isWeekend, DFactivity.filled, mean)
```

```r
par(mfrow = c(2,1), mar = c(4,4,2,2),oma = c(0, 0, 3, 0))
with(subset(interval.steps_w, isWeekend == TRUE), plot(interval, steps, type = "l", xlab = "interval - 5 minutes", ylab = "average across weekedns", col = "red"))
with(subset(interval.steps_w, isWeekend == FALSE), plot(interval, steps, type = "l", xlab = "interval - 5 minutes", ylab = "average across weekdays", col = "blue"))
mtext("Average number of steps per intervals", outer = TRUE)
```

![](PA1_template_files/figure-html/plot4-1.png)<!-- -->
