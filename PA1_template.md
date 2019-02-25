---
title: "Analysis of Activity Movement Data - Project 1"
author: "Glenn Walters"
date: "2/24/2019"
output: 
  html_document:
          keep_md: true
   
---



This analysis is to analyze the data from a personal activity montitoring device. The devices collected data in 5 minute increments from October and November 2012.  The variables collected are:

- Steps
- Date of measurementsteps were taken
- The 5 minute interval in which the measurement was taken

The following is the code that read in the data:



```r
rm(list=ls())

activity <- read.csv("activity.csv", header=TRUE)
```

First we want to review the total steps taken per day.  Here is a histogram showing the total number of steps taken each day.  The missing values are removed.


```r
dailysteps <- with(activity, by(steps, date, sum))
hist(dailysteps,
     xlab ="Number of Steps",
     ylab ="Frequency",
     main ="Steps per Day")
```

![](PA1_template_files/figure-html/tot_steps_per_day_hist-1.png)<!-- -->

```r
meanDailySteps <- mean(dailysteps, na.rm=TRUE)
medDailySteps <- median(dailysteps, na.rm=TRUE)
```

The average number of steps per day is 10766.19.
The median number of steps per day is 10765.

The average number of steps taken each over time is shown below.


```r
## Calculates the Interval Means and the Maximum mean

meanInterval <- with(activity, tapply(steps, interval, mean, na.rm=TRUE))
maxInterval <- which.max(meanInterval)

ints <- unique(activity$interval)
newactivity <- data.frame(cbind(meanInterval, ints))

## Plotting time series

with(newactivity, plot(ints, meanInterval,type= "l",
                       xlab ="Intervals (5-min)",
                       ylab ="Average Steps",
                       main ="Daily Activity Pattern"))
```

![](PA1_template_files/figure-html/act_patern_plot-1.png)<!-- -->

```r
numstepsMax <- newactivity[maxInterval,1]

numMissing <- sum(is.na(activity$steps))
```

The interval that possessed the highest average number of steps was 104 with an average number of steps of 206.17.

Since there were 2304 missing values, the strategy used to impute was to use the Interval Mean.  A new dataset was created using the original data and filling the missing values for STEPS with the Interval Means.



```r
## Merging of the datasets

act.imputed <- merge(activity, newactivity, by.x="interval", by.y="ints")

## Replacing the Missing Values with the Imputed Value

act.imputed$steps[is.na(act.imputed$steps)] <- act.imputed$meanInterval[is.na(act.imputed$steps)]

## Calculate Daily Steps

imp.dailysteps <- with(act.imputed, by(steps, date, sum))
```

Here is a Histogram of the dataset with the missing values imputed.

```r
## Histogram

hist(imp.dailysteps,
     xlab ="Number of Steps",
     ylab ="Frequency",
     main ="Steps per Day - imputed")
```

![](PA1_template_files/figure-html/impute_hist-1.png)<!-- -->

```r
imp.meanDailySteps <- mean(imp.dailysteps)
imp.medDailySteps <- median(imp.dailysteps)
```

The average number of steps per day using the imputed data is 10766.19, compared to the unimputed data which is 10766.19.

The median number of steps per day using the imputed data is 10766.19, compared to the unimputed data which is 10765.

We can see that the Mean values do not change, however, the Median is slightly different.

The final piece of the analysis is to compare the Weekday and Weekend activity captured by the measurement devices.  As expected, we can see the activity patterns are distinctly different between weekdays and weekend.


```r
## Code to calculate the Weekday and Weekend data

day1 <- c("Monday","Tuesday","Wednesday","Thursday","Friday")

act.imputed$date2 <- weekdays(as.Date(act.imputed$date))
act.imputed$wDay <- factor((act.imputed$date2) %in% day1, 
                           levels=c(FALSE,TRUE),
                           labels = c("weekend","weekday"))


dailysteps.weekday <- tapply(act.imputed[act.imputed$wDay == "weekday",]$steps,
                             act.imputed[act.imputed$wDay == "weekday",]$interval,
                             mean, na.rm=TRUE)

dailysteps.weekend <- tapply(act.imputed[act.imputed$wDay == "weekend",]$steps,
                             act.imputed[act.imputed$wDay == "weekend",]$interval,
                             mean, na.rm=TRUE)

## Panel Plot

par(mfrow=c(1,2))

plot(dailysteps.weekday, type="l",
     xlab ="Interval",
     ylab ="Daily Steps",
     main ="Weekday Activity Pattern")

plot(dailysteps.weekend, type="l",
     xlab ="Interval",
     ylab ="Daily Steps",
     main ="Weekend Activity Pattern")
```

![](PA1_template_files/figure-html/two_line_plots-1.png)<!-- -->
