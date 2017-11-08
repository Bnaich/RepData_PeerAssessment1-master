---
title: 'Reproducible Research: Peer Assessment 1'
author: "Maxim Scherbakov"
date: "6 november 2017"
output:
  html_document: default
  word_document: default
---



## Loading and preprocessing the data
* Load the data with read.csv()

* Process/transform the data (if necessary) into a format suitable for your analysis


```r
dset <- read.csv('activity.csv')

str(dset)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
class(dset)
```

```
## [1] "data.frame"
```
As we've seen it's ok that dset is dataframe, but dates as factors aren't convinient.
Let's transform it with as.Date() function:

```r
dset$date <- as.Date(dset$date, format = "%Y-%m-%d")
```
Now look at the data:

```r
str(dset)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(dset)
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

1. Calculate the total number of steps taken per day


```r
steps_per_day <- tapply(dset$steps,INDEX = dset$date, sum, na.rm = TRUE)
head(steps_per_day)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420
```

2. Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
dates <- as.Date(names(steps_per_day))
m <- ggplot(as.data.frame(steps_per_day),aes(steps_per_day))
m + geom_histogram(bins = 61)
```

![plot of chunk histogram](figure/histogram-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean_vals <- tapply(dset$steps[!is.na(dset$steps)],INDEX = as.Date(dset$date[!is.na(dset$steps)]), mean)
head(mean_vals)
```

```
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##    0.43750   39.41667   42.06944   46.15972   53.54167   38.24653
```

```r
median_vals <- tapply(dset$steps[!is.na(dset$steps)],INDEX = dset$date[!is.na(dset$steps)], median)
head(median_vals)
```

```
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##          0          0          0          0          0          0
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
av_activity <- tapply(dset$steps,INDEX = as.factor(dset$interval), mean, na.rm = TRUE)
plot(x = names(av_activity), y = av_activity, type = 'l', xlab = "Time (5-minute interval)", ylab = "Activity (average number of steps")
```

![plot of chunk average activity pattern](figure/average activity pattern-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
av_activity[which.max(av_activity)]
```

```
##      835 
## 206.1698
```

We see there is a the maximum number of steps in interval 8:35 - 8:40 (about 206 steps)

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
    sum(is.na(dset$steps))
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Replace every NA with mean steps in it`s time interval:

```r
    mean_int <- tapply(dset$steps, INDEX = as.factor(dset$interval), mean, na.rm = TRUE)
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
    dset_nafree <- dset
    for(i in c(1:length(dset_nafree$steps)))
      if(is.na(dset_nafree$steps[i]))
         dset_nafree$steps[i] = as.integer(mean_int[names(mean_int) == dset_nafree$interval[i]])
    head(dset_nafree)
```

```
##   steps       date interval
## 1     1 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
## 6     2 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steps_per_day2 <- tapply(dset_nafree$steps,INDEX = dset_nafree$date, sum)
head(steps_per_day2)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##      10641        126      11352      12116      13294      15420
```

```r
m2 <- ggplot(as.data.frame(steps_per_day2),aes(steps_per_day2))
m2 + geom_histogram(bins = 61)
```

![plot of chunk histogram2](figure/histogram2-1.png)

```r
mean_vals2 <- tapply(dset_nafree$steps,INDEX = as.Date(dset_nafree$date), mean)
head(mean_vals2)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##   36.94792    0.43750   39.41667   42.06944   46.15972   53.54167
```

```r
median_vals2 <- tapply(dset_nafree$steps,INDEX = dset_nafree$date, median)
head(median_vals2)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##       33.5        0.0        0.0        0.0        0.0        0.0
```

As we can see the second histogram differs from the first like mean & median values. So we say that imputting missing data moved the max count of steps from 0 to about 10000. It`s a very good effect.

##Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.


```r
  week_ends <- c("Sunday","Saturday")
  week_factor <- ifelse(weekdays(dset_nafree$date) %in% week_ends,"Weekends","Weekdays")
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
dset_nafree <- mutate(dset_nafree, week_factor = as.factor(week_factor))
df <- dset_nafree %>% group_by(week_factor, interval) %>% summarise(steps = mean(steps))
ggplot(df, aes(interval, steps)) + facet_wrap(facets = ~week_factor, ncol = 1, nrow = 2) + geom_line() 
```

![plot of chunk last plot](figure/last plot-1.png)

As we can see weekdays morgnings are much more active than weekends ones, but at weekends we observe more evenly
