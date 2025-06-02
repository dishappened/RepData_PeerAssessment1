---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

##Loading and preprocessing the data 

``` r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

``` r
library(lattice)

activity<-read.csv("activity.csv")
activity$date<-as.Date(activity$date)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

``` r
head(activity)
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

## What is the mean total number of steps taken per day?

``` r
daily_steps<- activity %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))
hist(daily_steps$total_steps,
     main="Histogram of Total Steps per Day",
     xlab="Total Steps per Day",
     ylab="Frequency",
     col="red",
     breaks=20)
```

![Histogram of total steps per day](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

``` r
mean_steps<-mean(daily_steps$total_steps)
median_steps<-median(daily_steps$total_steps)

print(paste("Mean total steps per day:", round(mean_steps,2)))
```

```
## [1] "Mean total steps per day: 10766.19"
```

``` r
print(paste("Median total steps per day:", median_steps))
```

```
## [1] "Median total steps per day: 10765"
```

## What is the average daily activity pattern?

``` r
interval_avg<-activity %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarise(avg_steps = mean(steps))
plot(interval_avg$interval, interval_avg$avg_steps,
     type="l",
     main="Average Daily Activity Pattern",
     xlab="5-Minute Interval",
     ylab="Average Number of Steps",
     col="brown")
```

![Plot of average daily activity](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

``` r
max_interval<-interval_avg$interval[which.max(interval_avg$avg_steps)]
max_steps<-max(interval_avg$avg_steps)

print(paste("5 minute interval with maximum average steps:", max_interval))
```

```
## [1] "5 minute interval with maximum average steps: 835"
```

``` r
print(paste("Maximum average steps:", round(max_steps, 2)))
```

```
## [1] "Maximum average steps: 206.17"
```

## Imputing missing values 

``` r
total_missing <- sum(is.na(activity$steps))
print(paste("Total number of missing values:", total_missing))
```

```
## [1] "Total number of missing values: 2304"
```

``` r
#new dataset with filled in missing values 
activity_filled<-activity
for(i in 1:nrow(activity_filled)){
  if(is.na(activity_filled$steps[i])){
    interval_val<-activity_filled$interval[i]
    activity_filled$steps[i]<- interval_avg$avg_steps[interval_avg$interval == interval_val]
  }
}
print(paste("Missing values after imputation:", sum(is.na(activity_filled$steps))))
```

```
## [1] "Missing values after imputation: 0"
```

``` r
#calculate total steps per day with imputed data 
daily_steps_filled <- activity_filled %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))
hist(daily_steps_filled$total_steps,
     main="Histogram of Total Steps per Day ( After Imputation)",
     xlab="Total Steps per Day",
     ylab="Frequency",
     col="green",
     breaks=20)
```

![Histogram of Imputed Missing values](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

``` r
mean_steps_filled <- mean(daily_steps_filled$total_steps)
median_steps_filled <- median(daily_steps_filled$total_steps)

print(paste("Mean total steps per day (after imputation):",round(mean_steps_filled, 2)))
```

```
## [1] "Mean total steps per day (after imputation): 10766.19"
```

``` r
print(paste("Median total steps per day (after imputation):",round(median_steps_filled, 2)))
```

```
## [1] "Median total steps per day (after imputation): 10766.19"
```

``` r
#comparing with original 
print("Comparison:")
```

```
## [1] "Comparison:"
```

``` r
print(paste("Original mean:", round(mean_steps, 2), "vs Imputed mean:", round(mean_steps_filled, 2)))
```

```
## [1] "Original mean: 10766.19 vs Imputed mean: 10766.19"
```

``` r
print(paste("Original median:", median_steps, "vs Imputed median:", round(median_steps_filled, 2)))
```

```
## [1] "Original median: 10765 vs Imputed median: 10766.19"
```

## Are there differences in activity patterns between weekdays and weekends?

``` r
activity_filled$day_type<-ifelse(weekdays(activity_filled$date) %in% c("Saturday", "Sunday"),
                                 "weekend", "weekday")
activity_filled$day_type <- as.factor(activity_filled$day_type)
interval_daytype <- activity_filled %>%
  group_by(interval, day_type) %>%
  summarise(avg_steps = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the
## `.groups` argument.
```

``` r
xyplot(avg_steps~interval | day_type,
       data=interval_daytype,
       type="l",
       layout = c(1, 2),
       main="Average Steps by 5-Minute Interval: Weekdays vs Weekends",
       xlab="5-Minute Interval",
       ylab="Average Number of Steps")
```

![Plot of weekend and weekday activities](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
