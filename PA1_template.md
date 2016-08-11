# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(dplyr);
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

```r
activity = read.csv("./data/activity.csv");
activity$date = as.Date(activity$date);
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day
2. Calculate and report the mean and median total number of steps taken per day

```r
daily_activity = aggregate(steps ~ date, activity, sum);
hist(daily_activity$steps, 
     breaks=12,
     col = 'blue',
     xlab ='Daily Steps',
     ylab = 'Count of Days',
     main = 'Total Number of Steps Taken Per Day');
```

![](PA1_template_files/figure-html/mean_per_day-1.png)<!-- -->

```r
mean(daily_activity$steps);
```

```
## [1] 10766.19
```

```r
median(daily_activity$steps);
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Time series plot of the 5-minute interval (x-axis) and the average number of steps taken



```r
interval_activity = aggregate(steps ~ interval, activity, mean);
with(interval_activity, 
     plot (  x = interval,
             y = steps, 
             type = 'l',
             xlab ='Five-Minute Interval',
             ylab = 'Average of Steps Taken',
             main = 'Average Daily Activity Pattern'));
```

![](PA1_template_files/figure-html/daily_pattern-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
interval_activity[which.max(interval_activity$steps), ]$interval;
```

```
## [1] 835
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset

```r
sum(is.na(activity$steps));
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. 
Decided to use mean of 5-minute interval.  Not if use daily mean, we'll run into missing value again

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
fill_value_interval = interval_activity;
colnames(fill_value_interval)[2] = 'fill_value_steps';
activity_filled = left_join(activity, fill_value_interval, by = 'interval');
colnames(activity_filled)[1] = 'original_steps';
activity_filled = mutate(activity_filled, 
                         steps = ifelse(is.na(original_steps), fill_value_steps, original_steps));
final_activity_filled = select(activity_filled, -c(original_steps, fill_value_steps));
sum(is.na(final_activity_filled$steps));
```

```
## [1] 0
```

4. Make a histogram of the total number of steps taken each day 
and Calculate and report the mean and median total number of steps taken per day.
(no change on mean, but skewed the median)

```r
daily_activity_filled = aggregate(steps ~ date, final_activity_filled, sum);
hist(daily_activity_filled$steps, 
     breaks=12,
     col = 'blue',
     xlab ='Daily Steps',
     ylab = 'Count of Days',
     main = 'Total Number of Steps Taken Per Day - Missing Value Filled');
```

![](PA1_template_files/figure-html/missing_4-1.png)<!-- -->

```r
mean(daily_activity_filled$steps);
```

```
## [1] 10766.19
```

```r
median(daily_activity_filled$steps);
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend"
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
  and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
final_activity_filled = mutate(final_activity_filled, 
    if_weekday = as.factor(ifelse(weekdays(date) %in% c('Saturday', 'Sunday'), 'weekend', 'weekday')));
interval_activity_filled = aggregate(steps ~ if_weekday + interval, final_activity_filled, mean);

library(ggplot2);
g <- ggplot(interval_activity_filled, aes(x=interval, y=steps));
g + geom_line(color='blue') + ggtitle("Activity Patterns Weekdays vs. Weekend") +
    facet_grid(. ~ if_weekday, margins = TRUE);
```

![](PA1_template_files/figure-html/weekday-1.png)<!-- -->
