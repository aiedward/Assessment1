
Setwd(C:/Users/Edward/Assessment1)

## Loading and preprocessing the data

library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)

unzip("activity.zip")
activity <- fread("activity.csv", na.strings = "NA")
activity$date <- ymd(activity$date)

## What is mean total number of steps taken per day?

# Calculate the total number of steps taken per day

activity.byday <- activity %>% 
  group_by(date) %>%
  mutate(totalsteps = sum(steps, na.rm = T))

# Histogram of the total number of steps taken each day

ggplot(data = activity.byday, aes(x = totalsteps)) +
  geom_histogram() +
  labs(title = "Histogram: number of steps by day",
       x = "Number of steps",
       y = "Frequency")
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.


## Calculate and report the mean and median of the total number of steps taken per day

meansteps <- mean(activity.byday$totalsteps, na.rm = T)
mediansteps <- median(activity.byday$totalsteps, na.rm = T)

# The average amount of steps per day is 9354.2295082, the median amount is 1.0395\times 10^{4}.

## What is the average daily activity pattern?

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

activity.byinterval <- group_by(activity, interval) %>% 
  mutate(meansteps = mean(steps, na.rm = T))

ggplot(data = activity.byinterval, 
       aes(x = interval, y = meansteps)) +
  geom_line(na.rm = T) + 
  labs(title = "Average steps taken by interval",
       x = "interval (min)",
       y = "average amount of steps")


## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

maxsteps <- activity.byinterval[which.max(activity.byinterval$meansteps), ]

# The 835 minute interval contains the maximum number of steps (206.1698113) on average across all the days.

## Imputing missing values

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

complete <- sum(complete.cases(activity))
missing <- nrow(activity) - complete

# There are 2304 rows with missing values in the dataset.

# Fill missing values in the dataset.

# Missing values have been replaced with the mean value for that interval.

imputed <- group_by(interval, .data = activity) %>% 
  mutate(steps = ifelse(is.na(steps), as.integer(mean(steps, na.rm = T)), steps))
Make a histogram of the total number of steps taken each day

imputed.byday <- group_by(date, .data = imputed) %>%
  mutate(totalsteps = sum(steps))

ggplot(data = imputed.byday, aes(x = totalsteps)) +
  geom_histogram() +
  labs(title = "Histogram: number of steps by day",
       x = "Number of steps",
       y = "Frequency")
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.


## Calculate and report the mean and median total number of steps taken per day.

imputed.meansteps <- mean(activity.byday$totalsteps)
imputed.mediansteps <- median(activity.byday$totalsteps)

# With the missing values filled, the average amount of steps per day is 9354.2295082, the median amount is 1.0395\times 10^{4}.

#In contrast when missing values are not taken into account the average amount of steps per day is 9354.2295082, the median amount is 1.0395\times 10^{4}.

# This is a difference in avg steps per day of 0 and median amount of steps per day of 0.

## Are there differences in activity patterns between weekdays and weekends?

# Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

imputed <- imputed %>% 
  mutate(weekday = as.factor(
    ifelse(wday(date) %in% c(1,7), "weekend", "weekday"))) 

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

imputed.byinterval <- group_by(interval, weekday, .data = imputed) %>% 
  mutate(meansteps = mean(steps, na.rm = T))

ggplot(data = imputed.byinterval, 
       aes(x = interval, y = meansteps)) +
  geom_line() + 
  facet_grid(weekday ~ .) +
  labs(title = "Average steps taken by interval",
       x = "interval (min)",
       y = "average amount of steps") 
