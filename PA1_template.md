# Reproducible Research: Peer Assessment 1

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data
The data for this assignment is downloaded from the course web site: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip), and is stored in a
CSV file with a total of 17,568 observations in this dataset.

The variables included in this dataset are:
1) steps: Number of stepas taking in a 5-minute interval (missing values are coded as NA)
2) date: The date on which the measurement was taken in YYYY-MM-DD format
3) interval: Identifier for the 5-minute interval in which measurement was taken

## Setting global chunk options.
- Set size for all figures in this report, also set echo = TRUE so that the code can read by others.
```
library(knitr)
opts_chunk$set(fig.width = 9, fig.height = 5, fig.path='Figs/', dpi = 144, 
                      echo = TRUE, results = 'hold', warning=FALSE, message=FALSE)
```

## Loading and preprocessing the data
- Unzip the data, and load data into data frame. 
```
data <- read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))
```

## What is mean total number of steps taken per day?
- Ignoring the missing values in the dataset, calculate the total number of steps taken per day.
```
steps_per_day <- aggregate(steps ~ date, data, sum, na.action = na.pass)
```

- Make a histogram of the total number of steps taken each day. 
```
library(ggplot2)
p <- ggplot(steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "red", binwidth = 1000) + 
       labs(x = "Number of Steps per Day", y = "Frequency") + theme_bw() 
print(p)
```

- Calculate and report the mean and median of the total number of steps taken per day.
```
steps_mean <- mean(steps_per_day$steps, na.rm = TRUE)
steps_median <- median(steps_per_day$steps, na.rm = TRUE)
```
The mean of the total number of steps taken per day is 10766.189, and the median is 10765.

## What is the average daily activity pattern?
- Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```
steps_per_interval <- aggregate(data$steps, by = list(interval = data$interval),
                                FUN=mean, na.rm=TRUE)
colnames(steps_per_interval) <- c("interval", "steps")
p <- ggplot(steps_per_interval, aes(x=interval, y=steps)) +   
        geom_line(color="red", size=1) +  
        labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
        theme_bw()
print(p)        
```

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```
max_interval <- steps_per_interval[which.max(steps_per_interval$steps),]
str(max_interval)
```
The 835th 5-minute interval contains the maximum number of steps (206 steps). 

## Imputing missing values
- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)
```
missing_vals <- sum(is.na(data$steps))
```
The total number of missing values in the dataset is 2304.

- Use the mean at the same interval across all days to fill in all of the missing values in the dataset, and create a new dataset that is equal to the original dataset but with the missing data filled in.
```
library(plyr)
imputed_data <- adply(data, 1, function(x) if (is.na(x$steps)) {
    x$steps = round(steps_per_interval[steps_per_interval$interval == x$interval, 2])
    x
} else {
    x
})
```

- Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. 
```
new_steps_per_day <- aggregate(steps ~ date, imputed_data, sum)
p <- ggplot(new_steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "red", binwidth = 1000) + 
       labs(x = "Number of Steps per Day (with filled-in values)", y = "Frequency") + theme_bw() 
new_steps_mean <- mean(new_steps_per_day$steps)
new_steps_median <- median(new_steps_per_day$steps)
print(p)
qplot(steps_per_day)
```

With the missing values filled in, the mean and meadian total number of steps taken per day are 10765.639 and 10762, individually. These are slightly different than the mean and median from the original data set, where the mean and median values are 10766.189 and 10765, separately. Imputing missing data increases the estimates of the total daily number of steps.

## Are there differences in activity patterns between weekdays and weekends?
The dataset with the filled-in missing values is used for this part.

- Create a new factor variable in the dataset with two levels indicating whether a given date is a weekday or weekend day.
```
data.weekend <- subset(imputed_data, weekdays(date) %in% c("Saturday", "Sunday"))
data.weekday <- subset(imputed_data, !weekdays(date) %in% c("Saturday", "Sunday"))
data.weekend <- aggregate(steps ~ interval, data.weekend, mean)
data.weekday <- aggregate(steps ~ interval, data.weekday, mean)
data.weekend <- cbind(data.weekend, day = rep("weekends"))
data.weekday <- cbind(data.weekday, day = rep("weekdays"))
data.week <- rbind(data.weekday, data.weekend)
levels(data.week$day) <- c("Weekdays", "Weekends")
```

- Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```
p <- ggplot(data.week, aes(x = interval, y = steps)) + 
        geom_line(color = "red") + 
        facet_grid(day ~ .) +
        labs(x="Interval", y="Number of steps") +
        theme_bw()
print(p)        
```

From the plot above we can see that weekends activities seem to be more evenly distributed throughout the day, while weekdays activities are peaked early in the day, then quiet down for the rest of the day.
