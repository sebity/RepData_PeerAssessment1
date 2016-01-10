# Reproducible Research: Peer Assessment 1
Jan Tatham  


## Loading and preprocessing the data
Show any code that is needed to:


1. Load the data (i.e. `read.csv()`)


```r
# Loading the required Libraries
library(ggplot2)
library(lubridate)

# Check if Data folder exists
if(!dir.exists('./Data')) {
  dir.create('./Data')
}

# Check if figure folder exists
if(!dir.exists('./figure')) {
  dir.create('./figure')
}

# Check of activity.csv file exists in Data folder
if(file.exists("./Data/activity.csv") == FALSE) {
  fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileUrl, destfile = "./Data/activity.zip", mode = "wb")
  unzip("./Data/activity.zip")
}

# Load the dataset
activity <- read.csv("activity.csv", header = TRUE)
```


2. Process/transform the data (if necessary) into a format suitable for your analysis.


```r
# Converting date field class from "factor" to "Date"
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Make a histogram of the total number of steps taken each day.


```r
steps_per_day <- aggregate(steps ~ date, activity, sum, na.rm = TRUE)

png("./figure/plot1.png", width = 800, height = 600)

g <- ggplot(steps_per_day, aes(x=steps))
g <- g + geom_histogram(fill = "red", col = "black", bins=20) +
  labs(x="Total Steps/Day", y="Frequency") +
  labs(title="Total Number of Steps Taken per Day") + theme_bw()
print(g)

dev.off()
```

```
## png 
##   2
```


![TBA](./figure/plot1.png)



2. Calculate and report the **mean** and **median** total number of steps taken per day.


```r
steps_per_day_mean <- mean(steps_per_day$steps)

steps_per_day_median <- median(steps_per_day$steps)

print(paste("Mean:", steps_per_day_mean))
```

```
## [1] "Mean: 10766.1886792453"
```

```r
print(paste("Median:", steps_per_day_median))
```

```
## [1] "Median: 10765"
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
activity_interval <- aggregate(steps ~ interval, activity, mean, na.rm = TRUE)

png("./figure/plot2.png", width = 800, height = 600)

g <- ggplot(activity_interval, aes(x=interval, y=steps))
g <- g + geom_line() +
  labs(x="5-Minute Interval", y="Average Number of Steps") +
  labs(title="Average Number of Steps on a 5-Minute Interval") + theme_bw()
print(g)

dev.off()
```

```
## png 
##   2
```


![TBA](./figure/plot2.png)


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maximum_steps <- activity_interval[which.max(activity_interval$steps),c("interval")]

print(paste("Maximum Number of Steps:", maximum_steps))
```

```
## [1] "Maximum Number of Steps: 835"
```


## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)




2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.




3. Create a new dataset that is equal to the original dataset but with the missing data filled in.




4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?




## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.




2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


