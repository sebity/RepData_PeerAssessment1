# Reproducible Research: Peer Assessment 1
Jan Tatham  


## Loading and preprocessing the data
Show any code that is needed to:


**Load the data (i.e. `read.csv()`)**


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


**Process/transform the data (if necessary) into a format suitable for your analysis.**


```r
# Converting date field class from "factor" to "Date"
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

**Make a histogram of the total number of steps taken each day.**


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



**Calculate and report the mean and median total number of steps taken per day.**


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

**Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**


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


**Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**


```r
maximum_steps <- activity_interval[which.max(activity_interval$steps),c("interval")]

print(paste("Maximum Number of Steps:", maximum_steps))
```

```
## [1] "Maximum Number of Steps: 835"
```


## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

**Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**


```r
missing_values <- sum(is.na(activity))

print(paste("Number of missing values", missing_values))
```

```
## [1] "Number of missing values 2304"
```


**Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**


```r
activity_imputed <- replace(activity$steps, is.na(activity$steps), mean(steps_per_day_mean))
```


**Create a new dataset that is equal to the original dataset but with the missing data filled in.**


```r
# Replace each missing value with the mean value of its 5-minute interval
value_impute <- function(steps, interval) {
  
  val <- 0
  
  if (!is.na(steps)) {
    val <- c(steps)
  }
  else {
    val <- (activity_interval[activity_interval$interval==interval, "steps"])
  }

  return(val)
}

data_imputed <- activity

data_imputed$steps <- mapply(value_impute, data_imputed$steps, data_imputed$interval)
```


**Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**


```r
total_number_of_steps <- aggregate(steps ~ date, sum, data = data_imputed, na.action = na.omit)

png("./figure/plot3.png", width = 800, height = 600)

g <- ggplot(total_number_of_steps, aes(x=steps))
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

![TBA](./figure/plot3.png)



```r
steps_per_day_mean_2 <- mean(total_number_of_steps$steps)

steps_per_day_median_2 <- median(total_number_of_steps$steps)

print(paste("Mean:", steps_per_day_mean_2))
```

```
## [1] "Mean: 10766.1886792453"
```

```r
print(paste("Median:", steps_per_day_median_2))
```

```
## [1] "Median: 10766.1886792453"
```


## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

**Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.**


```r
weekday <- c('Monday', 'Tuesday', 'Wednesday','Thursday', 'Friday')
activity$day_of_week <- factor((weekdays(as.Date(activity$date)) %in% weekday), levels=c(TRUE,FALSE),labels=c('Weekday','Weekend'))
```


**Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).**


```r
average_steps <- aggregate(steps ~ interval + day_of_week, data = activity, mean)

png("./figure/plot4.png", width = 800, height = 600)

g <- ggplot(average_steps, aes(x=interval, y=steps, group = day_of_week))
g <- g + geom_line() + facet_wrap(~day_of_week, ncol = 1) +
  labs(x="Interval", y="Average Number of Steps") +
  labs(title="Average Number of Steps per Time Interval") + theme_bw()
print(g)

dev.off()
```

```
## png 
##   2
```

![](./figure/plot4.png)
