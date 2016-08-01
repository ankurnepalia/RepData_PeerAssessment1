Reproducible Research: Peer Assessment 1
==========================================
Created by Ankur Nepalia on July 30, 2016

### Loading and preprocessing the data
#### 1. Unzip and load data in a data frame
```{r}
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
activityData <- read.csv('activity.csv')
```

#### 2. Create a subset DF by removing NA entries
```{r}
activityData.NoNA <- subset(activityData, is.na(activityData$steps) == FALSE)
```


### What is mean total number of steps taken per day?
#### 1. Calculate the total number of steps taken per day
```{r}
TotalStepsPerDay <- aggregate(steps ~ date, activityData.NoNA, sum)
```

#### 2. Histogram of total number of steps taken each day
```{r}
hist(TotalStepsPerDay$steps, breaks = 20, col = "steelblue", main="Histogram of total number of steps taken each day", xlab="Total steps per day", ylab = "Number of Days")
```
#### 3. Calculate the mean and median of the total number of steps taken per day
```{r}
rmean <- mean(TotalStepsPerDay$steps)
rmedian <- median(TotalStepsPerDay$steps)
```

The `mean` is `r rmean` and the `median` is `r rmedian`.


### What is the average daily activity pattern?
#### 1. Calculate average steps for each interval for all days
```{r}
AvgStepsByInterval <- aggregate(steps ~ interval, activityData.NoNA, mean)
```

#### 2. Make a time-series plot (i.e. type="l")
```{r}
plot(AvgStepsByInterval$interval, AvgStepsByInterval$steps, type = "l", col="blue ",xlab = "Interval", ylab = "Average number of steps", main = "Average number of steps per day by interval")
```

#### 3. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
max5minInterval <- AvgStepsByInterval[which.max(AvgStepsByInterval$steps), ]
```

The 5-minute interval, on average across all the days in the dataset, containing the maximum number of steps is `r max5minInterval$interval`.


### Imputing missing values
#### 1. Calculate total number of missing values in the datasets
```{r}
missingRecords <- sum(!complete.cases(activityData))
```

Total number of missing values in the 'Activity' dataset is `r missingRecords`.

#### 2. My strategy to populate missing NA value is by filling it with its average 5-minute interval value. As part of this process I will also create a new dataset with all the NA values filled with some value.
```{r}
activityData.imputed <- activityData

for (i in 1:nrow(activityData.imputed)) {
    if(is.na(activityData.imputed$steps[i])){
        activityData.imputed$steps[i] <- AvgStepsByInterval$steps[which(activityData.imputed$interval[i] == AvgStepsByInterval$interval)]
    }
}

head(activityData.imputed)
sum(is.na(activityData.imputed))
```

#### 3. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.

```{r}
TotalStepsPerDay.imputed <- aggregate(steps ~ date, activityData.imputed, sum)

hist(TotalStepsPerDay.imputed$steps, breaks = 20, col = "blue", main="Histogram of total number of steps taken each day (imputed data)", xlab="Total steps per day", ylab = "Number of Days")

rmean.imputed <- mean(TotalStepsPerDay.imputed$steps)
rmedian.imputed <- median(TotalStepsPerDay.imputed$steps)
```

The `mean` with NO missing data is `r rmean.imputed` and the `median` with NO missing data is `r rmedian.imputed`.
The `mean` with missing data is `r rmean` and the `median` with missing data is `r rmedian`.

So, after imputing the missing data, the new mean of total steps taken per day is the SAME as that of the old mean; the new median of total steps taken per day is slightly GREATER than that of the old median.

### Are there differences in activity patterns between weekdays and weekends?
#### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r}
activityData.imputed$dow <-  ifelse(as.POSIXlt(activityData.imputed$date)$wday %in% c(0,6), 'weekend', 'weekday')

head(activityData.imputed)
```

#### 2. Make a panel plot containing a time series plot
```{r}

AverageActivityData.imputed <- aggregate(steps ~ interval + dow, activityData.imputed, mean)

library(lattice)

xyplot(AverageActivityData.imputed$steps ~ AverageActivityData.imputed$interval|AverageActivityData.imputed$dow, main="Average Steps per Day by Interval",xlab="5-minute Interval", ylab="Average number of steps",layout=c(1,2), type="l")

library(ggplot2)

ggplot(AverageActivityData.imputed, aes(interval, steps, color = dow)) + 
    geom_line() + 
    facet_grid(dow ~ .) +
    xlab("5-minute Interval") + 
    ylab("Avarage number of steps") + ggtitle("Average Steps per Day by Interval")

```
