---
title: 'Peer-graded Assignment: Course Project 1'
---



### Loading and preprocessing the data

```{r}
data_activity <- read.csv(file = "activity.csv", header = TRUE)
```

### What is mean total number of steps taken per day?

```{r}
#Remove NAs from data:
processed_data <- data_activity[complete.cases(data_activity),] 

#Format dates to date format
processed_data$date <- as.Date(processed_data$date)

#Create new dataset with the sum of steps per day
sum_steps <- aggregate(processed_data["steps"], by = processed_data["date"], sum)

#Create Histogram of Number of Steps per day
hist(sum_steps$steps, col = "blue", main = "Total Number of Steps per day", breaks = 10, 
     xlab = "Number of Steps", ylab = "Frequency")

#Calculate Mean and Median
mean_steps <- mean(sum_steps$steps)
median_steps <- median(sum_steps$steps)
```

The mean is `r mean_steps`.
The median is `r median_steps`.

### What is the average daily activity pattern?

``` {r}
#Create new dataset with the mean average steps per interval
avg_steps <- aggregate(processed_data["steps"], by = processed_data["interval"], mean)

#Create Time Series Plot
plot(steps ~ interval, data = avg_steps, type = "l", col = "blue", xlab = "Interval", ylab = "Number of Steps")

#Check Maximum number of steps and which interval
max_steps <- max(avg_steps$steps)
max_interval <- subset(avg_steps, steps == max_steps)
```

The Maximum Number of steps is `r max_steps`.
The Maximum Number of steps happened on the `r max_interval$interval` interval.