# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Load the `plyr` and `lattice` packages, load the data, and change the class of date column to `Date`:


```r
library(plyr)
library(lattice)

activity <- read.csv('activity.csv')
activity$date <- as.Date(activity$date,'%Y-%m-%d')
```

## What is mean total number of steps taken per day?

Barplot of the total number of steps taken each day:


```r
stepsByDate <- ddply(activity, .(date), summarize, totalSteps=sum(steps,na.rm=TRUE)) 

with(stepsByDate, {
	par(cex.axis=.7,oma=c(1,1,0,0), mar=c(7,7,3,3), mgp=c(5.5,0.75,0),las=2)
	barplot(totalSteps, names.arg=date,
		main="Total Steps by Date",
		xlab="Date",
		ylab="Steps",
		)
	}
)
```

![](PA1_template_files/figure-html/barplot-1.png) 

Mean and median of the total number of steps taken per day:


```r
mean(stepsByDate$totalSteps)
```

```
## [1] 9354.23
```

```r
median(stepsByDate$totalSteps)
```

```
## [1] 10395
```

The mean and median of the total number of steps taken per day are 9354.2295082 and 10395, respectively.

## What is the average daily activity pattern?

Time series plot of the number of steps taken, by interval and averaged accross all days:


```r
avgStepsByInterval <- ddply(activity, .(interval), summarize, avgSteps=mean(steps,na.rm=TRUE))
with(avgStepsByInterval, {
	plot(interval, avgSteps, 
		type='l',
		main='Average steps by 5-minute interval',
		xlab='5-minute interval',
		ylab='Average steps')
	}
)
```

![](PA1_template_files/figure-html/timeSeries-1.png) 

5-minute interval containing the maximum average number of steps:


```r
avgStepsByInterval$interval[which.max(avgStepsByInterval$avgSteps)]
```

```
## [1] 835
```

The time interval corresponding to `avgStepsByInterval$interval[which.max(avgStepsByInterval$avgSteps)]` contains the maximum number of steps.

## Imputing missing values

Total number of missing step values:

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

The total number of step values is 2304.

For the missing step values, we impute the data by using the mean number of steps of the corresponding 5-minute time interval. We will use the results in `avgStepsByInterval` from the previous section:


```r
missingIndex <- which(is.na(activity$steps))
imputed <- join(activity[missingIndex,], avgStepsByInterval, "interval", match="first")$avgSteps
activityImputed <- activity
activityImputed$steps[missingIndex] <- imputed
```

Barplot of the total number of steps taken each day, using the imputed dataset:


```r
stepsByDateImputed <- ddply(activityImputed, .(date), summarize, totalSteps=sum(steps,na.rm=TRUE)) 
with(stepsByDateImputed, {
	par(cex.axis=.7,oma=c(1,1,0,0), mar=c(7,7,3,3), mgp=c(5.5,0.75,0),las=2)
	barplot(totalSteps, names.arg=date,
		main="Total Steps by Date",
		xlab="Date",
		ylab="Steps",
		)
	}
)
```

![](PA1_template_files/figure-html/barplotImputed-1.png) 

Mean and median of the total number of steps taken per day, using the imputed dataset:


```r
mean(stepsByDateImputed$totalSteps)
```

```
## [1] 10766.19
```

```r
median(stepsByDateImputed$totalSteps)
```

```
## [1] 10766.19
```

Using the imputed data, the mean and median of the total number of steps taken per day are 1.0766189\times 10^{4} and 1.0766189\times 10^{4}, respectively. Both values increased.


## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable with levels `weekday` and `weekend`:


```r
activityImputed$dayType <- factor(ifelse(weekdays(activityImputed$date) %in% c("Saturday", "Sunday"), 
	"weekend", 
	"weekday"))
```

Panel plot of time series for the average number of steps taken, by day type:


```r
avgStepsByDayTypeInterval <- ddply(activityImputed, .(dayType,interval), summarize, avgsteps=mean(steps,na.rm=TRUE)) 

xyplot(avgsteps~interval|dayType,avgStepsByDayTypeInterval,type='l',layout=c(1,2))
```

![](PA1_template_files/figure-html/timeSeriesPanel-1.png) 
