# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
First unzip activity.zip.
Load the data from activity.csv with the following code.

```r
data <- read.csv("activity.csv", colClasses=c("numeric", "Date", "numeric"))
```

Clean the data by removing any NA values.

```r
cleanData <- data[complete.cases(data),]
```

## What is mean total number of steps taken per day?
Sum the steps taken per day by applying the aggregate function.

```r
stepsByDate <- aggregate(cleanData$steps, cleanData['date'], sum)
names(stepsByDate) <- c("date", "steps")
```

A histogram of the total number of steps taken each day

```r
hist(stepsByDate$steps, xlab="Steps/Day", ylab="Frequency", main="Frequency of Steps per Day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 



```r
mn <- mean(stepsByDate$steps)
med <- median(stepsByDate$steps)
```
The mean number of steps per day is 1.0766 &times; 10<sup>4</sup>. The median is 1.0765 &times; 10<sup>4</sup>.


## What is the average daily activity pattern?
Average out the steps taken per time interval for all days

```r
averageSteps <- aggregate(cleanData$steps, cleanData['interval'], mean)
names(averageSteps) <- c("interval", "steps")
```

a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
plot(averageSteps$interval, averageSteps$steps, type='l', xlab='interval', ylab='average steps')
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


```r
maxSteps <- max(averageSteps$steps)
interval <- averageSteps[averageSteps == maxSteps, ][['interval']]
```
The  5-minute interval containing the maximum number of steps is NA with a value of 206.1698.

## Imputing missing values

```r
missingCnt <- sum(!complete.cases(data))
```
The count of missing data is 2304.

Creating a new dataset and filling in the missing data

```r
newData <- data
for(i in 1:nrow(newData)){
        if (any(is.na(newData[i,]))){
                newData[i,'steps'] <- averageSteps[averageSteps$interval == newData[i,'interval'],][['steps']]
        }
}

newStepsByDate <- aggregate(newData$steps, newData['date'], sum)
names(newStepsByDate) <- c("date", "steps")
hist(newStepsByDate$steps, xlab="Steps/Day", ylab="Frequency", main="Frequency of Steps per Day")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
newMn <- mean(newStepsByDate$steps)
newMed <- median(newStepsByDate$steps)
meanDiff <- mn - newMn
medDiff <- med - newMed
```
The mean number of steps per day after filling the NA's is 1.0766 &times; 10<sup>4</sup>. The median is 1.0766 &times; 10<sup>4</sup>.

The change after filling the NA's is mean: 0 , median: -1.1887.
The mean has not changed. The median has reduced.

## Are there differences in activity patterns between weekdays and weekends?
Creating a new factor variable in the dataset with two levels – “weekday” and “weekend”

```r
newData$Day <- weekdays(newData$date)
newData$dayType[newData$Day=='Sunday' | newData$Day=='Saturday'] <- 'weekend'
newData$dayType[is.na(newData$dayType)] <- 'weekday'
newData$dayType <- as.factor(newData$dayType)
```

A panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
library(lattice)
averageSteps <- aggregate(newData$steps, by=list(newData$interval, newData$dayType), FUN=mean, na.rm=TRUE)
xyplot(averageSteps$x~averageSteps$Group.1|averageSteps$Group.2, layout=c(1,2), type="l", xlab="Interval", ylab="Number of steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

From the above plot it is evident that there are differences in activity patterns between weekdays and weekends.
