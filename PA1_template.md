---
title: "PA1_template"
author: "Ali CHAOUCHE"
date: "5/13/2020"
---
itle: "Reproducible Research: Peer Assessment 1"

output: 

  html_document:

    keep_md: true
    

# **Reproducible Research Coursera**
# **Peer-graded Assignment Course Project 1**


```r
# Install R libraries
library(data.table)
library(dplyr)
library(knitr)
library(Hmisc)
```


## **QUESTION 1: Code for reading in the dataset and/or processing the data** 
Load the data (i.e.read.csv())
Process/transform the data (if necessary) into a format suitable for your analysis

```r
# Read data
activity <- read.csv("activity.csv", header=TRUE)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "10/1/2012","10/10/2012",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
dim(activity)
```

```
## [1] 17568     3
```

```r
head(activity,5)
```

```
##   steps      date interval
## 1    NA 10/1/2012        0
## 2    NA 10/1/2012        5
## 3    NA 10/1/2012       10
## 4    NA 10/1/2012       15
## 5    NA 10/1/2012       20
```

```r
tail(activity,5)
```

```
##       steps       date interval
## 17564    NA 11/30/2012     2335
## 17565    NA 11/30/2012     2340
## 17566    NA 11/30/2012     2345
## 17567    NA 11/30/2012     2350
## 17568    NA 11/30/2012     2355
```
## **QUESTION 2: Histogram of the total number of steps taken each day**
Calculate the total number of steps taken per day
If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
Calculate and report the mean and median of the total number of steps taken per day

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

## **QUESTION 3: Mean and median number of steps taken each day**

```r
 mean (totalsteps)
```

```
## [1] 9354.23
```

```r
 median (totalsteps)
```

```
## [1] 10395
```
## **QUESTION 4: Time series plot of the average number of steps taken**
Make a time series plot (i.e. type = "l" of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
timeseries <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)

names(timeseries)<-c("interval","mean")
 
plot(timeseries,
main="Plot 2: Time series plot of the average number of steps taken",
xlab="Interval", xlim=c(0,2500),
ylab="Average steps ",
type="l",
col="black")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

## **QUESTION 5: The 5-minute interval that, on average, contains the maximum number of steps**

 
 ```r
 maxnumberofsteps<- which.max(timeseries$mean)
 intervalmax <- timeseries[maxnumberofsteps,]
 print(intervalmax)
 ```
 
 ```
 ##     interval     mean
 ## 104      835 206.1698
 ```
 
## **QUESTION 6 : Code to describe and show a strategy for imputing missing data**
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 

```r
 missing_data <- which(is.na(activity$steps))
```
Total missing values is equal to: 


```r
length(missing_data)
```

```
## [1] 2304
```
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
imputedata<-activity

imputedata$steps <- impute(activity$steps, fun=mean)
```
Total number of steps taken each day after missing values are imputed

```r
totalimpute <- with(imputedata, tapply(imputedata$steps, as.factor(imputedata$date), sum, na.rm = T))
```
## **QUESTION 7 : Histogram of the total number of steps taken each day after missing values are imputed** 

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
hist(totalimpute,
     main="Plot 3: Histogram of the total number of steps taken each day (imputed)",
     xlab="Total number of steps",
     ylim=c(0,25),breaks=10,
     col="grey",
     freq=TRUE)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)


Do these values differ from the estimates from the first part of the assignment ? 
What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
summary(totalimpute)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

## **QUESTION 8 : Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends**
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
imputedata['type_of_day'] <- weekdays(as.Date(imputedata$date))
imputedata$type_of_day[imputedata$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
imputedata$type_of_day[imputedata$type_of_day != "weekend"] <- "weekday"
# Convert type_of_day from character to factor
imputedata$type_of_day <- as.factor(imputedata$type_of_day)
# Calculate average steps by interval across all days
finaldata <- aggregate(steps ~ interval + type_of_day, imputedata, mean)
```
Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.




```r
qplot(interval, 
      steps, 
      data = finaldata, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "Plot 4") +
        facet_wrap(~ type_of_day, ncol = 1)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)
