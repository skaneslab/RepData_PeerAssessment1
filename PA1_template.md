---
title: "PA1_template"
author: "Ali CHAOUCHE"
date: "5/13/2020"
output: html_document
---


# **Reproducible Research Course**
# **Peer-graded Assignment Course Project 1**




```r
# Install R packages 
library(data.table)
library(dplyr)
library(knitr)
library(ggplot2)
library(ggthemes)
library(Hmisc)
```
## **QUESTION 1: Code for reading in the dataset and/or processing the data** 

```r
## Read data
activity <- read.csv("activity.csv", header=TRUE)
## Characterize & visualize data
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


```r
## Calculate the total number of steps taken per day
totalsteps <- with(activity, tapply(activity$steps, as.factor(activity$date), sum, na.rm = T))
# plot histogram
hist(totalsteps,
     main="Histogram of the total number of steps taken each day",
     xlab="Total number of steps",
     ylim=c(0,30),
     col="green",
     freq=TRUE)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)
## **QUESTION 3: Mean and median number of steps taken each day**

```r
# Question 3: Mean and median number of steps taken each day
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


```r
timeseries <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)

names(timeseries)<-c("interval","mean")
 
plot(timeseries,
main="Time series plot of the average number of steps taken",
xlab="Interval", xlim=c(0,2500),
ylab="Average steps ",
type="l",
col="pink")
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


```r
# Number of missing value
 missing_data <- which(is.na(activity$steps))
length(missing_data)
```

```
## [1] 2304
```

```r
imputedata<-activity

imputedata$steps <- impute(activity$steps, fun=mean)

#Total number of steps taken each day after missing values are imputed
totalimpute <- with(imputedata, tapply(imputedata$steps, as.factor(imputedata$date), sum, na.rm = T))
```
## **QUESTION 7 : Histogram of the total number of steps taken each day after missing values are imputed** 

```r
#plot histogram

hist(totalimpute,
     main="Histogram of the total number of steps taken each day after missing values are imputed",
     xlab="Total number of steps",
     ylim=c(0,35),
     col="red",
     freq=TRUE)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
#Mean and Median
summary(totalimpute)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```
## **QUESTION 8 : Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends**

```r
# identify type of day in the week
imputedata['type_of_day'] <- weekdays(as.Date(imputedata$date))
imputedata$type_of_day[imputedata$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
imputedata$type_of_day[imputedata$type_of_day != "weekend"] <- "weekday"
# Convert type_of_day from character to factor
imputedata$type_of_day <- as.factor(imputedata$type_of_day)
# Calculate average steps by interval across all days
finaldata <- aggregate(steps ~ interval + type_of_day, imputedata, mean)
#plot numberof steps ~ interval vs type_of_day
qplot(interval, 
      steps, 
      data = finaldata, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
        facet_wrap(~ type_of_day, ncol = 1)
```

```
## Warning: Ignoring unknown parameters: type
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)
