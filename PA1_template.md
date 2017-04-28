loading knitr package
=====================

require(knitr)

making sure that code and results are printed
=============================================

setting proper directory
========================

``` r
setwd("C:/Users/helmac1/Documents/Personal/Coursera/Class 5 Programming Assignment 1")

# Read in the data set for the assignment
activity <- read.csv(file="C:/Users/helmac1/Documents/Personal/Coursera/Class 5 Programming Assignment 1/activity.csv", header=TRUE, sep=",")
summary(activity)
```

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

``` r
raw.activity <-read.csv(file="C:/Users/helmac1/Documents/Personal/Coursera/Class 5 Programming Assignment 1/activity.csv", header=TRUE, sep=",") 
```

Transform the date column into a proper Date class
==================================================

``` r
activity <- transform(activity, date = as.Date(date))
str(activity)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

Remove NA values
================

``` r
activity <- activity[complete.cases(activity),]
```

Calculate Total Number of Steps per Day
=======================================

``` r
totalStepsPerDay <- aggregate(steps ~ date, activity, sum)
```

plot histogram of total number of steps per day
===============================================

``` r
hist(totalStepsPerDay$steps, breaks=20, xlab= "Total Number of Steps per day", ylab= "Frequency", main="Histogram of the total number of steps taken per day",col="blue", xlim=c(0,25000), ylim=c(0,12) )
```

![](PA1_template_files/figure-markdown_github/plot%20steps-1.png) \# calculate and print mean and median

``` r
steps.mean <- mean(totalStepsPerDay$steps)
steps.median <- quantile(totalStepsPerDay$steps, 0.5)

rbind("The Mean of the total number of steps per day", print(steps.mean))
```

    ## [1] 10766.19

    ##      [,1]                                           
    ## [1,] "The Mean of the total number of steps per day"
    ## [2,] "10766.1886792453"

``` r
rbind("The Median of the total number of steps per day", print(steps.median))
```

    ##   50% 
    ## 10765

    ##      50%                                              
    ## [1,] "The Median of the total number of steps per day"
    ## [2,] "10765"

plot daily activity patterns
============================

``` r
plot(activity$interval, activity$steps, type="l", col="blue", xlab="5 Minute Intervals", ylab="Average Number of Steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png) \#Which 5 minute interval contains the max number of steps across all days

``` r
averageStepsPerInterval <- aggregate(steps ~ interval, activity, mean)
rbind("The interval with the maximum number of steps", averageStepsPerInterval[which.max(averageStepsPerInterval$steps),1])
```

    ##      [,1]                                           
    ## [1,] "The interval with the maximum number of steps"
    ## [2,] "835"

Counting missing values
=======================

``` r
countNA <- length(raw.activity$steps)-length(activity$steps)
rbind("The number of rows with missing data is",countNA)
```

    ##         [,1]                                     
    ##         "The number of rows with missing data is"
    ## countNA "2304"

Substitute Missing Data with mean value of steps
================================================

``` r
mean(averageStepsPerInterval$steps)
```

    ## [1] 37.3826

``` r
impute.activity <- raw.activity
impute.activity[is.na(impute.activity)]<-mean(averageStepsPerInterval$steps)
```

Histogram of total number of daily steps
========================================

``` r
imptotalStepsPerDay <- aggregate(steps ~ date, impute.activity, sum)
hist(imptotalStepsPerDay$steps, breaks=20, xlab= "Total Number of Steps per day", ylab= "Frequency", main="Histogram of the total number of steps taken per day",col="blue", xlim=c(0,25000), ylim=c(0,20) )
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-7-1.png) \# Variance in activity between weekday and weekends

``` r
impute.activity <- transform(impute.activity, date = as.Date(date))
str(impute.activity)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : num  37.4 37.4 37.4 37.4 37.4 ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

``` r
impute.activity$days <- weekdays(impute.activity$date)
impute.activity$days[impute.activity$days == "Monday"] <- "Weekday"
impute.activity$days[impute.activity$days == "Tuesday"] <- "Weekday"    
impute.activity$days[impute.activity$days == "Wednesday"] <- "Weekday"
impute.activity$days[impute.activity$days == "Thursday"] <- "Weekday"    
impute.activity$days[impute.activity$days == "Friday"] <- "Weekday"
impute.activity$days[impute.activity$days == "Saturday"] <- "Weekend"    
impute.activity$days[impute.activity$days == "Sunday"] <- "Weekend"

weekdaymat <-subset(impute.activity, impute.activity$days == "Weekday")
weekendmat <- subset(impute.activity, impute.activity$days == "Weekend")

weekdayStepsPerInterval <- aggregate(steps ~ interval, weekdaymat, mean)
weekendStepsPerInterval <- aggregate(steps ~ interval, weekendmat, mean)

plot(weekdayStepsPerInterval$interval, weekdayStepsPerInterval$steps, type="l", col="blue", xlab="5 Minute Intervals", ylab="Average Number of Steps during the Weekdays", main = "Weekdays")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
plot(weekendStepsPerInterval$interval, weekendStepsPerInterval$steps, type="l", col="blue", xlab="5 Minute Intervals", ylab="Average Number of Steps during the Weekends", main = "Weekends")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-8-2.png)
