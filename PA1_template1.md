Personal Activity analyzer
================

Loading and preprocessing the data
==================================

``` r
setwd("C:\\Coursera\\DATA_SCIENCE\\Reproducible Research")
getwd()
```

    ## [1] "C:/Coursera/DATA_SCIENCE/Reproducible Research"

``` r
zipF1<- "C:\\Coursera\\DATA_SCIENCE\\Reproducible Research\\activity.zip"
ractivity <- read.csv("activity.csv", header = TRUE)
```

What is mean total number of steps taken per day?
=================================================

``` r
sums <- tapply(ractivity$steps, ractivity$date, sum)


hist(sums, breaks = 6, col = "green", xlab = "Steps", 
     main = "Total steps")
```

![](PA1_template1_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
#Mean value
mean(sums,na.rm = TRUE)
```

    ## [1] 10766.19

``` r
#Median value
median(sums, na.rm = TRUE)
```

    ## [1] 10765

What is the average daily activity pattern?
===========================================

Making a time series plot

``` r
means <- tapply(ractivity$steps, ractivity$interval, mean, na.rm = TRUE, simplify = T)

averages <- as.numeric(names(means))

plot(averages,means, type  = "l", col = "blue", xlab = "5-minute interval", ylab = "average steps across all the days")
```

![](PA1_template1_files/figure-markdown_github/unnamed-chunk-3-1.png)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
=============================================================================================================

``` r
maxval <- sort(means, decreasing = TRUE)
maxval[1]
```

    ##      835 
    ## 206.1698

``` r
#max number of step 206.17 on the 835th interval
```

Imputing missing values
=======================

Creating new data set without NA values

What is the average daily activity pattern? Make a time series plot

``` r
newdata <- ractivity

for(i in 1:ncol(newdata)){
  newdata[is.na(newdata[,i]),i] <- mean(newdata[,i], na.rm = TRUE)
}
```

    ## Warning in mean.default(newdata[, i], na.rm = TRUE): argument is not
    ## numeric or logical: returning NA

Number of missing values between old data set and new
=====================================================

``` r
sum(is.na(ractivity))
```

    ## [1] 2304

``` r
sum(is.na(newdata))
```

    ## [1] 0

``` r
head(ractivity)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

``` r
head(newdata)
```

    ##     steps       date interval
    ## 1 37.3826 2012-10-01        0
    ## 2 37.3826 2012-10-01        5
    ## 3 37.3826 2012-10-01       10
    ## 4 37.3826 2012-10-01       15
    ## 5 37.3826 2012-10-01       20
    ## 6 37.3826 2012-10-01       25

``` r
newsum <- tapply(newdata$steps,newdata$date, sum)
```

Comparing the sums for old and new data sets
============================================

``` r
hist(newsum)
```

![](PA1_template1_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
hist(sums)
```

![](PA1_template1_files/figure-markdown_github/unnamed-chunk-7-2.png)

``` r
newstat <- tapply(as.numeric(newdata$steps), newdata$date,sum)
hist(newstat, main = "Steps per day without missing values")
```

![](PA1_template1_files/figure-markdown_github/unnamed-chunk-7-3.png)

``` r
mean(newstat, na.rm = TRUE)
```

    ## [1] 10766.19

``` r
median(newstat, na.rm = TRUE)
```

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

Adding a column with days

``` r
newdata$Week_Day<- format(as.Date(newdata$date), format = "%A")
```

Weekday activity

``` r
weekday <- subset(newdata,newdata$Week_Day != c("Sunday","Saturday"))
head(weekday)
```

    ##     steps       date interval Week_Day
    ## 1 37.3826 2012-10-01        0   Monday
    ## 2 37.3826 2012-10-01        5   Monday
    ## 3 37.3826 2012-10-01       10   Monday
    ## 4 37.3826 2012-10-01       15   Monday
    ## 5 37.3826 2012-10-01       20   Monday
    ## 6 37.3826 2012-10-01       25   Monday

``` r
tail(weekday)
```

    ##         steps       date interval Week_Day
    ## 17563 37.3826 2012-11-30     2330   Friday
    ## 17564 37.3826 2012-11-30     2335   Friday
    ## 17565 37.3826 2012-11-30     2340   Friday
    ## 17566 37.3826 2012-11-30     2345   Friday
    ## 17567 37.3826 2012-11-30     2350   Friday
    ## 17568 37.3826 2012-11-30     2355   Friday

Weekend activity

``` r
weekend <- subset(newdata,newdata$Week_Day == c("Sunday","Saturday"))
head(weekend)
```

    ##      steps       date interval Week_Day
    ## 1442     0 2012-10-06        5 Saturday
    ## 1444     0 2012-10-06       15 Saturday
    ## 1446     0 2012-10-06       25 Saturday
    ## 1448     0 2012-10-06       35 Saturday
    ## 1450     0 2012-10-06       45 Saturday
    ## 1452     0 2012-10-06       55 Saturday

``` r
tail(weekend)
```

    ##       steps       date interval Week_Day
    ## 16117     0 2012-11-25     2300   Sunday
    ## 16119     0 2012-11-25     2310   Sunday
    ## 16121     0 2012-11-25     2320   Sunday
    ## 16123    17 2012-11-25     2330   Sunday
    ## 16125    94 2012-11-25     2340   Sunday
    ## 16127     0 2012-11-25     2350   Sunday

Weekday plot

``` r
meanweekday <- tapply(weekday$steps, weekday$interval, mean, na.rm = TRUE) 
meanweekday1 <- as.numeric(names(meanweekday))
plot (meanweekday1, meanweekday, type = "l", col = "red", main = "Weekday activity" )
```

![](PA1_template1_files/figure-markdown_github/unnamed-chunk-11-1.png) Weekend Plot

``` r
meanweekend <- tapply(weekend$steps, weekend$interval, mean, na.rm = TRUE) 
meanweekend1 <- as.numeric(names(meanweekend))
plot (meanweekend1, meanweekend, type = "l", col = "blue",main = "Weekend activity"  )
```

![](PA1_template1_files/figure-markdown_github/unnamed-chunk-12-1.png)
