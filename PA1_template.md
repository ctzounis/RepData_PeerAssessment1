---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

Loading and preprocessing the data


```r
activity <- read.csv("activity.csv")
activity1<-na.omit(activity)
```

What is mean total number of steps taken per day?

```r
df<-aggregate(activity1$steps, by=list(date=activity1$date), FUN=sum)
colnames(df)[2] <- "tot.steps.per.day"
hist(df$tot.steps.per.day, col="red", xlab="total number of steps taken per day", main="Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

Median of the total number of steps taken per day


```r
median(df[["tot.steps.per.day"]])
```

```
## [1] 10765
```

Mean of the total number of steps taken per day


```r
mean(df[["tot.steps.per.day"]])
```

```
## [1] 10766.19
```

What is the average daily activity pattern?


```r
df1<-aggregate(activity1$steps, by=list(interval=activity1$interval), FUN=mean)
colnames(df1)[2] <- "ave.steps.per.interval"
interval<-df1$interval
ave.steps<-df1$ave.steps.per.interval
plot(interval,ave.steps, type="l", main = "Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
subset(df1, df1$ave.steps.per.interval==max(df1$ave.steps.per.interval))
```

```
##     interval ave.steps.per.interval
## 104      835               206.1698
```

Imputing missing values. So we replace the missing values with column mean


```r
sum(is.na(activity))
```

```
## [1] 2304
```

```r
df2<-activity
for(i in 1:ncol(df2)){
    df2[is.na(df2[,i]), i] <- mean(df2[,i], na.rm = TRUE)
}
```

```
## Warning in mean.default(df2[, i], na.rm = TRUE): argument is not numeric or
## logical: returning NA
```

```r
df3<-aggregate(df2$steps, by=list(date=df2$date), FUN=sum)
colnames(df3)[2] <- "tot.steps.per.day"
hist(df3$tot.steps.per.day, col="red", xlab="total number of steps taken per day", main="Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
median(df3[["tot.steps.per.day"]])
```

```
## [1] 10766.19
```

```r
mean(df3[["tot.steps.per.day"]])
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment? 

**The median value is different. However, the mean is the same**

What is the impact of imputing missing data on the estimates of the total daily number of steps?

**The total number of steps taken per day for values between 10000 to 15000 have much higher frequency.**

Are there differences in activity patterns between weekdays and weekends?


```r
df2$date <- as.Date(df2$date)
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
df2$wDay <- factor((weekdays(df2$date) %in% weekdays1), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(ggplot2)
df2_by_date <- aggregate(steps~interval + wDay, df2, mean, na.rm = TRUE)
plot<- ggplot(df2_by_date, aes(x = interval , y = steps, color = wDay)) +
    geom_line() +
    labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
    facet_wrap(~wDay, ncol = 1, nrow=2)
print(plot)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

