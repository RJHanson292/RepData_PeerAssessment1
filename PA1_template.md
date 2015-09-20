---
output: html_document
---
#Peer Assesment 1

##Loading and Preprocessing the data


```r
unzip("activity.zip")
```

```r
activity <- read.csv("./activity.csv")
```

##What is the mean total number of steps taken per day?


```r
daily.steps <- tapply(activity$steps, activity$date, FUN = sum, na.rm = TRUE)
datestemp <- activity$date
datestemp2 <- levels(datestemp)
dates <- as.Date(datestemp2)
table <- cbind(dates, daily.steps)
plot(x = dates, y = daily.steps, type = "h")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean(daily.steps)
```

```
## [1] 9354.23
```

```r
median(daily.steps)
```

```
## [1] 10395
```

##What is the averaacge daily activity pattern?


```r
averageday <- aggregate.data.frame(x = list(steps = activity$steps), by = list(interval = activity$interval), FUN = mean, na.rm = TRUE)
plot(x = averageday$interval, y = averageday$steps, type = "l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
averageday[which.max(averageday$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

##Imputing Missing Values


```r
nummberofnastemp <- is.na(activity)
numberofnas <- sum(nummberofnastemp)
imput.missing.data <- function(steps, interval) {
  datapiece <- 0
  if (!is.na(steps)) {
      datapiece <- c(steps)
      }
  else {
      datapiece <- averageday[averageday$interval == interval, "steps"]
  }
}
edited.activity <- activity
edited.activity$steps <- mapply(imput.missing.data, edited.activity$steps, edited.activity$interval)

new.daily.steps <- tapply(edited.activity$steps, edited.activity$date, FUN = sum, na.rm = TRUE)
datestemp3 <- edited.activity$date
datestemp4 <- levels(datestemp3)
dates2 <- as.Date(datestemp4)
plot(x = dates2, y = new.daily.steps, type = "h")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
mean(new.daily.steps)
```

```
## [1] 10766.19
```

```r
median(new.daily.steps)
```

```
## [1] 10766.19
```
The values are higher as previously the NAs were set to 0 but now they are not. This leads to a higher mean and median as they are taking averages of a the same list but with all the smallest values significantly increased. 

##Are there differences in activity patterns between weekdays and weekends?


```r
weekdayfunction <- function(date) {  
if(weekdays(date) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) {
    return("Weekday")
  }
  else if(weekdays(date) %in% c("Saturday", "Sunday")) {
    return("Weekend")
 }
  else {
    print("error in finding weekday")
  }}
edited.activity$date <- as.Date(edited.activity$date)
edited.activity$weekday <- weekdays(edited.activity$date)
edited.activity$weekday2 <- sapply(edited.activity$date, FUN = weekdayfunction)

editedaverageday <- aggregate(steps ~ interval + weekday2, data = edited.activity, mean)

library(ggplot2)
graph <- ggplot(data = editedaverageday, aes(interval, steps))
graph <- graph + geom_line()
graph <- graph + facet_grid(weekday2~.)
graph
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
