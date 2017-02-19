Reproducible Research Assignment
================

Reading and intial processing of data
-------------------------------------

The first step is to ensure that the folder containing the data is set as the working directory. This can be done using the "setwd()" function.

Once this has been completed, the data can be read in as follows:

``` r
activitydata<-read.csv("activity.csv", header = TRUE, stringsAsFactors = FALSE)
```

The data was then created into a set in which fromatted the date colum correctly.

``` r
data<-activitydata
data$date<-as.Date(data$date, format = "%Y-%m-%d")
```

Calculation of daily statistics
-------------------------------

The data was then aggregated by day to find the mean number of steps taken on each day. From this, a histogram could be produced.

``` r
#histogram of total steps taken each day
stepdata<-subset(data, select=c("date", "steps"))
stepdata$steps<-as.numeric(stepdata$steps)
dailydata<-aggregate(.~date, stepdata, FUN=sum, na.action=na.omit)
colnames(dailydata)<-c("date", "dailysteps")
hist(dailydata$dailysteps, breaks=10, main="Histogram of total daily steps", xlab="Number of steps in day")
```

![](images/histogram%20of%20daily%20means-1.png)

From the daily data, the mean and median steps per day could also be calculated.

``` r
mean(dailydata$dailysteps, na.rm=TRUE)
```

    ## [1] 10766.19

``` r
median(dailydata$dailysteps, na.rm=TRUE)
```

    ## [1] 10765

Average daily activity pattern
------------------------------

To visualise the daily activity pattern, a time series plot showing the average number of steps taken per 5 minute interval was produced as follows.

``` r
intervaldata<-subset(data, select=c("interval", "steps"))
intervaldata<-aggregate(.~interval, intervaldata, FUN=mean, na.action = na.omit)
colnames(intervaldata)<-c("interval", "meansteps")
plot(intervaldata$interval, intervaldata$meansteps, type="line", ylab="Average steps in interval", xlab="Time Interval", main="Timeseries of average steps taken", col="green")
```

![](images/timeseries%20plot-1.png)

Using this data, the 5 minute interval with the highest steps taken per day on average could be identified.

``` r
maxavg<-max(intervaldata$meansteps)
maxinterval<-subset(intervaldata, meansteps==maxavg)
maxinterval<-maxinterval$interval
maxavg
```

    ## [1] 206.1698

``` r
maxinterval
```

    ## [1] 835

So we see that the maximum average steps is 206 and this occurs at interval 835.

Missing values
--------------

Missing values are common in real-life data sets and can have a big impact on any inferences made. As a result, it is important to identify how many there are.

``` r
sum(is.na(data$steps))
```

    ## [1] 2304

Here we see that there are 2304 missing values. To remove any bias from these, they were imputed using the mean value for that time interval as shown below.

``` r
imputed<-data
imputedframe<-merge(imputed, intervaldata, by.x="interval", by.y="interval", all.x=TRUE)
imputedframe$steps <- ifelse(test = is.na(imputedframe$steps), yes = imputedframe$meansteps, no = imputedframe$steps)
```

From the imputed data, a new histogram of daily steps could be created.

``` r
newstepdata<-subset(imputedframe, select=c("date", "steps"))
newstepdata$steps<-as.numeric(newstepdata$steps)
newhistdata<-aggregate(.~date, newstepdata, FUN=sum, na.action=na.omit)
hist(newhistdata$steps, breaks=10, main="Histogram of imputed total daily steps", xlab="Number of steps in day")
```

![](images/new%20histogram-1.png)

The impact of imputing data has increased the frequency of of obeservations as expected.

Calculating the new daily mean and median we see that the mean does not change from before but the median does.

``` r
newstepdata<-aggregate(.~date, newstepdata, FUN=sum, na.action=na.omit)
mean(newstepdata$steps, na.rm=TRUE)
```

    ## [1] 10766.19

``` r
median(newstepdata$steps, na.rm=TRUE)
```

    ## [1] 10766.19

Comparison of weekend and weekday behaviour
-------------------------------------------

To make the comparison of weekdays and weekends, a factor of weekend or weekday was created using tho following code.

``` r
library(timeDate)
imputedframe$daytype <- ifelse(test = isWeekday(imputedframe$date, wday=1:5), yes = "weekday", no = "weekend")
imputedframe$daytype<-as.factor(imputedframe$daytype)
```

To see the comparison visually, we create the following plot:

``` r
library(lattice)
xyplot(imputedframe$meansteps ~ imputedframe$interval|imputedframe$daytype, data=imputedframe, type="l", xlab="Interval", ylab="Mean Steps")
```

![](images/comparison%20plot-1.png)

We see that there are only minor differences between the two types of days.
