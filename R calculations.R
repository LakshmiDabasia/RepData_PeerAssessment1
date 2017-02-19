setwd("C:/Users/Lakshmi/Documents/Course/repdata_data_activity")

#read and format data
activitydata<-read.csv("activity.csv", header = TRUE, stringsAsFactors = FALSE)
data<-activitydata
data$date<-as.Date(data$date, format = "%Y-%m-%d")

#histogram of total steps taken each day
stepdata<-subset(data, select=c("date", "steps"))
stepdata$steps<-as.numeric(stepdata$steps)
dailydata<-aggregate(.~date, stepdata, FUN=sum, na.action=na.omit)
colnames(dailydata)<-c("date", "dailysteps")
hist(dailydata$dailysteps, breaks=10, main="Histogram of total daily steps", xlab="Number of steps in day")

#mean and median number of steps each day
mean(dailydata$dailysteps, na.rm=TRUE)
median(dailydata$dailysteps, na.rm=TRUE)

#time series plot of mean steps taken per interval
intervaldata<-subset(data, select=c("interval", "steps"))
intervaldata<-aggregate(.~interval, intervaldata, FUN=mean, na.action = na.omit)
colnames(intervaldata)<-c("interval", "meansteps")
plot(intervaldata$interval, intervaldata$meansteps, type="line", ylab="Average steps in interval", xlab="Time Interval", main="Timeseries of average steps taken", col="green")

#identify the 5 minute interval contain the most steps on average
maxavg<-max(intervaldata$meansteps)
maxinterval<-subset(intervaldata, meansteps==maxavg)
maxinterval<-maxinterval$interval
maxavg
maxinterval

#identify number of missing values
sum(is.na(data$steps))
#impute missing values
imputed<-data
imputedframe<-merge(imputed, intervaldata, by.x="interval", by.y="interval", all.x=TRUE)
imputedframe$steps <- ifelse(test = is.na(imputedframe$steps), yes = imputedframe$meansteps, no = imputedframe$steps)
#histogram of total steps taken each day
newstepdata<-subset(imputedframe, select=c("date", "steps"))
newstepdata$steps<-as.numeric(newstepdata$steps)
newhistdata<-aggregate(.~date, newstepdata, FUN=sum, na.action=na.omit)
hist(newhistdata$steps, breaks=10, main="Histogram of total daily steps", xlab="Number of steps in day")
#mean and median number of steps each day
newstepdata<-aggregate(.~date, newstepdata, FUN=sum, na.action=na.omit)
mean(newstepdata$steps, na.rm=TRUE)
median(newstepdata$steps, na.rm=TRUE)

#compare weekdays and weekends
install.packages("timeDate")
library(timeDate)
isWeekday(imputedframe$date, wday=1:5)
imputedframe$daytype <- ifelse(test = isWeekday(imputedframe$date, wday=1:5), yes = "weekday", no = "weekend")
imputedframe$daytype<-as.factor(imputedframe$daytype)
library(lattice)
xyplot(imputedframe$meansteps ~ imputedframe$interval|imputedframe$daytype, data=imputedframe, type="l", xlab="Interval", ylab="Mean Steps")
