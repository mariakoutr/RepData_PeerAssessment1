course_pr1<-function()
        
## download and unzip the file 
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = "activity.zip")
unzip("activity.zip")

## read the csv file
activity<- read.csv("activity.csv")

## make date var in date format
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")

## claculate the sum of steps each day and make an histogram
## of the sum of steps each day 
steps_sum<- aggregate(steps ~ date, data = activity, sum)
png("hist1.png")
hist(steps_sum$steps, 
     main = "Total Steps per Day Histogram", 
     xlab = "Total steps per day", col = "red")
dev.off()

## average steps per interval of the day & plot
steps_Int <- aggregate(steps ~ interval, data = activity, mean)
png("plot1.png")
plot(steps_Int$interval, steps_Int$steps, 
     main = "Average number of steps of interval of day",
     xlab = "time", ylab = "Average steps", type = "l")
dev.off()
maxInt <- steps_Int[which.max(steps_Int$steps),1]

## sum of missing values
sumNA<- sum(is.na(activity$steps)) 

## Fill missing values with the avearage of the specific interval 
## that contains the missing value. 
activity_imp <- activity
for(i in 1:length(activity_imp$steps)){
  if(is.na(activity_imp$steps[i])){
    activity_imp$steps[i] <- steps_Int[match(activity_imp$interval[i],steps_Int$interval),2]
  }}

## Create a histogram of the total steps per 
## day from the imputed data
activity_imp$date<- as.Date(activity_imp$date,
                            format = "%Y-%m-%d")
steps_sum2<-aggregate( steps ~ date, data = activity_imp,sum)
png("hist2.png")
hist(steps_sum2$steps, 
     main = "Total Steps per Day Histogram", 
     xlab = "Total steps per day", col = "red")
dev.off()
men2 <- mean(steps_sum2$steps)
med2 <- median(steps_sum2$steps)

## make an extra column with a factor var with levels 
## "weekday" and "weekend"

weekday <- rep(0,length(activity_imp$date))
for(i in 1:length(activity_imp$date)){
  if(weekdays(activity_imp$date[i]) == "Saturday" | 
     weekdays(activity_imp$date[i]) == "Sunday"){
        weekday[i] <- "weekend"
  }else{
     weekday[i]<- "weekday"}
  
}
activity_imp <- cbind(activity_imp, weekday)
activity_imp$weekday <- as.factor(activity_imp$weekday)

## Create panel plot of the average number of steps per interval 
## averaged over weekdays and weekends 
library(lattice)
steps_Int2<- aggregate(steps ~ interval + weekday, 
                       data = activity_imp, mean)
png("panelplot.png")
p<- xyplot(steps_Int2$steps ~ steps_Int2$interval|steps_Int2$weekday,
           main = "Average name of steps", xlab = "Interval", 
           ylab = "Number of steps", layout = c(1,2), type = "l")
print(p)
dev.off()
