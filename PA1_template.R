#libraries====
library(dplyr)
library(lattice)
#Unzip files====
unzip("activity.zip")
data <- read.csv("activity.csv")

#transforming the data =====

data$date <- as.Date(data$date)

### What is mean total number of steps taken per day?====
data1 <- data %>%
        group_by(date) %>%
        summarize(steps=sum(steps, na.rm=T))
hist(data1$steps, breaks = 20)
mean <- mean(data1$steps)
media<- median(data1$steps)

## What is the average daily activity pattern?=====
data2 <- data %>% 
        group_by(interval) %>% 
        summarize(steps=sum(steps, na.rm=T))
plot(data2$interval, data2$steps, type="l")
max <- data2$interval[which.max(data2$steps)]

## Imputing missing values=====
sum_NA <- sum(is.na(data)) 
data_without_na <- data
data_without_na$steps[which(is.na(data$steps))] <- mean(data$steps, na.rm=T)

data1 <- data_without_na %>%
        group_by(date) %>%
        summarize(steps=sum(steps, na.rm=T))
hist(data1$steps)
mean <- mean(data1$steps)
median <- median(data1$steps)

## Are there differences in activity patterns between weekdays and weekends?
week <- weekdays(data_without_na$date)
for(i in 1:length(week)){
ifelse(week[i] %in% c("субота", "неділя"), week[i] <- "weekend", week[i] <- "weekday")
}
week <- factor(week)
data_without_na$week <- week
data1 <- data_without_na %>%
        group_by(interval, week) %>%
        summarize(steps=sum(steps, na.rm=T))
xyplot(steps ~ interval|week, data=data1, type="b")









