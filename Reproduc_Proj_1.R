activity<-read.csv("./activity.csv", header = TRUE)
library(dplyr)
library(lubridate)
activity$date=as.character(activity$date)
activity$date=as.Date(activity$date)
activity$day=yday(activity$date)
activity_day=group_by(activity, day)
Total_step_day <- summarise(activity_day, steps_day=sum(steps, na.rm = TRUE ))
library(ggplot2)
g=ggplot(Total_step_day, aes(day, steps_day))+geom_histogram(stat = "identity", fill="blue")
plot(g)
step_day_stat <- summarise(activity_day, mean_steps_day=mean(steps, na.rm = TRUE ), median_steps_day=median(steps, na.rm = TRUE ))

step_interval = group_by(activity, interval)
step_interval_mean= summarise(step_interval, mean=mean(steps, na.rm= T))
with(step_interval_mean, plot(interval, mean, type="l"))
which.max(step_interval_mean$mean)
step_interval_mean[104,]
sum(is.na(activity$steps))

total_mean=mean(activity$steps, na.rm =TRUE)
activity_mean=merge(activity, step_day_stat, by=intersect(names(activity), names(step_day_stat)))
activity_mean2=merge(activity, step_day_stat, by=intersect(names(activity), names(step_day_stat)))

for(i in 1:length(activity_mean$steps)){if(is.na(activity_mean[i,5])){activity_mean[i,5]=total_mean}}
for(i in 1:length(activity_mean$steps)){if(is.na(activity_mean[i,2])){activity_mean[i,2]=activity_mean[i,5]}}


g3=ggplot(activity_mean, aes(day, mean_steps_day))+geom_histogram(stat = "identity", fill="red")
plot(g3)

activity_mean_day=group_by(activity_mean, day)
summarize(activity_mean_day, mean=mean(steps), median=median(steps))


activity_mean$wday=weekdays(activity_mean$date)
activity_wday=group_by(activity_mean, wday)
activity_wday_mean=summarize(activity_wday, mean_wday=mean(steps, na.rm =TRUE))
g4=ggplot(activity_wday_mean, aes(wday, mean_wday))+geom_histogram(stat = "identity", fill="blue")
plot(g4)

#activity_wday$fday=0
#for(i in 1:length(activity_wday$steps)){if(activity_wday$wday[i]=="Saturday"|activity_wday$wday[i]=="Sunday"){activity_wday$fday[i]=1}}


#activity_wday$ffday="Weekday"
#for(i in 1:length(activity_wday$steps)){if(activity_wday$wday[i]=="Saturday"|activity_wday$wday[i]=="Sunday"){activity_wday$ffday[i]="Weekend"}}
#ggplot(activity_wday, aes(day, mean_steps_day, fday))+geom_line()+facet_grid(ffday~.)



activity_mean$ffday="Weekday"
for(i in 1:length(activity_mean$steps)){if(activity_mean$wday[i]=="Saturday"|activity_mean$wday[i]=="Sunday"){activity_mean$ffday[i]="Weekend"}}


activity_interval=group_by(activity_mean, ffday, interval)
activity_interval_mean=summarize(activity_interval, mean_interval=mean(steps, na.rm =TRUE))
ggplot(activity_interval_mean, aes(interval, mean_interval))+geom_line(col="blue")+facet_grid(ffday~.)

