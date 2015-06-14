
f_path <- "./Reproducible_Research/activity.csv"

data1 <- read.csv(f_path)

sum1<-tapply(data1$steps,data1$date,sum)

hist(sum1,ylim=c(0,30),main="Histogram of total daily steps",
     xlab="Total daily steps", ylab="Total mumber of days",col="red")

mean(sum1,na.rm=TRUE)

median(sum1,na.rm=TRUE)

data1_mean<-tapply(data1$steps,data1$interval,mean,na.rm=TRUE)

plot(names(data1_mean),data1_mean,col="red", type="l",
     main="Average Daily Activity Pattern",
     xlab="5 minutes time interval",
     ylab="steps taken averaged accross all days")

max<-data1_mean[(which.max(data1_mean))]
as.integer(names(max)) # interval
max[[1]] # steps

sum(is.na(data1$steps))

data2 <- data1 # create a copy of data1

# create a data frame containing  the mean values
x <- data.frame(as.integer(names(data1_mean)),data1_mean) 
names(x)<-c("interval","steps")

p<-merge(data2,x,by="interval") # merge both table

p$date<-as.Date(p$date) # format date

m<-p[order(p$date),] # reorder the merged table to match the original layout

data2[which(is.na(data2$steps)),"steps"] <- 
        m[which(is.na(data2$steps)),"steps.y"] # reemplace mean values in NAs

data2_mean<-tapply(data2$steps,data2$date,sum)

hist(data2_mean,ylim=c(0,35),
     main="Histogram of total daily steps (after NA filling)",
     xlab="Total daily steps", ylab="Total mumber of days",col="green")

mean(data2_mean,na.rm=TRUE)

median(data2_mean,na.rm=TRUE)


data2$date<-as.Date(data2$date) # date format

Sys.setlocale("LC_TIME", "English") # this line sets date variables in English

# create a new column "weekdate" containing weekday and weekend tags

data2$weekdate<-weekdays(data2$date)

data2[data2$weekdate=="Saturday" | data2$weekdate=="Sunday","weekdate"]<-"weekend"

data2[data2$weekdate!="weekend","weekdate"]<-"weekday"

#par(mfrow=c(2,1))

tab<-aggregate(steps~weekdate+interval,data2,mean)

library(lattice)

xyplot(steps ~ interval | weekdate,
       layout=c(1, 2),main="Average Daily Activity Pattern",
       xlab="5 minutes time interval",ylab="steps taken averaged",
       type="l",data=tab,col="red")






