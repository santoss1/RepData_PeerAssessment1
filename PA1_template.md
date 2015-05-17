
# Reproducible Research : Assignment1 #

=========================================

## Setting global parameters #

*Load the data (i.e. read.csv())*
```{r setoptions, echo=TRUE}
        library("knitr")
        library(plyr)
        opts_chunk$set(echo=TRUE, results="hide")
        setwd("~/Desktop/Coursera")
        dataCSV<-read.csv("activity.csv",sep=',',header=TRUE)
```

## Preprocessing the data #

****

*What is mean total number of steps taken per day?*

- Total number of steps taken per day
- Histogram of the total number of steps taken each day
- Mean and median of the total number of steps taken per day

```{r process1, results='asis'}
        StepsDay<-ddply(dataCSV,~date,summarise,nbsteps=sum(steps))
        knitr::kable(StepsDay)
        hist(StepsDay$nbsteps,
             main="Histogram of total number of steps taken each day",
             xlab="Nb steps per day")
        StepsDayMean<-ddply(dataCSV,~date,summarise,mean=mean(steps))
        StepsDayMedian<-ddply(dataCSV,~date,summarise,median=median(steps))
        knitr::kable(StepsDayMean)
        knitr::kable(StepsDayMedian)
```

****

*What is the average daily activity pattern?*

- Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r process2, results='asis'}
        StepsDayMean<-ddply(dataCSV,~interval,summarise,
             mean=mean(steps,na.rm = TRUE))
        plot(StepsDayMean$interval,StepsDayMean$mean,type="l",
             xlab="5 min interval",ylab="Average number of steps taken across all days",
             main="Time series of 5min interval and average nb of steps across all days")
```

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r process3, results='asis'}
        StepsDay<-ddply(dataCSV,~interval,summarise,
             mean=mean(steps,na.rm = TRUE))
        StepsDayMax=max(StepsDay$mean)
        MinInterval<-subset(StepsDay$interval,StepsDay$mean==StepsDayMax)
        knitr::kable(MinInterval)
```

****

*Inputting missing values*

- Total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r process4, results='asis'}
        NbNA<-sum(is.na(dataCSV[,1])==TRUE)
        knitr::kable(NbNA)
```

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
The strategy is t replace the missing values by the population mean.

- New dataset that is equal to the original dataset but with the missing data filled in.

```{r process5, results='asis'}
       opts_chunk$set(echo=TRUE, results="hide")
       dataCSV_2<-dataCSV
       for (i in dataCSV$interval){
            if (is.na(dataCSV$steps==TRUE)){dataCSV_2$steps<-mean(dataCSV$steps,na.rm=T)}
      }
```

- Histogram of the total number of steps taken each day 

```{r process6, results='asis'}
      StepsDay2<-rowsum(dataCSV$steps,dataCSV$date,na.rm=TRUE)
      hist(StepsDay2,main="Histogram of the total number of steps taken each day",
           xlab="number of steps taken each day")
```

- Mean and median total number of steps taken per day. 
```{r process7, results='asis'}
      StepsDayMean2<-ddply(dataCSV,~date,summarise,
             mean=mean(steps,na.rm = TRUE), median=median(steps,na.rm=TRUE))
      knitr::kable(StepsDayMean2)
```
  
- Do these values differ from the estimates from the first part of the assignment? 
Yes

- What is the impact of inputing missing data on the estimates of the total daily number of steps?
Inputting missing data distorts the total dailynumber of steps towards the average input.

****

*Are there differences in activity patterns between weekdays and weekends?*

- New factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r process8, results='asis'}
        opts_chunk$set(echo=TRUE)
        require(dplyr)
        dataCSV$weekdays<-weekdays(as.Date(dataCSV$date))
        map <- data.frame(weekdays=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
                          daystype=c(rep("weekday",5),rep("weekend",2)))
        map$weekdays<-as.character(map$weekdays)
        dataCSV<-left_join(dataCSV, map, by="weekdays")
```

- Panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```{r process9, results='asis'}
        StepsWeekEnd<-subset(dataCSV,daystype=="weekend")
        StepsWeekDays<-subset(dataCSV,daystype=="weekday")
        par(mfrow=c(1,2),mar=c(2,5,7,2))
        plot(StepsWeekDays$interval,StepsWeekDays$mean,type="l", xlab="5min interval",
             ylab="mean steps across week days",
             main="Plot Week Days - Average steps")
        plot(StepsWeekEnd$interval,StepsWeekEnd$mean,type="l", xlab="5min interval",
             ylab="mean steps across weekend days",
             main="Plot Week End - Average steps")
```

```{r, include=FALSE}
   # add this chunk to end of mycode.rmd
   file.rename(from="PA1_template.rmd", 
               to="PA1_template.md")