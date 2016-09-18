# We'll use the packages "dplyr" and "ggplot2"
require(dplyr)
require(ggplot2)
require(reshape)

# Check if we have download the file, and extract it...
file <- "activity.csv"
if(!file.exists(file)){
     fileZip <- "activity.zip"
     if(!file.exists(fileZip)){
         temp <- tempfile()
          download.file("https://github.com/AMCampos/RepData_PeerAssessment1/blob/master/activity.zip", temp, mode="wb")
     }
     unzip(fileZip, file)
}

# Now we are going to load the data. It may take a bit of time, so I'm
# going to load only if the object "AMD" hasn't been loaded before:
if (!exists("AMD")){
     AMD <- read.csv(file,header = TRUE, sep=",", colClasses = c("numeric","Date","numeric"))     
}

# Taken a little view
head(AMD)
tail(AMD)
str(AMD)


## What is mean total number of steps taken per day?

# We calculate the total number of steps taken per day, and make the plot
AMDpd <- AMD %>% group_by(date) %>% summarise(stepsDaily=sum(steps, na.rm=TRUE))

png("")
g1 <- ggplot(data=AMDpd, aes(x=stepsDaily)) 
g1 <- g1 + geom_histogram(na.rm = TRUE, col=I("blue"), fill=I("blue"))
g1 <- g1 + labs(title="Total number of steps taken each day")
g1 <- g1 + labs(x="Steps per day", y="Frequency")
g1

# The mean and median total number of steps taken per day:
mean(AMDpd$stepsDaily, na.rm = TRUE)
median(AMDpd$stepsDaily, na.rm = TRUE)

# By intervals:

AMDpd2 <- AMD %>% group_by(date) %>% summarise(meanDay = mean(steps, na.rm=TRUE), medianDay = median(steps, na.rm=TRUE))


## What is the average daily activity pattern?
AMDin <- AMD %>% group_by(interval) %>% summarise(meanSteps=mean(steps,na.rm=TRUE))
qplot(interval, meanSteps, data=AMDin, geom = "line", col=I("blue"), main="Average daily activity pattern", xlab="Interval (minutes)", ylab="Average number of steps")

# The 5-minute interval that contains the maximum number of steps:
maxNumber <- AMDin[which.max(AMDin$meanSteps),]
maxNumber

## Imputing missing values
# Total number of missing values in the dataset
numNas <- AMD %>% filter(is.na(steps)==TRUE) %>% summarise(count=n())
numNas

#Filling in all of the missing values. We add a column with the
#average steps for every interval. And we'll fill the missing values
#with the average value for every interval across all the days
AMD2 <- inner_join(AMD,AMDin)
AMD2 <- AMD2 %>% mutate(steps=if_else(is.na(steps), meanSteps, steps))

#We create the new dataset (with the same structure than the original AMD)
AMD3 <- AMD2 %>% select(steps,date,interval)
head(AMD3)

#Generating the histogram: grouping by date and getting the total steps per day
AMD3pd <- AMD3 %>% group_by(date) %>% summarise(stepsDaily=sum(steps, na.rm=TRUE))

g2 <- ggplot(data=AMD3pd, aes(x=stepsDaily)) 
g2 <- g2 + geom_histogram(na.rm = TRUE, col=I("blue"), fill=I("blue"))
g2 <- g2 + labs(title="Total number of steps taken per day (missing values filled)")
g2 <- g2 + labs(x="Steps per day", y="Frequency")
g2

# The mean and median total number of steps taken per day:
mean(AMD3pd$stepsDaily, na.rm = TRUE)
median(AMD3pd$stepsDaily, na.rm = TRUE)

## Are ther differences in activity patterns between weekdays and weekends?

AMD3 <- AMD3 %>% mutate(typeofday = if_else(dia(date) %in% seq(1:5),"weekday","weekend"))
AMD3 <- AMD3 %>% mutate(typeofday = as.factor(typeofday))

# By intervals
AMD3pd <- AMD3 %>% group_by(date) %>% summarise(meanSteps=mean(steps,na.rm=TRUE), medianSteps=median(steps,na.rm=TRUE))
AMD3pd

# In order to get the plot with the average number of steps taken (averaged across
# all weekday days or weekend days):

AMD3in <- AMD3 %>% group_by(interval,typeofday) %>% summarise(meanSteps=mean(steps,na.rm=TRUE))
gr <- ggplot(AMD3in, aes(x=interval, y=meanSteps)) + geom_line()
gr <- gr + facet_grid(.~typeofday)

gr

# Another graphic: this one shows the differences between a weekday and a weekend

ex1 <- cast(AMD3in,interval ~ typeofday, value="meanSteps")
ex1 <- ex1 %>% mutate(dif=weekday-weekend)
p <- ggplot(ex1, aes(x=interval,y=dif)) + geom_line(colour="blue")
p <- p + labs(title="Differences in patterns (weekday and weekend)", x="Intervals (5-minutes)", y="Steps")
p







