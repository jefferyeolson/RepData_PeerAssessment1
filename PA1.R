# Open libraries
library(tidyverse)
# Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())
# Process/transform the data (if necessary) into a format suitable for your analysis
unzip("activity.zip")
active <- read.csv("activity.csv")
active$date <- as.Date(active$date, format = "%Y-%m-%d")

# What is mean total number of steps taken per day? ignore the missing values.
## Calculate the total number of steps taken per day.
steps <- active %>%
        group_by(date) %>%
        summarize(mean_steps = mean(steps, na.rm = TRUE), 
                  total_steps = sum(steps, na.rm = TRUE),
                  median_steps = median(steps, na.rm = TRUE))
mean(steps$total_steps, na.rm = TRUE)
## Make a histogram of the total number of steps taken each day
steps_hist <- ggplot(steps, aes(total_steps))
steps_hist + geom_histogram(na.rm = TRUE) + 
        labs(title = "Histogram of Total Steps Per Day",
             subtitle = "https://github.com/rdpeng/RepData_PeerAssessment1",
             x = "Total Steps Per Day", 
             y = "Days") +
        scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10), labels = c(0, 2, 4, 6, 8, 10))
##Calculate and report the mean and median of the total number of steps taken per day
mean(steps$total_steps)
median(steps$total_steps)
# What is the average daily activity pattern?
## Make a time series plot (type = "l") of the 5-minute interval (x-axis)
### and the average number of steps taken, averaged across all days (y-axis)
intervals <- active %>%
        group_by(interval) %>%
        summarize(mean_steps = mean(steps, na.rm = TRUE))
plot_intervals <- ggplot(intervals, aes(interval, mean_steps))
plot_intervals + geom_line() + 
        labs(title = "Average Steps Per Five-Minute Interval Per Day",
             subtitle = "https://github.com/rdpeng/RepData_PeerAssessment1",
             x = "Minutes",
             y = "Steps Per Five Minutes")

## Which 5-minute interval, on average across all the days in the dataset, 
### contains the maximum number of steps?
max_steps <- max(intervals$mean_steps, na.rm = TRUE)
intervals$interval[intervals$mean_steps == max_steps]

# Imputing missing values
## Calculate and report the total number of missing values in the dataset 
### (i.e. the total number of rows with NAs)
sum(!complete.cases(active))
sum(is.na(active$steps))

## Devise a strategy for filling in all of the missing values in the dataset. 
### The strategy does not need to be sophisticated. 
### For example, you could use the mean/median for that day, 
### or the mean for that 5-minute interval, etc.
### Create a new dataset that is equal to the original dataset 
### but with the missing data filled in.
active_not_na <- active %>%
        select(steps, date, interval) %>%
        group_by(interval) %>%
        mutate(mean_steps = mean(steps, na.rm = TRUE))
active_not_na$steps[is.na(active_not_na$steps)] <- 
        active_not_na$mean_steps[is.na(active_not_na$steps)]

## Make a histogram of the total number of steps taken each day
steps_not_na <- active_not_na %>%
        group_by(date) %>%
        summarize(total_steps = sum(steps))
steps_not_na_hist <- ggplot(steps_not_na, aes(total_steps))
steps_not_na_hist + geom_histogram(na.rm = TRUE) + 
        labs(title = "Histogram of Total Steps Per Day, NA Imputed",
             subtitle = "https://github.com/rdpeng/RepData_PeerAssessment1",
             x = "Total Steps Per Day", 
             y = "Days") +
        scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10), labels = c(0, 2, 4, 6, 8, 10))
## Calculate and report the mean and median total number of steps taken per day. 
mean(steps_not_na$total_steps)
median(steps_not_na$total_steps)

## Do these values differ from the estimates from the first part of the assignment? 
mean(steps_not_na$total_steps) - mean(steps$total_steps, na.rm = TRUE)
median(steps_not_na$total_steps) - median(steps$total_steps, na.rm = TRUE)
## What is the impact of imputing missing data 
### on the estimates of the total daily number of steps?
### it increases the mean, median, etc. 

# Are there differences in activity patterns between weekdays and weekends?
## For this part the weekdays() function may be of some help here. 
## Use the dataset with the filled-in missing values for this part.
## Create a new factor variable in the dataset with two levels – “weekday” and “weekend”
weekday_index <- weekdays(active_not_na$date)
active_not_na$weekday[weekday_index %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")] <- "weekday"
active_not_na$weekday[weekday_index %in% c("Saturday", "Sunday")] <- "weekend"
active_not_na$weekday <- as.factor(active_not_na$weekday)
## indicating whether a given date is a weekday or weekend day.
## Make a panel plot containing a time series plot (i.e. type = "l") 
## of the 5-minute interval (x-axis) and the average number of steps taken,
## averaged across all weekday days or weekend days (y-axis). 
weekday_steps <- active_not_na %>%
        group_by(weekday, interval) %>%
        summarize(average_steps = mean(steps))
weekday_plot <- ggplot(weekday_steps, aes(interval, average_steps))
weekday_plot + geom_line() + 
        labs(title = "Comparison of Weekday and Weekend Steps Per Five-Minute Interval Per Day",
             subtitle = "https://github.com/rdpeng/RepData_PeerAssessment1",
             x = "Minutes",
             y = "Steps Per Five Minutes") +
        facet_grid(weekday~.)