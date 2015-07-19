Project1 <- function{
     # Loading and preprocessing the data
     # Show any code that is needed to
     # Load the data (i.e. read.csv())
     # Process/transform the data (if necessary) into a format suitable for your analysis
     
     # Load data from csv file keeping headers
     df <- read.csv("./activity.csv", header = TRUE, stringsAsFactors = FALSE)
     
     # Transform text date into Date type class
     df$date <- as.Date(df$date)
     
     # What is mean total number of steps taken per day?     
     # For this part of the assignment, you can ignore the missing values in the dataset.     
     # Calculate the total number of steps taken per day     
     # If you do not understand the difference between a histogram and a barplot, 
     # research the difference between them. 
     # Make a histogram of the total number of steps taken each day
     # Calculate and report the mean and median of the total number of steps taken per day
     
     # Total steps per day ommiting missing values
     tot.steps.day <- aggregate(steps  ~ date, data = df, FUN = "sum")
     png(file="plot1.png",width=640,height=480)
     hist(tot.steps.day$steps, main = "Total steps per day", xlab = "Steps", ylab = "Frequency")
     dev.off()
     # Mean total number of steps
     steps.daily.mean <- mean(tot.steps.day$steps)
     
     # Median total numbre of steps
     steps.daily.median <- median(tot.steps.day$steps)
     
     # What is the average daily activity pattern?
     # Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken,
     # averaged across all days (y-axis)
     # Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
     
     # Average steps per interval
     steps.interval.average <- aggregate(steps  ~ interval, data = df, FUN = "mean")
     
     # Plot intervals and average steps
     png(file="plot2.png",width=640,height=480)
     plot(steps.interval.average$steps,steps.interval.average$inteval, type = "l", xlab = "steps" , ylab = "interval")
     dev.off()
     # Maximun number of steps
     steps.interval.average[which.max( steps.interval.average[,2] ),]
     
     # Imputing missing values
     # Note that there are a number of days/intervals where there are missing values (coded as NA). 
     # The presence of missing days may introduce bias into some calculations or summaries of the data.
     # Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
     # Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
     # For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
     # Create a new dataset that is equal to the original dataset but with the missing data filled in.
     # Make a histogram of the total number of steps taken each day 
     # Calculate and report the mean and median 
     # total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment?
     # What is the impact of imputing missing data on the estimates of the total daily number of steps?
     
     # Missing values quantity
     length(which(is.na(df)))
     
     # Fill missing values with mean from average observations per interval
     df1 <- df
     df1$steps[is.na(df1$steps)] <- steps.interval.average$steps[match(df1$interval,steps.interval.average$interval)]
     
     # Convert to date
     df1$date <- as.Date(df$date)
     
     # Plot histogram by daily total steps
     tot.steps.day <- aggregate(steps  ~ date, data = df1, FUN = "sum")
     png(file="plot3.png",width=640,height=480)
     hist(tot.steps.day$steps, main = "Total steps per day (without NA)", xlab = "Steps", ylab = "Frequency")
     dev.off()
     
     # Mean from data with replaced missing values
     print(mean(tot.steps.day$steps))
     
     # Median from data with replaced missing values
     print(median(tot.steps.day$steps))
     
     # Are there differences in activity patterns between weekdays and weekends?
     # For this part the weekdays() function may be of some help here. Use the dataset with 
     # the filled-in missing values for this part.
     # Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
     # indicating whether a given date is a weekday or weekend day.
     # Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
     # and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
     # See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
     
     # Indentify weekdays and weekends
     weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
     df1$weekdays <- c('weekend', 'weekday')[(weekdays(df1$date) %in% weekdays1)+1L]
     
     # Observations during weekdays
     weekday <- subset (df1, weekdays == "weekday" )
     
     # Observations during weekends
     weekend <- subset (df1, weekdays == "weekend" )
     
     # Plot by intervals, average steps by weekday
     weekday <- aggregate(steps  ~ interval, data = weekday, FUN = "mean")
     weekend <- aggregate(steps  ~ interval, data = weekend, FUN = "mean")
     
     png(file="plot3.png",width=640,height=480)
     par(mfrow = c(2, 1), mar = c(0, 4, 2, 1))
     plot(weekend, type = "l", main = "weekend" ,col = "blue3")
     plot(weekday, type = "l", main = "weekday", col = "blue3")
     dev.off()
     
}

