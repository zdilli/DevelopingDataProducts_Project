library(shiny)

# Read in and prepare the data.  This code will be run only once.
actdata <- read.csv("data/activity.csv")

# Imputing missing data: 
intlist <- unique(actdata$interval)
intmeans <- sapply(intlist,function(int) 
    mean(actdata$steps[actdata$interval==int],na.rm=TRUE))
actdata$daynumber <- as.numeric(actdata$date)
daylist <- unique(actdata$daynumber)
NAsperday <- sapply(daylist,function(day) 
    sum(is.na(actdata$steps[actdata$daynumber==day])))
actdata_filled <- actdata
for (thisday in daylist){
    if (NAsperday[thisday]!=0){
        actdata_filled$steps[actdata$daynumber==thisday] <- intmeans
    }
}

# Marking the days with the day names
actdata_filled$date <- as.Date(actdata_filled$date)
actdata_filled$DayofWeek <- weekdays(actdata_filled$date)
actdata_filled$daynumber <- actdata$daynumber

# First fill with "weekday", then correct Saturdays and Sundays
dayclass <- rep.int("weekday",length(actdata_filled$daynumber))
dayclass[actdata_filled$DayofWeek=="Saturday"] <- "weekend"
dayclass[actdata_filled$DayofWeek=="Sunday"] <- "weekend"
actdata_filled$dayclass <- as.factor(dayclass)

actdata_weekdays <- subset(actdata_filled,dayclass=="weekday")
actdata_weekends <- subset(actdata_filled,dayclass=="weekend")
actdata_mondays <- subset(actdata_filled,DayofWeek=="Monday")
actdata_tuesdays <- subset(actdata_filled,DayofWeek=="Tuesday")
actdata_wednesdays <- subset(actdata_filled,DayofWeek=="Wednesday")
actdata_thursdays <- subset(actdata_filled,DayofWeek=="Thursday")
actdata_fridays <- subset(actdata_filled,DayofWeek=="Friday")
actdata_saturdays <- subset(actdata_filled,DayofWeek=="Saturday")
actdata_sundays <- subset(actdata_filled,DayofWeek=="Sunday")

intmeans_weekdays <- sapply(intlist,function(int) 
    mean(actdata_weekdays$steps[actdata$interval==int],na.rm=TRUE))
intmeans_weekends <- sapply(intlist,function(int) 
    mean(actdata_weekends$steps[actdata$interval==int],na.rm=TRUE))
intmeans_mondays <- sapply(intlist,function(int) 
    mean(actdata_mondays$steps[actdata$interval==int],na.rm=TRUE))
intmeans_tuesdays <- sapply(intlist,function(int) 
    mean(actdata_tuesdays$steps[actdata$interval==int],na.rm=TRUE))
intmeans_wednesdays <- sapply(intlist,function(int) 
    mean(actdata_wednesdays$steps[actdata$interval==int],na.rm=TRUE))
intmeans_thursdays <- sapply(intlist,function(int) 
    mean(actdata_thursdays$steps[actdata$interval==int],na.rm=TRUE))
intmeans_fridays <- sapply(intlist,function(int) 
    mean(actdata_fridays$steps[actdata$interval==int],na.rm=TRUE))
intmeans_saturdays <- sapply(intlist,function(int) 
    mean(actdata_saturdays$steps[actdata$interval==int],na.rm=TRUE))
intmeans_sundays <- sapply(intlist,function(int) 
    mean(actdata_sundays$steps[actdata$interval==int],na.rm=TRUE))

sum_mondays <- sum(intmeans_mondays)
sum_tuesdays <- sum(intmeans_tuesdays)
sum_wednesdays <- sum(intmeans_wednesdays)
sum_thursdays <- sum(intmeans_thursdays)
sum_fridays <- sum(intmeans_fridays)
sum_saturdays <- sum(intmeans_saturdays)
sum_sundays <- sum(intmeans_sundays)
sum_weekdays <- sum(intmeans_weekdays)
sum_weekends <- sum(intmeans_weekends)

shinyServer(function(input, output) {

    #output$checktext = renderText({paste("Name is",input$dayname)})
    
    output$myplot1 = renderPlot({
        mytest <- input$dayname
        muin <- input$meanguess
        sumin <- input$sumguess    
        
        if(mytest=="mon") {
            plot(intlist,intmeans_mondays,type="l",col="blue",lwd=2,
                 main="Mondays, Average",ylab="No. of steps",
                 xlab="Interval of day")
            abline(muin,0,col="red",lwd=4)
        } else if(mytest=="tue") {
            plot(intlist,intmeans_tuesdays,type="l",col="blue",lwd=2,
                 main="Tuesdays, Average",ylab="No. of steps",
                 xlab="Interval of day")
            lines(c(0,max(intlist)),c(muin,muin),col="red",lwd=4)
        } else if (mytest=="wed") {
            plot(intlist,intmeans_wednesdays,type="l",col="blue",lwd=2,
                 main="Wednesdays, Average",ylab="No. of steps",
                 xlab="Interval of day")
            lines(c(0,max(intlist)),c(muin,muin),col="red",lwd=4)
        } else if (mytest=="thu") {
            plot(intlist,intmeans_thursdays,type="l",col="blue",lwd=2,
                 main="Thursdays, Average",ylab="No. of steps",
                 xlab="Interval of day")
            lines(c(0,max(intlist)),c(muin,muin),col="red",lwd=4)
        } else if (mytest=="fri") {
            plot(intlist,intmeans_fridays,type="l",col="blue",lwd=2,
                 main="Fridays, Average",ylab="No. of steps",
                 xlab="Interval of day")
            lines(c(0,max(intlist)),c(muin,muin),col="red",lwd=4)
        } else if (mytest=="sat") {
            plot(intlist,intmeans_saturdays,type="l",col="blue",lwd=2,
                 main="Saturdays, Average",ylab="No. of steps",
                 xlab="Interval of day")
            lines(c(0,max(intlist)),c(muin,muin),col="red",lwd=4)
        } else if (mytest=="sun") {
            plot(intlist,intmeans_sundays,type="l",col="blue",lwd=2,
                 main="Sundays, Average",ylab="No. of steps",
                 xlab="Interval of day")
            lines(c(0,max(intlist)),c(muin,muin),col="red",lwd=4)
        } else if (mytest=="wkday") {
            plot(intlist,intmeans_weekdays,type="l",col="blue",lwd=2,
                 main="Weekdays, Average",ylab="No. of steps",
                 xlab="Interval of day")
            lines(c(0,max(intlist)),c(muin,muin),col="red",lwd=4)
        } else if (mytest=="wkend") {
            plot(intlist,intmeans_weekends,type="l",col="blue",lwd=2,
                 main="Weekends, Average",ylab="No. of steps",
                 xlab="Interval of day")
            lines(c(0,max(intlist)),c(muin,muin),col="red",lwd=4)
        }   
    })
    
    output$myplot2 = renderPlot({
        mytest <- input$dayname
        muin <- input$meanguess
        sumin <- input$sumguess    
        
        if(mytest=="mon") {
            hist(intmeans_mondays,main="Mondays, Average",
                 xlab="Total number of steps per interval",ylab="Number of intervals",
                 breaks=20,col="darkblue")
            lines(c(muin, muin), c(0, 200),col="red",lwd=4)
            mse <- mean((intmeans_mondays - muin)^2)
            text(150, 80, paste("Guessed mean = ", muin))
            text(150, 65, paste("MSE = ", round(mse, 2)))
        } else if(mytest=="tue") {
            hist(intmeans_mondays,main="Tuesdays, Average",
                 xlab="Total number of steps per interval",ylab="Number of intervals",
                 breaks=20,col="darkblue")
            lines(c(muin, muin), c(0, 200),col="red",lwd=4)
            mse <- mean((intmeans_tuesdays - muin)^2)
            text(150, 80, paste("Guessed mean = ", muin))
            text(150, 65, paste("MSE = ", round(mse, 2)))
        } else if (mytest=="wed") {
            hist(intmeans_mondays,main="Wednesdays, Average",
                 xlab="Total number of steps per interval",ylab="Number of intervals",
                 breaks=20,col="darkblue")
            lines(c(muin, muin), c(0, 200),col="red",lwd=4)
            mse <- mean((intmeans_wednesdays - muin)^2)
            text(150, 80, paste("Guessed mean = ", muin))
            text(150, 65, paste("MSE = ", round(mse, 2)))
        } else if (mytest=="thu") {
            hist(intmeans_mondays,main="Thursdays, Average",
                 xlab="Total number of steps per interval",ylab="Number of intervals",
                 breaks=20,col="darkblue")
            lines(c(muin, muin), c(0, 200),col="red",lwd=4)
            mse <- mean((intmeans_thursdays - muin)^2)
            text(150, 80, paste("Guessed mean = ", muin))
            text(150, 65, paste("MSE = ", round(mse, 2)))
        } else if (mytest=="fri") {
            hist(intmeans_fridays,main="Fridays, Average",
                 xlab="Total number of steps per interval",ylab="Number of intervals",
                 breaks=20,col="darkblue")
            lines(c(muin, muin), c(0, 200),col="red",lwd=4)
            mse <- mean((intmeans_fridays - muin)^2)
            text(150, 80, paste("Guessed mean = ", muin))
            text(150, 65, paste("MSE = ", round(mse, 2)))
        } else if (mytest=="sat") {
            hist(intmeans_fridays,main="Saturdays, Average",
                 xlab="Total number of steps per interval",ylab="Number of intervals",
                 breaks=20,col="darkblue")
            lines(c(muin, muin), c(0, 200),col="red",lwd=4)
            mse <- mean((intmeans_saturdays - muin)^2)
            text(150, 80, paste("Guessed mean = ", muin))
            text(150, 65, paste("MSE = ", round(mse, 2)))
        } else if (mytest=="sun") {
            hist(intmeans_sundays,main="Sundays, Average",
                 xlab="Total number of steps per interval",ylab="Number of intervals",
                 breaks=20,col="darkblue")    
            lines(c(muin, muin), c(0, 200),col="red",lwd=4)
            mse <- mean((intmeans_sundays - muin)^2)
            text(150, 80, paste("Guessed mean = ", muin))
            text(150, 65, paste("MSE = ", round(mse, 2)))
        } else if (mytest=="wkday") {
            hist(intmeans_weekdays,main="Weekdays, Average",
                 xlab="Total number of steps per interval",ylab="Number of intervals",
                 breaks=20,col="darkblue")    
            lines(c(muin, muin), c(0, 200),col="red",lwd=4)
            mse <- mean((intmeans_weekdays - muin)^2)
            text(150, 80, paste("Guessed mean = ", muin))
            text(150, 65, paste("MSE = ", round(mse, 2)))
        } else if (mytest=="wkend") {
            hist(intmeans_weekends,main="Weekends, Average",
                 xlab="Total number of steps per interval",ylab="Number of intervals",
                 breaks=20,col="darkblue")    
            lines(c(muin, muin), c(0, 200),col="red",lwd=4)
            mse <- mean((intmeans_weekends - muin)^2)
            text(150, 80, paste("Guessed mean = ", muin))
            text(150, 65, paste("MSE = ", round(mse, 2)))
        }   
    })
    
    output$text1 = renderText({ 
        mytest <- input$dayname
        sumin <- input$sumguess    
        
        if (mytest=="mon"){
            sumratio <- sumin / sum_mondays
            paste("You guessed", as.character(sumin),"steps; the ratio to the real sum is", as.character(format(sumratio,digits=5))," .") 
        }
        else if (mytest=="tue"){
            sumratio <- sumin / sum_tuesdays
            paste("You guessed", as.character(sumin),"steps; the ratio to the real sum is", as.character(format(sumratio,digits=5))," .")
        }
        else if (mytest=="wed"){
            sumratio <- sumin / sum_wednesdays
            paste("You guessed", as.character(sumin),"steps; the ratio to the real sum is", as.character(format(sumratio,digits=5))," .") 
        }
        else if (mytest=="thu"){
            sumratio <- sumin / sum_thursdays
            paste("You guessed", as.character(sumin),"steps; the ratio to the real sum is", as.character(format(sumratio,digits=5))," .") 
        }
        else if (mytest=="fri"){
            sumratio <- sumin / sum_fridays
            paste("You guessed", as.character(sumin),"steps; the ratio to the real sum is", as.character(format(sumratio,digits=5))," .") 
        }
        else if (mytest=="sat"){
            sumratio <- sumin / sum_saturdays
            paste("You guessed", as.character(sumin),"steps; the ratio to the real sum is", as.character(format(sumratio,digits=5))," .") 
        }
        else if (mytest=="sun"){
            sumratio <- sumin / sum_sundays
            paste("You guessed", as.character(sumin),"steps; the ratio to the real sum is", as.character(format(sumratio,digits=5))," .") 
        }
        else if (mytest=="wkday"){
            sumratio <- sumin / sum_weekdays
            paste("You guessed", as.character(sumin),"steps; the ratio to the real sum is", as.character(format(sumratio,digits=5))," .") 
        }
        else if (mytest=="wkend"){
            sumratio <- sumin / sum_weekends
            paste("You guessed", as.character(sumin),"steps; the ratio to the real sum is", as.character(format(sumratio,digits=5))," .") 
        }
    }) 
})
