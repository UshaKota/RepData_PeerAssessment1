
      library(plyr)
      library(dplyr)
      library(reshape2)
      library(data.table)
      library(lattice)
      library(scales)
      library(tables)
      library(knitr)
      
      
      
      activity.data<-read.csv("activity//activity.csv",row.names=NULL,col.names=c("steps","on.date","interval"))
      
      #convert to data.table
      activity.data <-data.table(activity.data)
      
      freq.tbl<-activity.data[!is.na(steps), list(total.steps = sum(steps)), by = list(on.date)]
      hist(na.omit(freq.tbl$total.steps),breaks =20,main = "Distribution of total Steps/day",xlab = "Total Steps",col = "lightblue")
      
      print(paste("Mean :",mean(freq.tbl$total.steps,na.rm=T)))
      
      print(paste("Median :",median(freq.tbl$total.steps,na.rm=T)))
      
      
      ts.tbl <-activity.data[,list(mean.steps = mean(steps,na.rm=T)), by = list(interval)]
      format.hours<- function(x){
        format(as.POSIXct(x*60, origin="1970-01-01"), format="%H:%M")}
      ts.tbl <-ts.tbl[, list(mean.steps, interval = format.hours(interval))]
      
      plot.ts(ts.tbl$mean.steps, col = "blue", type="l",xaxt = "n",main = "Average Daily Activity pattern ", xlab="interval in minutes", ylab = "Average daily activity")
      axis(side =1, at = seq(0,300,5) )
      
      
      max.val<-ts.tbl[order(mean.steps, decreasing = T)]
      print(max.val[1:20,])
      
      count.vals<-activity.data[, list(miss.vals = length(which(is.na(steps))), comp.vals = length(which(!is.na(steps))))]
      print(count.vals[1,])
      barplot(t(matrix(count.vals)),col="coral", names.arg= names(count.vals), ylim = c(500,16000))
      
      #compute the mean of this data table for imputing mean to missing vals
      
     
      
      impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
      
      imputed.data <- ddply(activity.data, .(interval), transform, steps = impute.mean(steps))
      
      imputed.data<-data.table(imputed.data)
      new.freq.tbl<-imputed.data[, list(total.steps = sum(steps)), by = on.date]
      
      hist(new.freq.tbl$total.steps,breaks =20,main = "Distribution of total Steps/day",xlab = "Total Steps",col = "lightblue")
      
      print(paste("Mean :",mean(new.freq.tbl$total.steps)))
      
      print(paste("Median :",median(new.freq.tbl$total.steps)))
      
      wk.days.tbl<-imputed.data[, list(meansteps = mean(steps), weekdays = factor(weekdays(as.Date(on.date)) %in% c("Saturday","Sunday")+ 1L,levels=1:2, labels=c('weekend', 'weekday'))), by = list (on.date,interval)]
      p<-xyplot(meansteps ~ interval| weekdays,
                    data = wk.days.tbl,
                    type = "l", layout = c(1,2), col.line =  "red")
      print(p)