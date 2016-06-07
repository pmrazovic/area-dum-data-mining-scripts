rm(list=ls())
setwd("/Users/pero/Development/area-dum-data-mining-scripts")

# ------- Parameters -------
startDatetime <- as.POSIXlt("2016-01-01 00:00:00", tz = "UTC") # Beginning of time interval under study
endDatetime <- as.POSIXlt("2016-02-24 00:00:00", tz = "UTC") 	 # End of time interval under study
sectionId <- 8513 							 # Deliver depot ID
timeSlotSize <- 30 							 # Size of time slot

# ------- Setting DB connection -------
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='localhost', port='5432', dbname='area_dum', user='area_dum', password='area_dum')

# ------- Retrieving data from DB  -------
queryResponse1 <- dbGetQuery(con, statement = paste('SELECT timestamp AS timestamps',
										 		    'FROM check_ins',
										 		    'WHERE section_id = ', sectionId,
										 	        'AND timestamp >= \'', startDatetime, '\'',
										 		    'AND timestamp <= \'', endDatetime, '\'',
										 		    'AND extract(dow from timestamp) IN (1,2,3,4,5) AND is_holiday = FALSE',
										 		    'ORDER BY timestamp'))

queryResponse2 <- dbGetQuery(con, statement = paste('SELECT timestamp AS timestamps',
										 	   		'FROM check_ins',
										 		    'WHERE section_id = ', sectionId,
										 	        'AND timestamp >= \'', startDatetime, '\'',
										 		    'AND timestamp <= \'', endDatetime, '\'',
										 		    'AND extract(dow from timestamp) = 6 AND is_holiday = FALSE',
										 		    'ORDER BY timestamp'))

queryResponse3 <- dbGetQuery(con, statement = paste('SELECT timestamp AS timestamps',
										 	   		      'FROM check_ins',
										 		          'WHERE section_id = ', sectionId,
										 	              'AND timestamp >= \'', startDatetime, '\'',
										 		          'AND timestamp <= \'', endDatetime, '\'',
										 		          'AND (extract(dow from timestamp) = 0 OR is_holiday = TRUE)',
										 		          'ORDER BY timestamp'))

dbDisconnect(con)

# ------- Constructing time series  -------
if (nrow(queryResponse1) == 0) {
	timeSeriesWork <- head(data.frame(timeSlot = seq(startDatetime, endDatetime, as.difftime(timeSlotSize, units="mins")), occurrences=0 ),-1)
} else {
	timeSeriesWork <- data.frame(table(cut(queryResponse1$timestamp, breaks = seq(startDatetime, endDatetime, as.difftime(timeSlotSize, units="mins")))))
	 colnames(timeSeriesWork) <- c("timeSlot", "occurrences")
}

if (nrow(queryResponse2) == 0) {
	timeSeriesSat <- head(data.frame(timeSlot = seq(startDatetime, endDatetime, as.difftime(timeSlotSize, units="mins")), occurrences=0 ),-1)
} else {
	timeSeriesSat <- data.frame(table(cut(queryResponse2$timestamp, breaks = seq(startDatetime, endDatetime, as.difftime(timeSlotSize, units="mins")))))
	colnames(timeSeriesSat) <- c("timeSlot", "occurrences")
}

if (nrow(queryResponse3) == 0) {
	timeSeriesSun <- head(data.frame(timeSlot = seq(startDatetime, endDatetime, as.difftime(timeSlotSize, units="mins")), occurrences=0 ),-1)
} else {
	timeSeriesSun <- data.frame(table(cut(queryResponse3$timestamp, breaks = seq(startDatetime, endDatetime, as.difftime(timeSlotSize, units="mins")))))
	colnames(timeSeriesSun) <- c("timeSlot", "occurrences")
}

timeSeries <- data.frame(timeSlot=timeSeriesWork$timeSlot, workDayOcc=timeSeriesWork$occurrences, satOcc=timeSeriesSat$occurrences, sunOcc=timeSeriesSun$occurrences)

# ------- Check-ins per time of day -------
tmpTimeSlots <- data.frame(timeSlot = strftime(timeSeries$timeSlot, format="%H:%M"), workDayOcc=timeSeries$workDayOcc, satOcc=timeSeries$satOcc, sunOcc=timeSeries$sunOcc)
totalPerTimeSlot <- aggregate(. ~ timeSlot, data = tmpTimeSlots, FUN=sum)

# ------- Stats per time of day -------
library(plyr)
library(scales)

statsPerTimeSlot <- ddply(tmpTimeSlots, c("timeSlot"), summarise,
              		workN		= length(workDayOcc),
               		workMean	= mean(workDayOcc),
               		workSd   	= sd(workDayOcc),
             		workSe   	= workSd / sqrt(workN),
             		satN 	= length(satOcc),
               		satMean = mean(satOcc),
               		satSd   = sd(satOcc),
             		satSe   = satSd / sqrt(satN),
             		sunN 	= length(sunOcc),
               		sunMean = mean(sunOcc),
               		sunSd   = sd(sunOcc),
             		sunSe   = sunSd / sqrt(sunN))

# ------- Plotting results  -------
library(ggplot2)
source("./multiplot.r")

timeSeries$timeSlot <- as.POSIXct(timeSeries$timeSlot, tz = "UTC")

timeSeriesPlot <- ggplot() +
				  geom_line(data = timeSeries, aes(x = timeSlot, y = sunOcc, color = "Sunday/Holiday")) +
			      geom_line(data = timeSeries, aes(x = timeSlot, y = satOcc, color = "Saturday")) +
				  geom_line(data = timeSeries, aes(x = timeSlot, y = workDayOcc, color = "Working day")) + 
				  scale_x_datetime(breaks=date_breaks("1 day"),labels=date_format("%b %d")) +
				  labs(x = "Time slots", y = "Occurrences", title = "Check-ins as time series", color="Day type") +
         		  theme(axis.text.x = element_text(angle = 90, size=6, hjust = 1), 
         				axis.text.y = element_text(size=6),
         				axis.title=element_text(size=8,face="bold"),
         				title=element_text(size=9),
        	  			legend.title = element_text(size=8),
         	  			legend.text = element_text(size=6))

library(reshape2)		
totalPerTimeSlotMelted <- melt(totalPerTimeSlot[,c('timeSlot', 'workDayOcc', 'satOcc', 'sunOcc')],id.vars = 1)

totalPerTimeSlotPlot <- ggplot(totalPerTimeSlotMelted,aes(x = timeSlot,y = value)) + 
       					geom_bar(aes(fill = variable), position="dodge", stat="identity") +
       					scale_fill_discrete(name="Day type", breaks=c('satOcc', 'sunOcc', 'workDayOcc'), labels=c("Saturday", 'Sunday/Holiday', 'Working day')) +
        				labs(x = "Time slots", y = "Occurrences", title = "Total number of recorded check-ins per time of day") +
        				theme(axis.text.x = element_text(angle = 90, size=6, hjust = 1), 
        	  				  axis.text.y = element_text(size=6),
        	  				  axis.title=element_text(size=8,face="bold"),
         	  				  title=element_text(size=9),
         	  				  legend.title = element_text(size=8),
         	  				  legend.text = element_text(size=6))


statsPerTimeSlotPlot <- ggplot() +
						geom_errorbar(data = statsPerTimeSlot, aes(x = timeSlot, ymin=workMean-workSe, ymax=workMean+workSe), colour="black", size=0.2, width=0.5) +
						geom_line(data = statsPerTimeSlot, aes(x = timeSlot, y = workMean, group=1, color = "Working day")) +
						geom_point(data = statsPerTimeSlot, aes(x = timeSlot, y = workMean, group=1, color = "Working day")) + 
						geom_errorbar(data = statsPerTimeSlot, aes(x = timeSlot, ymin=satMean-satSe, ymax=satMean+satSe), colour="black", size=0.2, width=0.5) +
						geom_line(data = statsPerTimeSlot, aes(x = timeSlot, y = satMean, group=1, color = "Saturday")) +
						geom_point(data = statsPerTimeSlot, aes(x = timeSlot, y = satMean, group=1, color = "Saturday")) +
						geom_errorbar(data = statsPerTimeSlot, aes(x = timeSlot, ymin=sunMean-sunSe, ymax=sunMean+sunSe), colour="black", size=0.2, width=0.5) +
						geom_line(data = statsPerTimeSlot, aes(x = timeSlot, y = sunMean, group=1, color = "Sunday/Holiday")) +
						geom_point(data = statsPerTimeSlot, aes(x = timeSlot, y = sunMean, group=1, color = "Sunday/Holiday")) +
    					labs(x = "Time slots", y = "Occurrences (mean +- se)", title = "Mean and standard error of the number of check-ins per time of day", color = "Day type") +
         				theme(axis.text.x = element_text(angle = 90, size=6, hjust = 1), 
         				   	  axis.text.y = element_text(size=6),
         				      axis.title=element_text(size=8,face="bold"),
         				   	  title=element_text(size=9),
         				   	  legend.title = element_text(size=8),
         	  				  legend.text = element_text(size=6))

combinedPlot <- multiplot(timeSeriesPlot, totalPerTimeSlotPlot, statsPerTimeSlotPlot, cols=1)

