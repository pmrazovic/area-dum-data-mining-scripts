library(data.table)
library(fasttime)
library(plyr)
library(lubridate)
library(chron)
library(ggmap)

rm(list=ls())
setwd("/Users/pero/Development/area-dum-data-mining-scripts")

# ------- Parameters -------
start_datetime <- as.POSIXct("2016-01-01 00:00:00", tz = "UTC") # Beginning of time interval under study
end_datetime <- as.POSIXct("2016-02-24 00:00:00", tz = "UTC") # End of time interval under study
time_slot_size <- 30 # Size of time slot
time_slot_units <- "mins"

# ------- Reading section list with coordinates -------
depots <- fread("./CSV_data_exports/sections.csv", header=TRUE, sep=",", colClasses = c("character","numeric","numeric"))

# ------- Reading check-ins -------
check_ins <- fread("./CSV_data_exports/section_check_ins.csv", header=TRUE, sep=",", colClasses = c("character","POSIXct"))
# Translate depot_id to factor
check_ins$depot_id <- factor(check_ins$depot_id, levels=depots$depot_id)
# Translate timestamp to factor
check_ins$timestamp <- fastPOSIXct(check_ins$timestamp, tz = "UTC")
check_ins$timestamp <- cut(check_ins$timestamp, breaks = seq(start_datetime, end_datetime, as.difftime(time_slot_size, units=time_slot_units)))
# Count occurences per depot_id and timestamp
check_ins <- data.frame(table(check_ins))
# Remove weekend timestamps 
check_ins$timestamp <- fastPOSIXct(check_ins$timestamp, tz = "UTC") # Transform factors back to POSIXct
check_ins = check_ins[grepl("Mon|Tue|Wed|Thu|Fri", weekdays(check_ins$timestamp)),]
# Remove holidays
barcelona_holidays <- fread("./CSV_data_exports/barcelona_holidays.csv", header=TRUE, sep=",", colClasses = c("POSIXct"))
barcelona_holidays$date <- fastPOSIXct(barcelona_holidays$date, tz = "UTC")
check_ins = check_ins[! as.Date(check_ins$timestamp) %in% as.Date(barcelona_holidays$date),]
# Remove date part from timestamp (to character)
check_ins$timestamp <- format(check_ins$timestamp, "%H:%M")

  # Speed up!
  # check_ins <- data.table(check_ins)
  # stats <- check_ins[,list(mean=mean(Freq),sd=sd(Freq)),by=list(depot_id, timestamp)]

stats <- ddply(check_ins, c("depot_id","timestamp"), summarise,
               N	= length(Freq),
               mean = mean(Freq),
               sd = sd(Freq),
               se = sd / sqrt(N))

#time_slot_stats$depot_lat <- depots$depot_lat
#time_slot_stats$depot_lon <- depots$depot_lon
