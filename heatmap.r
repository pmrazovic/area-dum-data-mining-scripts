library(fasttime)
library(ggmap)

rm(list=ls())
setwd("/Users/pero/Development/area-dum-data-mining-scripts")

# ------- Parameters -------
start_datetime <- as.POSIXct("2016-01-01 00:00:00", tz = "UTC") # Beginning of time interval under study
end_datetime <- as.POSIXct("2016-02-24 00:00:00", tz = "UTC") # End of time interval under study
time_slot_size <- 2 # Size of time slot
time_slot_units <- "hours"

# ------- Reading check-ins -------
check_ins <- fread("./CSV_data_exports/section_check_ins_with_coords.csv", header=TRUE, sep=",", colClasses = c("character","numeric","numeric","POSIXct"))
# Translate timestamp to factor
check_ins$timestamp <- fastPOSIXct(check_ins$timestamp, tz = "UTC")
check_ins$timestamp <- cut(check_ins$timestamp, breaks = seq(start_datetime, end_datetime, as.difftime(time_slot_size, units=time_slot_units)))
# Remove weekend timestamps 
check_ins$timestamp <- fastPOSIXct(check_ins$timestamp, tz = "UTC") # Transform factors back to POSIXct
check_ins = check_ins[grepl("Mon|Tue|Wed|Thu|Fri", weekdays(check_ins$timestamp)),]
# Remove holidays
barcelona_holidays <- fread("./CSV_data_exports/barcelona_holidays.csv", header=TRUE, sep=",", colClasses = c("POSIXct"))
barcelona_holidays$date <- fastPOSIXct(barcelona_holidays$date, tz = "UTC")
check_ins = check_ins[! as.Date(check_ins$timestamp) %in% as.Date(barcelona_holidays$date),]
# Remove date part from timestamp (to character)
check_ins$timestamp <- format(check_ins$timestamp, "%H:%M")

# Removing some time slots as we don't wont to plot them
check_ins <- check_ins[check_ins$timestamp != "00:00" & 
                       check_ins$timestamp != "02:00" & 
                       check_ins$timestamp != "04:00" &
                       check_ins$timestamp != "08:00" &
                       check_ins$timestamp != "20:00" &
                       check_ins$timestamp != "22:00",]
# Transform the timestamps as factors in order to rename them
check_ins$timestamp <- as.factor(check_ins$timestamp)
# Renaming time slots
levels(check_ins$timestamp) <- c("08:00 - 10:00", 
                                 "10:00 - 12:00", 
                                 "12:00 - 14:00", 
                                 "14:00 - 16:00", 
                                 "16:00 - 18:00",
                                 "18:00 - 20:00")

# Bounding box for map view
bbox <- ggmap::make_bbox(depot_lon, depot_lat, check_ins, f = 0.5)
# Getting the map from Google
myMap <- get_map(location=bbox,source="google", maptype="roadmap", zoom=13, crop=FALSE, color = "bw")

# Plotting
map_plot <- ggmap(myMap, base_layer = ggplot(aes(x = depot_lon, y = depot_lat), data = check_ins)) 
plot <- map_plot + geom_density2d(data = check_ins, aes(x = depot_lon, y = depot_lat), size = 0.2) +
   stat_density2d(data = check_ins, aes(x = depot_lon, y = depot_lat, fill = ..level.., alpha = ..level..), bins = 16, geom = "polygon") +
   scale_fill_gradient('Check-in\ndensity', low = "green", high = "red") + 
   scale_alpha(range = c(0, 0.6), guide = FALSE) +
   labs(x=expression("Longitude"),y=expression("Latitude"), title="Check-in distributions (day)" ) +
   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +
   facet_wrap(~ timestamp, ncol = 3) +
   theme(axis.text.x = element_text(size=6), 
        axis.text.y = element_text(size=6),
        axis.title=element_text(size=8,face="bold"),
        title=element_text(size=9),
        legend.title = element_text(size=8),
        legend.text = element_text(size=6))

print(plot)

