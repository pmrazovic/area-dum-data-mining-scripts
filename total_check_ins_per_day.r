library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='localhost', port='5432', dbname='area_dum', user='area_dum', password='area_dum')
out <- dbGetQuery(con, 'select date(timestamp), count(*) from check_ins group by 1')

p <- ggplot() + geom_line(data = out, aes(x = date, y = count)) +
    labs(x = "Date", y = "Number of check-ins", title = "Total number of check-ins per day")

print(p)