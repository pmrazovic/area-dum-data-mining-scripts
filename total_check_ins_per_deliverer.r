library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='localhost', port='5432', dbname='area_dum', user='area_dum', password='area_dum')
out <- dbGetQuery(con, 'select deliverer_id, count(*) from check_ins where deliverer_id <> 217010 group by 1')
histInfo <- ggplot(out, aes(count)) + geom_histogram(binwidth=20) +
  labs(x = "Number of check-ins", y = "Number of deliverer", title = "Distribution of check-ins per deliverers")

print(histInfo)
