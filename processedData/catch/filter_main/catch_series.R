# generate catch time-series for emily
ftl <- readRDS("/Users/efuller/1/CNH/processedData/catch/5_add_length/ftl_len.RDS")

# vessels have to be there for each year and remove any metiers that make 0 dollars ever. 
# make sure to have vessel ID, date, port, metier, length

library(dplyr)
catch.series <- ftl %>%
  group_by(drvid) %>%
  mutate(n.years = length(unique(year))) %>%
  filter(n.years == 5) %>%
  group_by(metier) %>%
  mutate(total.value = sum(adj_revenue, na.rm = T)) %>%
  filter(total.value > 0)

metier.left <- catch.series %>%
  group_by(metier) %>%
  distinct(total.value) %>%
  select(metier, total.value) 

library(ggplot2) 
ggplot(filter(metier.left, total.value>40000), aes(x = reorder(metier, -total.value), y = total.value)) + geom_bar(stat = 'identity', aes(order = desc(total.value))) + theme_classic() + theme( axis.text.x = element_text(angle=90, vjust=1))

# looking at bottom of pile
subset(ftl, metier == "HKL_12", select =c("drvid",'trip_id', "adj_revenue","round_wt","ppp","modified","grid","pcid"))

# looks decent - subset to those
catch.series <- ftl %>%
  group_by(drvid) %>%
  mutate(n.years = length(unique(year))) %>%
  filter(n.years == 5) %>%
  group_by(metier) %>%
  mutate(total.value = sum(adj_revenue, na.rm = T)) %>%
  filter(total.value > 40000) %>%
  select(drvid, trip_id, modified, adj_revenue, round_wt, ppp, pcid, month, day, year, grid, metier, dahl_sector, len, hp)

write.csv(catch.series,"/Users/efuller/1/CNH/processedData/catch/filter_main/catch_series.csv",row.names = FALSE)
