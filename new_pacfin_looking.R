# load new pacfin take 2013 and compare to fish-tickets metiers

new_pac <- read.csv("/Users/efuller/Desktop/pacfin (51).csv",stringsAsFactors = FALSE)
colnames(new_pac) <- tolower(colnames(new_pac))
new13 <- new_pac %>%
  dplyr::select(drvid, ftid, dahl_sector,year) %>%
  distinct() %>%
  mutate(trip_id = paste0(ftid,year)) %>%
  dplyr::select(-year) %>%
  inner_join(dplyr::select(tickets, drvid, metier.2010, trip_id))

tickets %>% dplyr::filter(year==2013, metier.2010=="POT_7") %>%
  group_by(trip_id, modified) %>% summarize(lbs = sum(landed_wt), rev = sum(adj_revenue)) %>%
  group_by(trip_id) %>%
  mutate(percent_lbs = lbs/sum(lbs)) %>%
  filter(percent_lbs==max(percent_lbs))%>%
  group_by(modified)%>%
  summarize(average_precent_lbs = median(percent_lbs), ntimes = n()) %>%
  ggplot(aes(x = reorder(modified, average_precent_lbs), y = average_precent_lbs)) + 
  geom_bar(stat='identity', aes(fill = ntimes)) + 
  theme(axis.text=element_text(angle=90))

with(tickets %>% dplyr::filter(year==2013, metier.2010=="POT_7") %>%
  dplyr::select(trip_id, grid) %>%
  distinct(), table(grid))


# fluctuations in ppp by species
ppp_ts <- tickets %>% mutate(tdate = as.Date(tdate, format='%d-%b-%y'),
                             week_num = as.numeric(format(tdate, "%U"))) %>% 
  group_by(week_num, modified, year) %>%
  summarize(median_ppp = median(ppp), max_ppp = max(ppp), min_ppp = min(ppp))

top_10 = tickets %>% group_by(modified) %>%
  summarize(lbs = sum(landed_wt), rev = sum(adj_revenue)) %>%
  arrange(-rev)

ggplot(subset(ppp_ts, modified %in% top_10$modified[1:10] & !(modified %in% c("DCRB","LOBS"))), aes(x = week_num, y = median_ppp)) + geom_path(aes(color = modified)) + facet_wrap(~year,scale='free_x')



ggplot(subset(ppp_ts, modified %in% c("LOBS")), aes(x = week_num, y = median_ppp)) + geom_bar(stat='identity',aes(fill = modified)) + facet_wrap(~year,scale='free_x')

# median latitdue of fishing
all_ports <- read.csv("processedData/spatial/ports/all_ports.csv", stringsAsFactors = FALSE) %>%
  rename(pcid=Pcid)

tickets %>% filter(modified %in% top_10$modified[1:10]) %>% group_by(year, modified) %>%
  summarize(median_lat = weighted.mean(lat, adj_revenue, na.rm=T)) %>%
  ggplot(aes(x=year, y = median_lat, color = modified)) + geom_line()

tickets %>% filter(modified %in% c("CHNK")) %>% 
  group_by(year, modified) %>% summarize(lbs = sum(landed_wt), rev = sum(adj_revenue),
                               ntrips = length(unique(trip_id)), 
                               nvessels = length(unique(drvid)),
                               trip_ves = ntrips/nvessels) %>%
  ggplot(aes(x = year, y = lbs, color=modified)) + geom_line()

# looking at crab
tickets %>% filter(spid %in% c("DCRB")) %>% 
  group_by(year, spid) %>% summarize(lbs = sum(landed_wt),
                                         nvessels = length(unique(drvid))) %>%
  ggplot(aes(x = year, y = lbs, color=spid)) + geom_line()
# looking at fuel
fuel_wa <- read.csv("/Users/efuller/Downloads/fuelwa.xls", skip=28)

