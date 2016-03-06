# looking at where CA halibut is landed - no resolution yet
vessel_landings <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/vessel_landings_data.RDS")
vessel_stats <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/vessel_stats.RDS")

library(dplyr)

ak_boats <- vessel_stats %>%
  dplyr::select(drvid, alaska) %>%
  distinct()

trips <- vessel_landings %>%
  dplyr::select(trip_id, drvid, grgroup, ifq_landing, metier.2010, year,pcid, month) %>%
  distinct() %>%
  left_join(ak_boats)

with(subset(trips, year>2011 & grgroup=="TWL" & alaska == 0), table(ifq_landing, metier.2010))

# now look at twl_2 trips, where are they being landed?

with(subset(trips, year > 2011 & grgroup == "TWL" & alaska == 0 & metier.2010 == "TWL_2"), table(ifq_landing, pcid))
# looks like all in CA. SF is the only place where quota has to be landed. 

# when are they being landed?

with(subset(trips, year > 2011 & grgroup == "TWL" & alaska == 0 & metier.2010 == "TWL_2"), 
     table(month,ifq_landing))

# how many trips have california halibut in them?

chl_trips <- vessel_landings %>%
  dplyr::select(drvid, trip_id, spid, grgroup, 
                ifq_landing, metier.2010, year,pcid, month, modified, landed_wt, adj_revenue) %>%
  left_join(ak_boats) %>%
  filter(modified == c("CHLB"), year > 2011, metier.2010=="TWL_2", alaska==0) %>%
  distinct()

table(chl_trips$month, chl_trips$ifq_landing)


nrow(chl_trips)/nrow(subset(trips, year > 2011 & grgroup == "TWL" & alaska == 0 & metier.2010 == "TWL_2"))

twl_2 <- subset(trips, year > 2011 & grgroup == "TWL" & alaska == 0 & metier.2010 == "TWL_2")
# which ones not there?
twl_2[which(!(twl_2$trip_id %in% chl_trips$trip_id)),]

# so not perfect. but looking at distribution of revenue from CH halibut during
# closed season seems similar to open (so not like catching a lot of small amounts)

library(ggplot2); library(ggthemes)
chl_trips %>% mutate(season = ifelse( month > 3 & month < 6, "closed","open"), 
                     season = factor(season)) %>%
ggplot(aes(x=adj_revenue)) +  geom_density(aes(fill=season, color=season), alpha = .5) + 
  theme_pander()

