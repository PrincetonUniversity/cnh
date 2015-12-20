# looking at where CA halibut is landed - no resolution yet
vessel_landings <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/vessel_landings_data.RDS")
vessel_stats <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/vessel_stats.RDS")

library(dplyr)

ak_boats <- vessel_stats %>%
  dplyr::select(drvid, alaska) %>%
  distinct()

trips <- vessel_landings %>%
  dplyr::select(trip_id, drvid, grgroup, ifq_landing, metier.2010, year,pcid) %>%
  distinct() %>%
  left_join(ak_boats)

with(subset(trips, year>2011 & grgroup=="TWL" & alaska == 0), table(ifq_landing, metier.2010))

# now look at twl_2 trips, where are they being landed?

with(subset(trips, year > 2011 & grgroup == "TWL" & alaska == 0 & metier.2010 == "TWL_2"), table(ifq_landing, pcid))
# looks like all in CA. SF is the only place where quota has to be landed. 
