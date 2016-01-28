# comparing metiers to observer defined sector

# load data ----
tickets <- readRDS("/Users/efuller/Desktop/CNH/processedData/catch/1_cleaningData/tickets.RDS")
obs <- read.csv("/Users/efuller/Desktop/CNH/rawData/Observers/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv", stringsAsFactors = FALSE)

# having a hard time with one of the columns - there's two of the column spgrftob1, so this makes one spgrftob1.1
# not sure why this is
colnames(obs) <- tolower(colnames(obs))
valid_column_names <- make.names(names=names(obs), unique=TRUE, allow_ = TRUE)
names(obs) <- valid_column_names

# reduce to just columns needed
library(dplyr)

# prepare fish tickets
trips <- tickets %>%
  group_by(trip_id) %>%
  distinct() %>%
  select(trip_id, metier.2010)

# prepare obs
# some obs have more than one fish ticket, so find those and pull out to deal with later
obs_trips <- obs %>%
  mutate(mult.tix = ifelse(grepl(";",fish_tickets),1,0)) %>%
  filter(mult.tix == 0) %>%
  mutate(trip_id = paste0(fish_tickets, year)) %>%
  select(trip_id, sector, ifq.grp, license_or_permit) %>%
  mutate(trip_id = gsub("X", "", trip_id)) %>%
  distinct(trip_id)

trip_cats <- left_join(trips, obs_trips)

# compare classifications
cat_table <- with(trip_cats[!is.na(trip_cats$sector),], table(metier.2010, sector))

library(RColorBrewer)
heatmap(t(cat_table), keep.dendro = FALSE, col = colorRampPalette(brewer.pal(9,"Blues"))(100))

# when obs sector is defined as a species-gear combination, would expect obs data to have 1-1 match with metier sector. 
# In some cases multiple obs sectors refer to the same target species but different gear (i.e. limted entry versus open access sablefish) would expect metier to go across both. 

# maybe go sector by sector: realized fisheries accurately classified pink shrimp 100% of the time, hake 99% of the time (exceptions where trawls dominated by bycatch). 

# other sectors compared favorably.

# look at adjusted rand index (ARI)
library(mclust)
ari <- with(trip_cats[!is.na(trip_cats$sector),], adjustedRandIndex(metier.2010, sector))
# only 60% but this might be due to the sablefish, limted entry trawl/catch share stuff. 

