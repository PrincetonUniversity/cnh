library(dplyr)
library(maps)
library(scales)
library(ggplot2)

# load data ----
load("processedData/spatial/2_coastline.Rdata")
tickets <- readRDS("processedData/catch/1_cleaningData/tickets.RDS")
fp <- "processedData/spatial/vms/intermediate/04_link_mets_vms/tw_0hr/"
fp <- paste0(fp, dir(fp))
vms <- lapply(fp, readRDS); rm(fp) # vms loaded as list of dataframes

# check if ncol is same for each df in list, drop the column not all data has
n_cols <- unique(sapply(vms, ncol))
if(length(n_cols) > 1){
  short_cols <- which(sapply(vms, ncol)==min(n_cols))
  reg_cols <- which(sapply(vms, ncol)==max(n_cols))
  missing_col <- colnames(vms[[reg_cols[1]]])[
    which(!(colnames(vms[[reg_cols[1]]]) %in% colnames(vms[[short_cols[1]]])))]
  vms[reg_cols] <- lapply(vms[reg_cols], 
                          function(x) dplyr::select(x, -contains(missing_col)))
  vms <- lapply(vms, function(x) as.data.frame(x, stringsAsFactors = FALSE))
  if(length(unique(sapply(vms, ncol)))>1){
    warning("fixing cols failed, please take a look")
  }
}
# historically: some missing rev_dist

# convert from list of dataframes to one dataframe
vms_df <- do.call(rbind, vms) # takes awhile

# filter to crab (POT_1)----
crab_landings <- tickets %>%
  filter(metier.2010 == "POT_1", year > 2012 & year < 2016) %>%
  group_by(drvid,trip_id,metier.2010, year) %>%
  summarize(lbs = sum(pounds, na.rm = T), 
            rev = sum(adj_revenue, na.rm = T))
crab_landings <- as.data.frame(crab_landings)

crab_trips <- vms_df %>%
  filter(metier.2010 == "POT_1", longitude < -100)

# basic stats---
# total vessels
  n_vms_ves = length(unique(crab_trips$docnum)) # total VMS data
  tot_ves = nrow(crab_landings %>% distinct(drvid))
  n_vms_ves/tot_ves # percentage
  
# total trips
  n_vms_trips <- length(unique(crab_trips$trip_id1))
  tot_trips = length(crab_landings$trip_id)
  n_vms_trips/tot_trips
  
# number of trips by vessel 
  n_trips <- crab_landings %>% group_by(drvid) %>% 
    summarize(n_trips_tickets = length(unique(trip_id)))
  
  vms_trips <- crab_trips %>% group_by(docnum) %>%
    summarize(n_trips_vms = length(unique(trip_id1))) %>%
    left_join(n_trips, by = c("docnum" = "drvid")) %>%
    mutate(percent_covered = n_trips_vms/n_trips_tickets)
  
any(vms_trips$percent_covered>1)  # should be FALSE
median(vms_trips$percent_covered)

ggplot(vms_trips, aes(x = percent_covered)) + geom_histogram() + 
  theme_classic() + xlab("percent of vessels' crab trips with vms data") +
  geom_vline(xintercept = median(vms_trips$percent_covered), 
             color = 'indianred') + 
  annotate("text", x = .35, y = 27, label="median",
           color = 'indianred')

# plotting fun ----
pdf("Analysis/new_analysis/whale_entanglement/crab_trip_map.pdf", 
    width = 10, height = 20)
par(bg = 'steelblue', mai = rep(0,4))
with(crab_trips, 
     plot(longitude, latitude,asp = 1, cex = .05, 
          col = alpha('indianred', 0), bty = "n", axes = FALSE))
plot(WC, add = T, col = 'grey60', bor = F)
with(crab_trips, 
     points(longitude, latitude,asp = 1, cex = .05, 
          col = alpha('indianred', .25)))
dev.off()

# versus declarations ----
# number of VMS pings found for each declaration identified as crab trips
write.csv(with(subset(vms_df, metier.2010 == "POT_1"), table(declarations)),
          "Analysis/new_analysis/whale_entanglement/declaration_v_tickets.csv")