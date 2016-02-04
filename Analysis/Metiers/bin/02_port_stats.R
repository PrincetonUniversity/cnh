# generate port dataset - use vessel landings
calc_port_df <- function(){
  library(dplyr); library(reshape2); library(igraph); library(tidyr)
  vessel_landings <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/vessel_landings_data.RDS")
  ports <- read.csv("/Users/efuller/Desktop/CNH/processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE)
  colnames(ports) <- tolower(colnames(ports))
  
# calculate before and after connectance ----
  source("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/02_define_participationPlot.R")
  
# calculate average connectance before and after 2011 ----
  port_list <- list()
  prts <- unique(vessel_landings$pcid)
  for(p in 1:length(prts)){
    port_list[[p]] <- define_participationPlot(year_choose = 2009:2010, 
                                               port = prts[p], restrict = FALSE, 
                                               tickets = vessel_landings)
  }
  
  names(port_list) <- prts
  # remove ports that are NA
  port_list <- port_list[-which(is.na(port_list))]
  
  # changed to link density since it already exists, very similar to mine
  ic <- as.data.frame(unlist(lapply(port_list, function(x)length(E(x)$weight)/vcount(x))))
  ic$pcid <- rownames(ic)
  
  
  port_post <- list()
  
  for(p in 1:length(prts)){
    port_post[[p]] <- define_participationPlot(year_choose = 2012:2013, port = prts[p], restrict = FALSE, tickets = vessel_landings)
  }
  
  names(port_post) <- prts
  # remove ports that aren't in port_post
  port_post <- port_post[-which(is.na(port_post))]
  
  ic_post <- as.data.frame( unlist(lapply(port_post, function(x)sum(E(x)$weight)/vcount(x))))
  ic_post$pcid <- rownames(ic_post)
  colnames(ic_post) <- c("ic_post","pcid")
  
  colnames(ic) <- c("ic_pre","pcid")
  
  port_df <- full_join(ic, ic_post)
  port_df <- port_df[complete.cases(port_df),]
  port_df$ic_delta <- port_df$ic_post - port_df$ic_pre
  

# calculate number of vessels landing before, after, overall ----
  n.ves = vessel_landings %>%
    mutate(period = ifelse(year < 2011, "before" , "after")) %>%
    dplyr::select(pcid, period, drvid) %>%
    distinct() %>%
    group_by(pcid, period) %>%
    summarize(n.ves = length(drvid)) %>%
    spread(period, n.ves) %>%
    group_by(pcid) %>%
    mutate(overall_nves = mean(c(after, before), na.rm = T)) %>%
    rename(before.nves = before, after.nves = after)
  
# calculate amount of revenue before, after overall ----
  rev.ves = vessel_landings %>%
    mutate(period = ifelse(year < 2011, "before", "after")) %>%
    group_by(pcid, period) %>%
    summarize(revenue = sum(adj_revenue,na.rm = T)) %>%
    spread(period, revenue) %>%
    group_by(pcid) %>%
    mutate(overall_rev = mean(c(after, before), na.rm = T)) %>%
    rename(before.rev = before, after.rev = after)
  
# calculate amount of lbs before, after, overall ----
  lbs.ves = vessel_landings %>%
    mutate(period = ifelse(year < 2011, "before", "after")) %>%
    group_by(pcid, period) %>%
    summarize(lbs = sum(landed_wt,na.rm = T)) %>%
    spread(period, lbs) %>%
    group_by(pcid) %>%
    mutate(overall_lbs = mean(c(after, before), na.rm = T)) %>%
    rename(before.lbs = before, after.lbs = after)
  
# calculate number of trips before, after, overall ----
  trips.ves = vessel_landings %>%
    mutate(period = ifelse(year < 2011, "before", "after")) %>%
    dplyr::select(pcid, period, trip_id) %>%
    distinct() %>%
    group_by(pcid, period) %>%
    summarize(ntrips = length(unique(trip_id))) %>%
    spread(period, ntrips) %>%
    group_by(pcid) %>%
    mutate(overall_trips = mean(c(after, before), na.rm = T)) %>%
    rename(before.trips = before, after.trips = after)

  
# put all together ----
  port_stats <- port_df %>%
    full_join(n.ves) %>%
    full_join(rev.ves) %>%
    full_join(lbs.ves) %>%
    full_join(trips.ves) %>%
    full_join(ports)
  
# look for how ports are affected by catch shares ----
  twl_prior = vessel_landings %>%
    dplyr::select(trip_id, pcid, metier.2010, year) %>%
    distinct() %>%
    filter(year < 2011) %>%
    group_by(pcid) %>%
    summarize(twl_prior = ifelse(any(metier.2010 %in% c("TWL_1","TWL_5","TWL_6","TWL_7","TWL_8","TWL_9","TWL_10","TWL_11","TWL_12","TWL_13","TWL_14")), 1, 0))
  
  quota_post = vessel_landings %>%
    dplyr::select(trip_id, pcid, ifq_landing, year) %>%
    distinct() %>%
    filter(year > 2011) %>%
    group_by(pcid) %>%
    summarize(itq_post = ifelse(any(ifq_landing == "Y"), 1, 0))
  
  twl_partip = full_join(twl_prior, quota_post)
  twl_partip$ifq_flag = NA
  # this means they didn't fish in twl before and didn't land itqs after but 
  # because both values are 0, means they're in the dataset both before and 
  # after 2011. 
  twl_partip$ifq_flag[which(twl_partip$twl_prior==0 & twl_partip$itq_post == 0)] = "unaffected"
  # this means they didn't fish in twl before but did land itqs after 2011
  twl_partip$ifq_flag[which(twl_partip$twl_prior==0 & twl_partip$itq_post == 1)] = "itq entrant: general landings"
  # this means they didn't fish twl before, but aren't found in the post 2011
  # dataset
  twl_partip$ifq_flag[which(twl_partip$twl_prior==0 & is.na(twl_partip$itq_post))] = "unaffected exit"
  # this means they fished twl before, but don't land any quota post 2011
  twl_partip$ifq_flag[which(twl_partip$twl_prior==1 & twl_partip$itq_post==0)] = "LE gf exit, still landings"
  # this means they fished in twl before and land quota after
  twl_partip$ifq_flag[which(twl_partip$twl_prior==1 & twl_partip$itq_post==1)] = "itq stay on"
  # this means they fished in twl before don't land quota (or anything else) after
  twl_partip$ifq_flag[which(twl_partip$twl_prior==1 & is.na(twl_partip$itq_post))] = "LE gf total exit"
  # this means they weren't landing anything before 2011, but now land some quota
  twl_partip$ifq_flag[which(is.na(twl_partip$twl_prior) & twl_partip$itq_post==1)] = "itq entrant"
  # didn't fish before, now fish in non itq fleet after
  twl_partip$ifq_flag[which(is.na(twl_partip$twl_prior) & twl_partip$itq_post==0)] = "general landing entrant"
  
  twl_partip <- dplyr::select(twl_partip, pcid, ifq_flag)

  port_df <- port_stats %>%
    full_join(twl_partip) 
  
  saveRDS(port_df,
          file="/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/port_stats.RDS")
  saveRDS(port_list,
          file="/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/port_pre2011_networks.RDS")
  saveRDS(port_post,
          file="/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/port_post2011_networks.RDS")
  
  return(port_stats)
}
