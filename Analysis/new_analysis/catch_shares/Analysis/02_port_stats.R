# generate port dataset - use vessel landings
calc_port_df <- function(){
  library(dplyr); library(reshape2); library(igraph); library(tidyr)
  vessel_landings <- readRDS("Analysis/new_analysis/catch_shares/Analysis/vessel_landings_data.RDS")
  ports <- read.csv("processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE) %>%
    left_join(unique(vessel_landings[,c("pcid","pcgroup")]), by = c("Pcid"="pcid"))
  colnames(ports) <- tolower(colnames(ports))
  ports <- ports %>% group_by(pcgroup, state) %>%
    summarize(lon = mean(lon, na.rm=T), lat = mean(lat, na.rm=T))
  
# calculate before and after connectance ----
  source("Analysis/new_analysis/participation_networks/participation_networks_fun.R")
  
# calculate average connectance before and after 2011 ----
  port_list <- list()
  prts <- unique(vessel_landings$pcgroup)
  for(p in 1:length(prts)){
    port_list[[p]] <- participation_network(year_choose = 2009:2010,
                                            pcid_choose = prts[p], 
                                            filter = TRUE,
                                            tickets = vessel_landings)
  }
  
  names(port_list) <- prts
  # remove ports that are NA
  if(any(is.na(port_list))){
    port_list <- port_list[-which(is.na(port_list))]
  }
  
  
  # calculate modularity
  ic <- data.frame(pcgroup = names(port_list), stringsAsFactors = FALSE)
  ic$m <- NA
  ic$ld <- NA
  
  for(i in 1:length(port_list)){
    g <- port_list[[i]]
    wtc <- cluster_walktrap(g, weights = E(g)$weight)
    ic$m[i] <- modularity(g, membership(wtc))
    ic$ld[i] = length(E(g))/length(V(g))
    
  }
  
  port_post <- list()
  
  for(p in 1:length(prts)){
    port_post[[p]] <- participation_network(year_choose = 2012:2013, 
                                            pcid_choose = prts[p], 
                                            filter = TRUE, 
                                            tickets = vessel_landings)
  }
  
  names(port_post) <- prts
  # remove ports that aren't in port_post
  if(any(is.na(port_post))){
    port_post <- port_post[-which(is.na(port_post))]
  }
  
  ic_post <- data.frame(pcgroup = names(port_post), stringsAsFactors = FALSE)
  ic_post$m_post <- NA
  ic_post$ld_post <- NA
  
  
  for(i in 1:length(port_post)){
    g <- port_post[[i]]
    wtc <- cluster_walktrap(g, weights = E(g)$weight)
    ic_post$m_post[i] <- modularity(g, membership(wtc))
    ic_post$ld_post[i] = length(E(g))/length(V(g))
  }
  
  port_df <- full_join(ic, ic_post)
  port_df <- port_df[complete.cases(port_df),]
  port_df$m_delta <- port_df$m_post - port_df$m
  port_df$ld_delta <- port_df$ld_post - port_df$ld
  
  

# calculate number of vessels landing before, after, overall ----
  n.ves = vessel_landings %>%
    mutate(period = ifelse(year < 2011, "before" , "after")) %>%
    dplyr::select(pcgroup, period, drvid) %>%
    distinct() %>%
    group_by(pcgroup, period) %>%
    summarize(n.ves = length(drvid)) %>%
    spread(period, n.ves) %>%
    group_by(pcgroup) %>%
    mutate(overall_nves = mean(c(after, before), na.rm = T)) %>%
    rename(before.nves = before, after.nves = after)
  
# calculate amount of revenue before, after overall ----
  rev.ves = vessel_landings %>%
    mutate(period = ifelse(year < 2011, "before", "after")) %>%
    group_by(pcgroup, period) %>%
    summarize(revenue = sum(adj_revenue,na.rm = T)) %>%
    spread(period, revenue) %>%
    group_by(pcgroup) %>%
    mutate(overall_rev = mean(c(after, before), na.rm = T)) %>%
    rename(before.rev = before, after.rev = after)
  
# calculate amount of lbs before, after, overall ----
  lbs.ves = vessel_landings %>%
    mutate(period = ifelse(year < 2011, "before", "after")) %>%
    group_by(pcgroup, period) %>%
    summarize(lbs = sum(pounds,na.rm = T)) %>%
    spread(period, lbs) %>%
    group_by(pcgroup) %>%
    mutate(overall_lbs = mean(c(after, before), na.rm = T)) %>%
    rename(before.lbs = before, after.lbs = after)
  
# calculate number of trips before, after, overall ----
  trips.ves = vessel_landings %>%
    mutate(period = ifelse(year < 2011, "before", "after")) %>%
    dplyr::select(pcgroup, period, trip_id) %>%
    distinct() %>%
    group_by(pcgroup, period) %>%
    summarize(ntrips = length(unique(trip_id))) %>%
    spread(period, ntrips) %>%
    group_by(pcgroup) %>%
    mutate(overall_trips = mean(c(after, before), na.rm = T)) %>%
    rename(before.trips = before, after.trips = after)

  
# put all together ----
  port_stats <- port_df %>%
    left_join(n.ves) %>%
    left_join(rev.ves) %>%
    left_join(lbs.ves) %>%
    left_join(trips.ves) %>%
    left_join(ports)
  
# look for how ports are affected by catch shares ----
  twl_prior = vessel_landings %>%
    dplyr::select(trip_id, pcgroup, metier.2010, year) %>%
    distinct() %>%
    filter(year < 2011) %>%
    group_by(pcgroup) %>%
    summarize(twl_prior = ifelse(any(metier.2010 %in% c("TWL_1","TWL_5","TWL_7","TWL_8","TWL_9","TWL_10","TWL_11","TWL_12","TWL_13")), 1, 0))
  
  quota_post = vessel_landings %>%
    dplyr::select(trip_id, pcgroup, fleet, year) %>%
    distinct() %>%
    filter(year > 2011) %>%
    group_by(pcgroup) %>%
    summarize(itq_post = ifelse(any(fleet == "LE"), 1, 0))
  
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
  
  twl_partip <- dplyr::select(twl_partip, pcgroup, ifq_flag)

  port_df <- port_stats %>%
    left_join(twl_partip) 
  
  saveRDS(port_df,
          file="Analysis/new_analysis/catch_shares/Analysis/port_stats.RDS")
  saveRDS(port_list,
          file="Analysis/new_analysis/catch_shares/Analysis/port_pre2011_networks.RDS")
  saveRDS(port_post,
          file="Analysis/new_analysis/catch_shares/Analysis/port_post2011_networks.RDS")
  
  return(port_df)
}
