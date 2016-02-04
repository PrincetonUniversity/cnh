# make file for analysis
rm(list=ls())
library(dplyr)
# create vessel level data set ----
  
  # landings data
    source("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/01_create_vessel_df.R")
    vessel_landings <- create_vessel_landings()
  
  # response and explanatory variables
    source("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/01_vessel_stats.R")
    # will warn about NAs, for length calc and are real, removed
    vessel_stats <- calc_vessel_vars()

# create port level data set ----
  
  source("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/02_port_stats.R")
  port_stats <- calc_port_df()

# descriptive stats ----
  # number of vessels and trips
    vessel_landings %>% dplyr::select(drvid) %>% distinct %>% summarize(n.vessels = n())
    vessel_landings %>% dplyr::select(trip_id) %>% distinct %>% summarize(n.trips = n())
  
  # percent lbs
    dat <- readRDS("/Users/efuller/Desktop/CNH/processedData/catch/1_cleaningData/tickets.RDS")
    dat %>% filter(year!=2011, removal_type %in% c("C","D"), pargrp == "C") %>% 
      dplyr::select(landed_wt) %>% 
      summarize(total.lbs = sum(as.numeric(landed_wt), na.rm=T))
  
    vessel_landings %>% dplyr::select(landed_wt) %>% 
      summarize(total.lbs = sum(as.numeric(landed_wt), na.rm=T))
  
  # percent rev
    dat %>% filter(year!=2011, removal_type %in% c("C","D"), pargrp == "C") %>% 
      dplyr::select(adj_revenue) %>% 
      summarize(total.rev = sum(as.numeric(adj_revenue), na.rm=T))
  
    vessel_landings %>% 
      dplyr::select(adj_revenue) %>% 
      summarize(total.rev = sum(as.numeric(adj_revenue), na.rm=T))
  
  vessel_stats %>% filter(alaska == 0 & both.periods==1) %>% 
    group_by(ifq_flag) %>% 
    summarize(n = length(unique(drvid)))
  
  # look at changes in portfolio before and after
  
  table(vessel_stats$composition,vessel_stats$delta.nfisheries)
  
  # looking at just those boats that exited ITQs but kept fishing
  # stay in
  rowSums(with(subset(vessel_stats, ifq_flag =="itq stay on" & c.halibut==0), table(composition,delta.nfisheries)))
  
  # drop out
  with(subset(vessel_stats, ifq_flag =="LE gf exit, still fish" & c.halibut==0), table(composition,delta.nfisheries))
  
  # the cumulative revenue by metier 
  head(vessel_landings %>% group_by(metier.2010) %>% summarize(revenue = sum(adj_revenue)) %>% arrange(-revenue) %>% mutate(cumulative.per = cumsum(revenue)/sum(revenue)),20)

# run models and make figures ----
  # compare diversity before and after 2011
  with(subset(vessel_stats, alaska==0 & both.periods == 1 & c.halibut==0), t.test(eff.shannon_2010, eff.shannon_2010 + delta.eff.shannon_2010))
  
  # compare diversity for vessels landing in catch shares
  with(subset(vessel_stats, alaska==0 & both.periods == 1 & c.halibut==0 & ifq_flag=="itq stay on"), t.test(eff.shannon_2010, eff.shannon_2010 + delta.eff.shannon_2010))
  
  with(subset(vessel_stats, alaska==0 & both.periods == 1 & c.halibut==0 & ifq_flag=="general fleet"), t.test(eff.shannon_2010, eff.shannon_2010 + delta.eff.shannon_2010))
  
  # get metier statistics
  dat %>% dplyr::select(metier.2010) %>% distinct %>% summarize(n())
  
  # vessel model fits

  # port model fits
  source("/Volumes/LA-PRIVATE/CNH/Analysis/Metiers/bin/03_fig_drafts.R")
  