# create vessel level landings data

create_vessel_landings <- function(){
  library(dplyr)
  dat <- readRDS("/Users/efuller/Desktop/CNH/processedData/catch/1_cleaningData/tickets.RDS")
  
  # vessels should have less than min_rev on average across 5 years AND
  # flagg landings before and after catch shares
  # then drop 2011 data
  # also drop any ZZZ drvids
  # and drop non-commercial landings (pargrp = C and removal type in C, D)
  
  min_rev = 5000
  
  div_dat <- dat %>%                      
    filter(pargrp == "C" & removal_type %in% c("C","D")) %>%      # commercial, direct commercial landings
    group_by(drvid, year) %>%                                   
    summarize(annual_revenue = sum(adj_revenue, na.rm = T)) %>%   # calculate yr rev
    mutate(av.annual.rev = mean(annual_revenue, na.rm = T)) %>%   # mean yr rev
    filter(av.annual.rev >= min_rev) %>%                          # drop vessels with < min_rev
    filter(year != 2011) %>%
    group_by(drvid) %>%
    summarize(both.periods = ifelse(any(year %in% c(2009, 2010)) & any(year %in% c(2012,2013)), 1, 0))
  
  div_landings <- subset(dat, drvid %in% unique(div_dat$drvid) & 
                           year!=2011 & pargrp == "C" & 
                           removal_type %in% c("C","D")) %>%
    left_join(div_dat)
  
  saveRDS(div_landings,
          file="/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/vessel_landings_data.RDS")
  return(div_landings)
}