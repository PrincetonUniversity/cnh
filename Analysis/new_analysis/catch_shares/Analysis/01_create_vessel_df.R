# create vessel level landings data

create_vessel_landings <- function(minrev){
  library(dplyr)
  dat <- readRDS("processedData/catch/1_cleaningData/tickets.RDS")
  
  # vessels should have less than min_rev on average across 5 years AND
  # flagg landings before and after catch shares
  # then drop 2011 data
  # also drop any ZZZ drvids
  # and drop non-commercial landings (pargrp = C and removal type in C, D)
  
  min_rev = 5000
  
  div_dat <- dat %>%
    filter(drvid != "NONE") %>%
    group_by(drvid, year) %>%                                   
    summarize(annual_revenue = sum(adj_revenue, na.rm = T)) %>%   # calculate yr rev
    mutate(av.annual.rev = mean(annual_revenue, na.rm = T)) %>%   # mean yr rev
    filter(av.annual.rev >= min_rev) %>%                          # drop vessels with < min_rev
    filter(year != 2011) %>%
    group_by(drvid) %>%
    summarize(both.periods = ifelse(any(year %in% c(2008:2010)) & any(year %in% c(2012:2014)), 1, 0))
  
  div_landings <- subset(dat, drvid %in% unique(div_dat$drvid) & 
                           year!=2011) %>% left_join(div_dat, by = 'drvid')
  
  saveRDS(div_landings,
          file="Analysis/new_analysis/metiers/analysis/vessel_landings_data.RDS")
  return(div_landings)
}