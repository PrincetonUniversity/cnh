# calculate annual revenue for each vessel
# filter data ----
# load data
  dat <- readRDS("/Users/efuller/1/CNH/processedData/catch/1_cleaningData/tickets.RDS")

  library(dplyr)

# filter data: ----
  # vessels should have less than min_rev on average across 5 years AND
  # landings before and after catch shares
  # then drop 2011 data
  # also drop any ZZZ drvids

  min_rev = 5000

  div_dat <- dat[-grep("ZZZ",dat$drvid)] %>%                    # drop ZZZ vessels
    group_by(drvid, year) %>%                                   
    summarize(annual_revenue = sum(adj_revenue, na.rm = T)) %>% # calculate yr rev
    mutate(av.annual.rev = mean(annual_revenue, na.rm = T)) %>%     # mean yr rev
    filter(av.annual.rev >= min_rev) %>%           # drop vessels with < min_rev
    filter(year != 2011) %>%
    mutate(post.itq = ifelse(year>2011, 1, 0)) %>%  # mark before and after itqs
    group_by(drvid) %>%
    mutate(both = ifelse(length(unique(post.itq))==2, 1, 0)) %>% # mark ves present both
    filter(both == 1)
  
    num.ves = length(unique(div_dat$drvid)) # number of vessels considered
    percent.lbs = with(subset(dat, drvid %in% unique(div_dat$drvid) & year!=2011), sum(as.numeric(landed_wt), na.rm = T))/with(subset(dat, year!=2011), sum(as.numeric(landed_wt), na.rm = T))
    percent.dollars = with(subset(dat, drvid %in% unique(div_dat$drvid) & year!=2011), sum(as.numeric(adj_revenue), na.rm = T))/with(subset(dat, year!=2011), sum(as.numeric(adj_revenue), na.rm = T))
    
    save(num.ves, percent.lbs, percent.dollars, min_rev, file = "/Users/efuller/1/CNH/Analysis/Metiers/bin/vessel_subset_stats.Rdata")
    
    saveRDS(subset(dat, drvid %in% unique(div_dat$drvid) & year!=2011), "/Users/efuller/1/CNH/Analysis/Metiers/bin/diversity_landings_data.RDS")
# calculate diversity ----
# calculate simpsons, shannons, effective simpsons, effective shannons 
# for metiers by pre/post
library(vegan)
  
  metier10_div <- subset(dat, drvid %in% unique(div_dat$drvid) & year!=2011) %>%
    mutate(post.itq = ifelse(year %in% c(2012, 2013), 1, 0)) %>%
    group_by(drvid, post.itq, metier.2010) %>%
    summarize(revenue = sum(adj_revenue)) %>%
    group_by(drvid, post.itq) %>%
    summarize(shannon = diversity(revenue, index = 'shannon'),
              simpson = diversity(revenue, index = 'simpson'),
              eff.shannon = exp(diversity(revenue, index = 'shannon')),
              eff.simpson = 1/(1-diversity(revenue, index = 'simpson'))) %>%
    group_by(drvid) %>%
    mutate(delta.shannon = diff(shannon), delta.simpson = diff(simpson),
           delta.eff.shannon = diff(eff.shannon), 
           delta.eff.simpson = diff(eff.simpson)) %>%
    filter(post.itq == 0) %>%
    select(-post.itq) 
  colnames(metier10_div) <- paste(colnames(metier10_div), "2010",sep="_")
  colnames(metier10_div)[which(colnames(metier10_div)=="drvid_2010")] <- "drvid"
  
# do same thing, but using 2012 metiers
    div_landings <- subset(dat, drvid %in% unique(div_dat$drvid) & year!=2011) %>%
      mutate(post.itq = ifelse(year %in% c(2012, 2013), 1, 0)) %>%
      group_by(drvid, post.itq, metier.2012) %>%
      summarize(revenue = sum(adj_revenue)) %>%
      group_by(drvid, post.itq) %>%
      summarize(shannon_2012 = diversity(revenue, index = 'shannon'),
                simpson_2012 = diversity(revenue, index = 'simpson'),
                eff.shannon_2012 = exp(diversity(revenue, index = 'shannon')),
                eff.simpson_2012 = 1/(1-diversity(revenue, index = 'simpson'))) %>%
      group_by(drvid) %>%
      mutate(delta.shannon_2012 = diff(shannon_2012), delta.simpson_2012 = diff(simpson_2012),
             delta.eff.shannon_2012 = diff(eff.shannon_2012), 
             delta.eff.simpson_2012 = diff(eff.simpson_2012)) %>%
      filter(post.itq == 0) %>%
      left_join(metier10_div)
  
# got effective diversity transformations from 
# http://jonlefcheck.net/2012/10/23/diversity-as-effective-numbers/

# mark for IFQ participation, hake boats, and single fisheries ----
  ifq_yes = unique(dat$drvid[which(dat$ifq_landing=="Y")])
  hake_yes = unique(dat$drvid[which(dat$metier.2010=="TWL_4")])
  div_landings$ifq <- factor(ifelse(div_landings$drvid %in% ifq_yes, 1, 0))
  div_landings$hake <- factor(ifelse(div_landings$drvid %in% hake_yes, 1, 0))
 
   single_boats <- dat %>%
    group_by(drvid) %>%
    summarize(n.fisheries_2010 = length(unique(metier.2010)),
              n.fisheries_2012 = length(unique(metier.2012)))
  div_landings$single.fishery_2010 <- factor(ifelse(div_landings$drvid %in% single_boats$drvid[which(single_boats$n.fisheries_2010==1)], 1, 0))
  div_landings$single.fishery_2012 <- factor(ifelse(div_landings$drvid %in% single_boats$drvid[which(single_boats$n.fisheries_2012==1)], 1, 0))
  
  # save data
    saveRDS(div_landings, "/Users/efuller/1/CNH/Analysis/Metiers/bin/vessel_diversity_landings.RDS")
    