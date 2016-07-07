# calculate independent and response variables for vessel level 
calc_vessel_vars <- function(){
  library(dplyr); library(tidyr)
  vessel_landings <- readRDS("Analysis/Metiers/bin/04_data_output/vessel_landings_data.RDS")
  
# vessel length ----
    # federal registration
    cg1 <- read.csv("rawData/Catch/vessel_registration/CG_2009-2012_woc_141210_three.csv",
                    stringsAsFactors = FALSE, skip = 2)
    cg2 <- read.csv("rawData/Catch/vessel_registration/CG_2013_woc_141210_three.csv",
                    stringsAsFactors = FALSE, skip = 2)
    cg <- rbind(cg1, cg2); rm(cg1, cg2) # lots of blank rows in there
    cg <- cg[-which(cg$pubyr < 2009 | cg$pubyr > 2013),]
    cg_vessels <- unique(cg[,c("hull_number","vessel_id","vessel_name","pubyr",
                               "length","horsepower","hp_main_astern","breadth",
                               "depth")])
    # state registration
    sv <- read.csv("rawData/Catch/vessel_registration/SV_2009-2013_woc_141210_two.csv", 
                   stringsAsFactors = FALSE, skip = 2)
    colnames(sv)[1] <- "year"
    sv$year <- as.integer(sv$year) # NAs are real, remove
    sv <- sv[-which(is.na(sv$year)),]
    sv_vessels <- unique(sv[,c("year","svid","plate","name","length",
                               "weight","horsepower","charterboat")])
    # combining vessel length data
    sv_cg <- merge(sv_vessels, cg_vessels, by.x = c("svid","year"), 
                   by.y = c("vessel_id", "pubyr"), all.x = TRUE, all.y = TRUE)
    sv_cg <- rename(sv_cg, drvid = svid)
    
    # keep only boats that are in diversity dataset
    length_data <- sv_cg[which(sv_cg$drvid %in% unique(vessel_landings$drvid)),]
    length_ref <- length_data %>%
      group_by(drvid) %>%
      summarize(len = mean(length.x, length.y, na.rm = TRUE, trim = 0),
                hp = mean(horsepower.x, horsepower.y, hp_main_astern,trim = 0, na.rm = TRUE),
                weight = mean(weight, na.rm = T, trim = 0),
                breadth = mean(breadth, na.rm = T, trim = 0),
                depth = mean(depth, na.rm = T, trim = 0))
    
    
# average latitude of landings (pre, post and overall) ----
    ports <- read.csv("processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE)
    ports <- rename(ports, pcid = Pcid)
    
    # get average location
    loc_ref <- vessel_landings %>%
      left_join(ports, by = 'pcid') %>%
      dplyr::select(drvid, trip_id, lat, year) %>%
      distinct() %>%
      mutate(period = ifelse(year<2011, "before","after")) %>%
      group_by(drvid, period) %>%                                   
      summarize(average_lat = mean(lat, na.rm = T)) %>%
      spread(period, average_lat) %>%
      group_by(drvid) %>%
      mutate(overall.lat = mean(c(before, after), na.rm = T)) %>%
      rename(after.lat = after, before.lat = before)

# revenue prior and post 2011, average overall ----
    rev_ref <- vessel_landings %>%
      mutate(period = ifelse(year<2011, "before","after")) %>%
      group_by(drvid, period) %>%
      summarize(rev = sum(adj_revenue, na.rm = T)) %>%
      spread(period, rev) %>%
      group_by(drvid) %>%
      mutate(overall.rev = mean(c(before, after), na.rm = T)) %>%
      rename(after.rev = after, before.rev = before)

# flags: alaska, ifq participation, california halibut participation ----
    ak <- read.csv("Analysis/Metiers/data/WC_drvids_alaska.csv",stringsAsFactors = FALSE)
    ak <- filter(ak, drvid %in% unique(vessel_landings$drvid))
    
    # mark trawl participation beforehand. 
    # Will underestimate because can't reliably distinguish open access TWL_2 from LE
    
    twl_prior = vessel_landings %>%
      dplyr::select(trip_id, drvid, metier.2010, year) %>%
      distinct() %>%
      filter(year < 2011) %>%
      group_by(drvid) %>%
      summarize(twl_prior = ifelse(any(metier.2010 %in% c("TWL_1","TWL_5","TWL_6","TWL_7","TWL_8","TWL_9","TWL_10","TWL_11","TWL_12","TWL_13","TWL_14")), 1, 0))
    
    quota_post = vessel_landings %>%
      dplyr::select(trip_id, drvid, ifq_landing, year) %>%
      distinct() %>%
      filter(year > 2011) %>%
      group_by(drvid) %>%
      summarize(itq_post = ifelse(any(ifq_landing == "Y"), 1, 0))
    
    twl_partip = full_join(twl_prior, quota_post)
    twl_partip$ifq_flag = NA
    # this means they didn't fish in twl before and didn't land itqs after but 
    # because both values are 0, means they're in the dataset both before and 
    # after 2011. 
    twl_partip$ifq_flag[which(twl_partip$twl_prior==0 & twl_partip$itq_post == 0)] = "general fleet"
    # this means they didn't fish in twl before but did land itqs after 2011
    twl_partip$ifq_flag[which(twl_partip$twl_prior==0 & twl_partip$itq_post == 1)] = "itq entrant: general fleet"
    # this means they didn't fish twl before, but aren't found in the post 2011
    # dataset
    twl_partip$ifq_flag[which(twl_partip$twl_prior==0 & is.na(twl_partip$itq_post))] = "general fleet exit"
    # this means they fished twl before, but don't land any quota post 2011
    twl_partip$ifq_flag[which(twl_partip$twl_prior==1 & twl_partip$itq_post==0)] = "LE gf exit, still fish"
    # this means they fished in twl before and land quota after
    twl_partip$ifq_flag[which(twl_partip$twl_prior==1 & twl_partip$itq_post==1)] = "itq stay on"
    # this means they fished in twl before don't land quota (or anything else) after
    twl_partip$ifq_flag[which(twl_partip$twl_prior==1 & is.na(twl_partip$itq_post))] = "LE gf total exit"
    # this means they weren't landing anything before 2011, but now land some quota
    twl_partip$ifq_flag[which(is.na(twl_partip$twl_prior) & twl_partip$itq_post==1)] = "itq entrant"
    # didn't fish before, now fish in non itq fleet after
    twl_partip$ifq_flag[which(is.na(twl_partip$twl_prior) & twl_partip$itq_post==0)] = "general fleet entrant"
    
    twl_partip <- dplyr::select(twl_partip, drvid, ifq_flag)
    
    period_ref <- vessel_landings %>%
      dplyr::select(drvid, both.periods) %>%
      distinct()
    
    c.halibut_ref <- vessel_landings %>%
      dplyr::select(trip_id, drvid, metier.2010, year) %>%
      distinct() %>%
      filter(year < 2011) %>%
      group_by(drvid) %>%
      summarize(c.halibut = ifelse(any(metier.2010 == "TWL_2"), 1, 0))

 
# diversity ----
    library(vegan)
    
    metier10_div <- vessel_landings %>%
      filter(both.periods==1) %>%
      mutate(period = ifelse(year<2011, 0,1)) %>%
      group_by(drvid, period, metier.2010) %>%
      summarize(revenue = sum(adj_revenue)) %>%
      group_by(drvid, period) %>%
      summarize(shannon = diversity(revenue, index = 'shannon'),
                simpson = diversity(revenue, index = 'simpson'),
                eff.shannon = exp(diversity(revenue, index = 'shannon')),
                eff.simpson = 1/(1-diversity(revenue, index = 'simpson'))) %>%
      group_by(drvid) %>%
      mutate(delta.shannon = diff(shannon), delta.simpson = diff(simpson),
             delta.eff.shannon = diff(eff.shannon), 
             delta.eff.simpson = diff(eff.simpson)) %>%
      filter(period == 0) %>%
      dplyr::select(-period) 
    colnames(metier10_div) <- paste(colnames(metier10_div), "2010",sep="_")
    colnames(metier10_div)[which(colnames(metier10_div)=="drvid_2010")] <- "drvid"
    
    # do same thing, but using 2012 metiers
    div_metrics <- vessel_landings %>%
      filter(both.periods==1) %>%
      mutate(period = ifelse(year<2011, 0,1)) %>%
      group_by(drvid, period, metier.2012) %>%
      summarize(revenue = sum(adj_revenue)) %>%
      group_by(drvid, period) %>%
      summarize(shannon_2012 = diversity(revenue, index = 'shannon'),
                simpson_2012 = diversity(revenue, index = 'simpson'),
                eff.shannon_2012 = exp(diversity(revenue, index = 'shannon')),
                eff.simpson_2012 = 1/(1-diversity(revenue, index = 'simpson'))) %>%
      group_by(drvid) %>%
      mutate(delta.shannon_2012 = diff(shannon_2012), delta.simpson_2012 = diff(simpson_2012),
             delta.eff.shannon_2012 = diff(eff.shannon_2012), 
             delta.eff.simpson_2012 = diff(eff.simpson_2012)) %>%
      filter(period == 0) %>%
      dplyr::select(-period) %>%
      left_join(metier10_div)
    
    # got effective diversity transformations from 
    # http://jonlefcheck.net/2012/10/23/diversity-as-effective-numbers/

    
# change in composition ----
    le_boats <- vessel_landings %>%
      filter(both.periods==1) %>% 
      mutate(period = ifelse(year < 2011, 0,1)) %>%
      group_by(drvid, period,metier.2010) %>%
      summarize(ntrips=length(unique(trip_id)), nrev = sum(adj_revenue), nlbs = sum(landed_wt)) %>%
      group_by(drvid, period) %>%
      summarize(nfisheries = length(unique(metier.2010)), rev = sum(nrev), lbs = sum(nlbs)) %>%
      group_by(drvid) %>%
      summarize(delta.nfisheries = diff(nfisheries))
    
    comp <- vessel_landings %>%
      filter(both.periods==1) %>%
      mutate(period=ifelse(year<2011, 0, 1)) %>%
      select(drvid, period, metier.2010) %>%
      distinct() %>%
      group_by(drvid) %>%
      summarize(composition = ifelse(all(metier.2010[period==0] %in% metier.2010[period==1]) & all(metier.2010[period==1] %in% metier.2010[period==0]), "unchanged",
                                     ifelse(any(!(metier.2010[period==0] %in% metier.2010[period==1])) & 
                                              any(!(metier.2010[period==1] %in% metier.2010[period==0])), 
                                            "added and lost",
                                            ifelse(any(!(metier.2010[period==0] %in% metier.2010[period==1])) & all(metier.2010[period==1] %in% metier.2010[period==0]), "lost",
                                                   ifelse(any(!(metier.2010[period==1] %in% metier.2010[period==0])) & all(metier.2010[period==0] %in% metier.2010[period==1]), "gained",NA))))) %>%
      left_join(le_boats)
    
# merge all variables together ----
    vessel_stats <-  ak %>%
      full_join(twl_partip) %>%
      full_join(length_ref) %>%
      full_join(loc_ref) %>%
      full_join(rev_ref) %>%
      full_join(period_ref) %>%
      full_join(c.halibut_ref) %>%
      full_join(div_metrics) %>%
      full_join(comp)
    
    vessel_stats$alaska[is.na(vessel_stats$alaska)] <- 0

# add some descriptive categories
    vessel_stats$zone <- cut(x = vessel_stats$overall.lat, 
                            breaks = rev(c(32, 36, 40, 43, 49)), 
                            labels = c("N. Cape Blanco", 
                                       "Cape Blanco - Cape Mendocino", 
                                       "Cape Mendocino - Point Sur", 
                                       "S. Point Sur"), include.lowest = TRUE)
    vessel_stats$type <- ifelse(vessel_stats$eff.shannon_2010==1, "specialist", "generalist")

    
# save data ----    
    saveRDS(vessel_stats, "Analysis/Metiers/bin/04_data_output/vessel_stats.RDS")
    return(vessel_stats)
  }