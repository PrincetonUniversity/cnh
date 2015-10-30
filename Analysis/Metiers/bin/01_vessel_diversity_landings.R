# calculate annual revenue for each vessel
library(dplyr)

# rename and set up additional data sources (registration, ports) ----
# ports
  ports <- read.csv("/Volumes/LA-PRIVATE/CNH/processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE)
  ports <- rename(ports, pcid = Pcid)

# length data
  # federal registration
    cg1 <- read.csv("/Volumes/LA-PRIVATE/CNH/rawData/Catch/vessel_registration/CG_2009-2012_woc_141210_three.csv",
                    stringsAsFactors = FALSE, skip = 2)
    cg2 <- read.csv("/Volumes/LA-PRIVATE/CNH/rawData/Catch/vessel_registration/CG_2013_woc_141210_three.csv",
                    stringsAsFactors = FALSE, skip = 2)
    cg <- rbind(cg1, cg2); rm(cg1, cg2) # lots of blank rows in there
    cg <- cg[-which(cg$pubyr < 2009 | cg$pubyr > 2013),]
    cg_vessels <- unique(cg[,c("hull_number","vessel_id","vessel_name","pubyr",
                               "length","horsepower","hp_main_astern","breadth",
                               "depth")])
  # state registration
    sv <- read.csv("/Volumes/LA-PRIVATE/CNH/rawData/Catch/vessel_registration/SV_2009-2013_woc_141210_two.csv", 
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
    

# filter landings data ----
# load data
  dat <- readRDS("/Volumes/LA-PRIVATE/CNH/processedData/catch/1_cleaningData/tickets.RDS")

  # vessels should have less than min_rev on average across 5 years AND
  # landings before and after catch shares
  # then drop 2011 data
  # also drop any ZZZ drvids

  min_rev = 5000
  
  div_dat <- dat[-grep("ZZZ",dat$drvid)] %>%                    # drop ZZZ vessels
    left_join(ports) %>%
    group_by(drvid, year) %>%                                   
    summarize(annual_revenue = sum(adj_revenue, na.rm = T),
              average_lat = mean(lat, na.rm = T)) %>% # calculate yr rev
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
    
    save(num.ves, percent.lbs, percent.dollars, min_rev, 
         file = "/Volumes/LA-PRIVATE/CNH/Analysis/Metiers/bin/vessel_subset_stats.Rdata")
    
    saveRDS(subset(dat, drvid %in% unique(div_dat$drvid) & year!=2011),
            file="/Volumes/LA-PRIVATE/CNH/Analysis/Metiers/bin/data_output/diversity_landings_data.RDS")
    

# calculate diversity ----
# calculate simpsons, shannons, effective simpsons, effective shannons, put together with length/lat
# for metiers by pre/post
    
    # calc vessel length
    # merge vessel length here so we can incorporate yearly changes
    # keep only boats that are in dataset
    length_data <- sv_cg[which(sv_cg$drvid %in% unique(div_dat$drvid)),]
    length_ref <- length_data %>%
      group_by(drvid) %>%
      summarize(len = mean(length.x, length.y, na.rm = TRUE, trim = 0),
                hp = mean(horsepower.x, horsepower.y, hp_main_astern,trim = 0, na.rm = TRUE),
                weight = mean(weight, na.rm = T, trim = 0),
                breadth = mean(breadth, na.rm = T, trim = 0),
                depth = mean(depth, na.rm = T, trim = 0))
    
    # get average location
    loc_ref <- div_dat %>%
      group_by(drvid) %>%
      summarize(lat = mean(average_lat, na.rm = T), 
                annual.revenue = mean(av.annual.rev,na.rm=T),
                sd.revenue = sd(av.annual.rev, na.rm = T))
    
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
      left_join(metier10_div) %>%
      left_join(length_ref) %>%
      left_join(loc_ref)
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
  
  
# also save version with eff.shannon for pre and post with hake and ifq measured
  div_periods <- subset(dat, drvid %in% unique(div_dat$drvid) & year!=2011) %>%
    mutate(post.itq = ifelse(year %in% c(2012, 2013), 1, 0)) %>%
    group_by(drvid, post.itq, metier.2010) %>%
    summarize(revenue = sum(adj_revenue)) %>%
    group_by(drvid, post.itq) %>%
    summarize(eff.shannon = exp(diversity(revenue, index = 'shannon')))
  
  div_periods$single_fishery <- factor(ifelse(div_periods$drvid %in% single_boats$drvid[which(single_boats$n.fisheries_2010==1)], 1, 0))
  div_periods$ifq <- factor(ifelse(div_periods$drvid %in% ifq_yes, 1, 0))
  div_periods$hake <- factor(ifelse(div_periods$drvid %in% hake_yes, 1, 0))
         
  ggplot(subset(div_periods, hake ==0), aes(x = as.factor(post.itq), y = eff.shannon)) + geom_boxplot()
  t.test(eff.shannon ~ as.factor(post.itq), subset(div_periods, hake ==0))
  
  t.test(eff.shannon ~ as.factor(post.itq), subset(div_periods, ifq == 1 & hake == 0))
  
    
# save data ----
  saveRDS(div_landings, "/Volumes/LA-PRIVATE/CNH/Analysis/Metiers/bin/data_output/vessel_diversity_landings.RDS")
    
