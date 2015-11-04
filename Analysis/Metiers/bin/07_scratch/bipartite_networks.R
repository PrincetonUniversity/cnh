# new version of ports that does bipartite network
library(dplyr)
# filter landings data for vessels with min 5000 average annual revenue and ZZZ vessels, and vessels that are present before and after ITQs

# load data ----
dat <- readRDS("/Volumes/LA-PRIVATE/CNH/processedData/catch/1_cleaningData/tickets.RDS")

ports <- read.csv("/Volumes/LA-PRIVATE/CNH/processedData/spatial/ports/all_ports.csv", stringsAsFactors = FALSE)
ports <- rename(ports, pcid = Pcid)
# filter vessels ----
# vessels should have less than min_rev on average across 5 years AND
# landings before and after catch shares
# then drop 2011 data
# also drop any ZZZ drvids

min_rev = 5000

retain_vessels <- dat[-grep("ZZZ",dat$drvid)] %>%              # drop ZZZ vessels
  group_by(drvid, year) %>%                                   
  summarize(annual_revenue = sum(adj_revenue, na.rm = T)) %>% # calculate yr rev
  mutate(av.annual.rev = mean(annual_revenue, na.rm = T)) %>%     # mean yr rev
  filter(av.annual.rev >= min_rev) %>%           # drop vessels with < min_rev
  filter(year != 2011) %>%
  mutate(post.itq = ifelse(year>2011, 1, 0)) %>%  # mark before and after itqs
  group_by(drvid) %>%
  mutate(both = ifelse(length(unique(post.itq))==2, 1, 0)) %>% # mark ves present both
  filter(both == 1) %>%
  select(drvid) %>%
  distinct()

retain_vessels <- as.vector(retain_vessels$drvid)

# for each of these retained vessels
# merge with length/hp data ----
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

# retain only boats in retain_vessels

length_data <- sv_cg[which(sv_cg$drvid %in% unique(retain_vessels)),]
length_ref <- length_data %>%
  group_by(drvid) %>%
  summarize(len = mean(length.x, length.y, na.rm = TRUE, trim = 0),
            hp = mean(horsepower.x, horsepower.y, hp_main_astern,trim = 0, na.rm = TRUE),
            weight = mean(weight, na.rm = T, trim = 0),
            breadth = mean(breadth, na.rm = T, trim = 0),
            depth = mean(depth, na.rm = T, trim = 0))
# calculate before and after diversity based on 2010 metiers ----
library(vegan)
metier10_div <- subset(dat, drvid %in% retain_vessels & year!=2011) %>%
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
# calculate delta diversity
# mark max port, and percent at that max port prior to catch shares
# also get average latitude of landings prior to catch shares
# mark for hake boats, IFQ participation
# mark for boats that did TWL_1 but didn't land quota post IFQ (or didn't land TWL_1 post ITQ)?

# make port networks
# for each port present in the above list, use data from these boats to construct bipartite network. pre and post. Save as list

# for each network calculate statistics
# edge weight is considered as a distance/cost. So take 1/proportion revenue to transform into cost. Thus the smallest costs are close to 1, anything < 1 turns into a value > 1
# also check whether any quota was landed
# calculate delta stats

# merge port stats to vessel div stats

# look for new additions
# for each vessel that did TWL_1 prior to 2011 but not post, check to see if they added, lost, and total change in number of fisheries participated in before and afte.r also calculate BC similarity in revenue composition pre and post,

# is new entrant fisheries predicted? is the nearest fishery that hte vessel presently is NOT in one of any of the  newly added fisheries present post 2011?
# subset to vessels that did TWL_1 prior to 2011 and don't land quota post. 
# for each of these vessels 



