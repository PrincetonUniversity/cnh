# troubleshooting (spurious) correlations

# working directory should be set to CNH/
# for me that's:
  setwd("/Users/efuller/Desktop/CNH/")
  library(dplyr); library(tidyr); library(ggplot2)

# load data ----
tickets <- readRDS("processedData/catch/1_cleaningData/tickets.RDS")

# rebuild correlation matrices ----

# get top 10 metiers since that's what we were looking at
top10 <- tickets %>%
  group_by(metier.2010) %>%
  summarise(dollars = sum(adj_revenue, na.rm=TRUE), pounds = sum(landed_wt, na.rm=TRUE)) %>%
  arrange(-dollars) %>% # sort from most to least revenue
  ungroup() %>% 
  mutate(percent.rev = 100*cumsum(as.numeric(dollars))/sum(as.numeric(dollars)), percent.pounds = 100*cumsum(as.numeric(pounds))/sum(as.numeric(pounds))) %>% 
  filter(percent.rev < 91) 

# reproduce data frame with vessel - year - number of metiers participated in
df <- tickets %>%
  dplyr::select(drvid, year, metier.2010) %>%
  distinct() %>% # give me unique rows
  arrange(drvid, year) %>%
  group_by(drvid, year) %>%
  summarise(n.metiers = length(unique(metier.2010)))

df.cor <- tickets %>%
  filter(metier.2010 %in% top10$metier.2010) %>%
  group_by(drvid, year, metier.2010) %>%
  summarise(revenue = sum(adj_revenue, na.rm=TRUE), pounds = sum(landed_wt, na.rm=TRUE), trips = length(unique(trip_id))) %>%
  left_join(df)

df.cor.rev <- df.cor %>%
  dplyr::select(-pounds, - trips) %>%
  spread(metier.2010, revenue) 

df.cor.lbs <- df.cor %>%
  dplyr::select(-revenue, -trips) %>%
  spread(metier.2010, pounds)

df.cor.trips <- df.cor %>%
  dplyr::select(-revenue, -pounds) %>%
  spread(metier.2010, trips) 

# crab and sardine
df.cor.trips[which(!is.na(df.cor.trips$POT_1) & !is.na(df.cor.trips$NET_2)),]
table(df.cor.rev[which(!is.na(df.cor.rev$POT_1) & !is.na(df.cor.rev$NET_2)),c("drvid","year")])
# only 3 boats that do it consistently out of 16 (>3 years). others do it once or twice
# also these boats are mostly doing sardine and adding a bit of crab

# crab and urchin ----
# first see who does it
as.data.frame(df.cor.trips[which(!is.na(df.cor.trips$POT_1) & !is.na(df.cor.trips$MSC_1)),])
# looking at some of those top ones, did they really do msc_1 that one time
subset(tickets, drvid == "245645" & metier.2010 == "MSC_1", 
       select = c("drvid","year","modified","metier.2010","adj_revenue","grgroup","grid")) # this one was octopus in a pot trap. 
subset(tickets, drvid == "295549" & metier.2010 == "MSC_1", 
       select = c("drvid","year","modified","metier.2010","adj_revenue","grgroup","grid")) # this one was octopus in a pot trap. 
subset(tickets, drvid == "544392" & metier.2010 == "MSC_1", 
       select = c("drvid","year","modified","metier.2010","adj_revenue","grgroup","grid")) # this one was octopus in a pot trap. 
subset(tickets, drvid == "624681" & metier.2010 == "MSC_1", 
       select = c("drvid","year","modified","metier.2010","adj_revenue","grgroup","grid")) # this one was octopus in a pot trap. 
# all of these do have grgroup = MSC, but grid = OTH rather than DVG. Wonder if this is worth pulling out
subset(tickets, drvid == "CF2335GS" & metier.2010 == "MSC_1", 
       select = c("drvid","year","modified","metier.2010","adj_revenue","grgroup","grid")) # this one is legit, RURC and DVG.
subset(tickets, drvid == "978135" & metier.2010 == "MSC_1", 
       select = c("drvid","year","modified","metier.2010","adj_revenue","grgroup","grid")) # this one is legit, RURC. But grid == OTH. gah

subset(tickets, drvid == "958865" & metier.2010 == "MSC_1", 
       select = c("drvid","year","modified","metier.2010","adj_revenue","grgroup","grid")) # this one is legit, RURC and DVG

# how often do they do it? 0 = didn't do both in that year, 1 = did do both in that year
table(df.cor.rev[which(!is.na(df.cor.rev$POT_1) & !is.na(df.cor.rev$MSC_1)),c("drvid","year")])
# only 10 vessels do it, and only 5 do it more than once

# take away on crab and urchin: worrying problem with bycatch species 
# possible to come up with rule like if < 20% of trips/revenue and happened once. probably not real?
# that would leave the CF2335GS boat but drop the others. Would also mean you have fewer than 10
# vessels that do this. 

# crab and sardine ----
as.data.frame(df.cor.trips[which(!is.na(df.cor.trips$POT_1) & !is.na(df.cor.trips$NET_2)),])

# looking at a few
# this one only had one POT_1 trip, see if it's real
subset(tickets, drvid == "1061917" & metier.2010 == "POT_1" ) # this one is legit, is DCRB
# this one only did one NET_2 which seemed weird. so check
subset(tickets, drvid == "556454" & metier.2010 == "NET_2" ) # this one is legit too, gillnetter out of SF. 
# realizet that NET_2 includes purse-seines and gillnets. Gillnets much smaller boats and more local
# easier to imagine them doing sardine. 

# this is a big guy since doing seine but doesn't do either very much. but looks real.
subset(tickets, drvid == "1230071" & metier.2010 == "NET_2" ) 

# take away, this is real combination, no problem with bycatch. 
table(df.cor.rev[which(!is.na(df.cor.rev$POT_1) & !is.na(df.cor.rev$NET_2)),c("drvid","year")])
# but not many boats do it frequently together every year. I think most of these boats are mainly sardine 
# with some crab on the side. but a few do more crab than sardine. 

# lobster and chinook ----
as.data.frame(df.cor.trips[which(!is.na(df.cor.trips$POT_2) & !is.na(df.cor.trips$TLS_1)),])
table(df.cor.rev[which(!is.na(df.cor.rev$POT_2) & !is.na(df.cor.rev$TLS_1)),c("drvid","year")])
# only 7 vessels total, not a lot. and only 2011-2013. Maybe that's when there was enough
# salmon to catch?

# looking at a few assymetric ones
subset(tickets, drvid == "544831" & metier.2010 == "POT_2" ) # looks real
subset(tickets, drvid == "928057" & metier.2010 == "TLS_1" ) # looks real, out of BRG and catching chinook
# took home about 2600, probably a good trip. 

# this makes sense to me since they're both marketable and trophy-esque species. 

# lobster and sablefish ----
as.data.frame(df.cor.trips[which(!is.na(df.cor.trips$POT_2) & !is.na(df.cor.trips$HKL_1)),])
table(df.cor.rev[which(!is.na(df.cor.rev$POT_2) & !is.na(df.cor.rev$HKL_1)),c("drvid","year")])
# lots of boats, fair number of people doing it multiple years

# look at a few
subset(tickets, drvid == "CF0576SN" & metier.2010 == "POT_2" ) # looks real, out of SB and catching pot_2
subset(tickets, drvid == "CF0576SN" & metier.2010 == "HKL_1" ) # looks real, out of SB and catching sable, thds, sspn, etc. 
subset(tickets, drvid == "CF4476ET" & metier.2010 == "HKL_1" ) # looks real

# again both very profitable species. seems real

# urchin and chinook ----
as.data.frame(df.cor.trips[which(!is.na(df.cor.trips$MSC_1) & !is.na(df.cor.trips$TLS_1)),])
table(df.cor.rev[which(!is.na(df.cor.rev$MSC_1) & !is.na(df.cor.rev$TLS_1)),c("drvid","year")])
# hm looks not good. few boats adn only one guy that does it all the years. 

# look in depth
subset(tickets, drvid == "245645" & metier.2010 == "MSC_1" ) # octopus from newport
subset(tickets, drvid == "254755" & metier.2010 == "MSC_1" ) # real, rurc, dvg from brg
subset(tickets, drvid == "509680" & metier.2010 == "MSC_1" ) # real, rurc, dvg from brg
subset(tickets, drvid == "609556" & metier.2010 == "MSC_1" ) # real, rurc, dvg from mro
subset(tickets, drvid == "CF1718SZ" & metier.2010 == "MSC_1" ) # real, rurc, dvg from alb
subset(tickets, drvid == "CF8277SM" & metier.2010 == "TLS_1" ) # real, chnk, tls from brg

# then look at the guy that does it every year
df.cor.trips[which(df.cor.trips$drvid=="CF7496FF"),]

# aside from one that was due to octopus, rest look good. it's rare, but is done. 

# take aways: need to think about problem of octopus in MSC_1 for pot_1 and msc_1 correlations. 

