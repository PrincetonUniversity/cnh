# filter fish ticket data to drop rare species
# changes: now adjust for 2009 before do filtering, also grouping by drvid not veid. 

library(reshape2); library(plyr); library(dplyr); library(scales)

# script does two things: finds mean annual income and removes vessels which have less than 5000 annual income. then after those trips removed, looks for species rarely caught and removes those also. 

# load data
#----
  ftl <- read.csv(
    "/Users/efuller/1/CNH/Data/Catch/FTL_2009-2013_w-o-c_samhouri.csv", 
    stringsAsFactors = F)
  # because ftid is not unique by year, compile ftid and year together
  ftl$trip_id <- paste0(ftl$ftid, ftl$year)

# remove vessel 0, *****, and UNKNOWN
ftl <- ftl[-grep(pattern = "[****]",ftl$veid),]
ftl <- ftl[-which(ftl$veid %in% c("0","UNKNOWN")),]
#----
# Part I: find annual revenue, remove vessels which have average revenue < $5000
#----
# Adjust income to 2009 using CPI (see here http://stackoverflow.com/questions/12590180/inflation-adjusted-prices-package)
library(quantmod)
getSymbols("CPIAUCSL", src="FRED") # get CPI. The CPI inflation calculator at the BLS uses the latest monthly value for a given year. 
library(lubridate)

ftl$revenue <- ftl$ppp *ftl$landed_wt

annual.cpi <- CPIAUCSL[.indexmon(CPIAUCSL)==11] # it's two lagged... but want jan for each year

cf <- annual.cpi/as.numeric(annual.cpi['2009']) #using 2009 as the base year
# need to change cf from december to january 
cf <- as.data.frame(cf)
cf$year = strftime(as.Date(row.names(cf)),"%Y")
ftl <- merge(ftl, cf, by = "year", all=FALSE)
ftl$adj_revenue <- ftl$revenue / ftl$CPIAUCSL

grouped_ves <- group_by(ftl, drvid, year)
revenues <- summarise(grouped_ves, revenue = sum(adj_revenue))
annual_rev <- summarise(revenues, mean_annual = mean(revenue))
vessel_drops <- annual_rev$drvid[which(annual_rev$mean_annual < 5000)]
# loose about a third of vessels
ftl_major <- ftl[-which(ftl$drvid %in% vessel_drops),]
rm(ftl)
#----
# merge nominal and species market categories
#----
  # load species ids
  spid <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/data/spid.csv", 
                   stringsAsFactors=F)

  spid <- spid[which(spid$X==1),]
  species <- spid$common_name
  species <- gsub("NOM. ", "", species)
  spid$common <- species

  # build species key
  nominal <- spid[grep("NOM.", spid$common_name),]
  nominal <- select(nominal, SPID, common)
  colnames(nominal) <- c("nominal", "common")
  reg <- spid[-grep("NOM.",spid$common_name),]
  reg <- select(reg, SPID, common)
  colnames(reg) <- c("reg", "common")

  species.key <- merge(reg, nominal, by = "common", all.x = TRUE, all.y = TRUE)

  # make loop through species key for those with nominal spids
  inds <- which(!is.na(species.key$nominal))
  new_df <- ftl_major
  new_df$modified <- NA
  for(i in inds){
    new_df$modified[which(new_df$spid==species.key$nominal[i])] <- species.key$reg[i]
    cat(i," ")
  }

  new_df$modified <- ifelse(is.na(new_df$modified), new_df$spid, new_df$modified)
#----
# first make trip table (col = sp, row = trip)
#----
  # prepare for recasting
  melt_ftl <- melt(new_df, 
                   id.vars = c("drvid","trip_id","modified","tdate","grid"), 
                   measure.vars = "landed_wt")
  saveRDS(melt_ftl, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/melt_ftl.RDS")
  # not fast transposing
  cast_ftl <- dcast(melt_ftl, trip_id ~ modified, fun.aggregate = sum)
#----
# Using threshold values for number trips and median pounds per catch, find species to drop
#----
# what about the number of trips these species show up in. Assuming that there is only one species entry for each trip
  num_trips <- table(melt_ftl$modified)
  # conditions for dropping should be that it's rarely caught, and of the times that it is caught it's always in small amounts. Avoids things like yellowfin tuna, who is sometimes caught in large volumes and for which there is a market.

  medians <- apply(cast_ftl[2:ncol(cast_ftl)], 2, function(x) median(x[x>0]))
  # when sp is present in catch, take median. 
  sd <- apply(cast_ftl[2:ncol(cast_ftl)], 2, function(x) sd(x[x>0]))
  # when sp is present in catch, take sd

  spdf <- data.frame(spid = names(num_trips), num_trips = as.vector(num_trips), median_catch = medians, sd = sd)
  saveRDS(spdf, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/spdf.RDS") 
  
  small_spds <- spdf$spid[which(spdf$num_trips < 100 & spdf$median_catch < 100)]
#----
# remove species from trip table
#----
  filter_trips <- cast_ftl[,!(names(cast_ftl) %in% small_spds)]
  ## remove trips which have zero catch
  filter_trips <- filter_trips[-which(rowSums(filter_trips[,-1], na.rm=T)==0),]
  
  # how many trips lost?
  nrow(cast_ftl) - nrow(filter_trips)
  # how many species lost
  ncol(cast_ftl) - ncol(filter_trips)

  # make new ftl dataset that only has correct trips and drop dredges
  filtered_ftl <- subset(new_df, trip_id %in% filter_trips$trip_id & grgroup != "DRG")
#----
# remove any vessels which have more than one type of gear on it per trip
#----
dub_gears <- unique(filtered_ftl[,c("trip_id","grgroup")])
rm_ves <- unique(dub_gears$trip_id[which(duplicated(dub_gears$trip_id))])
no_dups <- subset(filtered_ftl, !(trip_id %in% rm_ves))
saveRDS(no_dups, "code/1_cleaningData/filtered_ftl.RDS")
