# filter fish ticket data to drop rare species
library(reshape2); library(plyr); library(dplyr); library(scales)

# setwd to be top level of code folder
setwd("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-10/")

# load data
#----
  ftl <- read.csv(
    "/Users/efuller/1/CNH/Data/Catch/FTL_2009-2013_w-o-c_samhouri.csv", 
    stringsAsFactors = F)
  # because ftid is not unique by year, compile ftid and year together
  ftl$trip_id <- paste0(ftl$ftid, ftl$year)
#----
# drop vessels which have fewer than $5000 in annual revenue
#----
# get annual revenue
  annual_rev <- ddply(ftl, .(veid, year), summarize, rev = sum(landed_wt*ppp))
# drop vessel with veid of 0
  annual_rev <- annual_rev[-which(annual_rev$veid=="0"),]
# drop vessel with veid of "UNKNOWN"
  annual_rev <- annual_rev[-which(annual_rev$veid=="UNKNOWN"),]
# find mean annual revenue
  mean_rev <- ddply(annual_rev, .(veid), summarize, mean = mean(rev))
# remove vessels with a mean annual income < 5000
  vessels <- mean_rev$veid[mean_rev$mean>=5000]
# subset tickets
  ftl <- ftl[which(ftl$veid %in% vessels),]
#----
# merge nominal and species market categories
#----
  # load species ids
  spid <- read.csv("1_cleaningData/spid.csv", stringsAsFactors=F)
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
  new_df <- ftl
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
                   id.vars = c("veid","trip_id","modified","tdate","grid"), 
                   measure.vars = "landed_wt")
  saveRDS(melt_ftl, "1_cleaningData/melt_ftl.RDS")
  # not fast transposing
  cast_ftl <- dcast(melt_ftl, trip_id ~ modified, fun.aggregate = sum)

  # make boxplots, but exclude any 0 catches, so those are NA
  cast_ftl[cast_ftl==0] <- NA 
#----
# Using threshold values for number trips and median pounds per catch, find species to drop
#----
# what about the number of trips these species show up in. Assuming that there is only one species entry for each trip
  num_trips <- sort(table(melt_ftl$modified), decreasing=T)
  # conditions for dropping should be that it's rarely caught, and of the times that it is caught it's always in small amounts. Avoids things like yellowfin tuna, who is sometimes caught in large volumes and for which there is a market.
  medians <- rep(NA, length(num_trips))
  sd <- rep(NA, length(num_trips))
  for(i in 1:length(num_trips)){
    medians[i] <- median(melt_ftl$value[which(melt_ftl$modified == names(num_trips[i]))])
    sd[i] <- sd(melt_ftl$value[which(melt_ftl$modified==names(num_trips[i]))])
  }

  spdf <- data.frame(spid = names(num_trips), num_trips = num_trips, median_catch = medians, sd = sd)
  saveRDS(spdf, "1_cleaningData/spdf.RDS") 
  
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
  
  # remove dropped rare species
  filtered_ftl <- subset(filtered_ftl, !(modified %in% small_spds))

saveRDS(filtered_ftl, "1_cleaningData/filtered_ftl.RDS")
