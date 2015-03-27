filter_rare <- function(data){
  # drop species rarely caught (makes clustering easier)
  # details: drop nominal distinction in market categories, drop dredge gear
  
# merge nominal and species market categories ----
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

  data$modified <- NA
  for(i in inds){
    data$modified[which(data$spid==species.key$nominal[i])] <- species.key$reg[i]
    #cat(i," ")
  }
  
  data$modified <- ifelse(is.na(data$modified), data$spid, data$modified)

# first make trip table (col = sp, row = trip) ----
  # prepare for recasting
  melt_ftl <- melt(data, 
                   id.vars = c("drvid","trip_id","modified","tdate","grid"), 
                   measure.vars = "landed_wt")
  #saveRDS(melt_ftl, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/melt_ftl.RDS")
  # not fast transposing
  cast_ftl <- dcast(melt_ftl, trip_id ~ modified, fun.aggregate = sum)

# Using threshold values for number trips and median pounds per catch, find species to drop ----
  # what about the number of trips these species show up in. Assuming that there is only one species entry for each trip
  num_trips <- table(melt_ftl$modified)
  # conditions for dropping should be that it's rarely caught, and of the times that it is caught it's always in small amounts. Avoids things like yellowfin tuna, who is sometimes caught in large volumes and for which there is a market.
  
  medians <- apply(cast_ftl[2:ncol(cast_ftl)], 2, function(x) median(x[x>0]))
  # when sp is present in catch, take median. 
  sd <- apply(cast_ftl[2:ncol(cast_ftl)], 2, function(x) sd(x[x>0]))
  # when sp is present in catch, take sd
  
  spdf <- data.frame(spid = names(num_trips), num_trips = as.vector(num_trips), median_catch = medians, sd = sd)
  #saveRDS(spdf, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/spdf.RDS") 
  
  small_spds <- spdf$spid[which(spdf$num_trips < 100 & spdf$median_catch < 100)]

# remove species from trip table ----
  filter_trips <- cast_ftl[,!(names(cast_ftl) %in% small_spds)]
  ## remove trips which have zero catch
  filter_trips <- filter_trips[-which(rowSums(filter_trips[,-1], na.rm=T)==0),]
  
  # how many trips lost?
  #nrow(cast_ftl) - nrow(filter_trips)
  # how many species lost
  #ncol(cast_ftl) - ncol(filter_trips)
  
  # make new ftl dataset that only has correct trips and drop dredges
  filtered_ftl <- subset(data, trip_id %in% filter_trips$trip_id & grgroup != "DRG")
  # remove any rare species from the catch. 
  filtered_2 <- subset(filtered_ftl, modified %in% colnames(filter_trips)[2:ncol(filter_trips)])

# remove any vessels which have more than one type of gear on it per trip ----
  dub_gears <- unique(filtered_2[,c("trip_id","grgroup")])
  rm_ves <- unique(dub_gears$trip_id[which(duplicated(dub_gears$trip_id))])
  no_dups <- subset(filtered_2, !(trip_id %in% rm_ves))
#  saveRDS(no_dups, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/filtered_ftl.RDS")
#----
return(no_dups)
}