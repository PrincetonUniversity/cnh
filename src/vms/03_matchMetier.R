# match VMS to metiers

# load data ----
tickets <- readRDS("/Users/efuller/1/CNH/processedData/catch/tickets_plus.RDS")

landings <- unique(tickets[,c("trip_id","drvid","tdate","metier")])
landings$tdate <- as.POSIXlt(landings$tdate, format = "%d-%b-%y", tz = "Etc/GMT-8")
rm(tickets)

# for each vessel track, check if it's in landings data. if not, next
setwd("/Users/efuller/1/CNH/processedData/spatial/vms/intermediate/02_cleaned/")
fish_tracks <- dir()

# find out if VMS is in landing tickets ----
for(i in 1:length(fish_tracks)){
  
  track <- readRDS(fish_tracks[i])
  doc.num <- unique(track$doc.num)
  
  if(length(doc.num)>1) break # shouldn't be the case. because split on doc.num earlier
  
  # if there's no track 
  # (which happens because didn't properly filter for vessels that never leave 
  # land upstream in 02_cleaning_data.R)
  if(nrow(track)==0) next
  
    # if the landing gear is there at all
    if(doc.num %in% landings$drvid){ 
      single_landing <- landings[(which(landings$drvid == doc.num)),]
    
      # check if there's any overlap between dates of VMS and landings
      if(any(track$date.time > min(single_landing$tdate) & 
            track$date.time < max(single_landing$tdate))){
      
      saveRDS(track, paste0(
        "/Users/efuller/1/CNH/processedData/spatial/vms/intermediate/03_matchedMetier/v_",
        unique(track$doc.num),".RDS")) }
  }
}

# assign vessels to metiers ----
# for each vessel in VMS dataset, take last landing, assign all points prior
# that metier. Then work back to the next data. Repeat until no more landings. 


# get all vessels
setwd("/Users/efuller/1/CNH/processedData/spatial/vms/intermediate/03_matchedMetier/")
vms_vessels <- gsub("v_","",dir())
vms_vessels <- gsub(".RDS","",vms_vessels)

# get all landings
vms_landings <- subset(landings, drvid %in% vms_vessels)
# clean up
rm(landings)

# go through each vessel, save new vessl track with trip_id to a list
vessel_list <- vector("list", length = length(vms_vessels))

for(i in 1:length(vms_vessels)){
  
  # subset to just one track
  ves = subset(VMS_land, doc.num == vms_vessels[i])
  # order by date and time
  ves <- ves[order(ves$date.time),]
  
  # find landings
  ves_landings <- subset(vms_landings, drvid == vms_vessels[i])
  
  # need to adjust landings date to be in comparable format
  ves_landings$tdate <- as.Date(ves_landings$tdate, format = "%d-%b-%y",
                                tz = "Etc/GMT-8")
  
  # landing days - may have more than on trip per day so use trip_id
  landing_days <- unique(ves_landings[,c("tdate","trip_id","metier")])
  # order from last to first
  landing_days <- landing_days[order(landing_days$tdate, decreasing = T),]
  # find unique days to loop through
  days <- sort(unique(landing_days$tdate),decreasing = T)
  
  # set a column for landing ticket id(s)
  # need to if they land two different metiers in same day
  ves$trip_id1 <- NA
  ves$trip_id2 <- NA
  
  # go through landings one by one, find VMS points that associated
  for(j in 1:length(days)){
    
    # find landing date
    land_date <- days[j]
    possible_ids <- landing_days$trip_id[which(landing_days$tdate==days[j])]
    trip_id1 <- possible_ids[1]
    if(length(possible_ids)>1){
      trip_id2 <- possible_ids[2]
    } else {
      trip_id2 <- NA
    }
    
    # find last VMS point for this day
    # if the landing date is before my landings date, skip 
    if(as.Date(ves$date.time[1], tz = "Etc/GMT-8") > land_date){
      next
      # else find the last VMS point for this day
    }else{
      at_dock <- tail(which(as.Date(ves$date.time, tz = "Etc/GMT-8")<=land_date),1)
      #       if(ves$date.time[at_dock] > ves$date.time[1]) # if 
      ves$trip_id1[1:at_dock] <- trip_id1
      ves$trip_id2[1:at_dock] <- trip_id2
    }
    
  }
  # merge to metier
  # get trip_id1 metiers
  ves_complete <- merge(ves, ves_landings[,c("trip_id","metier")], 
                        by.x = "trip_id1", by.y = "trip_id")
  ves_complete$metier1 <- ves_complete$metier
  ves_complete$metier <- NULL
  
  # get trip_id2 metiers
  if(!any(is.na(unique(ves_complete$trip_id2))) & length(unique(ves_complete$trip_id1==1))){
    ves_complete<- merge(ves_complete, ves_landings[,c("trip_id","metier")], 
                         by.x = "trip_id2", by.y = "trip_id")
    ves_complete$metier2 <- ves_complete$metier
    ves_complete$metier <- NULL 
  }else{
    ves_complete$metier2 <- NA
  } 
  
  vessel_list[[i]] <- ves_complete
  if(i %% 10 == 0){
    cat(i," finished\n")
  }
}

vms_catch <- do.call(rbind, vessel_list)
