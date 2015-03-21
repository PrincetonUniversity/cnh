#' # How to validate backwards assignment method
#' 
#' The interest is, how similar is it to vessels with trips that are observed. 
#' 
#' + This metric would be $\pm$ days from departure and return on average. 
#' + Whether the listed fish-ticket matched my assigned
#' + And if there is a period of time that has been "over assigned" using my method, how far did the vessel travel during that period (distance of path).
#' + Record how long the actual trip was using observer data, in order to see the extra difference in relation to the "true" trip. 
#' 
#' Because a vessel may be observed more than once, I'll have entries for the same vessel more than once. 
#' 
#' After gathering this information, 
#' 
#' + make histogram of days $\pm$ on departures
#' + make histogram of days $\pm$ on returns
#' + make histogram of extra distance
#' + plot days additional and movement
#' 
#' I hope that there's almost no distance moved, and that the days are around 1 or 2. At a minimum, this gives an estimate of the error. I don't have a good cut-off of what's not acceptable. Obviously the concern is that there is movement being accorded to a trip that is not related.  
#' 
#' ## If outliers...
#' Investigate how this varies with gear type, vessel identity, or possibly strategy, or total number of trips if there are . 
#' 
#' # Code
#' Step 1 is building a dataframe of start and end dates for my method versus available from observer data
#' ## Infer dates - Observer Data
#' I load observer data, find unique departure and return dates for each vessel observed

obs <- read.csv(
  "/Users/efuller/1/CNH/Data/Observers/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv", 
  stringsAsFactors = FALSE)

trip_dates <- unique(obs[,c("DRVID","D_DATE","R_DATE","FISH_TICKETS")])

trip_dates$D_DATE <- as.POSIXlt(trip_dates$D_DATE, format = "%m/%d/%Y", tz = "Etc/GMT-8")
trip_dates$R_DATE <- as.POSIXlt(trip_dates$R_DATE, format = "%m/%d/%Y", tz = "Etc/GMT-8")
#' To find my assigned trip dates

# loop through each trip_id, find first and last date associated
my_trips <- data.frame("trip_id"=unique(vms_catch$trip_id1), stringsAsFactors = FALSE)
my_trips$drvid <- NA
my_trips$d_date <- NA
my_trips$r_date <- NA

for(i in 1:nrow(my_trips)){
  trip <- subset(vms_catch, trip_id1==my_trips$trip_id[i])
  my_trips$d_date[i] <- head(trip$date.time,1)
  my_trips$r_date[i] <- tail(trip$date.time, 1)
  my_trips$drvid[i] <- unique(trip$doc.num)
}

# ideally obs trips will have shorter durations, so should be fully included in my trip intervals. 

# work through trips and look for obs data
vessels <- unique(my_trips$drvid)
obs_vessels <- vessels[which(vessels %in% trip_dates$DRVID)]



for(i in 1:length(obs_vessels)){
  vessel_trips <- subset(my_trips, drvid == obs_vessels[i])
  obs_trips <- subset(trip_dates, DRVID == obs_vessels[i])
  full_vms <- subset(vms_catch, doc.num == obs_vessels[i])
  
  for(k in 1:nrow(obs_trips)){
    obs_trips$D_DATE[k]>= vessel_trips$d_date[1] & obs_trips$R_DATE[k] <= vessel_trips$r_date[1]
  }
}

extra_vms <- subset(full_vms, date.time >= my_trips$d_date[12] & date.time <=my_trips$r_date[12])




# more final draft ----

# get observed trip dates ----
  obs <- read.csv(
    "/Users/efuller/1/CNH/Data/Observers/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv", 
    stringsAsFactors = FALSE)
  
  trip_dates <- unique(obs[,c("DRVID","D_DATE","R_DATE","FISH_TICKETS","sector")])
  trip_dates$D_DATE <- as.POSIXlt(trip_dates$D_DATE, format = "%m/%d/%Y", 
                                  tz = "Etc/GMT-8")
  trip_dates$R_DATE <- as.POSIXlt(trip_dates$R_DATE, format = "%m/%d/%Y", 
                                  tz = "Etc/GMT-8")

# match VMS to metiers ----

tickets <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/fisheries_participation_profiles/tickets_plus.RDS")

landings <- unique(tickets[,c("trip_id","drvid","tdate","metier")])
# change tdate to date format

VMS <- readRDS("/Users/efuller/1/CNH/VMS_cleaning/src/VMS.RDS")

VMS_land <- subset(VMS, doc.num %in% unique(landings$drvid))
# clean up
rm(obs, tickets) 

# for each vessel in VMS dataset, take last landing, assign all points prior
# that metier. Then work back to the next data. Repeat until no more landings. 

# get all vessels
vms_vessels <- unique(VMS_land$doc.num)
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

# get my derived trip dates ----
# loop through each trip_id, find first and last date associated
  my_trips <- data.frame("trip_id"=unique(vms_catch$trip_id1), 
                         stringsAsFactors = FALSE)
  my_trips$drvid <- NA
  my_trips$d_date <- NA
  my_trips$r_date <- NA
  
  for(i in 1:nrow(my_trips)){
    trip <- subset(vms_catch, trip_id1==my_trips$trip_id[i])
    my_trips$d_date[i] <- head(trip$date.time,1)
    my_trips$r_date[i] <- tail(trip$date.time, 1)
    my_trips$drvid[i] <- unique(trip$doc.num)
  }

# for each vessel with observed trips ----
observed_vessels <- unique(trip_dates$DRVID)

list_compared <- vector("list",length(observed_vessels))

for(id in 1:length(observed_vessels)){
  # find observed trips
  obs_dates <- subset(trip_dates, DRVID == observed_vessels[id])
  # find vms for this vessel
  vms_vessel <- subset(vms_catch, doc.num == observed_vessels[id])
  
  if(nrow(vms_vessel) == 0) next # if this vessel isn't present
  
  # find observed trips that are within the vms period
  obs_in <- subset(obs_dates, D_DATE>=min(vms_vessel$date.time) & 
                     R_DATE <=max(vms_vessel$date.time))
  
  if(nrow(obs_in)==0) next
  
  # find my derived start-end dates
  vessel_trips <- subset(my_trips, drvid == observed_vessels[id])
  
  compare_trips <- data.frame(o.d_date = rep(NA,nrow(obs_in)))
  compare_trips$o.r_date = NA
  compare_trips$o.sector = NA
  compare_trips$m.d_date = NA
  compare_trips$m.r_date = NA
  compare_trips$m.sector = NA
  compare_trips$diff_days = NA
  
  # for each of these observed trips
  for(trip in 1:nrow(obs_in)){
    # find VMS date.time that is closest to departure
    assigned_trip <- vms_vessel$trip_id1[
      which.min(abs(obs_in$D_DATE[trip]-vms_vessel$date.time))]
    assigned_sector <- vms_vessel$metier1[
      which.min(abs(obs_in$D_DATE[trip]-vms_vessel$date.time))]
    found_trip <- vessel_trips[which(vessel_trips$trip_id == assigned_trip),]
    
    
    compare_trips$o.d_date <- obs_in$D_DATE[trip]
    compare_trips$o.r_date <- obs_in$R_DATE[trip]
    compare_trips$o.sector <- obs_in$sector[trip]
    
    compare_trips$m.d_date <- as.Date(found_trip$d_date)
    compare_trips$m.r_date <- as.Date(found_trip$r_date)
    compare_trips$m.sector <- assigned_sector
    
  }

  compare_trips$diff_days <- (compare_trips$o.r_date - compare_trips$o.d_date) -
    (compare_trips$m.r_date - compare_trips$m.d_date)
 
  list_compared[[i]] <- compare_trips
}
