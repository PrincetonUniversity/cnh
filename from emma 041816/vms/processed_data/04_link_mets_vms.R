# this script links metiers to VMS.
# 
# First it searches through observer data in order to find a list of observed 
# trips for which multiple fish tickets exist. 
# Once all metiers are assigned, any fish tickets assigned that are in
# this "double list" will be removed. 

# Finally for all fish tickets assigned, 
# a revenue and pounds column is calculated and added for each time stamp. 

link_vms.tickets <- function(window_size){

# find double observed data ----
  library(dplyr)
  obs <- read.csv("/Users/efuller/Desktop/CNH/rawData/Observers/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv",stringsAsFactors = FALSE)
  
  dubs_tix <- obs %>%
    dplyr::select(DRVID, D_DATE, TRIPID, sector, FISHERY, FISH_TICKETS) %>%
    distinct() %>%
    mutate(dubs = ifelse(grepl(";", FISH_TICKETS), 1, 0)) %>%
    filter(dubs == 1) %>%
    mutate(D_DATE = as.POSIXct(D_DATE, format = "%m/%d/%Y %H:%M:%S %p")) 
  
  for(i in 1:nrow(dubs_tix)){
    dubs_tix$tix1[i] <- ifelse(dubs_tix$dubs[i] == 1, 
                               paste0(unlist(strsplit(dubs_tix$FISH_TICKETS[i], split = ";"))[1],
                                      format(dubs_tix$D_DATE[i], "%Y")), NA)
    dubs_tix$tix2[i] <- ifelse(dubs_tix$dubs[i] == 1, 
                               paste0(unlist(strsplit(dubs_tix$FISH_TICKETS[i], split = ";"))[2],
                                      format(dubs_tix$D_DATE[i], "%Y")), NA)
    dubs_tix$tix3[i] <- ifelse(dubs_tix$dubs[i] == 1, 
                               paste0(unlist(strsplit(dubs_tix$FISH_TICKETS[i], split = ";"))[3],
                                      format(dubs_tix$D_DATE[i], "%Y")), NA)
    dubs_tix$tix4[i] <- ifelse(dubs_tix$dubs[i] == 1, 
                               paste0(unlist(strsplit(dubs_tix$FISH_TICKETS[i], split = ";"))[4],
                                      format(dubs_tix$D_DATE[i],"%Y")), NA)
    dubs_tix$tix5[i] <- ifelse(dubs_tix$dubs[i] == 1, 
                               paste0(unlist(strsplit(dubs_tix$FISH_TICKETS[i], split = ";"))[5],
                                      format(dubs_tix$D_DATE[i],"%Y")), NA)
  }
  
  # remove those that begin with NA
  dubs_tix$tix3[grep("^NA",dubs_tix$tix3)] <- NA
  dubs_tix$tix4[grep("^NA",dubs_tix$tix4)] <- NA
  dubs_tix$tix5[grep("^NA",dubs_tix$tix5)] <- NA
  
  # make vector of all fish tickets that are part of an observed trip 
  # which is linked to more than 1 fish ticket
  duplicate_ftids <- unique(c(dubs_tix$tix1, dubs_tix$tix2, dubs_tix$tix3, dubs_tix$tix4, dubs_tix$tix5))
  duplicate_ftids <- duplicate_ftids[-which(is.na(duplicate_ftids))] # drop NA
  
# what's the maximum number of catches landed by a single vessel in a day? ----
  # load catch
  catch <- readRDS("/Users/efuller/Desktop/CNH/processedData/catch/1_cleaningData/tickets.RDS")
  
# finding discrete trips ----
# ok to generalize, boats get distance to coast measured, any point that's > 1.5 km from coastline is a trip
load("/Users/efuller/Desktop/CNH/processedData/spatial/2_coastline.Rdata")
library(sp)
proj4string(WC) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")
wc_proj <- spTransform(WC,CRS("+proj=aea +lat_1=35.13863306500551 +lat_2=46.39606296952133 +lon_0=-127.6171875"))

# go through each track, save both new trajectory and fish tickest that are not found in VMS
data.dir <- "/Users/efuller/Desktop/CNH/processedData/spatial/vms/intermediate/03_overlapMetier/"
vms_files <- dir(data.dir)

for(b in 1:length(vms_files)){

ves <- readRDS(paste0(data.dir,vms_files[b]))

find_trips <- function(vessel_track, coastline = wc_proj,
                       projection = CRS("+proj=aea +lat_1=35.13863306500551 
                                        +lat_2=46.39606296952133 +lon_0=-127.6171875"))
  {
  # project for gDistance
  # default is Equal Area Albers
  # make sure no positive longitudes
  if(any(vessel_track$longitude>0 | vessel_track$longitude < -150)){
    vessel_track <- subset(vessel_track, longitude < 0 & longitude > -150)
  }
  
  # make vessel track sp object, assign default lat/lon of NAD83
  library(sp)
  coordinates(vessel_track) <- ~longitude + latitude
  proj4string(vessel_track) <- CRS(
    "+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")
  # convert into equal albers projection for gDistance
  vessel_track <- spTransform(vessel_track, projection)

  # calculate pairwise distance to coast, because projected measures in meters
  library(rgeos)
  vessel_track@data$dist_coast <- NA
  for(i in 1:nrow(vessel_track)){
    vessel_track@data$dist_coast[i] <- gDistance(vessel_track[i,], coastline)
  }
  
  # any points that are > 1.5 km from the coast are "out" on a trip
  vessel_track@data$trip <- ifelse(vessel_track@data$dist_coast/1000>1.5, 1, 0)
  vessel_track <- vessel_track[order(vessel_track@data$date.time),]
  # if there's a gap > 6 hours in middle of trip
  # if vessel starts in the middle of a trip, discard it because will reverse algorithm
  if(vessel_track@data$trip[1]==1){
    # find first zero
    first.zero = which(vessel_track@data$trip==0)[1]
    # make first trips into 0s, even though on water, won't consider. 
    vessel_track@data$trip[1:first.zero] <- 0
  }
  
  time_diffs <- diff(vessel_track@data$date.time)
  # if diff > 3 hours, new trip
  vessel_track@data$time_diff <- c(NA, time_diffs)
  gap_marker <- which(vessel_track@data$time_diff>60*3 & vessel_track@data$trip==1)
  if(length(gap_marker)>0){
    # so point before needs to be inserted as 0. will eventually drop these duplicate points. 
    new_rows <- vessel_track[gap_marker,]
    new_rows$trip <- 0
    foo <- rbind(vessel_track, new_rows)
    foo <- foo[order(foo$date.time, foo$trip),]
    vessel_track <- foo
  }
  vessel_track@data$trip_num <- c(0,cumsum(abs(diff(vessel_track@data$trip))))
  vessel_track@data$only_trips <- ifelse(vessel_track@data$trip_num %% 2 == 0, 0, vessel_track@data$trip_num)
  # drop duplicates i inserted
  vessel_track <- vessel_track[-which(duplicated(vessel_track@data[,c("dist_coast","time_diff")])),]
  # remove the other two indicators
  vessel_track@data$trip <- NULL
  vessel_track@data$trip_num <- NULL
  
  # returns distance from coast and unique ID for trip
  return(vessel_track)
}

v2_track <- find_trips(vessel_track = ves)

# next is assigning landings to trips, argument is time window to look back in
# look_back is in hours, represents num hours to look back for vms data
assign_landings <- function(time_window, v2 = ves){
  # convert look_back window to seconds
  look_back <- time_window *60 *60
  # find landings: subset trip ID, landing dates, metier, and port
  c2 <- unique(subset(catch, drvid == unique(v2$doc.num), select = c("trip_id","pcid","metier.2010","tdate")))
  c2$tdate.start <- as.POSIXct(c2$tdate, format = "%d-%b-%y",tz = "Etc/GMT-8") - look_back
  c2$tdate.end <- as.POSIXct(c2$tdate, format = "%d-%b-%y",tz = "Etc/GMT-8")+23.9999*60*60
  c2 <- c2[order(c2$tdate.start),]

# for each 24 period, is there a trip in it? if yes, then assign that trip the landing ID. 

  # change to only allow one row for each tdate, need to sort by tdate.start 
  library(dplyr)
  c2_bydate <- c2 %>%
    group_by(tdate) %>%
    mutate(trip_id1 = unique(trip_id)[1], 
           trip_id2 = as.character(ifelse(length(unique(trip_id))>1, unique(trip_id)[2], NA)), 
           trip_id3 = as.character(ifelse(length(unique(trip_id))>2, unique(trip_id)[3], NA)),
           trip_id4 = as.character(ifelse(length(unique(trip_id))>3, unique(trip_id)[4], NA)),
           trip_id5 = as.character(ifelse(length(unique(trip_id))>4, unique(trip_id)[5], NA)),
           trip_id6 = as.character(ifelse(length(unique(trip_id))>5, unique(trip_id)[6], NA))) %>%
    dplyr::select(-trip_id) %>%
    distinct()
  
  # reorder
  c2_bydate <- c2_bydate[order(c2_bydate$tdate.start),]
  
  # check to make sure metiers and ports are the same
  any(duplicated(c2_bydate[,3:8])) # should be false, means that all ports and metiers are same
  return(c2_bydate)
}

c2_bydate <- assign_landings(time_window = window_size, v2 = ves)
# will be more trips than landings, so go through landings
v2_track$trip_id1 <- NA
v2_track$trip_id2 <- NA
v2_track$trip_id3 <- NA
v2_track$trip_id4 <- NA
v2_track$trip_id5 <- NA
v2_track$trip_id6 <- NA

# rule: if there's a trip during the time interval of landings (determined above by time window argument), assign landings. If more than two trips, check to make sure same fishery and same port, and assign both trip_ids. Then in future, sum landings for these two trips and attribute to entire trip trajectory. Thus in future, should be able to take metier from either of these trip IDs and assign to both trips and shouldn't matter (because have already checked that they're from the same fishery)

trips_wo_vms <- NA

for(j in 1:nrow(c2_bydate)){
  # find all trips
  trips <- as.character(dplyr::select(as.data.frame(c2_bydate[j,]), contains("trip_id")))
  
  if(all(trips=="NA")){
    cat("corrupted catch dates")
    break
  }
  
  # was a trip will landed on the day? 
  prior_trips <- unique(v2_track$only_trips[which(v2_track$date.time > c2_bydate$tdate.start[j] & 
                                                    v2_track$date.time < c2_bydate$tdate.end[j] &
                                                    v2_track$only_trips!=0)])
  
  # if no trips within time window hours, record trip id and move on
  if(length(prior_trips)==0) {
    trips_wo_vms <-  c(trips_wo_vms, trips)
    next
  }else{ # but if there are some prior trips
    # make sure the trip(s) all occurred before tdate.end, 
    # if not replace with NA. then drop NAs again
    for(q in 1:length(prior_trips)){
      return_time <- max(v2_track$date.time[which(v2_track$only_trips==prior_trips[q])])
      prior_trips[q] <- ifelse(return_time > c2_bydate$tdate.end[j], NA, prior_trips[q])
    }
    
    # check that those trips don't belong to another landing ticket
    for(q in 1:length(prior_trips)){
      assigned_previous <- any(!is.na(v2_track$trip_id1[which(v2_track$only_trips %in% prior_trips[q])]))
      prior_trips[q] <- ifelse(assigned_previous, NA, prior_trips[q])
    }
    
    # possible that you could loose all trips, if all are NOT NA, then can use those trips, else look earlier
    if(!(all(is.na(prior_trips)))) {
      if(any(is.na(prior_trips))){
        prior_trips <- prior_trips[-which(is.na(prior_trips))]
      }
      # then make sure that trip wasn't already assigned a landing ticket 
      if(!(any(is.na(v2_track$trip_id1[which(v2_track$only_trips %in% unique(prior_trips))])))){ 
        # means that there was a landing prior to this one that already claimed that VMS trip, so 
        # this landing ticket has no VMS
        trips_wo_vms <- c(trips_wo_vms, trips)
        next
      }
      
      # assign the trip to VMS!
      v2_track[which(v2_track$only_trips %in% prior_trips),
               c("trip_id1","trip_id2", "trip_id3","trip_id4","trip_id5","trip_id6")] <- 
        c2_bydate[j,c("trip_id1","trip_id2", "trip_id3","trip_id4","trip_id5","trip_id6")]
    }else{
      if(all(is.na(prior_trips))) {
        trips_wo_vms <-  c(trips_wo_vms, trips)
        next
      }else{
        cat("warning:shouldn't get here")
        break
      }
    }
  }
}

# merge metier with trip_id
met_track <- merge(as.data.frame(v2_track), dplyr::select(c2_bydate, starts_with("trip_id"), metier.2010),all.x = TRUE, all.y = FALSE)
met_track <- met_track[order(met_track$date.time),]

# rename aggregate trips - adds an agg_id
met_agg <- met_track %>%
  dplyr::select(only_trips, starts_with("trip_id")) %>%
  distinct() %>%
  filter(!is.na(trip_id1)) %>%
  group_by(trip_id1) %>%
  mutate(agg_id = unique(only_trips)[1]) %>%
  ungroup() %>%
  dplyr::select(only_trips, agg_id, - trip_id1) %>%
  arrange(agg_id) %>%
  right_join(met_track)

# add indicator for whether it's a duplicate observed trip
met_agg$obs_dup <- ifelse(met_agg$trip_id1 %in% duplicate_ftids, 1, 0)

# reproject trajectory to lat/lon
met_agg <- as.data.frame(met_agg)
coordinates(met_agg) <- ~longitude+latitude
proj4string(met_agg) <- proj4string(wc_proj)
met_agg <- spTransform(met_agg, proj4string(WC))
met_agg <- as.data.frame(met_agg)

# calculate revenue and lbs for each trip ----
trips_landed <- unique(c(met_agg$trip_id1, met_agg$trip_id2, met_agg$trip_id3, met_agg$trip_id4, met_agg$trip_id5, met_agg$trip_id6))
trips_landed <- trips_landed[-which(is.na(trips_landed))]
if(length(trips_landed)==0){
  met_all <- met_agg
  met_all[,c("lbs","revenue","n.trips","time","distance","lbs_time","rev_time","lbs_dist","lbs_trips","rev_trips")] <- NA
}else{
  trip_tots <- subset(catch, trip_id %in% trips_landed) %>%
    group_by(trip_id) %>%
    summarize(lbs = sum(landed_wt,na.rm=T), revenue = sum(adj_revenue, na.rm = T))
  
  # use only_trips to make trip_id vector long format
  library(tidyr)
  trip_amts <- met_agg %>%
    dplyr::select( agg_id, starts_with("trip_id")) %>%
    distinct() %>%
    filter(!is.na(trip_id1)) %>%
    gather(key=ids, value = trip_id, -agg_id) %>%
    filter(trip_id!="NA") %>%
    arrange(agg_id) %>%
    left_join(trip_tots) %>%
    group_by(agg_id) %>%
    summarize(lbs = sum(lbs), revenue = sum(revenue), n.trips = length(agg_id))
  
  # for each of these agg_id trips need to get effort data (duration of time for each `only_trips` and distance)
  # returns sequential steps in km
  path_dist <- function(lon, lat, dist_coast.vec){
    if(length(lon)==1){ # if only one point out, then it's distance from coast
      path_dist = dist_coast.vec/1000
    }else{
      path_dist = rep(NA, length(lon))
      dist_mat <- cbind(lon, lat)
      for(i in 2:length(lon)){
        path_dist[i] <- spDistsN1(t(as.matrix(dist_mat[i-1,])), t(as.matrix(dist_mat[i,])), longlat = TRUE)
      }
      path_dist[1] <- dist_coast.vec[1]/1000
      path_dist <- c(path_dist, dist_coast.vec[length(dist_coast.vec)]/1000)
    }
    return(path_dist)
  }
  
  effort_dat <- met_agg %>%
    filter(only_trips > 0 & !is.na(agg_id)) %>%
    group_by(only_trips) %>%
    summarize(agg_id = unique(agg_id), 
              time =  ifelse(length(date.time)==1, 1, difftime(max(date.time),min(date.time),units="hours")), 
              distance = sum(path_dist(lon = longitude, lat = latitude, dist_coast.vec = dist_coast))) %>%
    group_by(agg_id) %>%
    summarize(time = sum(time), distance =sum(distance))
  # returns time in hours, distance in km
  
  cpue <- merge(trip_amts, effort_dat) %>%
    mutate(lbs_time = lbs/time, rev_time = revenue/time, lbs_dist = lbs/distance, 
           rev_dist = revenue/distance, lbs_trips = lbs/n.trips, 
           rev_trips = revenue/n.trips)
  
  met_all <- left_join(met_agg, cpue)
}

saveRDS(met_all, paste0("/Users/efuller/Desktop/CNH/processedData/spatial/vms/intermediate/04_link_mets_vms/tw_",window_size,"hr/",unique(v2_track$doc.num),".RDS"))

}
}

link_vms.tickets(window_size = 0)
link_vms.tickets(window_size = 24)
link_vms.tickets(window_size = 36)
link_vms.tickets(window_size = 72)
link_vms.tickets(window_size = 168)



