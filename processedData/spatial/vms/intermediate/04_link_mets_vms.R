# load catch
rm(list=ls())
catch <- readRDS("/Users/efuller/1/CNH/processedData/catch/1_cleaningData/tickets.RDS")
#vms <- readRDS("/Users/efuller/1/CNH/processedData/spatial/vms/intermediate/04_matchMetier/VMS_catch.RDS")

# what's the maximum number of catches landed by a single vessel in a day?
all_boats <- catch[-grep("ZZZ",catch$drvid),] %>% # drop pesky ZZZ vboats
  select(drvid, trip_id, tdate) %>%
  distinct() %>%
  group_by(drvid, tdate) %>%
  summarize(n.landings = length(unique(trip_id)))

sort(table(all_boats$n.landings))

any(vms$doc.num == all_boats$drvid[which.max(all_boats$n.landings)]) # max isn't there

any(vms$doc.num %in% unique(all_boats$drvid[which(all_boats$n.landings==7)])) # 7 isn't there

any(vms$doc.num %in% unique(all_boats$drvid[which(all_boats$n.landings==6)])) # but 6 is. shoot. 

# finding discrete trips ----
# ok to generalize, boats get distance to coast measured, any point that's > 1.5 km from coastline is a trip
load("/Users/efuller/1/CNH/processedData/spatial/2_coastline.Rdata")
proj4string(WC) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")
wc_proj <- spTransform(WC,CRS("+proj=aea +lat_1=35.13863306500551 +lat_2=46.39606296952133 +lon_0=-127.6171875"))

# go through each track, save both new trajectory and fish tickest that are not found in VMS
data.dir <- "/Users/efuller/1/CNH/processedData/spatial/vms/intermediate/03_overlapMetier/"
vms_files <- dir(data.dir)
for(b in 298:length(vms_files)){

v2 <- readRDS(paste0(data.dir,vms_files[b]))

find_trips <- function(vessel_track, coastline = wc_proj,
                       projection = CRS("+proj=aea +lat_1=35.13863306500551 
                                        +lat_2=46.39606296952133 +lon_0=-127.6171875"))
  {
  # project for gDistance
  # default is Equal Area Albers
  # make sure no positive longitudes
  if(any(vessel_track$longitude>0)){
    vessel_track <- subset(vessel_track, longitude < 0)
  }
  
  # make vessel track sp object, assign default lat/lon of NAD83
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
  # if vessel starts in the middle of a trip, discard it because will reverse algorithm
  if(vessel_track@data$trip[1]==1){
    # find first zero
    first.zero = which(vessel_track@data$trip==0)[1]
    # make first trips into 0s, even though on water, won't consider. 
    vessel_track@data$trip[1:first.zero] <- 0
  }
  vessel_track@data$trip_num <- c(0,cumsum(abs(diff(vessel_track@data$trip))))
  vessel_track@data$only_trips <- ifelse(vessel_track@data$trip_num %% 2 == 0, 0, vessel_track@data$trip_num)
  # remove the other two indicators
  vessel_track@data$trip <- NULL
  vessel_track@data$trip_num <- NULL
  
  # remove any trips that are only 2 time points in duration - not sure
  #trip_table <- table(subset(vessel_track@data,only_trips>0)$only_trips)
  #short_trips <- names(trip_table)[which(trip_table<=2)]
  # vessel_track@data$only_trips[which(vessel_track@data$only_trips %in% short_trips)] <- 0
  
  # returns distance from coast and unique ID for trip
  return(vessel_track)
}

v2_track <- find_trips(vessel_track = v2)

# ----
# next is assigning landings to trips
# for each 24 period, is there a trip in it? if yes, then assign that trip the landing ID. If not, look back in time, what's the first trip in that period? also want it to be close to port. 

# find landings: subset trip ID, landing dates, metier, and port
  c2 <- unique(subset(catch, drvid == unique(v2$doc.num), select = c("trip_id","pcid","metier.2010","tdate")))
  c2$tdate.start <- as.POSIXct(c2$tdate, format = "%d-%b-%y",tz = "Etc/GMT-8")
  c2$tdate.end <- as.POSIXct(c2$tdate, format = "%d-%b-%y",tz = "Etc/GMT-8")+23.9999*60*60
  c2 <- c2[order(c2$tdate.start),]
  
  # change to only allow one row for each tdate
  library(dplyr)
  c2_bydate <- c2 %>%
    group_by(tdate) %>%
    mutate(trip_id1 = unique(trip_id)[1], 
           trip_id2 = ifelse(length(unique(trip_id))>1, unique(trip_id)[2], "NA"), 
           trip_id3 = ifelse(length(unique(trip_id))>2, unique(trip_id)[3], "NA"),
           trip_id4 = ifelse(length(unique(trip_id))>3, unique(trip_id)[4], "NA"),
           trip_id5 = ifelse(length(unique(trip_id))>4, unique(trip_id)[5], "NA"),
           trip_id6 = ifelse(length(unique(trip_id))>5, unique(trip_id)[6], "NA")) %>%
    select(-trip_id) %>%
    distinct()
  
  # reorder
  c2_bydate <- c2_bydate[order(c2_bydate$tdate.start),]
  
  # check to make sure metiers and ports are the same
  any(duplicated(c2_bydate[,3:8])) # should be false, means that all ports and metiers are same

# will be more trips than landings, so go through landings
v2_track$trip_id1 <- NA
v2_track$trip_id2 <- NA
v2_track$trip_id3 <- NA
v2_track$trip_id4 <- NA
v2_track$trip_id5 <- NA
v2_track$trip_id6 <- NA
# rule: if there's a trip during the 24 period of landings, assign landings. Else look back 36 hours. If more than two trips, check to make sure same fishery and same port, and assign both trip_ids. Then in future, sum landings for these two trips and attribute to entire trip trajectory. Thus in future, should be able to take metier from either of these trip IDs and assign to both trips and shouldn't matter (because have already checked that they're from the same fishery)
trips_wo_vms <- NA
for(j in 1:nrow(c2_bydate)){
  # find all trips
  trips <- as.character(select(as.data.frame(c2_bydate[j,]), contains("trip_id")))
  if(all(trips=="NA")){
    cat("corrupted catch dates")
    break
  }
  
  # was a trip will landed on the day? 
    possible_trips <- unique(v2_track$only_trips[which(v2_track$date.time > c2_bydate$tdate.start[j] & 
                                                  v2_track$date.time < c2_bydate$tdate.end[j] &
                                                  v2_track$only_trips!=0)])
    # if we still have possible trips left, assign to VMS
      if(length(possible_trips)>0){
        
        # make sure the trip(s) all occurred before tdate.end, if not replace with NA. then drop NAs again
        for(q in 1:length(possible_trips)){
          return_time <- max(v2_track$date.time[which(v2_track$only_trips==possible_trips[q])])
          possible_trips[q] <- ifelse(return_time > c2_bydate$tdate.end[j], NA, possible_trips[q])
        }
        
        # possible that you could loose all trips, if all are NOT NA, then can use those trips, else look earlier
        if(!(all(is.na(possible_trips)))) {
          if(any(is.na(possible_trips))){
            possible_trips <- possible_trips[-which(is.na(possible_trips))]
          }
          # then make sure that trip wasn't already assigned a landing ticket 
          if(!(any(is.na(v2_track$trip_id1[which(v2_track$only_trips %in% unique(possible_trips))])))){ 
            cat("overwritting trips6")
            break}
          
          # assign the trip to VMS!
          v2_track[which(v2_track$only_trips %in% possible_trips),
                   c("trip_id1","trip_id2", "trip_id3","trip_id4","trip_id5","trip_id6")] <- 
            c2_bydate[j,c("trip_id1","trip_id2", "trip_id3","trip_id4","trip_id5","trip_id6")]
          }else{
          # other option is that all possible trips are NA, now need to look in prior 36 hours for trips
          # were trips landed within 36 hours?
          # if so, how many?
          earlier_start <- c2_bydate$tdate.start[j]-36*60*60
          prior_trips <- unique(v2_track$only_trips[which(v2_track$date.time > earlier_start & 
                                                            v2_track$date.time < c2_bydate$tdate.end[j] &
                                                            v2_track$only_trips!=0)])
          
          # if no trips within 36 hours, record trip id and move on
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
                  next}
                
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
        }else{
        if(length(possible_trips)==0){
          # if no possible trips, need to look 36 hours earlier
          # were trips landed within 36 hours?
          # if so, how many?
          earlier_start <- c2_bydate$tdate.start[j]-36*60*60
          prior_trips <- unique(v2_track$only_trips[which(v2_track$date.time > earlier_start & 
                                                            v2_track$date.time < c2_bydate$tdate.end[j] &
                                                            v2_track$only_trips!=0)])
          
          # if no trips within 36 hours, record trip id and move on
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
        }else{
          cat("there are no trips for this landing date, double check plox")
          break
        }
      }
    }


#length(which(v2_track$only_trips!=0 & is.na(v2_track$trip_id1))) # there are trips without landings
# percentage: length(which(v2_track$only_trips!=0 & is.na(v2_track$trip_id1)))/length(v2_track$only_trips!=0)
# and there are landings without trips

# so would like to plot around those trips that have no tracks to make sure

  trips_wo_vms <- trips_wo_vms[-which(trips_wo_vms=="NA")]
  if(length(trips_wo_vms)!=0){
    no_vms_landings <- data.frame(trip_id= trips_wo_vms[-1,drop=FALSE],stringsAsFactors=FALSE)
    no_vms_landings <- left_join(no_vms_landings, c2)
    saveRDS(no_vms_landings, paste0("/Users/efuller/1/CNH/processedData/both/matching_metiers/VMS_tracks_wMets/","no_vms_landings_v",unique(v2_track$doc.num),".RDS"))
    rm(no_vms_landings, trips_wo_vms)
  }

# merge metier with trip_id
met_track <- merge(as.data.frame(v2_track), dplyr::select(c2, trip_id, metier.2010), by.x = "trip_id1",by.y="trip_id",all.x = TRUE, all.y = FALSE)
met_track <- met_track[order(met_track$date.time),]

saveRDS(met_track, paste0("/Users/efuller/1/CNH/processedData/spatial/vms/intermediate/04_link_mets_vms/",unique(v2_track$doc.num),".RDS"))

rm(v2_track, possible_trips, prior_trips, earlier_start, c2, c2_bydate, trips, j, q, return_time, v2, met_track)

}

# should make diagnostic plots for each one of these to make sure things are working (dist from port colored by metier along with landing dates)

# for each of these, plot 3 days before and behind
# for(line in 1:nrow(foo)){
# with(as.data.frame(subset(v2_track, date.time > foo$tdate.start[line]-36*60*60 & date.time < foo$tdate.end[line]+12*60*60)), plot(date.time, dist_coast/1000,type = 'h'))
# 
# }
# # and would like to plot with color based on metier
# # make new column for metier
new_try <- merge(as.data.frame(v2_track), dplyr::select(c2, trip_id, metier.2010), by.x = "trip_id1",by.y="trip_id",all.x = TRUE, all.y = FALSE)

new_try <- new_try[order(new_try$date.time),]
new_try$paint <- ifelse(new_try$metier.2010 == "TLS_1","purple", 
                        ifelse(new_try$metier.2010 == "TLS_2","dodgerblue",
                               ifelse(new_try$metier.2010 == "POT_13","orange",
                                     ifelse(new_try$metier.2010=="POT_1", "indianred",
                                            ifelse(new_try$metier.2010 == "TWS_1","pink",
                                                   ifelse(new_try$metier.2010=="POT_2","magenta",
                                                          ifelse(new_try$metier.2010=="HKL_1","brown",
                                                                 ifelse(new_try$metier.2010=="HKL_8", "olivedrab",
                                                                 ifelse(new_try$metier.2010=="HKL_6","blue",
                                            ifelse(new_try$only_trips==1,"grey50","white"))))))))))

plot(new_try$longitude, new_try$latitude, asp = 1, pch=19, cex=.25, col=new_try$paint)
  plot(wc_proj, add = T, col="grey90",bor="grey95")
#   legend("topright",legend=c("spiny lobster pot","rockfish hkl","sablefish pot"),col=c("magenta","blue","brown"),pch=19)
plot(new_try$date.time, new_try$dist_coast/1000, col=new_try$paint,type='h')
abline(v = as.numeric(c2_bydate$tdate.start),col='red')
