# spatially filter VMS data for fishing events
library(dplyr); library(tidyr); library(sp)

# load obs data and filter for no-onland points in haul locs ----
load("processedData/spatial/2_coastline.Rdata")
proj4string(WC) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
load_clean_obs <- function(WC){
  obs <- readRDS("processedData/both/obs_data/obs.RDS")

  # add set and up lat/lons
  obs_locs <- obs %>%
    mutate(set_date = paste0(set_day,"/", set_month,"/",set_year," ",
                             floor(set_time),":",round(set_time %% 1*60)),
           up_date = paste0(up_day,"/", up_month,"/",up_year," ",
                            floor(up_time),":",round(up_time %% 1*60)),
           set_date = as.POSIXct(set_date, format= "%d/%m/%Y %H:%M", tz="Etc/GMT-8"), 
           up_date = as.POSIXct(up_date, format= "%d/%m/%Y %H:%M", tz="Etc/GMT-8")) %>%
    dplyr::select(drvid, fish_tickets, set_lat, set_long, up_lat, up_long, 
                  set_date, up_date, sector, haul_id, d_date, r_date, cg_num,
                  tripid, pcid) %>%
    distinct() %>%
    filter(sector=="Pink Shrimp")
  
  obs_set <- obs_locs
  obs_up <- obs_locs
  coordinates(obs_set) <- ~set_long+set_lat
  proj4string(obs_set) <- proj4string(WC)
  coordinates(obs_up) <- ~up_long+up_lat
  proj4string(obs_up) <- proj4string(WC)
  
  set_over <- as.numeric(!is.na(over(obs_set, WC))) # 1 = onland, 0 = at sea
  up_over <- as.numeric(!is.na(over(obs_up, WC)))
  
  # make points that are onland NA
  obs_locs[which(set_over==1),c("set_long","set_lat")] <- NA
  obs_locs[which(up_over==1),c("up_long","up_lat")] <- NA
  
  any(obs_locs$set_date>obs_locs$up_date) # should be FALSE
  any(is.na(obs_locs$set_date)) # should be FALSE
  any(is.na(obs_locs$up_date)) # should be FALSE
  
  # double check departure and return times
  # some d_dates, if at midnight, don't have times. 
  # hence the need for something fancier
  add_fullTime <- function(date_vec){
    no_time <- which(nchar(date_vec)<11)
    date_vec[no_time] <- paste(date_vec[no_time], "12:00:00 AM")
    
    full_time <- as.POSIXct(date_vec, format = "%m/%d/%Y %I:%M:%S %p", 
                            tz = "Etc/GMT-8")
    return(full_time)
  }
  obs_locs$d_date <- add_fullTime(obs_locs$d_date)
  obs_locs$r_date <- add_fullTime(obs_locs$r_date)
  
  any(is.na(obs_locs$d_date)) # should be false
  any(is.na(obs_locs$r_date)) # should be false
  
  all(obs_locs$d_date < obs_locs$r_date) # should be true
  
  # double check set and return dates relative to depart and return
  length(which(obs_locs$set_date<obs_locs$d_date)) # good
  length(which(obs_locs$up_date<obs_locs$d_date)) # good
  
  which(obs_locs$up_date>obs_locs$r_date) # good
  which(obs_locs$set_date>obs_locs$r_date) # good
  return(obs_locs)
}
obs_locs <- load_clean_obs(WC=WC)

# load vms and clean up coordinates ----

window = 24 # 0, 24, 36, 72, 168

# names of vessel tracks
path = paste0("processedData/spatial/vms/intermediate/04_link_mets_vms/tw_",window,"hr/")
file_names = dir(path)

# drop any no_vms_landings names
if(any(grepl("no_vms_landings", file_names))){
  names = names[-grep("no_vms_landings",names)]
}

# keep track of fish ticket, sector, vessel, port, and date for 
# observed trips and amount of time spent fishing

# keep track of trip data
total_trip_dat <- data.frame()

# load vessel data ----

for (i in 1:length(file_names)){
  
  ### Load up
  infile = file_names[i]
  vms <- readRDS(paste0(path,infile))
  
  # create vectors of known behavior
  vms$fishing <- NA
  vms$fish_tickets <- NA
  #! Ensure vms time to correct time zone
  vms$date.time <- as.POSIXct(vms$date.time,tz="Etc/GMT-8")
  # make both spatial objects so we can calculate distance
  coordinates(vms) <- ~longitude+latitude
  proj4string(vms) <- proj4string(WC)
  
  #! find vms - obs matches
  ID_found = any(obs_locs$cg_num %in% unique(vms$doc.num))
  
  if (ID_found){
    
    # subset observer dataframe (all recorded fishing events [start and stop times])
    # In v_obs look for set_time and up_time, this is the timing of a fishing event
    # search in data_vms any lon lat points that fall in this interval
    # note: v_obs is in decimal time (change to mins secs)
    
    #! Streamline this by getting rid of non_vessels
    v_obs <- obs_locs %>% filter(cg_num == unique(vms$doc.num)) 
    
    all_trips <- unique(v_obs$tripid)
    
    trip_dat <- data.frame()
    # keep track of trips
    for (j in 1:length(all_trips)){
      trip_dips <- v_obs %>% filter(tripid == all_trips[j])

      # make separate df for sets and ups, because both my have NAs and 
      # will drop. Thus these dfs will not be identical. 
      sets <- trip_dips[complete.cases(trip_dips[,c("set_long","set_lat")]),]
      coordinates(sets) <- ~set_long+set_lat
      ups <- trip_dips[complete.cases(trip_dips[,c("up_long","up_lat")]),]
      coordinates(ups) <- ~up_long+up_lat
      
      proj4string(sets) <- proj4string(vms)
      proj4string(ups) <- proj4string(vms)
      
      # find trip corresponding in vms trip
      f_id <- which(vms$date.time >= unique(trip_dips$d_date) & 
                      vms$date.time <= unique(trip_dips$r_date))
      if(length(f_id)==0){
        next
      }else{
        # calculate pairwise distances
        set_dists <- lapply(split(vms[f_id,], vms$date.time[f_id]), 
                            function(x) spDistsN1(sets, x,longlat = TRUE))
        set_dists <- as.data.frame(do.call(rbind, set_dists))
        colnames(set_dists) <- paste0("dist",1:ncol(set_dists))
        
        up_dists <- lapply(split(vms[f_id,], vms$date.time[f_id]),
                           function(x) spDistsN1(ups,x, longlat = TRUE))
        up_dists <- as.data.frame(do.call(rbind, up_dists))
        colnames(up_dists) <- paste0("dist",1:ncol(up_dists))
        
        set_dists$time <- as.POSIXct(rownames(set_dists), tz="Etc/GMT-8")
        up_dists$time <- as.POSIXct(rownames(up_dists), tz="Etc/GMT-8")
        
        min_set <- apply(dplyr::select(set_dists, -time), 1, min)
        min_up <- apply(dplyr::select(up_dists, -time), 1, min)
        
        # for each pairwise distance calculate distance in time from set/haul
        # whichever is closest. If in interval distance = 0
        time_idx <- vector() 
        # initalize to track for each point whether in interval
        for(f in f_id){
          # find differences between each VMS point and set/up times
          time_set <- as.numeric(difftime(
            vms$date.time[f],trip_dips$set_date, units="mins"))
          time_up <- as.numeric(difftime(
            vms$date.time[f],trip_dips$up_date, units="mins"))
        time_diffs <- cbind(time_set, time_up)
        
        # find those that are between
          time_dist <- which(time_diffs[,1]>0 & time_diffs[,2]<0)
        time_idx[f] <- ifelse(length(time_dist)>0, 1, 0) 
        }
        
        time_idx <- time_idx[-which(is.na(time_idx))]
        
        dist_idx <- ifelse(min_set < 5 | min_up < 5, 
                              1, 0)
        
        idx <- cbind(time_idx, dist_idx)
        rownames(idx) <- NULL
        fishing_idx <- rep(0, nrow(idx))
        fishing_idx[which(rowSums(idx)==2)] <- 1 # certain fishing
        fishing_idx[which(rowSums(idx)==1)] <- 3 # uncertain fishing
        
        vms$fishing[f_id] <- fishing_idx
        vms$fish_tickets[f_id] <- unique(trip_dips$fish_tickets)

        trip_list <- data.frame(vessel.ID =gsub(".RDS","",file_names[i]),
                                fish_ticket = unique(trip_dips$fish_tickets),
                                sector = unique(trip_dips$sector), 
                                pcid = unique(trip_dips$pcid),
                                d_date = unique(trip_dips$d_date),
                                r_date = unique(trip_dips$r_date),
                                hrs_fishing_certain = length(which(fishing_idx==1)),
                                hrs_fishing_total = length(which(fishing_idx>0)),
                                stringsAsFactors = FALSE)
        trip_dat <- rbind(trip_dat, trip_list) 
      }
    }
    
    total_trip_dat <- rbind(total_trip_dat, trip_dat)
    
    # Save as new file
    len_name = nchar(infile)
    name = substr(infile,1,len_name-4)
    write.csv(vms, file = paste0("processedData/spatial/vms/intermediate/05_make_obs_to_vms/tw_",window,"hr/",name,".csv"), row.names = FALSE)
    
  }else{
    
    ### Save as new file
    file_name <- gsub(".RDS",".csv",file_names[i])
    save_path <- paste0("processedData/spatial/vms/intermediate/05_make_obs_to_vms/tw_",window,"hr/")
    vms <- as.data.frame(vms)
    write.csv(vms, file = paste0(save_path,file_name), row.names = FALSE)
  }
  
}

# total_trip_dat has a row for each fishing event because going through
# each haul. So want to summarize across each fish_ticket

total_trip_dat <- total_trip_dat %>%
  group_by(vessel.ID, fish_ticket, sector, pcid, d_date, r_date) %>%
  summarize(hrs_fishing_certain = sum(hrs_fishing_certain), 
            hrs_fishing_total = sum(hrs_fishing_total))

# save the trip_total_dat
write.csv(total_trip_dat, 
          paste0("processedData/spatial/vms/intermediate/05_make_obs_to_vms/trip_total_tw",
                 window,"hr.csv"))

