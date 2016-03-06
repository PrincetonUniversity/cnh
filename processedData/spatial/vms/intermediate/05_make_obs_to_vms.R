library(dplyr)
library(tidyr)
# make sure working directory is set to CNH/
# load data ----
# load condensed obs version, if doesn't exist load and format raw
  if(file.exists("processedData/both/obs_data/obs.RDS")){
    obs <- readRDS("processedData/both/obs_data/obs.RDS")
  }else{
    # load observer data
    obs <- read.csv(
      "rawData/Observers/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv",
      stringsAsFactors = FALSE)
    colnames(obs) <- tolower(colnames(obs))
    obs <- obs[,-grep("spgrftob1",colnames(obs))[1]]
    
    obs$cg_num <- as.character(obs$cg_num)
    saveRDS(obs, "processedData/both/obs_data/obs.RDS")
  }
  
# load VMS data
  # choose time window
  window = 24 # 0, 24, 36, 72, 168
  
  # names of vessel tracks
  path = paste0("processedData/spatial/vms/intermediate/04_link_mets_vms/tw_",window,"hr/")
  file_names = dir(path)
  
  # drop any no_vms_landings names
  if(any(grepl("no_vms_landings", file_names))){
    names = names[-grep("no_vms_landings",names)]
    }

# double check time makes sense for set and up ----
  # up and set times are decimal minutes. clean up
  make_fullHrMin <- function(time_vector){
    hour_vec <- sprintf("%02.0f",floor(time_vector))
    min_vec <- sprintf("%02.0f",(time_vector - floor(time_vector))*60)
    min_hour <- paste(hour_vec, min_vec, sep=":")
    return(min_hour)
  }
  set_time <- make_fullHrMin(obs$set_time) 
  up_time <- make_fullHrMin(obs$up_time)
  
  set_datetime <- as.POSIXct(
    paste0(obs$set_year,"-",obs$set_month,"-",obs$set_day," ", set_time),
    format= "%Y-%m-%d %H:%M", tz = "Etc/GMT-8"
    )
  
  up_datetime <- as.POSIXct(
    paste0(obs$up_year,"-",obs$up_month,"-",obs$up_day," ", up_time),
    format= "%Y-%m-%d %H:%M", tz = "Etc/GMT-8"
  )
  
  any(set_datetime>up_datetime) # should be FALSE
  
# double check departure and return times
  # d/r_dates at midnight don't have any hours, add them
  add_fullTime <- function(date_vec){
    no_time <- which(nchar(date_vec)<11)
    date_vec[no_time] <- paste(date_vec[no_time], "12:00:00 AM")
    
    full_time <- as.POSIXct(date_vec, format = "%m/%d/%Y %I:%M:%S %p", 
                              tz = "Etc/GMT-8")
    return(full_time)
  }
  d_dates <- add_fullTime(obs$d_date)
  r_dates <- add_fullTime(obs$r_date)
  
  any(is.na(d_dates)) # should be false
  any(is.na(r_dates)) # should be false
  
  all(d_dates < r_dates) # should be true
  
# double check set and return dates relative to depart and return
  length(which(set_datetime<d_dates)) # bad
  length(which(up_datetime<d_dates)) # good
  
  which(up_datetime>r_dates) # bad
  which(set_datetime>r_dates) # bad, but all in the above
  
  # all is fixed gear  - could theoretically make sense 
  # but how do observers know these times?
  
  table(obs$gear_type[which(up_datetime>r_dates)]) 
  table(obs$sector[which(set_datetime<d_dates)]) 
  
  # save for further inspection
  mismatch_time <- obs[c(which(up_datetime>r_dates), 
                         which(set_datetime<d_dates)),]
  write.csv(mismatch_time, "processedData/both/obs_data/mismatch_time.csv")
  
  # drop these for now
  obs <- obs[-c(which(up_datetime>r_dates), 
              which(set_datetime<d_dates)),]

  
  
# set up data objects ----
# keep track of fish ticket, sector, vessel, port, and date for 
# observed trips and amount of time spent fishing
  
  # keep track of trip data
  total_trip_dat <- data.frame()
#! load vessel data ----
  
for (i in 1:length(file_names)){

  ### Load up
  infile = file_names[i]
  vms <- readRDS(paste0(path,infile))
  
  # create vectors of known behavior
  vms$fishing <- NA
  vms$observed <- NA
  vms$sector <- NA # keep this for curiousity
  vms$pcid <- NA # keep this for curiousity

  #! Ensure vms time to correct time zone
  vms$date.time <- as.POSIXct(vms$date.time,tz="Etc/GMT-8")
  
  #! find vms - obs matches
  ID_found = any(obs$cg_num %in% unique(vms$doc.num))
  
  if (ID_found){

    # subset observer dataframe (all recorded fishing events [start and stop times])
    # In v_obs look for set_time and up_time, this is the timing of a fishing event
    # search in data_vms any lon lat points that fall in this interval
    # note: v_obs is in decimal time (change to mins secs)
    
    #! Streamline this by getting rid of duplicates
    v_obs <- obs %>%
      filter(cg_num == unique(vms$doc.num)) %>%
      dplyr::select(tripid, haul_id, set_year, set_month, set_day, set_time, up_year,
                    up_month, up_day, up_time, d_date, r_date, fish_tickets, pcid,
                    sector) %>%
      distinct()
    
    # make times for set and up
    set_time <- make_fullHrMin(v_obs$set_time) 
    up_time <- make_fullHrMin(v_obs$up_time)
    
    set_datetime <- as.POSIXct(
      paste0(v_obs$set_year,"-",v_obs$set_month,"-",v_obs$set_day," ", set_time),
      format= "%Y-%m-%d %H:%M", tz = "Etc/GMT-8"
    )
    
    up_datetime <- as.POSIXct(
      paste0(v_obs$up_year,"-",v_obs$up_month,"-",v_obs$up_day," ", up_time),
      format= "%Y-%m-%d %H:%M", tz = "Etc/GMT-8"
    )
    
    # make depart and return real times
    d_dates <- add_fullTime(v_obs$d_date)
    r_dates <- add_fullTime(v_obs$r_date)
    
    trip_dat <- data.frame()
    # keep track of trips
    for (j in 1:nrow(v_obs)){
      # Find vms between in and out times
      f_id <- which(vms$date.time >= set_datetime[j] & 
                      vms$date.time <= up_datetime[j])
      
      # add to fishing
      vms$fishing[f_id] = as.character(v_obs$fish_tickets[j])
      
      # Was trip observerd
      o_id <- which(vms$date.time >= d_dates[j] & 
                      vms$date.time <= r_dates[j])
      
      # add to observed list
      vms$observed[o_id] = as.character(v_obs$fish_tickets[j])
      vms$pcid[o_id] <- v_obs$pcid[j]
      vms$sector[o_id] <- v_obs$sector[j]
      
      # add to table
      trip_list <- data.frame(vessel.ID =gsub(".RDS","",file_names[i]),
                     fish_ticket = v_obs$fish_tickets[j],
                     sector = v_obs$sector[j], 
                     pcid = v_obs$pcid[j],
                     d_date = d_dates[j],
                     r_date = r_dates[j],
                     hrs_fishing = length(f_id),
                     stringsAsFactors = FALSE)
      trip_dat <- rbind(trip_dat, trip_list)
    }
    
    total_trip_dat <- rbind(total_trip_dat, trip_dat)
    
    any(!(is.na(vms$fishing)) & is.na(vms$observed)) # should be false
    
  # Save as new file
  len_name = nchar(infile)
  name = substr(infile,1,len_name-4)
  write.csv(vms, file = paste0("processedData/spatial/vms/intermediate/05_make_obs_to_vms/tw_",window,"hr/",name,".csv"), row.names = FALSE)
  
  }else{

  ### Save as new file
  file_name <- gsub(".RDS",".csv",file_names[i])
  save_path <- paste0("processedData/spatial/vms/intermediate/05_make_obs_to_vms/tw_",window,"hr/")
  write.csv(vms, file = paste0(save_path,file_name), row.names = FALSE)
  }
  
}

# total_trip_dat has a row for each fishing event because going through
# each haul. So want to summarize across each fish_ticket
  
total_trip_dat <- total_trip_dat %>%
  group_by(vessel.ID, fish_ticket, sector, pcid, d_date, r_date) %>%
  summarize(hrs_fishing = sum(hrs_fishing))

# save the trip_total_dat
write.csv(total_trip_dat, 
          paste0("processedData/spatial/vms/intermediate/05_make_obs_to_vms/trip_total_tw",
                 window,"hr.csv"))




