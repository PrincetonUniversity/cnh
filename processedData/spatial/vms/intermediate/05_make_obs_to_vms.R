library(dplyr)
library(tidyr)
# load data ----
  #! delete existing files
  system("rm -f /Users/efuller/1/CNH/processedData/spatial/vms/intermediate/05_make_obs_to_vms/*.csv")
  # load observer data
  data_obs <- read.csv(
    "/Users/efuller/Desktop/CNH/rawData/Observers/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv",stringsAsFactors = FALSE)
  colnames(data_obs) <- tolower(colnames(data_obs))
  data_obs <- data_obs[,-grep("spgrftob1",colnames(data_obs))[1]]

  # choose time window
  window = 24 # 0, 24, 36, 72, 168
  
  # names of vessel tracks
  path = paste0("/Users/efuller/Desktop/CNH/processedData/spatial/vms/intermediate/04_link_mets_vms/tw_",window,"hr/")
  names = dir(path)

  if(any(grepl("no_vms_landings", names))){
    names = names[-grep("no_vms_landings",names)]
    }

# set up data objects ----
# change observer cg.num to character to match classes
data_obs$cg_num <- as.character(data_obs$cg_num)
data_obs$d_date <- as.POSIXct(data_obs$d_date, format = "%")

#! load vessel data
for (i in 1:length(names)){
  ### Load up
  infile = names[i]
  data_vms <- readRDS(paste0(path,infile))
  
  # create vectors of known behavior
  data_vms$fishing <- NA
  data_vms$observed <- NA

  #! Ensure vms time to correct time zone
  data_vms$date.time <- as.POSIXct(data_vms$date.time,tz="Etc/GMT-8")
  
  #! find vms - obs matches
  ID = which(data_obs$cg_num %in% unique(data_vms$doc.num))
  
  if (length(ID)>0){

    # subset observer dataframe (all recorded fishing events [start and stop times])
    # In v_obs look for set_time and up_time, this is the timing of a fishing event
    # search in data_vms any lon lat points that fall in this interval
    # note: v_obs is in decimal time (change to mins secs)
    
    #! Streamline this by getting rid of duplicates
    v_obs <- data_obs %>%
      filter(cg_num == unique(data_vms$doc.num)) %>%
      dplyr::select(haul_id, set_year, set_month, set_day, set_time, up_year,
                    up_month, up_day, up_time, d_date, r_date, fish_tickets) %>%
      distinct()
    
    for (j in 1:nrow(v_obs)){
      ### Fishing event
      SET <- v_obs[j,]
      
      ### Fishing gear in
      yr <- SET$SET_YEAR
      mt <- SET$SET_MONTH
      dy <- SET$SET_DAY
      hr <- SET$SET_TIME
      mn <- (hr - floor(hr)) * 60
      hr <- floor(hr)
      TIME_in <- as.POSIXct(paste(yr,mt,dy,hr,mn,sep='-'),
                            format="%Y-%m-%d-%H-%M",tz="Etc/GMT-8") - (1*60*60)
      
      ### Fishing gear out
      yr <- SET$UP_YEAR
      mt <- SET$UP_MONTH
      dy <- SET$UP_DAY
      hr <- SET$UP_TIME
      mn <- (hr - floor(hr)) * 60
      hr <- floor(hr)
      TIME_out <- as.POSIXct(paste(yr,mt,dy,hr,mn,sep='-'),
                             format="%Y-%m-%d-%H-%M",tz="Etc/GMT-8") + (1*60*60)
      
      ### Find vms between in and out times
      f_id <- which(data_vms$date.time >= TIME_in & data_vms$date.time <= TIME_out)
      
      ### add to fishing
      fishing[f_id] = as.character(SET$FISH_TICKETS)
      
      
      ### Observed start
      d_date  <-  as.character(SET$D_DATE)
      OBS_dep <-  as.POSIXct(d_date,format="%m/%d/%Y %I:%M:%S %p",tz="Etc/GMT-8")
      
      ### Observed end
      r_date  <- as.character(SET$R_DATE)
      OBS_ret <-  as.POSIXct(r_date,format="%m/%d/%Y %I:%M:%S %p",tz="Etc/GMT-8")
      
      ### Was trip observerd
      o_id <- which(data_vms$date.time >= OBS_dep & data_vms$date.time <= OBS_ret)
      
      ### add to observed list
      observed[o_id] = as.character(SET$FISH_TICKETS)
      
      ### Create histogram of haul times
      #HAUL = rbind(HAUL,c(TIME_out-TIME_in,as.character(SET$COMMON_NAME)))
    }
    
  ### fished and observed lists to vms dataframe
  #data_vms = cbind(data_vms,fishing)
  #data_vms = cbind(data_vms,observed)
  save_vms <- data.frame(data_vms$longitude,data_vms$latitude,data_vms$date.time,data_vms$dist_coast,data_vms$only_trips,
                    data_vms$metier.2010,fishing,observed,data_vms$trip_id1,data_vms$trip_id2,data_vms$trip_id3,data_vms$trip_id4,
                    data_vms$trip_id5,data_vms$trip_id6, stringsAsFactors = FALSE)
  
  ### Save as new file
  len_name = nchar(infile)
  name = substr(infile,1,len_name-4)
  if(user == "james") write.csv(save_vms,file=paste("./VMS/CSV/",name,".csv",sep = ""),row.names = FALSE)
  if(user == "emma") write.csv(save_vms, file = paste0("/Users/efuller/1/CNH/processedData/spatial/vms/intermediate/05_make_obs_to_vms/",name,".csv"), row.names = FALSE)
  
  }else{

  save_vms <- dplyr::select(data_vms, longitude, latitude, date.time, 
                            dist_coast, only_trips, metier.2010, fishing, 
                            observed, contains("trip_id"))

  ### Save as new file
  file_name <- gsub(".RDS",".csv",names[i])
  save_path <- paste0("/Users/efuller/Desktop/CNH/processedData/spatial/vms/intermediate/05_make_obs_to_vms/tw_",window,"hr/")
  write.csv(save_vms, file = paste0(save_path,file_name), row.names = FALSE)
  }
  
}








