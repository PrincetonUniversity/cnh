
##### CONNECT OBSERVER DATA TO VMS DATA
#! goal: add another column to the VMS dataframes (for each vessel)
#! with an index for what was caught when

#! delete existing files
system("rm -f ./VMS/CSV/*.csv")

#! load observer data
data_obs <- read.csv(file="./Observer/Observer_data.csv", header=TRUE, sep=",")


#! Names of all files (organized by vessel)
names = dir('./VMS/RDS/')


#! Create empty array to be filled with haul times
#HAUL = matrix(0,0,2)

# load spatial transform package
library(rgdal)

#! load vessel data
#ANY_MATCH = rep(NA,length(names))
for (i in 1:length(names)){
  ### Load up
  print(i / length(names))
  infile = names[i]
  data_vms <- readRDS(paste("./VMS/RDS/",infile,sep = ""))
  
  #! New dataframe for fishing and observer reference
  fishing  = matrix(0,dim(data_vms)[1])
  observed = matrix(0,dim(data_vms)[1])
  
  #! Set vms time to correct time zone
  data_vms$date.time <- as.POSIXct(data_vms$date.time,tz="Etc/GMT-8")
  
  #! For each vessel, check the presense of any match
  ANY_MATCH <- length(which(unique(as.character(data_obs$CG_NUM)) %in% unique(data_vms$doc.num)))
  
  #! Change projection
  # make a spatial object
  coordinates(data_vms) <- ~longitude + latitude
  proj4string(data_vms) <- CRS("+proj=aea +lat_1=35.13863306500551 +lat_2=46.39606296952133 +lon_0=-127.6171875")
  
  # re-project to lat/lon decimal degrees
  data_vms <- spTransform(data_vms, CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
  
  # change back into data.frame
  data_vms <- as.data.frame(data_vms)
  
  if (ANY_MATCH[1] != 0){

    #! find vms - obs matches
    ID = which(data_obs$CG_NUM %in% unique(data_vms$doc.num))
    
    # subset observer dataframe (all recorded fishing events [start and stop times])
    # In v_obs look for set_time and up_time, this is the timing of a fishing event
    # search in data_vms any lon lat points that fall in this interval
    # note: v_obs is in decimal time (change to mins secs)
    
    #! Streamline this by getting rid of duplicates
    v_obs <- data_obs[ID,] # vessel that has been observed
    v_obs <- unique(v_obs[,c("HAUL_ID","SET_YEAR","SET_MONTH","SET_DAY","SET_TIME",
                             "UP_YEAR","UP_MONTH","UP_DAY","UP_TIME",
                             "D_DATE","R_DATE","FISH_TICKETS")])
 
    for (j in 1:dim(v_obs)[1]){
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
                    data_vms$trip_id5,data_vms$trip_id6)
  
  ### Save as new file
  len_name = nchar(infile)
  name = substr(infile,1,len_name-4)
  write.csv(save_vms,file=paste("./VMS/CSV/",name,".csv",sep = ""),row.names = FALSE)
  
  }else{

  ### fished and observed lists to vms dataframe
  #data_vms = cbind(data_vms,fishing)
  #data_vms = cbind(data_vms,observed) 
  save_vms <- data.frame(data_vms$longitude,data_vms$latitude,data_vms$date.time,data_vms$dist_coast,data_vms$only_trips,
                           data_vms$metier.2010,fishing,observed,data_vms$trip_id1,data_vms$trip_id2,data_vms$trip_id3,data_vms$trip_id4,
                           data_vms$trip_id5,data_vms$trip_id6)
    
  ### Save as new file
  len_name = nchar(infile)
  name = substr(infile,1,len_name-4)
  write.csv(save_vms,file=paste("./VMS/CSV/",name,".csv",sep = ""),row.names = FALSE)
  }
  
}









