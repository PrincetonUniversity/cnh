# taking processed VMS data and sewing together
location = "local"

if(location == "della"){
  libz = "/tigress/efuller/R_packages"
  library(sp, lib.loc = libz)
  library(rgdal, lib.loc = libz)
  library(rgeos, lib.loc = libz)
  library(maps, lib.lo = libz)
  setwd("/tigress/efuller/process_vms/processed")
}

if(location == "local"){
  library(sp)
  library(rgdal)
  library(rgeos)
  library(maps)
  setwd("/Users/efuller/1/CNH/processedData/spatial/vms/intermediate/01_processed/")
}
# aggregate dataframes ----
  processed_vms <- dir()

# make one big dataset
  df <- as.data.frame(do.call(rbind, lapply(processed_vms, "readRDS")))
# remove duplicates where vessel has same ID and same date/time
  df <- df[-which(duplicated(df[,c("vessel.name","doc.num","date.time")]) | duplicated(df[,c("vessel.name","doc.num","date.time")], fromLast = TRUE)),]
# make sure we have complete cases for vessel ID, date-time, lat-lon
  df <- df[complete.cases(
    df[,c("vessel.name","doc.num","longitude","latitude","date.time")]),]

# find onland points ----
  # load coastline polygon
    load("../2_coastline.Rdata")
    proj4string(WC) <- CRS("+proj=longlat +datum=WGS84")
  # convert lat/lon into spatial pointsi
  # remove points that are smaller than -180
    df <- df[-which(df$longitude< -180),]
    sp_df <- SpatialPoints(coords = df[,c("longitude","latitude")],
                           proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  df$onland <- as.vector(gContains(WC, sp_df, byid=TRUE)) # TRUE values are on land
# need as.vector() because gContains returns a vector 
# (albeit 1 column, many rows -- but messes things up)

# remove sequential on-land points ----
# arrange by vessel name, and date-time. 
  df <- df[order(df$vessel.name, df$date.time),]
# then split into a list
  vessel_tracks <- split(df, df$doc.num)

# intermediate: split the VMS tracks by vessel and process one by one

for(k in 1:length(vessel_tracks)){
   saveRDS(vessel_tracks[[k]], paste0("vessel_track",k,".RDS"))
}
  
  # helper function
    rm_onland <- function(data){
      if(!any(as.numeric(data$onland)==0)){
        warning('no on land points!')
      }else{
        px.df <- data.frame(p = as.numeric(data$onland), 
                            dp = c(0,diff(as.numeric(data$onland))), 
                            idp = c(diff(as.numeric(data$onland)),0))
        # make sure there are data to remove
        to_remove <- which(px.df$p == 1 & px.df$dp == 0 & px.df$idp == 0) 
        if(length(to_remove)>0){
          try.df <- data[-which(px.df$p == 1 & px.df$dp == 0 & px.df$idp == 0),]
        }else{
          # else just return the original data
          try.df <- data
        }
        return(try.df)
      }
    }

vessel_tracks <- dir()[grep("vessel_track",dir())]

# merge with doc.numers ----
  vessel_codes <- read.csv("../West_Coast_Vessels.csv",
                           header=TRUE,stringsAsFactors=FALSE)
  colnames(vessel_codes) <- c("ship.number","doc.num","alt.vessel.name")

for(i in 1:length(vessel_tracks)){
  # load vms track
  one_ves <- readRDS(vessel_tracks[i])
  # remove onland points
  fish_tracks <- rm_onland(one_ves)
  if(length(fish_tracks)==1) next
  # remove any abnormally high speeds
  fish_tracks <- fish_tracks[-which(fish_tracks$avg.speed>25),]
  # if there are no off-land points, don't save
  fish_tracks <- merge(x = fish_tracks, y = vessel_codes, by = "doc.num", all.x = TRUE, all.y = FALSE)
  # save the processed vms track
  saveRDS(fish_tracks, paste0("fish_tracks",unique(fish_tracks$doc.num),".RDS"))
}

