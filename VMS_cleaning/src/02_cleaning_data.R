# taking processed VMS data and sewing together
library(rgdal)
library(rgeos)
library(sp)
library(maps)
setwd("/Volumes/storage/processed/")

# aggregate dataframes ----
  processed_vms <- dir()

# make one big dataset
  df <- as.data.frame(do.call(rbind, lapply(processed_vms, "readRDS")))
# remove duplicates where vessel has same ID and same date/time
  df <- df[-which(duplicated(df[,c("vessel.name","doc.num","date.time")])),]
# make sure we have complete cases for vessel ID, date-time, lat-lon
  df <- df[complete.cases(
    df[,c("vessel.name","doc.num","longitude","latitude","date.time")]),]

# find onland points ----
  # load coastline polygon
    load("/Users/efuller/1/CNH/Analysis/VMS/results/2014-10-29/2_coastline.Rdata")
    proj4string(WC) <- CRS("+proj=longlat +datum=WGS84")
  # convert lat/lon into spatial points
    sp_df <- SpatialPoints(coords = df[,c("longitude","latitude")],
                           proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  df$onland <- as.vector(gContains(WC, sp_df, byid=TRUE)) # TRUE values are on land
# need as.vector() because gContains returns a vector 
# (albeit 1 column, many rows -- but messes things up)

# remove sequential on-land points ----
# arrange by vessel name, and date-time. 
  df <- df[order(df$vessel.name, df$date.time),]
# then split into a list
  vessel_tracks <- split(df, df$vessel.name)
# and each list remove all onland points except for start and end
  fish_tracks <- vector("list", length = length(vessel_tracks))
  
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

  for(i in 1:length(vessel_tracks)){
    one_ves <- vessel_tracks[[i]]
    fish_tracks[[i]] <- rm_onland(one_ves)
  }

# remove all vessels that never leave land 
  moving_fish <- fish_tracks[-which(summary(fish_tracks)[,1]==1)]

# make a new dataframe 
  moving_df <- do.call(rbind, moving_fish)

# filter to remove vessels with speeds greater than 25 knots ----
  moving_df <- moving_df[-which(moving_df$avg.speed>25),]

# merge with doc.numers ----
  vessel_codes <- read.csv("/Users/efuller/1/CNH/Data/VMS/West_Coast_Vessels.csv",
                           header=TRUE,stringsAsFactors=FALSE)
  colnames(vessel_codes) <- c("ship.number","doc.num","alt.vessel.name")

  merged_df <- base::merge(x = moving_df, y = vessel_codes, by="doc.num",
                     all.x = TRUE, all.y = FALSE)

# save final dataframe ----
  saveRDS(merged_df, "/Users/efuller/1/CNH/VMS_cleaning/src/VMS.RDS")