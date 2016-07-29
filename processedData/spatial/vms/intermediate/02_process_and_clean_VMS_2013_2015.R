# This script will combined the 2013-2015 VMS data received in May 2016 into a single data frame, cleanse locations we don't want, and split tracks into trips

# Combine all clean RDS files into a single data frame
# Find vessel points that occur at different places at the same time, and on-land and non-EEZ coast points, and deal with them
  # the on-land and non-EEZ part requires splitting the VMS tracks by vessel and processing each one by one
# Also drop all speeds >dropspeeds, currently=25

rm(list=ls())

library(stringr)
library(rgdal)
library(rgeos)
library(sp)
library(maps)
library(reshape2)

# Combine all clean RDS files into a single data frame

dropspeeds <- 25

# read in data files

# load files
setwd("/Users/jameal.samhouri/Documents/CNH_to_github/cnh/")

fn3 <-dir("rawData/VMS/VMSdata_csv_NMFS16-002_clean")

# ignore .csv files
fn3a <- grep(".csv",fn3 )
if(length(fn3a)>0){
  fn3b <- fn3[-fn3a]
}else{
  fn3b <- fn3
}

fn4 <- paste0("rawData/VMS/VMSdata_csv_NMFS16-002_clean/",fn3b)
fn4 <- as.list(fn4)
data.list <- lapply(fn4, function(x) readRDS(x))

# paste all the data files together
df <- do.call(rbind,data.list)
colnames(df) <- tolower(colnames(df))

rm("data.list")

# Cleaning


# remove any rows which are duplicated in their entirety
df <- df[-which(duplicated(df)),]
# started at 15286753 obs. lost 2% of data

# Find vessel points that occur at different places at the same time

# remove duplicates where vessel has same ID and same date/time
heisenberg <- which(duplicated(df[,c("vesselname","docnum","datetime")]) | duplicated(df[,c("vesselname","docnum","datetime")], fromLast = TRUE))
# length(heisenberg)
# [1] 46327

df <- df[-heisenberg,]

# make sure we have complete cases (ie, no NAs) for vessel ID, date-time, lat-lon
complete <- complete.cases(  df[,c("vesselname","docnum","longitude","latitude","datetime")] )
# length(complete)
df <- df[complete,]

rm("complete")

# Find on-land and non-EEZ coast points, and deal with them
# the on-land and non-EEZ part requires splitting the VMS tracks by vessel and processing each one by one. 

# find onland points, only want to drop sequential onland points ----
# load coastline polygon
load("processedData/spatial/2_coastline.Rdata") # this is the NOAA US coastline polygon. note that it is loaded with the name WC
proj4string(WC) <- CRS("+proj=longlat +datum=WGS84") # this assigns a projection to the coastline polygon

# convert lat/lon into spatial points
# remove points that are smaller than -180
wpacific <- which(df$longitude< -180)
if(length(wpacific) >0) df <- df[-which(df$longitude< -180),]


sp_df <- SpatialPoints(coords = df[,c("longitude","latitude")],
                       proj4string = CRS("+proj=longlat +datum=WGS84")) # this assigns a projection to the VMS points

# generate a vector of 1s if on land and 0s if not
# need to do this as a loop to break it into smaller parts

rows_sp_df <- nrow(as.data.frame(sp_df))
# rows_sp_df/(10^5)
# rows_sp_df %% (10^5) # this is the remainder
loops <- round(rows_sp_df/(10^5))

onland <- c() # initialize a vector

#loops <- 2

# Start the clock!
ptm <- proc.time()
for(i in 1:(loops)){
  
  #df$onland[((i*10^5)-10^5+1):(i*10^5)] <- as.vector(gContains(WC, sp_df[((i*10^5)-10^5+1):(i*10^5),], byid=TRUE)) # TRUE values are on land
  
  onland <- cbind(onland,
                  as.vector(gContains(WC, sp_df[((i*10^5)-10^5+1):(i*10^5),], byid=TRUE))
                  ) # TRUE values are on land
  # (albeit 1 column, many rows -- but messes things up)
  # need as.vector() because gContains returns a vector 
  print(paste("Loop",i,"complete"))
}

onland <- cbind(onland,
                as.vector(gContains(WC, sp_df[((i*10^5)+1):rows_sp_df,], 
                                    byid=TRUE))) 
# Stop the clock
proc.time() - ptm

# because the 105th column has fewer rows than the rest of the columns, check to see that there is just some recycling happening. and there is! so cut those junk rows on line 125
# all(onland[55482:nrow(onland),105] == onland[1:(10^5-55481),105])

# need to melt first
d <- melt(onland)[,3]
df$onland <- d[1:rows_sp_df]

# remove sequential on-land points ----

# arrange by vessel name, and date-time. 
  df <- df[order(df$docnum, df$datetime),]

# then split into a list, where each element is a vessel
  vessel_tracks <- split(df, df$docnum)


# head(df %>% filter(vesselname == "3 Sons") %>% dplyr::select(vesselname, docnum) %>% distinct()) 
#      %>% group_by(vesselname)
#      %>% summarize(n_docnums = length(docnum)))

# process the VMS tracks by vessel one by one


for(k in 1:length(vessel_tracks)){
  saveRDS(vessel_tracks[[k]], paste0("processedData/spatial/vms/intermediate/02_cleaned/vessel_track",k,".RDS"))
}

# helper function to identify and remove sequential on-land points
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

vessel_tracks_dir <- dir("processedData/spatial/vms/intermediate/02_cleaned")[grep("vessel_track",dir("processedData/spatial/vms/intermediate/02_cleaned"))] #

# merge with doc.numers ----
# vessel_codes <- read.csv("../West_Coast_Vessels.csv",
#                          header=TRUE,stringsAsFactors=FALSE)
# colnames(vessel_codes) <- c("ship.number","doc.num","alt.vessel.name")

# use the helper function to remove sequential on-land points and remove abnormally high speeds
# will produce an .RDS file for each vessel that represents all of its trips over the length of the time series
for(i in 1:length(vessel_tracks_dir)){
  # load vms track
  one_ves <- readRDS(paste0("processedData/spatial/vms/intermediate/02_cleaned/",vessel_tracks_dir[i]))
  # remove onland points
  fish_tracks <- rm_onland(one_ves)
  if(length(fish_tracks)==1) next
  # remove any abnormally high speeds
  if(length(which(fish_tracks$avg.speed>dropspeeds)) >0) {fish_tracks <- fish_tracks[-which(fish_tracks$avg.speed>dropspeeds),]}
  # if there are no off-land points, don't save
  #fish_tracks <- merge(x = fish_tracks, y = vessel_codes, by = "docnum", all.x = TRUE, all.y = FALSE)
  # save the processed vms track
  saveRDS(fish_tracks, paste0("processedData/spatial/vms/intermediate/02_cleaned/fish_tracks",unique(fish_tracks$docnum),".RDS"))
}

# next overlap metiers (03), which subsets to VMS trips that are also in the fish ticket data. should only need to change paths. need to copy tickets.RDS and Samhouri observer data 
# will still need to run 04_link_mets with emma
# this gives each trip a unique ID by breaking trips up based on each return to land
# the output will be what we Blake needs to map crab trips and we need to look at VMS coverage of crab trips

