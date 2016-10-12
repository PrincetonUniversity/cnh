# This script will combine the Processed VMS csv files 2009-2016 received in Sep 2016 into a single data frame, cleanse locations we don't want, and split tracks into trips

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
library(raster)
library(dismo)
library(sp)

# Combine all clean RDS files into a single data frame

dropspeeds <- 25

# read in data files

# load files
setwd("/Users/jameal.samhouri/Documents/CNH_to_github/cnh/")
setwd("/Users/jamealsamhouri/Documents/cnh/")

fn3 <-dir("rawData/VMS/Processed VMS csv files 2009-2016_clean")

# ignore .csv files
fn3a <- grep(".csv",fn3 )
if(length(fn3a)>0){
  fn3b <- fn3[-fn3a]
}else{
  fn3b <- fn3
}

fn4 <- paste0("rawData/VMS/Processed VMS csv files 2009-2016_clean/",fn3b)
fn4 <- as.list(fn4)
data.list <- lapply(fn4, function(x) readRDS(x))

# paste all the data files together
df <- do.call(rbind,data.list)
colnames(df) <- tolower(colnames(df))
dim(df)

rm("data.list")

# Cleaning


# remove any rows which are duplicated in their entirety
df <- df[-which(duplicated(df)),]
# started at 15286753 obs. lost 2% of data

# 9/19/16. df is too big to remove rows in one step so break into 4 steps
#length(which(duplicated(df)))
df_tmp <- df[1:(dim(df)[1]/4),]
df_tmp <- df_tmp[-which(duplicated(df_tmp)),]
df_tmp2 <- df[((dim(df)[1]/4)+1):(dim(df)[1]/2),]
df_tmp2 <- df_tmp2[-which(duplicated(df_tmp2)),]
df_tmp3 <- df[((dim(df)[1]/2)+1):((dim(df)[1]/4)*3),]
df_tmp3 <- df_tmp3[-which(duplicated(df_tmp3)),]
df_tmp4 <- df[(((dim(df)[1]/4)*3)+1):(dim(df)[1]),]
df_tmp4 <- df_tmp4[-which(duplicated(df_tmp4)),]

# Find vessel points that occur at different places at the same time

# remove duplicates where vessel has same ID and same date/time
#heisenberg <- which(duplicated(df[,c("vesselname","docnum","datetime")]) | duplicated(df[,c("vesselname","docnum","datetime")], fromLast = TRUE))
# length(heisenberg)
# [1] 46327

#df <- df[-heisenberg,]

heisenberg_tmp <- which(duplicated(df_tmp[,c("vesselname","docnum","datetime")]) | duplicated(df_tmp[,c("vesselname","docnum","datetime")], fromLast = TRUE))
df_tmp <- df_tmp[-heisenberg_tmp,]
heisenberg_tmp2 <- which(duplicated(df_tmp2[,c("vesselname","docnum","datetime")]) | duplicated(df_tmp2[,c("vesselname","docnum","datetime")], fromLast = TRUE))
df_tmp2 <- df_tmp2[-heisenberg_tmp2,]
heisenberg_tmp3 <- which(duplicated(df_tmp3[,c("vesselname","docnum","datetime")]) | duplicated(df_tmp3[,c("vesselname","docnum","datetime")], fromLast = TRUE))
df_tmp3 <- df_tmp3[-heisenberg_tmp3,]
heisenberg_tmp4 <- which(duplicated(df_tmp4[,c("vesselname","docnum","datetime")]) | duplicated(df_tmp4[,c("vesselname","docnum","datetime")], fromLast = TRUE))
df_tmp4 <- df_tmp4[-heisenberg_tmp,]


# make sure we have complete cases (ie, no NAs) for vessel ID, date-time, lat-lon
#complete <- complete.cases(  df[,c("vesselname","docnum","longitude","latitude","datetime")] )
# length(complete)
#df <- df[complete,]
#rm("complete")

complete_tmp <- complete.cases(  df_tmp[,c("vesselname","docnum","longitude","latitude","datetime")] )
# length(complete)
df_tmp <- df_tmp[complete_tmp,]
complete_tmp2 <- complete.cases(  df_tmp2[,c("vesselname","docnum","longitude","latitude","datetime")] )
# length(complete)
df_tmp2 <- df_tmp2[complete_tmp2,]
complete_tmp3 <- complete.cases(  df_tmp3[,c("vesselname","docnum","longitude","latitude","datetime")] )
# length(complete)
df_tmp3 <- df_tmp3[complete_tmp3,]
complete_tmp4 <- complete.cases(  df_tmp4[,c("vesselname","docnum","longitude","latitude","datetime")] )
# length(complete)
df_tmp4 <- df_tmp4[complete_tmp4,]

rm("complete_tmp","complete_tmp2","complete_tmp3","complete_tmp4")

dim(df_tmp)
dim(df_tmp2)
dim(df_tmp3)
dim(df_tmp4)

df <- rbind(df_tmp,df_tmp2,df_tmp3,df_tmp4)

# remove points that are smaller than -180
wpacific <- which(df$longitude< -180)
if(length(wpacific) >0) df <- df[-which(df$longitude< -180),]

saveRDS(df, "processedData/spatial/vms/intermediate/02_cleaned/df_clean.RDS")
write.csv(df,"processedData/spatial/vms/intermediate/02_cleaned/df_clean.csv", row.names=FALSE)

df <- readRDS("processedData/spatial/vms/intermediate/02_cleaned/df_clean.RDS")

sp_df <- SpatialPoints(coords = df[,c("longitude","latitude")],
                       proj4string = CRS("+proj=longlat +datum=WGS84")) # this assigns a projection to the VMS points

nrow(as.data.frame(sp_df))
#[1] 46533674

sp_df2 <- SpatialPointsDataFrame(coords = coordinates(sp_df), data=df,
                       proj4string = CRS("+proj=longlat +datum=WGS84")) # this assigns a projection to the VMS points

as.data.frame(sp_df2[1:10,])

#sp:::head.Spatial(sp_df)
#sp_df[1:7,]
coordinates(sp_df2)[1:10,]
# longitude latitude
# 1  -123.9480 46.07720
# 2  -124.0437 44.63000
# 3  -117.2390 32.75970
# 5  -124.4253 44.42551
# 6  -124.2357 43.65483
# 7  -145.6460 53.23733
# 8  -121.7880 36.80070
# 9  -123.8016 39.42406
# 10 -121.7870 36.80130
# 11 -122.4190 37.80870

# read in west coast relief ascii file from blake
relief <- raster("processedData/spatial/w_coast_dm/w_coast_dm.asc")

##########################################
#########################################
##### try with a small portion of sp_df
##########################################
##########################################
# Start the clock!
ptm <- proc.time()

# try extracting depths corresponding to each location
#df_tmp<- extract(relief,coordinates(sp_df)[1:10,], sp=TRUE)
sp_df3 <- extract(relief,sp_df2[1:10,],sp=TRUE)

# Stop the clock
proc.time() - ptm
# > proc.time() - ptm
# user  system elapsed 
# 29.651  75.307 141.844

# check to see what i did
as.data.frame(sp_df3[1:10,])

##########################################
##########################################
##########################################

##########################################
#########################################
##### try for realz
##########################################
##########################################
# Start the clock!
ptm <- proc.time()

# try extracting depths corresponding to each location
#df_tmp<- extract(relief,coordinates(sp_df)[1:10,], sp=TRUE)
sp_df_full <- extract(relief,sp_df2,sp=TRUE)

# Stop the clock
proc.time() - ptm


# check to see what i did
as.data.frame(sp_df_full[1:10,])

##########################################
##########################################
##########################################

# Find on-land and non-EEZ coast points, and deal with them
# the on-land and non-EEZ part requires splitting the VMS tracks by vessel and processing each one by one. 



# find onland points, only want to drop sequential onland points ----
# load coastline polygon
load("processedData/spatial/2_coastline.Rdata") # this is the NOAA US coastline polygon. note that it is loaded with the name WC
proj4string(WC) <- CRS("+proj=longlat +datum=WGS84") # this assigns a projection to the coastline polygon

# # subset vms points to bounding box of SF bay (based on a quick peek at google maps)
# sfbay <- subset(df, latitude > 36 & latitude < 38 & longitude > -128 & longitude < -122)
# # plot before projection thing
# plot(x=sfbay$longitude, y=sfbay$latitude, asp=1, cex=.15) 
# map("state", add=TRUE)
# # projection thing
# sp_sfbay <- SpatialPoints(coords = sfbay[,c("longitude","latitude")],
#                        proj4string = CRS("+proj=longlat +datum=WGS84"))
# # plot after projection thing
# plot(x=sp_sfbay$longitude, y=sp_sfbay$latitude, asp=1, cex=.15) 
# map("state", add=TRUE)

# no projection issues, so convert lat/lon into spatial points
sp_df <- SpatialPoints(coords = df[,c("longitude","latitude")],
                       proj4string = CRS("+proj=longlat +datum=WGS84")) # this assigns a projection to the VMS points

# generate a vector of 1s if on land and 0s if not
# need to do this as a loop to break it into smaller parts

############################################################
############################################################

# commercial break to figureout how big sp_df can be before breaking my computer

sp_df1 <- sp_df[1:10000,]
# 100
#    user  system elapsed 
#1.115   0.115   1.260 
# 10000
# > proc.time() - ptm
# user  system elapsed 
# 66.694   0.353  67.158 

rows_sp_df1 <- nrow(as.data.frame(sp_df1))
# rows_sp_df/(10^5)
# rows_sp_df1 %% (10^5) # this is the remainder

onland <- c() # initialize a vector

#loops <- 2

# Start the clock!
ptm <- proc.time()
for(i in 1:(rows_sp_df1)){
  
  #df$onland[((i*10^5)-10^5+1):(i*10^5)] <- as.vector(gContains(WC, sp_df[((i*10^5)-10^5+1):(i*10^5),], byid=TRUE)) # TRUE values are on land
  onland <- as.vector(gContains(WC, sp_df1, byid=TRUE)) # TRUE values are on land
  # (albeit 1 column, many rows -- but messes things up)
  # need as.vector() because gContains returns a vector 
  print(paste("Loop",i,"complete"))
}

#onland <- cbind(onland,
#                as.vector(gContains(WC, sp_df[((i*10^5)+1):rows_sp_df,], 
#                                    byid=TRUE))) 
# Stop the clock
proc.time() - ptm

############################################################
############################################################

# end of commercial break. get back to business
# generate a vector of 1s if on land and 0s if not

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

