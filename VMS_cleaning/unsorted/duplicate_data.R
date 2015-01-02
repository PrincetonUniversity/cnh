# This is a script starts the same as format_data.R, but is focused on examining characteristics of duplicate relocations


rm(list = ls())

library(plyr)
library(sp)
library(corpcor)
library(ggplot2)
library(maps)
require(mapdata)
require(adehabitatLT)

# setwd('~/Documents/Harvesting_Strategies/EmpiricalAnalysis/NOAA/Data_Analysis/Data')  # local
setwd('~/NOAA/Data_Analysis/Data')  # ursus

data <- read.csv("VMS.txt",header=FALSE,strip.white=TRUE,stringsAsFactors=FALSE)

foo <- data[1:5000000,]    # take smaller subset to play with 
nrow(foo)/nrow(data)              # approx 20% of total data
data <- foo


###################################
# Reading and formatting Raw Data #
###################################

# since adding the file name to each row (for tracking purposes) There are more blank lines. To find these, search for column 3 NAs and remove
data <- data[-which(data[,3]==""),]

# convert latitude and longitude to decimal degrees. To do this I need to make a seperate column for seconds. 
lat_minutes <- as.numeric(data[,4]) # this generates some NAs in remnent header rows. remove them. 

# this generates NAs in a few more empty rows. These rows originally said something like 'xxxx rows selected." where xxx is a huge number. Now I can search for and remove any NAs in data$minutes
data <- data[-which(is.na(lat_minutes)),]

lat_minutes <- lat_minutes[-which(is.na(lat_minutes))]
lon_minutes <- as.numeric(as.character(data[,6]))

# Now I can make a column for seconds
lat_seconds <- lat_minutes - floor(lat_minutes)
lon_seconds <- lon_minutes - floor(lon_minutes)

# Now for decimal degrees. According to this website (http://andrew.hedges.name/experiments/convert_lat_long/) I can get decimal degrees by dividing the minutes by 60 and the seconds by 3600.
latitude <- as.numeric(as.character(data[,3])) + lat_minutes/60 + lat_seconds/3600
longitude <-as.numeric(as.character(data[,5])) - lon_minutes/60 - lon_seconds/3600
# because longitudes are all negative, we're dealing with the west coast

# clean up dataframe so only characters and numeric data
VMS <- data.frame(Vessel_Name = data[,1])
VMS$Ship_No <- as.character( data[,2])
VMS$Latitude <- latitude
VMS$Longitude <- longitude
VMS$Date_Time <- data[,7]
VMS$Avg_Speed <- as.numeric(as.character(data[,8]))
VMS$Avg_Direction <- as.numeric(as.character(data[,9]))
VMS$Declarations <- as.numeric(as.character(data[,10])) 
# this should generate some NAs because some declarations are NA

# next is to convert the dates and times into date and time objects, first need to split dates and times apart  Because they are currently in a format POSIX recognizes I can call POSIX directly
dt <- strptime(VMS$Date_Time, format='%Y-%m-%d %H:%M', tz = 'America/Los_Angeles' )
dt.pos <- as.POSIXct(dt, tz = 'America/Los_Angeles')
dtl <- as.POSIXlt(dt.pos, tz = 'America/Los_Angeles')

VMS$year = dtl$year + 1900 # month, add 1 since POSIXlt starts at 0	
VMS$month = dtl$mon+1 # month, add 1 since POSIXlt starts at 0	
VMS$day = dtl$mday # day of the month
VMS$time = paste(formatC(dtl$hour, width=2, flag=0), formatC(dtl$min, width=2, flag=0), sep=':')

###############################
# Looking for duplicate pings #
###############################

anyDuplicated(VMS)	# 152

# look at duplicates
c <- duplicated(VMS) | duplicated(VMS, fromLast=TRUE)
look_dupz <- VMS[c,]
head(look_dupz)   # do indeed look like replicates

# if duplicated elements are fully duplicated (all info is the same, can remove those)
nrow(VMS)	# 22305300
VMS <- VMS[!duplicated(VMS),]
nrow(VMS)	# 22174033

# just look for lines where vessel name, date and time are duplicated. 
# this will include lat/lons, speeds, directions that are different
search_dups <- VMS[,c(1,2,9,10,11,12)]

i = duplicated(search_dups) | duplicated(search_dups, fromLast=TRUE)	
# gives TRUE for all entries of duplicate element	

# dataframe of all duplicates
examine_dups <- VMS[i,]

# flagging in VMS data set
VMS$dups <- rep(0, nrow(VMS))
VMS$dups[i] <- 1

# load VMS relocations in ltraj class, has to be type I because duplicate steps. type II would not accept duplicates since it has a date-time associated with each relocation
  
  # VMS date-time needs to be POSIX
  VMS$Date_Time <- strptime(VMS$Date_Time, format='%Y-%m-%d %H:%M', tz = 'America/Los_Angeles' )
  
  trjVMS <- as.ltraj(xy = VMS[,c("Longitude","Latitude")], id = VMS$Vessel_Name, typeII = FALSE)

  # turn trajectories into dataframes
  df_trjVMS <- ld(trjVMS)
# look at distance between points (dist, dt), turning angles (abs.angle, rel.angle), and squared dist from first relocation because it's calculated (R2n)
  
  
# look at a fishermen which has duplicates and another in same declaration and look at histogram of inferred speed, change in heading, change in distance and change in recorded speed
Double_Eagle <- VMS[which(VMS$Vessel_Name=="Double Eagle"),]  # has dups
Double_Eagle <- Double_Eagle[order(Double_Eagle$Date_Time),]
Orca <- VMS[which(VMS$Vessel_Name=="Orca (509235)"),]         # has dups
Orca <- Orca[order(Orca$Date_Time),]
SeaStar <- VMS[which(VMS$Vessel_Name=="Sea Star (594033)"),]  # no dups
SeaStar <- SeaStar[order(SeaStar$Date_Time),]

# look at maps of distribution
vessel = Orca
plot(vessel$Longitude, vessel$Latitude, pch=19, type="o",col="dodgerblue",cex=0.25)
map("worldHires","usa",add=T,fill=T,col="grey",border=F)
lines(vessel$Longitude, vessel$Latitude,pch=19,col="dodgerblue",type="o",cex=0.25)

points(vessel$Longitude[which(vessel$dups==1)],vessel$Latitude[which(vessel$dups==1)],pch=19,col="red",cex=0.35)

Double_Eagle$distance <- rep(NA,nrow(Double_Eagle))    # make a column to calculate distance


# find vessel that has lots of duplicates, column 'tot' has number of times duplicate show up
count_dups <- ddply(VMS, .(Vessel_Name), summarize, freq=length(Vessel_Name), tot=sum(dups))

# vessel name of most duplicates
dup_index <- which(count_dups$tot == max(count_dups$tot))

Vessel_Name <- count_dups[dup_index,1]	# Miss Sarah

Miss_Sarah <- subset(VMS,Vessel_Name=="Miss Sarah")

# calculate differences in distance between points, average direction and average speed. 
# first need to give each duplicate it's own ID

idmaker = function(vec){
  return(paste(sort(vec), collapse=""))
}

ID <- apply(as.matrix(examine_dups[,c("Ship_No","year","month","day","time")]), 1, idmaker)
examine_dups <- cbind(examine_dups,ID)

# For each ID, calculate average difference and stdv between locations, speeds, distances. If difference is 0, then can record that. Also below are thresholds for how close before I'll take an average

dist_thresh <- 0.1	# km, equiv to 100 m. abritrary, but the degree of accuracy Ole has in habitat maps
speed_thresh <- 0.001	# knots, totally abritrary 
direction_thresh <- 1   # difference less than 1 degree, totally abritrary

unique_IDS <- unique(ID)

all_dists = data.frame(
  ID = rep(NA,length(unique_IDS)), 
  Min = rep(NA,length(unique_IDS)), 
  Q1 = rep(NA,length(unique_IDS)), 
  Median = rep(NA,length(unique_IDS)), 
  Mean = rep(NA,length(unique_IDS)), 
  Q3 = rep(NA,length(unique_IDS)), 
  Max = rep(NA,length(unique_IDS)))

all_speeds = data.frame(
  ID = rep(NA,length(unique_IDS)), 
  Min = rep(NA,length(unique_IDS)), 
  Q1 = rep(NA,length(unique_IDS)), 
  Median = rep(NA,length(unique_IDS)), 
  Mean = rep(NA,length(unique_IDS)), 
  Q3 = rep(NA,length(unique_IDS)), 
  Max = rep(NA,length(unique_IDS)))

all_directions = data.frame(
  ID = rep(NA,length(unique_IDS)), 
  Min = rep(NA,length(unique_IDS)), 
  Q1 = rep(NA,length(unique_IDS)), 
  Median = rep(NA,length(unique_IDS)), 
  Mean = rep(NA,length(unique_IDS)), 
  Q3 = rep(NA,length(unique_IDS)), 
  Max = rep(NA,length(unique_IDS)))

new_ping <- data.frame(
  ID=rep(NA, length(unique_IDS)), 
  Longitude=rep(NA, length(unique_IDS)), 
  Latitude=rep(NA, length(unique_IDS)), 
  Avg_Speed=rep(NA, length(unique_IDS)), 
  Avg_Direction=rep(NA, length(unique_IDS)))

for(i in 1:length(unique_IDS)){
  foo <- subset(examine_dups, ID == unique_IDS[i])
  new_ping[i,1] = unique_IDS[i]
  
  coordinates(foo) = c("Longitude", "Latitude")
  if(any(is.na(coordinates(foo)))){
    all_dists[i,] <- rep(NA,7)
  } else{
    dists <- spDists(foo)
    dist_vec <- sm2vec(dists)
    all_dists[i,] <- c(unique_IDS[i], summary(dist_vec))
  }	
  
  # if differences zero, can just use whatever the value is for the first entry in the table
  foo <- as.data.frame(foo)
  new_ping[i,2] = ifelse(all_dists$Mean[i] == 0,foo$Longitude[1], ifelse(all_dists$Mean[i] < dist_thresh, mean(foo$Longitude), "diff"))
  new_ping[i,3] = ifelse(all_dists$Mean[i] == 0,foo$Latitude[1], ifelse(all_dists$Mean[i] < direction_thresh, mean(foo$Latitude),"diff"))
  
  if(any(is.na(foo$Avg_Speed))) {all_speeds[i,] <- rep(NA, 7)
  } else{
    speeds <- dist(foo$Avg_Speed)
    speed_vec <- sm2vec(as.matrix(speeds))
    all_speeds[i,] <- c(unique_IDS[i], summary(speed_vec))
  }
  # if differences are 0 can just use value of first entry in foo table
  new_ping[i,4] = ifelse(all_speeds$Mean[i] == 0, foo$Avg_Speed[1],ifelse(all_speeds$Mean[i] < speed_thresh, mean(foo$Avg_Speed),"diff"))
  
  if(any(is.na(foo$Avg_Direction))) {all_directions[i,] <- rep(NA,7)	
  } else{
    directions <- dist(foo$Avg_Direction)
    direct_vec <- sm2vec(as.matrix(directions))
    # use smallest difference between angles
    direct_vec[direct_vec>180] = 360 - direct_vec[direct_vec>180]
    all_directions[i,] <- c(unique_IDS[i], summary(direct_vec))	
  }
  # if differences are 0 can just use value of first entry in foo table
  new_ping[i,5] = ifelse(all_directions$Mean[i] == 0, foo$Avg_Speed[1],ifelse(all_directions$Mean[i] < direction_thresh, mean(foo$Avg_Direction), "diff"))
  
  print(i)/length(unique_IDS)
}

# replicate examine_dups 
replaced_dups <- examine_dups
replaced_dups$new_lon <- rep(NA,nrow(replaced_dups))
replaced_dups$new_lat <- rep(NA,nrow(replaced_dups))
replaced_dups$new_speed <- rep(NA,nrow(replaced_dups))
replaced_dups$new_direction <- rep(NA,nrow(replaced_dups))

replaced_dups[which(replaced_dups$ID==new_ping$ID[i]),c("new_lon","new_lat","new_speed","new_direction")] = c(new_ping[i,2:ncol(new_ping)])	

replaced_dups[-which(is.na(replaced_dups[,14:17])),]

ggplot(examine_dups, aes(x = Longitude, y = Latitude)) + geom_point(aes(colour=factor(Vessel_Name)))
plot(examine_dups$Longitude[1:4], examine_dups$Latitude[1:4], type="o")

