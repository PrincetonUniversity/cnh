# load all vessels that have shrimp observers
library(data.table); library(geosphere);library(maps)

#----
# what overlap between VMS and OBS do we have?
#----
obs <- fread("/Users/efuller/1/CNH/Data/Observers/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv")

shrimp_obs <- subset(obs, sector == "Pink Shrimp", 
                     select=c("TRIPID","VESSEL","CG_NUM","CATCH_ID",
                              "LOGBOOK_NUMBER","D_PORT","R_PORT","SET_MONTH",
                              "SET_DAY","SET_YEAR","SET_TIME","SET_LONG",
                              "SET_LAT","SET_DEPTH","UP_MONTH","UP_DAY",
                              "UP_YEAR","UP_TIME","UP_LAT","UP_LONG",
                              "HAUL_DURATION","DMONTH","DDAY","DYEAR",
                              "RMONTH","RDAY","RYEAR","sector")
                     )
length(unique(shrimp_obs$VESSEL)) # 74 different vessels. 

# for which observed vessels do I have VMS data for?
  # load VMS data
  load("/Users/efuller/1/CNH/Analysis/VMS/results/2014-10-29/3_VMSdf.Rdata")

  # subset 
  vms_obs <- subset(VMSdf, Doc_Number %in% unique(shrimp_obs$CG_NUM))
  
  # how many vessels?
  length(unique(vms_obs$Doc_Number)) # so good, all 74
  rm(VMSdf)
  rm(obs)
#----
# OBS: format observer data dates and times -- add leading zeros
#----
# have to add leading zeros to dates -- prob should make this a function

fixDate <- function(vec){
  vec <- ifelse(nchar(vec)==1, paste0(0,vec), vec)
}

shrimp_obs$DMONTH <- fixDate(shrimp_obs$DMONTH)
shrimp_obs$DDAY <- fixDate(shrimp_obs$DDAY)
shrimp_obs$RMONTH <- fixDate(shrimp_obs$RMONTH)
shrimp_obs$RDAY <- fixDate(shrimp_obs$RDAY)
shrimp_obs$UP_MONTH <- fixDate(shrimp_obs$UP_MONTH)
shrimp_obs$UP_DAY <- fixDate(shrimp_obs$UP_DAY)
shrimp_obs$SET_MONTH <- fixDate(shrimp_obs$SET_MONTH)
shrimp_obs$SET_DAY <- fixDate(shrimp_obs$SET_DAY)

# changing times to minutes, seconds
fixTime <- function(vec){
  hours <- floor(vec)
  minutes <- (vec - hours)*60
  minutes <- round(minutes, 2)
  minutes <- ifelse(nchar(minutes)==1,paste0(0,minutes),minutes)
  time <- paste(hours,minutes,sep=":")
}

shrimp_obs$UP_TIME <- fixTime(shrimp_obs$UP_TIME)
shrimp_obs$SET_TIME <- fixTime(shrimp_obs$SET_TIME)

# make new date_time
shrimp_obs$D_DATE <- as.POSIXct(
  with(shrimp_obs, paste(DMONTH, DDAY, DYEAR, sep = "-")), 
  format = "%m-%d-%Y", tz = "Etc/GMT-8")

shrimp_obs$R_DATE <- as.POSIXct(
  with(shrimp_obs, paste(RMONTH, RDAY, RYEAR, sep = "-")), 
  format = "%m-%d-%Y", tz = "Etc/GMT-8")

# make new up and set dates and times

shrimp_obs$up_dateTime <- as.POSIXct(
  with(shrimp_obs, paste0(UP_MONTH,"-",UP_DAY,"-",UP_YEAR," ",UP_TIME)), 
  format = "%m-%d-%Y %H:%M", tz = "Etc/GMT-8")

shrimp_obs$set_dateTime <- as.POSIXct(
  with(shrimp_obs, paste0(SET_MONTH,"-",SET_DAY,"-",SET_YEAR," ",SET_TIME)), 
  format = "%m-%d-%Y %H:%M", tz = "Etc/GMT-8")
#----
# VMS: now for each vms, have to add date, time and speed
#----
vms_obs$Date_Time=as.POSIXct(vms_obs$Date_Time, format="%Y-%m-%d %H:%M",tz="Etc/GMT-8")
setkey(vms_obs, Doc_Number, Date_Time)

vms_obs <- subset(vms_obs, Avg_Speed < 30) # remove some fliers

# distance
  ves_names <- unique(vms_obs$Doc_Number)
  vms_list <- vector("list")

  # loop through each vessel -- find speed, pull out port points
  for(i in 1:length(ves_names)){
    cat("subsetting to vessel, ")
    v1 <- subset(vms_obs, Doc_Number==ves_names[i])
    
    cat("finding distance, ")
      v1$dist <- c(NA, sapply(2:nrow(v1),function(i){
        distm(v1[,c("Longitude","Latitude"),with=FALSE][i-1,],
              v1[,c("Longitude","Latitude"),with=FALSE][i,])/1000
      }))
    
    # time
    v1$time <- c(NA,diff(v1$Date_Time))/60
    
    # speed
    v1$speed <- (v1$dist/v1$time) * 0.539956803
    
    # re-subset to remove any new fliers
#     while(any(v1$speed>=30, na.rm=T)){
#       cat("still fliers, re-subsetting... ")
#       v1 <- subset(v1, speed < 30)
#       
#       v1$dist <- c(NA, sapply(2:nrow(v1),function(i){
#         distm(v1[,c("Longitude","Latitude"),with=FALSE][i-1,],
#               v1[,c("Longitude","Latitude"),with=FALSE][i,])/1000
#       }))
#       
#       # time
#       v1$time <- c(NA,diff(v1$Date_Time))/60
#       
#       # speed
#       v1$speed <- (v1$dist/v1$speed) * 0.539956803
#     }
    
    # find trips
      trips <- rle(v1$speed>0 & v1$onland==FALSE) # find segments in which the speed is greater than zero and it's not onland. 
      v1$run <- rep(trips$lengths, trips$lengths) # length of each segment 
      v1$cond <- rep(trips$values, trips$lengths)
      total_ids <- length(trips$lengths) # total number of segments
      v1$seg_id <- rep(1:total_ids,trips$lengths)  #give each segment own id
    
    v1_trips <- subset(v1, run >= 3 & cond) # trips have to be more than 3 points
    
    vms_list[[i]] <- v1_trips
    cat("vessel ", i," finished.\n")
  }

# need to reconsider vessels 10, 27, 31, 38, 42, 49, 61, 62, 66, 71, 72, 73 -- filtering is weird.   

  # loop through each vessel -- find observed points
# for james: just give VMS data
vms_df <- do.call('rbind', vms_list)
write.csv(vms_df, "/Users/efuller/1/CNH/Analysis/VMS/results/2014-11-17/vms_df.csv",row.names=FALSE)
vms_df <- fread("/Users/efuller/1/CNH/Analysis/VMS/results/2014-11-17/vms_df.csv")

#----
# better speed filtering
#----
v1 <- subset(vms_df, Ship_Number == "X00763")

v1$dist2 <- c(sapply(2:nrow(v1),function(i){
           distm(v1[,c("Longitude","Latitude"),with=FALSE][i-1,],
                 v1[,c("Longitude","Latitude"),with=FALSE][i,])/1000}),NA)
# rows 239-240: this is a multiple day drop, is this data gone in the original?

# look in raw data
raw_v1 <- subset(VMSdf, Ship_Number == "X00763")
raw_v1 <- raw_v1[order(raw_v1$Date_Time),]

with(raw_v1[760:769,],plot(Longitude, Latitude, asp=1,type='o')); map('state',add=T)
# looks legit that the vessel doesn't leave again until may. 

# thinking instead should subset long zero sequences and leave everything else. rather than the other way. need to have bookends in order to calculate speed for entire trajeotory. otherwise calculating speed across could be weird. and I should pull out trips like the one above that looks like it ends in the middle. maybe if the high speed is at the end of the trip. s
#----
# Am here: --------------
# subset to a vessel

obsv1 <- subset(shrimp_obs, CG_NUM==918594)

# find VMS which was observed
# for each observed trip, mark in VMS data
library(lubridate)
Odepart <- unique(obsv1$D_DATE)
Oreturns <- unique(obsv1$R_DATE)
Oreturns <- paste(Oreturns,"23:59") # otherwise defauls to just when the day turns into a new one. 
Oreturns <- as.POSIXct(Oreturns, tz = "Etc/GMT-8")
Ointervals <- as.interval(Odepart, Oreturns)
Fstart <- unique(obsv1$set_dateTime)
Fend <- unique(obsv1$up_dateTime)
Fintervals <- as.interval(Fstart, Fend)

v1_trips$obs <- NA
v1_trips$fishing <- NA
for(i in 1:length(Ointervals)){
  # check for observations
  v1_trips$obs <- ifelse(v1_trips$Date_Time %within% Ointervals[i],
                         1,v1_trips$obs) 
  # if it's already been marked as observed, leave it or if it's in this new observed period, mark as 1
}

for(j in 1:length(Fintervals)){
  v1_trips$fishing <- ifelse(v1_trips$Date_Time %within% Fintervals[j],
                             1, v1_trips$fishing)
}

# for times when not fishing, but observed, put a zero
v1_trips$fishing <- ifelse(is.na(v1_trips$fishing) & v1_trips$obs==1, 0, v1_trips$fishing)

# next step is building an ROC analysis to classify fishing points.
library(pROC)

with(subset(v1_trips,obs==1), roc(v1_trips$fishing, v1_trips$speed,plot=TRUE))

# need to get CI for threshold and use that to call fishing

# need to plot histogram of speeds, see what it looks like. 
with(subset(v1_trips, fishing==1), plot(density(speed))) # bimodal, weird
head(subset(v1_trips, fishing==1))

with(subset(v1_trips, seg_id==79), plot(Longitude, Latitude, type="l",pch=19,asp=1))
map('state',add=T, col="grey",fill=TRUE, bor="ivory")
with(subset(v1_trips, seg_id==79), points(Longitude, Latitude,pch=19,col=(fishing+1),cex=.5, asp=1))

# am here: this looks weird, would like to plot one by one. the base graphics are giving me grief by not being big enough for hte window. need to mess with this more later. 