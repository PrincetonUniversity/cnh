# ground truthing shrimp data. first step: are these vessels observed?
rm(list=ls())
library(lubridate); library(ggplot2)
# 1. load vms data ----
  shrimp <- read.csv("/Users/efuller/1/CNH/Analysis/VMS/2014-07-24/ref_jittered.csv", stringsAsFactors=F)
  shrimp$X <- NULL
# 2. load observer data ----
  obs <- read.csv("/Users/efuller/1/CNH/Data/Observers/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv",stringsAsFactors=F)
# 3. Which vessels observed? ----
  shrimp_names <- unique(shrimp$Vessel_Name)
  length(which(shrimp_names %in% obs$VESSEL)) # 5 out of 8. not bad. 
  shrimp_names <- shrimp_names[which(shrimp_names %in% obs$VESSEL)]
# 4. subset relevant observer data ----
  shrimp_obs <- subset(obs, VESSEL %in% shrimp_names, select = c("VESSEL","TRIPID","HAUL_ID","HAUL_NUM", "SET_YEAR","SET_MONTH","SET_DAY","SET_TIME","SET_LAT","SET_LONG","UP_YEAR","UP_MONTH","UP_DAY","UP_TIME","UP_LAT","UP_LONG","HAUL_DURATION","D_DATE","R_DATE"))

  # remove duplicates
  shrimp_obs <- shrimp_obs[!duplicated(shrimp_obs),]
# 5. format observer data ----
  # need for each vessel in obs, to find intervals in which observer on boat
  # change to POSIX.ct D_DATES and R_DATES
    shrimp_obs$D_DATE <- as.POSIXct(shrimp_obs$D_DATE, "%m/%d/%Y %H:%M:%S", tz = "US/Pacific")
    shrimp_obs$R_DATE <- as.POSIXct(shrimp_obs$R_DATE, "%m/%d/%Y %H:%M:%S", tz = "US/Pacific")
  # make a date_time column for set and up
    # change decimal hours to hours and minutes
    decimal_to_hrs_mins <- function(time){
      hrs <- trunc(time)
      mins <- round((time-hrs)*60) # round to nearest minute
      hours_mins <- paste(hrs, mins, sep=":")
      return(hours_mins)
    }

    shrimp_obs$up_time <- decimal_to_hrs_mins(shrimp_obs$UP_TIME)
    shrimp_obs$set_time <- decimal_to_hrs_mins(shrimp_obs$SET_TIME)
    shrimp_obs$up <- paste(shrimp_obs$UP_YEAR,"-",shrimp_obs$UP_MONTH,"-", shrimp_obs$UP_DAY," ",shrimp_obs$up_time,sep="")
    shrimp_obs$set <- paste(shrimp_obs$SET_YEAR,"-", shrimp_obs$SET_MONTH,"-", shrimp_obs$SET_DAY," ", shrimp_obs$set_time,sep="")
    shrimp_obs$up <- as.POSIXct(shrimp_obs$up, tz="US/Pacific")
    shrimp_obs$set <- as.POSIXct(shrimp_obs$set, tz="US/Pacific")
  # for each vessel name, is VMS within interval?
    shrimp$Date_Time <- as.POSIXct(shrimp$Date_Time, "%Y-%m-%d %H:%M", tz = "US/Pacific")
# 6. for each vessel, for each start and end date, is VMS time point in interval? ----
  findFishing <- function(vessel_name, VMS_sub){
    # find haul intervals
    haul.int <- subset(shrimp_obs, VESSEL == vessel_name, select = c("set","up"))
    haul.int <- haul.int[!duplicated(haul.int),]
    fishing_intervals = new_interval(haul.int$set, haul.int$up)
    
    VMS_sub$fishing <- 0
    for(i in 1:nrow(VMS_sub)){
      VMS_sub$fishing[i] <- length(which(VMS_sub$Date_Time[i] %within% fishing_intervals))
    }
    return(VMS_sub)
  }
  findObservedTrips <- function(vessel_name){
      trips.int <- subset(shrimp_obs, VESSEL == vessel_name, select=c("D_DATE","R_DATE"))
      trips.int <- trips.int[!duplicated(trips.int),]
      trip_intervals = new_interval(trips.int$D_DATE, trips.int$R_DATE)
  
      # subset VMS 
      VMS_sub <- subset(shrimp, Vessel_Name == vessel_name)
      VMS_sub$observed <- 0 # set to none observed
      
      # for VMS points in any of intervals, put 1
      for(i in 1:nrow(VMS_sub)){
        VMS_sub$observed[i] <- length(which(VMS_sub$Date_Time[i] %within% trip_intervals))
      }
      
      VMS_fished <- findFishing(vessel_name, VMS_sub)
      return(VMS_fished)
  }
# 7. run through each vessel ----
  # fix shrimp names
  shrimp_assigned <- gsub(pattern=" ", "_",shrimp_names)
  for(i in 1:length(shrimp_names)){  
   assign(shrimp_assigned[i],findObservedTrips(shrimp_names[i]))
  }
# 8. plot to check ----
ggplot(Darin_Alan, aes(x=Longitude, y = Latitude, shape=factor(observed), color=factor(fishing))) + geom_path() + geom_point()

  # turns out some VMS points are in more than one interval. doesn't matter, should be changed to one. just means that intervals are non-exclusive. 
  VMS_groundtruthed <- rbind(Darin_Alan, Jackpot, Patience, Pollux, Wrangler)
  VMS_groundtruthed$fishing[which(VMS_groundtruthed$fishing==2)] <- 1

# save groundtruthed reference dataset----
write.csv(VMS_groundtruthed,"/Volumes/untitled/CNH/Analysis/VMS/2014-08-19/groudtruthed_ref.csv")

library(dplyr)

# save an anonymized version for Angela ----
  VMS_gt_anonymous <- select(VMS_groundtruthed, vessel_id, Date_Time, Avg_Speed, Avg_Direction, status, onland, newcomplex, observed, fishing)
  VMS_gt_anonymous$Longitude <- Re(VMS_gt_anonymous$newcomplex)
  VMS_gt_anonymous$Latitude <- Im(VMS_gt_anonymous$newcomplex)

  VMS_gt_anonymous <- select(VMS_gt_anonymous, - newcomplex)
  
  write.csv(VMS_gt_anonymous, "/Volumes/untitled/CNH/Analysis/VMS/2014-08-19/anonymized_groundtruthed.csv")
