# goal: find periods in which (to the best of my knowledge) there is no fishing
# date: 2014-04-12
# author: emma fuller
# notes: During periods when observers are on the vessels and have not recording fishing bouts it could either be that there is no fishing, or that they missed it. I bet that the observers are more likely to miss fishing bouts if they are late at night. 

require(lubridate)

# look at one vessel (this loads for 12 vessels most commonly observed)
  load("unsorted/2nd_wkProject_animove/data/subObs.Rdata")
# look at one vessel, remove duplicates
  ves_obs <- subset(subObs, CG_NUM==981390,
                    select=c("TRIPID","CG_NUM","D_DATE","R_DATE","SET_DAY","SET_MONTH","SET_YEAR",
                             "SET_TIME","UP_DAY","UP_MONTH","UP_YEAR","UP_TIME"))
  ves_obs <- ves_obs[!duplicated(ves_obs),]
  rm(subObs)

# need vector of known fishing intervals, and intervals of times when the observers were on the vessel, but unknown if fishing occurred.  

# times of observer data not ideal, function to convert
# have to convert time away from decimals, to minutes
dec2min <- function(search_term, data){
  # get time
  col <- grep(paste(search_term,"TIME",sep="_"),names(data))
  remain <- data[,col][data[,col]>1] %% floor(data[,col][data[,col]>1])
  minutes <- round(remain*60)
  time_hrs <- rep(0,length(minutes))
  time_hrs[data[,col]>1] <- floor(data[,col][data[,col]>1])
  time_hrs <- paste(time_hrs, minutes, sep=":")
  
  # get  date
  cDay <- grep(paste(search_term,"DAY",sep="_"),names(data))
  cMonth <- grep(paste(search_term,"MONTH",sep="_"),names(data))
  cYear <- grep(paste(search_term,"YEAR",sep="_"),names(data))
  up_day <- paste(data[,cYear],data[,cMonth],data[,cDay],sep="-")
  UPdate_time <- paste(up_day, time_hrs, sep=" ")
  UPdate_time <- strptime(strftime(UPdate_time, format="%Y-%m-%d %H:%M"), format="%Y-%m-%d %H:%M",tz="US/Pacific")
  return(UPdate_time)
}

# getting concatenated date-time of correct format
ves_obs$SETdate_time <- dec2min("SET",ves_obs)
ves_obs$UPdate_time <- dec2min("UP",ves_obs)

  # check (should be false)
  any(ves_obs$SETdate_time > ves_obs$UPdate_time)

observed <- new_interval(ves_obs$SETdate_time,ves_obs$UPdate_time, tzone="US/Pacific")
  # take a look
  hist(observed/3600,col="grey",bor="darkgrey",freq=FALSE,xlab="hours",main="Tow durations")  
  lines(density(observed/3600),col="indianred",lwd=3)

# now make a list of intervals that are between D_DATE and R_DATE but are not within observed
  # make intervals for trips
    trips <- data.frame(Depart = unique(ves_obs$D_DATE), Return = unique(ves_obs$R_DATE))
    trip.dur = new_interval(as.POSIXct(trips$Depart, format="%m/%d/%Y %I:%M:%s %p",tz="US/Pacific"),
                            as.POSIXct(trips$Return, format="%m/%d/%Y %T %p",tz="US/Pacific"),
                            tzone="US/Pacific")
  # load VMS for vessel of interest
    load("unsorted/2nd_wkProject_animove/data/VMS_limited.Rdata")
    VMSsub <- subset(VMS_limited,Doc_Number==981390)
    VMSsub$Date_Time <- as.POSIXct(VMSsub$Date_Time,format="%Y-%m-%d %H:%M",tz="US/Pacific")
    VMSsub <- VMSsub[order(VMSsub$Date_Time),]
    # check
      any(diff(VMSsub$Date_Time)<0)
      rm(VMS_limited)
  
  # make dummy variable for known fishing (2), likely non-fishing (1), unknown (0)
      # check if VMS$Date_Time is within observed trip
      # then check if it's within any of the observed fishing times (2 if yes, 1 if no)
      # else mark 0
      # (takes a long time to run)
  VMSsub$behavior.obs <- rep(NA,nrow(VMSsub))
  pb <- txtProgressBar(min = 0, max = nrow(VMSsub), style = 3)

  for(i in 1:length(VMSsub$Date_Time)){
      for(j in 1:length(trip.dur)){
        if(VMSsub$Date_Time[i] %within% trip.dur[j]){
          for(k in 1:length(observed)){
            VMSsub$behavior.obs[i] <- ifelse(VMSsub$Date_Time[i] %within% observed[k],2,1)
          }
        }else{VMSsub$behavior.obs[i] <- 0}
      }
      setTxtProgressBar(pb, i)
    }
    close(pb)

    # check: any not assigned? (should be false)
      any(is.na(VMSsub$behavior.obs))
      any(VMSsub$behavior.obs==2)

    # plot to see how it looks
    plot(VMSsub$Longitude[VMSsub$behavior.obs==2], VMSsub$Latitude[VMSsub$behavior.obs==2],pch=19,col="indianred")
    

  
  
  pb <- txtProgressBar(min = 0, max = length(V), style = 3)
for(j in 1:length(fish_time)){
  int <- subVMS1_clean$new_time %within% fish_time[j]
  obs_fishing$fish[which(int==TRUE)] <- 1
  obs_fishing$haul_id[which(int==TRUE)] <- v.times$HAUL_ID[j]
  setTxtProgressBar(pb, j)
}
close(pb)
  

