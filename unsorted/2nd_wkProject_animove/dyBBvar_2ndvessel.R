# 2014-04-10 
# Running dynamic brownian bridge on a different individual
# slighly more updated than dyBBvar.R

require(rgeos)
require(rgdal)
require(move)
require(lubridate)

load("data/all_vessels") 
# these are the doc_numbers for top 5 most active vessels from shrimp, catch share trawls and limited entry trawls in observer dataset (i.e. have the most trips over 4 years). Turns out that at least one vessel 

load("data/VMS_limited.Rdata")
# this is the VMS dataset subset to 12 of the 15 vessels in `all_vessel`, it's because two vessels are represented twice, once in catch chares and once in limited entry. this makes sense, because there's overlap in those populations

load("data/coastline.Rdata")
# load coastline in order to buffer for at port or not. 
# check projection(WC), should be "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

load("data/subObs.Rdata")

  bpoly <- gBuffer(WC, width=0.03,) # buffering by 0.03 of a decimal degree to shore, which I think is a little over two miles. 

# subset data
  subVMS1 = subset(VMS_limited, Doc_Number==all_vessels[1])
  
  #filter out any ridiculous speeds to start
    subVMS1_clean <- subset(subVMS1, Avg_Speed < 30)
    hist(subVMS1_clean$Avg_Speed[subVMS1_clean$Avg_Speed>0],col="grey",bor="darkgrey",freq=F,main="Speeds as recorded by VMS",xlab="speeds, mph?")

# subset known behavior data
  subObs1 = subset(subObs, CG_NUM==all_vessels[1])

# change to spatial points dataframe for filtering on/off land
  coordinates(subVMS1_clean) <- c("Longitude","Latitude")
  projection(subVMS1_clean) <- projection(WC)
# buffer to find vessels close to land, label "port", else "water"
  behav <- gContains(bpoly,subVMS1_clean,byid=TRUE) # now TRUE values are "at port"
  behav <- as.vector(behav)
  behav[behav==TRUE] <- "port"
  behav[behav==FALSE] <- "water"
  subVMS1_clean$behavior <- behav
  rm(behav)

# now build vector of known fishing occurances 
  v.times <- subset(subObs1,select=c("CG_NUM","HAUL_ID",
                                     "SET_LONG","SET_LAT",
                                     "SET_MONTH","SET_DAY","SET_YEAR","SET_TIME",
                                     "UP_LONG","UP_LAT",
                                     "UP_MONTH","UP_DAY","UP_YEAR","UP_TIME"))
  
# remove duplicates -- originally had rows for each species caught
    v.times <- v.times[!duplicated(v.times),]

# times of observer data not ideal, function to conver

# have to convert time away from decimals, to minutes
# get remainder
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
  v.times$SETdate_time <- dec2min("SET",v.times)
  v.times$UPdate_time <- dec2min("UP",v.times)

# finding fishing intervals
  fish_time <- new_interval(v.times$SETdate_time,v.times$UPdate_time,tzone="US/Pacific")

  # making empty dataframe
    obs_fishing <- data.frame(fish=rep(NA,nrow(subVMS1_clean)),haul_id=rep(0,nrow(subVMS1_clean)))
  # make subVMS1_clean$Date_Time as posix
    subVMS1_clean$Date_Time <- as.POSIXlt(subVMS1_clean$Date_Time,"%Y-%m-%d %H:%M", tzone="US/Pacific")

  # check for each relocation whether within start/end time of observed fishing bout
    pb <- txtProgressBar(min = 0, max = length(fish_time), style = 3)
    for(i in 1:length(fish_time)){
      int <- subVMS1_clean$Date_Time %within% fish_time[i]
      obs_fishing$fish[which(int==TRUE)] <- 1
      obs_fishing$haul_id[which(int==TRUE)] <- v.times$HAUL_ID[i]
      setTxtProgressBar(pb, i)
    }
    close(pb)

    length(unique(v.times$HAUL_ID)) - length(unique(obs_fishing$haul_id))
      # not all observed hauls were picked up.. 
      # plot where the observed hauls were that weren't picked up
      # maybe were too short? should check later
      missed_hauls <- v.times[which(!unique(v.times$HAUL_ID) %in% unique(obs_fishing$haul_id)==TRUE),]
      plot(v.times$SET_LONG, v.times$SET_LAT, col=rgb(0.15,0.15,0.15,0.15),pch=19)
      plot(WC,add=T, col="wheat")
      points(missed_hauls$SET_LONG,missed_hauls$SET_LAT,pch=19,col=alpha("red",0.25))


# now order VMS, make sure no duplicates
subVMS1_order <- subVMS1_clean[order(subVMS1_clean$Doc_Number,subVMS1_clean$Date_Time),]

# make move object
VMS1_move <- move(x=subVMS1_order@coords[,1], y=subVMS1_order@coords[,2],
                  time=as.POSIXct(subVMS1_order$Date_Time,tz="US/Pacific"), 
                  data=as.data.frame(subVMS1_order),
                  proj=projection(subVMS1_order), 
                  sensor="VMS",
                  animal=subVMS1_order$Doc_Number)

# transform so equidistant about center of track
  data <- spTransform(VMS1_move,center=T)

# run brownian motion variance
dBBMM<- brownian.motion.variance.dyn(data, location.error=rep(100,n.locs(data)),margin=11,window.size=31)

save(dBMM, file=paste("data/",all_vessels[1],"_dBMMvar_",Sys.Date(),sep=""))
save(VMS1_move, file=paste("data/",all_vessels[1],"_move_",Sys.Date(),sep=""))