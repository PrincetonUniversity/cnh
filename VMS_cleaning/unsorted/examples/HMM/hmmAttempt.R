# Goal: Choose one pink shrimp vessel for which I have observer data and apply Ari's HMM to it, and then look to see how well it did. Try other HMM packages that I have

## Load data

# locally
  VMS <- read.csv("/Users/efuller/Documents/Projects/Harvesting_Strategies/EmpiricalAnalysis/NOAA/Data_Analysis/VMS/Code/processed_data/shrimpVMS.csv")

  Doc_Nums <- unique(VMS$Doc_Number)
  sub1 <- subset(VMS, Doc_Number == Doc_Nums[1])
  sub1$Date_Time <- as.POSIXct(sub1$Date_Time, tz = "Pacific")
  
  ordSub1 <- sub1[order(sub1$Date_Time),]
  
# calculating distance between steps (euclidean) -> should it be great circle?
  dx <- ordSub1$Longitude[1:(nrow(ordSub1)-1)] - ordSub1$Longitude[2:nrow(ordSub1)]
  dy <- ordSub1$Latitude[1:(nrow(ordSub1)-1)] - ordSub1$Latitude[2:nrow(ordSub1)]
  ordSub1$dist = c(NA,sqrt(dx^2+dy^2))

  
  sub2 <- subset(VMS, Doc_Number == Doc_Nums[2])
  sub2$Date_Time <- as.POSIXct(sub2$Date_Time, tz = "Pacific")
  
  ordSub2 <- sub2[order(sub2$Date_Time),]
  
  # calculating distance between steps (euclidean) -> should it be great circle?
  dx <- ordSub1$Longitude[1:(nrow(ordSub1)-1)] - ordSub1$Longitude[2:nrow(ordSub1)]
  dy <- ordSub1$Latitude[1:(nrow(ordSub1)-1)] - ordSub1$Latitude[2:nrow(ordSub1)]
  ordSub1$dist = c(NA,sqrt(dx^2+dy^2))
  
# calculating speed (distance/time) -> should it be in knots?
  

# plot trajectories, color by speed


# calculating turning angle
  
  
# mapping
  require(maps, maptools, mapdata)
  map('state', fill = TRUE, col = "grey",xlim=range(ordSub1$Longitude),border=FALSE, ylim=range(ordSub1$Latitude)+c(-.5,1))
  points(ordSub1$Longitude,ordSub1$Latitude,type='o',pch=19,cex=0.15, col=alpha(colour="blue",0.1))
  points(ordSub2$Longitude,ordSub2$Latitude,type='o',pch=19,cex=0.15, col=alpha(colour="orange",.5))

  
  # misc
  
  
  
  # Mark which VMS datapoints have are part of observed trips
  # how many observed trips are associated with each vessel
  numTrips <- data.frame(CG_NUM = rep(NA,length(uniqueShrimps)), numTRIPID = rep(NA, length(uniqueShrimps)))
  for(i in 1:length(uniqueShrimps)){
    foo <- shrimps[which(shrimps$CG_NUM == uniqueShrimps[i]),]
    numTrips[i,2] <- length(unique(foo$TRIPID))
    numTrips[i,1] <- uniqueShrimps[i]
  }
  # find range of dates for observed trip
  # change R_DATE to POSIXct
  shrimps$RdateTime <- as.POSIXct(shrimps$R_DATE,"%m/%d/%Y %H:%M:%S", tz = "PST")
  shrimps$DdateTime <- as.POSIXct(shrimps$D_DATE,"%m/%d/%Y %H:%M:%S", tz = "PST")
  # make a table of all tripIDs and departing and return dates for each
  shrimpTripIDs <- ddply(shrimps, .(TRIPID), summarize, c(D_DATE = DdateTime, R_DATE = RdateTime))
  
  