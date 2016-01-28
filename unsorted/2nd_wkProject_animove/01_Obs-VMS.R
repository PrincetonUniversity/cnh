# Goal: Find one vessel which is in Observer data set, parse into trips, and remove any trips for which the vessel leaves the EEZ
require(plyr)
require(move)
require(rgeos)
require(sp)
require(maptools)
require(lubridate)
require(maps)
#-------------------------------------------------------------------
#Step 1: Find a vessel from the observer dataset
  setwd("../")
  Obs <- read.csv("Data/Observers/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv")

  # which sectors are present?
    unique(Obs$sector)

  # will choose 5 boats from Pink Shrimp, and 5 boats from Limited Entry Trawl and 5 boats from Catch Shares
    # subset by vessel ID, what are the vessel IDs that are associated with Pink Shrimp
    shrimp <- subset(Obs, sector=="Pink Shrimp")
    LE_trawl <- subset(Obs, sector=="Limited Entry Trawl")
    CS <- subset(Obs, sector=="Catch Shares")
  
  # plot locations of trawls for pink shrimp, color by individual on top of bathymetry
  plot_obs <- function(data, color_scheme){
  n_boats = length(unique(data$CG_NUM))
  boaters = unique(data$CG_NUM)
  point_cols = rep(grep(color_scheme,colors(), ignore.case=TRUE, value=TRUE), length.out= n_boats)
  
  for(i in 1:length(unique(data$CG_NUM))){
    if(i==1){
      plot(data$SET_LON[data$CG_NUM==boaters[i]],
           data$SET_LAT[data$CG_NUM==boaters[i]],
           col=point_cols[i],
           ylim=range(data$SET_LAT),
           xlim=range(data$SET_LON),
           cex=.15,pch=19, bty="n",
           xlab="Longitude",ylab="Latitude",
           main=paste("Observed",as.character(unique(data$sector)),sep=" "),asp=1)
    }else{
      points(data$SET_LON[data$CG_NUM==boaters[i]],
             data$SET_LAT[data$CG_NUM==boaters[i]],
             col=point_cols[i],
             cex=.15,pch=19)}
}
map("state",border=FALSE,fill=TRUE, col="grey",add=T)
}

  # LE trawl
    data = LE_trawl; color_scheme="green"
    plot_obs(data, color_scheme)
  # pink shrimp
    data = shrimp; color_scheme="pink"
    plot_obs(data, color_scheme)
  # Catch Shares
    data = CS; color_scheme="blue"
    plot_obs(data, color_scheme)

  # plot the number of tripIDs per vessel, all the same, some variety?
    shrimp_numtrips <- rev(sort(table(shrimp$CG_NUM)))
    plot(shrimp_numtrips,type="h") 
  # take top 5 vessels
    shrimp_top <- names(shrimp_numtrips)[1:5]

  # plot the number of tripIDs per vessel, LE_trawl
    LE_numtrips <- rev(sort(table(LE_trawl$CG_NUM)))
    plot(LE_numtrips,type="h")
    LE_top <- names(LE_numtrips)[1:5]

  # plot the number of tripIDs per vessel, catch shares
    CS_numtrips <- rev(sort(table(CS$CG_NUM)))
    plot(CS_numtrips,type='h')
    CS_top <- names(CS_numtrips)[1:5]

    all_vessels <- c(shrimp_top,LE_top, CS_top)

  # subset observers to relevent vessels
    subObs <- subset(Obs, CG_NUM %in% all_vessels)
#-------------------------------------------------------------------
# Step 2: take these vessel trajectories from VMS data

  # read in VMS data
  VMS <- read.csv("VMS_cleaning/results/2014-03-02/VMS_woDups.csv",as.is=T)
  VMS_limited <- VMS[which(VMS$Doc_Number %in% all_vessels),] # only 12 out of 15
  
  # order the data by ID, then by date-time, move complains if not
  VMS_limited$new_time <- as.POSIXct(VMS_limited$Date_Time,"%Y-%m-%d %H:%M",tz="US/Pacific")
  #VMS_ordered <- VMS_limited[order(VMS_limited$Doc_Number, VMS_limited$Date_Time),]
  VMS_ordered <- VMS_limited[order(VMS_limited$Doc_Number, VMS_limited$new_time),]

  # make move object
  VMS_move <- move(
  	x = VMS_ordered$Longitude, 
  	y = VMS_ordered$Latitude, 
  	time=as.POSIXct(VMS_ordered$new_time, "%Y-%m-%d %H:%M",tz="US/Pacific"), 
  	data=VMS_ordered, 
  	proj=CRS("+proj=longlat"),
  	sensor="GPS",
  	animal=VMS_ordered$Doc_Number)

#--------------------------------------------
# #old stuff, for posterity
# # looking at distance and speed and turning angle
#   	dist = distance(VMS_move) # in meters
#   # can see resolution is 100m
#   	min(dist[[1]][which(dist[[1]]!=0)])
# 	
# 	speed <- speed(VMS_move) # meters per second
# 	angle <- angle(VMS_move)
# 
# # filtering for trip with a combination onland and port variables. 
# 	# to check what onland does, plot
# 	par(mfrow=c(3,4))
# 	for(i in 1:nrow(VMS_move@idData)){
# 		plot(VMS_move[[i]][which(VMS_move[[i]]$onland==1)],ylim=range(VMS_move[[i]]$Latitude))
# 		map("worldHires",add=TRUE,fill=TRUE,col=rgb(.5,.5,.5,0.15))
# 	} # looks quite good
# 	
# 	# to check what (is.na(status)) does, plot
# 	par(mfrow=c(3,4))
# 	for(i in 1:nrow(VMS_move@idData)){
# 		plot(VMS_move[[i]][which(!is.na(VMS_move[[i]]$status))],ylim=range(VMS_move[[i]]$Latitude))
# 		map("worldHires",add=TRUE,fill=TRUE,col=rgb(.5,.5,.5,0.15))
# 	} # also looks good
# 	
# 	par(mfrow=c(3,4))
# 	ol = list()
# 	po = list()
# 	behav = list()
# 	for(i in 1:nrow(VMS_move@idData)){
# 	# index of onland points
# 		ol[[i]] <- vector(length=nrow(VMS_move[[i]]))
# 		ol[[i]][which(VMS_move[[i]]$onland==1)] <- 1
# 	# index of port points
# 		po[[i]] <- vector(length=nrow(VMS_move[[i]]))
# 		po[[i]][which(!is.na(VMS_move[[i]]$status))] <- 1
# 	# behavior vector should be 1 if either ol or po is 1
# 		behav[[i]] <- vector(length=nrow(VMS_move[[i]]))
# 		
# 			# needs to be 1 less, is a segment property
# 		behav[[i]][po[[i]]==1] <- "port"
# 		behav[[i]][ol[[i]]==1] <- "port"
# 		behav[[i]][po[[i]]==0 & ol[[i]]==0] <- "water"
# 		any(is.na(behav[[i]]))
# 	# check "water" points
# 		plot(VMS_move[[i]][which(behav[[i]]=="water")],pch=19,cex=.5)
# 		map("worldHires",add=TRUE,fill=TRUE,col=rgb(.5,.5,.5,0.15))
# 	} # looks ok
# 	
# 	# trying to shift both forward and back
# 	foo <- head(behav[[1]],-1) # takes off the last element
# 	bar <- behav[[1]][-1] # takes off the first element
# 	length(which(head(foo,200)=="water"))
# 	length(which(head(bar,200)=="water"))
# 	plot(foo[1:200]=="water",lwd=2,type="l")
# 	lines(bar[1:200]=="water",lwd=2,lty=2,col="orange")
# 		# where they disagree should also be "port"
# 	new <- foo
# 	new[foo!=bar] <- "port"
# 	lines(new=="water",lty=3,lwd=2,col="purple")
# 	
# 	testb <- burst(VMS_move[[1]],new)
# 	plot(testb[testb@burstId=="port"],ylim=c(44,48))
# 	map("worldHires",add=T,fill=TRUE,col=rgb(0.5,0.5,0.5,0.15))
# 		# hmm, looks like it's picking up on the water points
# 		# all of these are identified as Astoria points: let's plot Astoria
# 			points(-123.833613,46.192882,pch=5,lwd=3,col="red",cex=3)
# 		# add a circle 9 nautical miles around, 9 nautical mile radius, nautical mile goes to 16.668 km. 1 Decimal degree is 78.71 km at 45° N
# 		km <- 9*16.668
# 		dd <- km/78.71
# 		require(plotrix)
# 		draw.circle(x = -123.833613,y = 46.192882,radius = dd)
# 		# 9 nautical miles encompases quite a bit of possible fishing.. I think I need to lower this.
# 		# trying a smaller radius
# 		km <- 4*16.668
# 		dd <- km/78.71
# 		require(plotrix)
# 		draw.circle(x = -123.833613,y = 46.192882,radius = dd)
# 		# slightly smaller
# 		# trying a smaller radius
# 		km <- 3*16.668
# 		dd <- km/78.71
# 		require(plotrix)
# 		draw.circle(x = -123.833613,y = 46.192882,radius = dd)
# 		# still not great, instead will just buffer the coastline and remove points that are 0.5 km off the coast, then should look at how many little blip-trips I see. 
# 		require(raster)
# 		require(sp)
# 		require(maptools)
#--------------------------------------------	
# 		polys <- readShapePoly("/Volumes/NOAA_Data/CNH/Data/WCspatial/GSHHS_shp/h//GSHHS_h_L1.shp", force_ring = T)
#     cut <- as.data.frame(list(x = c(-143.85157, -107.18519, -91.83648, -130.20826,-143.85157), y=c(53.010045, 56.420870, 6.111194, 6.963900,53.010045)))
#     locator(4) # coordinates for cutting polygon
#     #plot(polys) # takes forever, so shortcutting above
#     cut <- locator(4)
#     cut <- as.data.frame(cut)
# 		# close polygon, put last entry at the end
#     cut <- rbind(cut,head(cut,1))
#     B1 <- Polygon(cut)
#     Bs1 <- Polygons(list(B1),ID="west_coast")
#     BSP <- SpatialPolygons(list(Bs1))
#     WC <- gIntersection(BSP,polys)
#     plot(WC)
# 		# now try buffering
# 		
# 
#     save(WC,file="coastline.Rdata") # name is WC
      load("coastline.Rdata") # is now called WC
  		bpoly <- gBuffer(WC, width=0.03)
      proj4string(WC) <- CRS("+proj=longlat +datum=WGS84")
      plot(bpoly,add=T,col=rgb(0.5,0.5,0.5,0.15))

## now try the filtering for behavior of "port" and on "water" again
VMS_sp <- as(VMS_move, "SpatialPointsDataFrame")
projection(VMS_sp) <- projection(bpoly)
VMS_sp$behav <- gContains(bpoly,VMS_sp,byid=TRUE) # now TRUE values are "at port"
plot(VMS_sp[which(VMS_sp$behav==TRUE),],pch=19,cex=0.15)
plot(WC,add=TRUE,col=rgb(0.5,0.5,0.5,0.15))
plot(bpoly,add=TRUE, col=rgb(0.5,0.5,0.5,0.15))

# conclusion: this may affect results: should play with this to assess sensitivity. will stick with width = 0.03 for now

# now look for where known fishing occurs
VMS_sp$fishing <- rep(0,length(nrow(VMS_sp)))

# would now like to filter for behavior, first where is the behavior known?
# for vessel 1, ID is 503182, what are the dates and times associated with fishing?
vObs <- subset(Obs, Obs$CG_NUM %in% all_vessels) # gearcode = 2, groundfish trawl according to WCGOP appendix code table. 

v.times <- subset(vObs,select=c("CG_NUM","HAUL_ID","SRC","SET_LONG","SET_LAT","SET_MONTH","SET_DAY","SET_YEAR","SET_TIME","UP_LONG","UP_LAT","UP_MONTH","UP_DAY","UP_YEAR","UP_TIME"))

# remove duplicates -- originally had rows for each species caught
v.times <- v.times[!duplicated(v.times),]

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

v.times$SETdate_time <- dec2min("SET",v.times)
v.times$UPdate_time <- dec2min("UP",v.times)

sub_VMS <- subset(VMS_sp, individual.local.identifier=="X503182")
sub_vtimes <- subset(v.times, CG_NUM=="503182",select=c(CG_NUM,HAUL_ID,SRC,SETdate_time,UPdate_time,SET_LONG, SET_LAT, UP_LONG,UP_LAT))
sub_VMS$CG_NUM <- rep("503182",nrow(sub_VMS))

bar <- new_interval(sub_vtimes$SETdate_time,sub_vtimes$UPdate_time,tzone="US/Pacific")

sub_VMS$Date_Time <- strptime(sub_VMS$Date_Time,"%Y-%m-%d %H:%M",tz="US/Pacific")

obs_fishing <- data.frame(fish=rep(NA,nrow(sub_VMS)),haul_id=rep(0,nrow(sub_VMS)))

pb <- txtProgressBar(min = 0, max = length(bar), style = 3)
  for(i in 1:length(bar)){
      int <- sub_VMS$Date_Time %within% bar[i]
      obs_fishing$fish[which(int==TRUE)] <- 1
      obs_fishing$haul_id[which(int==TRUE)] <- sub_vtimes$HAUL_ID[i]
      setTxtProgressBar(pb, i)
  }
close(pb)

length(unique(sub_vtimes$HAUL_ID)) - length(unique(obs_fishing$haul_id))
  # not all observed hauls were picked up.. 
  # plot where the observed hauls were that weren't picked up
 
length(which(!unique(sub_vtimes$HAUL_ID) %in% unique(obs_fishing$haul_id)==TRUE))

missed_hauls <- sub_vtimes[which(!unique(sub_vtimes$HAUL_ID) %in% unique(obs_fishing$haul_id)==TRUE),]

# range of time over which hauls were missed
range(missed_hauls$SETdate_time)

# range of time for VMS trawls
range(sub_VMS$Date_Time)

points(missed_hauls$SET_LONG, missed_hauls$SET_LAT,col="red")
# why aren't these picked up?

# what's the average length of hauls?
missed_hauls$SETdate_time <- strptime(missed_hauls$SETdate_time,"%Y-%m-%d %H:%M",tz="US/Pacific")
missed_hauls$UPdate_time <- strptime(missed_hauls$UPdate_time, "%Y-%m-%d %H:%M",tz="US/Pacific")

hist(as.numeric(missed_hauls$UPdate_time-missed_hauls$SETdate_time)/60,col="lightgrey",bor="darkgrey")
# looks like most of these hauls should have been picked up.

# were there any observed trips that were caught between the missed_hauls range


# plot the first trawl

haul_num=unique(obs_fishing$haul_id)[18]

plot(sub_VMS,cex=0.5,col=rgb(0.15,0.15,0.15,0.15))
plot(sub_VMS[which(obs_fishing$fish==1),],col="orange",cex=0.5,add=T)

plot(sub_VMS[which(obs_fishing$fish==1 & obs_fishing$haul_id==haul_num),],pch=19,cex=0.5,col="red",add=T)
points(sub_vtimes$SET_LONG[which(sub_vtimes$HAUL_ID==haul_num)],sub_vtimes$SET_LAT[which(sub_vtimes$HAUL_ID==haul_num)],col="orange",lwd=5,pch=19)

points(sub_vtimes$UP_LONG[which(sub_vtimes$HAUL_ID==haul_num)],sub_vtimes$UP_LAT[which(sub_vtimes$HAUL_ID==haul_num)],col="green",lwd=5,pch=19)
plot(WC,add=T,col=rgb(0.15,0.15,0.15,0.15))
plot(sub_VMS[which(obs_fishing==1),],col="dodgerblue",add=T,pch=19,cex=0.25)
points(sub_vtimes$SET_LONG, sub_vtimes$SET_LAT, col="red",pch=19,cex=0.15)
points(sub_vtimes$UP_LONG, sub_vtimes$UP_LAT, col="orange",pch=19,cex=0.15)
plot(WC,add=T,col="beige")

plot(VMS_sp[800:1400,],type="l")
points(sub_vtimes$SET_LONG, sub_vtimes$SET_LAT, col="red",pch=19,cex=0.15)
points(sub_vtimes$UP_LONG, sub_vtimes$UP_LAT, col="orange",pch=19,cex=0.15)

sub_VMS$fishing <- obs_fishing$fish
sub_VMS$haul_id <- obs_fishing$haul_id

#--------------------------------------------


# make VMS_sp back into move object, have a variable for whether or not in port. Also want to find whether or not points are known to be fishing

VMS_2 <- move(x=VMS_sp$Longitude,
              y=VMS_sp$Latitude,
              time = as.POSIXct(VMS_sp$Date_Time, format="%Y-%m-%d %H:%M", tz="PST"), 
              data=as.data.frame(VMS_sp), 
              proj=projection(VMS_sp), 
              animal=VMS_sp$individual.local.identifier,
              sensor="GPS")

# trying to shift both forward and back
foo <- head(VMS_2[[1]]$behav,-1) # takes off the last element
bar <- VMS_2[[1]]$behav[-1] # takes off the first element
length(which(head(foo,200)==FALSE))
length(which(head(bar,200)==FALSE))
plot(foo[1:200]==FALSE,lwd=2,type="l")
lines(bar[1:200]==FALSE,lwd=2,lty=2,col="orange")
# where they disagree should also be "port"
new <- foo
new <- bar
new[foo!=bar] <- TRUE
new <- as.vector(new)
new[new==TRUE] <- "port"
new[new==FALSE] <- "water"
lines(new=="water",lty=3,lwd=2,col="purple")

testb <- burst(VMS_2[[1]],new)
plot(testb[testb@burstId=="port"],cex=0.15,pch=19)
plot(new_poly,add=T,col=rgb(0.5,0.5,0.5,0.15))

# actually just going with foo here. I think this gets many fewer points that are out at sea. Not entirely sure why though. 
par(mfrow=c(3,4))
testb <- list()
for(i in 1:nrow(VMS_2@idData)){
  b <- head(VMS_2[[i]]$behav,-1) # takes off the last element
  b <- as.vector(b)
  b[b==TRUE] <- "port"
  b[b==FALSE] <- "water"
  testb[[i]] <- burst(VMS_2[[i]],b)
  plot(testb[[i]][testb[[i]]@burstId=="port"],cex=0.15,pch=19,col="red")
  plot(WC,add=T,col=rgb(0.5,0.5,0.5,0.15))
}

# now testb is a list, elements are each vessel,and are bursted by their ID. For each of these trips, would like to know how long each one is. Let's start with one vessel

v1 <- testb[[1]]
plot(v1[v1@burstId=="water"],type='l',pch=19,cex=0.15,asp=1)
plot(WC,add=T,col=rgb(0.5,0.5,0.5,0.15))



foo <- difftime(v1.times$UPdate_time,v1.times$SETdate_time)
# can see the distribution of trawl times for this individual
hist(as.numeric(foo),col="grey",bor="darkgrey")

# but not quite what I wanted

foo <- ddply(v1$Date_Time)


plot(v1.times$SET_LONG,v1.times$SET_LAT,pch=19,ylim=c(42,49),xlim=c(-126,-122),cex=0.5)
plot(new_poly,add=T,col="lightgrey")


plot(v1[v1@burstId=="water"],pch=19,cex=0.15,col="lightgrey")
points(v1.times$SET_LONG,v1.times$SET_LAT,pch=19,cex=.01,col="red")
points(v1.times$UP_LONG,v1.times$UP_LAT,pch=19,cex=.01,col="orange")



# speed should not be more than 15 meters/sec, that corresponds to > 30 knots/per hour
	anomolous <- which(speed[[1]]>15)
	off_points <- list()
	for(i in 1:length(anomolous)){
		beg <- anomolous[i]-5
		end <- anomolous[i]+5
		off_points[[i]]= data.frame(Long=VMS_move[[1]]$Longitude[beg:end], Lat=VMS_move[[1]]$Latitude[beg:end], Date_Time = VMS_move[[1]]$Date_Time[beg:end])
	}

points(VMS_move[[1]]$Longitude[anomolous[i]],VMS_move[[1]]$Latitude[anomolous[i]],pch=4,cex=2,col="red",lwd=2)

	# make equidistant projection, but centered right now around all boat data
	spTransform(VMS_move,center=TRUE)	

