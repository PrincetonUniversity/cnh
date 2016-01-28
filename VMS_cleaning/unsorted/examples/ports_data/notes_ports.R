# idea is to identify (and test) that high elevation depth are shorelines (plot). Then find all the VMS points that are close to these high eleveation points. Will need to test the threshold. Again by plotting the points that are grabbed. When satisfied with threshold, can label all those points as "port". 

# or just use GPS points of ports and search for all VMS data points that are within a threshold of these locations. Can correlate with bathymetry to double check? Label these as port? Look at Levy-flight Peruvian anchovetta paper to see what threshold they used. 


# at this point is to use open safari window to list all the VMS points that are close to the 

library(ncdf4)
library(maptools)
library(marmap)
library(sp)
library(foreach)

setwd('/Users/efuller/Dropbox/Projects/Harvesting_Strategies/PaperSeeds/EmpiricalAnalysis/NOAA/Data/VMS_Related/VMS/Code')

if(!exists("sorted")) {load("sorted.Rda")}


getNOAA.bathy(lon1 = -133, lon2 = -117, lat1 = 32, lat2 = 50, resolution = 1) -> west_coast
summary(west_coast)

colorRampPalette(c("red","purple","blue","cadetblue1", "white")) -> blues

plot(west_coast, image = TRUE, bpal = blues(100), deep = c(-9000, -3000, 0), shallow = c(-3000, -10, 0), step = c(1000, 1000, 0), lwd = c(0.8, 0.8, 1), col = c("lightgrey", "darkgrey", "black"), lty = c(1, 1, 1), drawlabel = c(FALSE, FALSE, FALSE))

scaleBathy(west_coast, deg = 2, x = "bottomleft", inset = 5) # not exactly sure about deg=2, that was from R vignette: http://cran.r-project.org/web/packages/marmap/vignettes/marmap.pdf

# want to know depth for fishing locations

# convert bathymetry to list

west_coast.sorted <- data.frame(longitude=rep(as.numeric(row.names(west_coast)),dim(west_coast)[2]),latitude=rep(as.numeric(colnames(west_coast)),each=dim(west_coast)[1]),depth=c(west_coast))

dec211_09 <- sorted[which(sorted$Declarations==211 & sorted$Date_Time$year==109),]

# sorted.sub <- sorted[1:1000,]

coordinates(west_coast.sorted) <- c("longitude","latitude")
coordinates(dec211_09) <- c("Longitude","Latitude")

depth = vector(length=nrow(dec211_09))

for(i in 719:nrow(dec211_09)){
depth[i] <- west_coast.sorted$depth[which.min(spDistsN1(west_coast.sorted,sorted.sub[i,],longlat=TRUE))]
print(i/nrow(dec211_09))
}

# doesn't look like depth is a great indication

depth.loop <- function(i, duration,VMS) {
	depth <- vector(length=duration)
	coordinates(VMS) <- c("Longitude","Latitude")
	for (i in 1:duration) {
		depth[i] <- west_coast.sorted$depth[which.min(spDistsN1(west_coast.sorted, VMS[i,], longlat = TRUE))]
	}
	return(depth)
}

depth.fun <- function(VMS) {
	# coordinates(VMS) <- c("Longitude","Latitude")
	# depth <- west_coast.sorted$depth[which.min(spDistsN1(west_coast.sorted, VMS, longlat = TRUE))]
	# return(depth)
	
	sample_lat <- VMS$Latitude[i]
	sample_lon <- VMS$Longitude[i]
	
	# columns are latitude, rows are longitude

	lon <- which.min(abs(as.numeric(row.names(west_coast))-sample_lon))
	lat <- which.min(abs(as.numeric(colnames(west_coast))-sample_lat))

	depth <- west_coast[lon,lat]
	return(depth)
}

ddply(VMS,"Declarations",function(x) depth = west_coast.sorted$depth[which.min(spDistsN1(west_coast.sorted, x, longlat = TRUE))])

sorted.test <- sorted[1:10,]
apply(sorted.test,1,depth.fun)

silly.1 <-mcparallel(depth.loop(1,3))
silly.2 <-mcparallel(depth.loop(1,200))
results <- mccollect(list(silly.1,silly.2), intermediate=status)

my.silly.loop <- function(j, duration) {
	i <- 0
	while(i < duration) {
		i <- i + 1
	}
	return(paste("Silly",j,"Done"))
}

status <- function(results.so.far) {
	jobs.completed <- sum(unlist(lapply(results.so.far,FUN=function(x) { !is.null(x) }))) 
	print(paste(jobs.completed ,"jobs completed so far."))
	}

closestSiteDepth <- vector(mode = "numeric", length = nrow(sorted.sub))
minDistDepth <- vector(mode = "numeric", length = nrow(sorted.sub))

# minDistVec stores distance from the closest depth measurement to each VMS ping.
# closestDepthVec stores the depth of the closest depth measurement to each VMS ping.

depth.try <- foreach(i=1:3, .combine='cbind', .packages='sp') %dopar% {
	 distVec <- spDistsN1(west_coast.sorted,sorted[i,],longlat = TRUE)
      minDistDepth[i] <- min(distVec)
      closestSiteDepth[i] <- west_coast.sorted$depth[which.min(distVec)]
}

depth <- function(VMS){
	distVec <- spDistsN1(west_coast.sorted,VMS,longlat=TRUE)
	closestSiteDepth <- west_coast.sorted$depth[which.min(distVec)]
	return(closestSiteDepth)
}

apply(sorted.sub,1,depth)

  for (i in 1 : nrow(sorted))
   {
      distVec <- spDistsN1(west_coast.sorted,sorted[i,],longlat = TRUE)
      minDistDepth[i] <- min(distVec)
      closestSiteDepth[i] <- west_coast.sorted$depth[which.min(distVec)]
  
   }

sample_lat <- sorted$Latitude[1]
sample_lon <- sorted$Longitude[1]

# columns are latitude, rows are longitude

lon <- which.min(abs(as.numeric(row.names(west_coast))-sample_lon))
lat <- which.min(abs(as.numeric(colnames(west_coast))-sample_lat))

depth_sample <- west_coast[lon,lat]
sorted$depth <- rep(NA,length(sorted$Longitude))

for(i in 3:dim(sorted)[1]){
	sample_lat <- sorted$Latitude[i]
	sample_lon <- sorted$Longitude[i]
	
	# columns are latitude, rows are longitude

	lon <- which.min(abs(as.numeric(row.names(west_coast))-sample_lon))
	lat <- which.min(abs(as.numeric(colnames(west_coast))-sample_lat))

	sorted$depth[i] <- west_coast[lon,lat]
	print(paste("i = ",i,sep=""))
	print(paste((i/dim(sorted)[1])*100,"%"," done",sep=""))
}