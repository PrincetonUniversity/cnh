		xlim = c(-121, -118)
		ylim = c(33,35.5)
		
		plot(c(min(dec_sea$Longitude), max(dec_sea$Longitude)), c(min(dec_sea$Latitude), max(dec_sea$Latitude)), type="n",bty="n", xlab="Longitude", ylab="Latitude", main=time_points[i], xlim=xlim, ylim=ylim)
	map('worldHires', xlim = xlim, ylim = ylim , add = T,col="darkgrey",fill=TRUE,border=FALSE)
	points(dec_sea$Longitude[which(dec_sea$floor_time==time_points[i])], dec_sea$Latitude[which(dec_sea$floor_time==time_points[i])],bty="n",xlab="Longitude",ylab="Latitude",col= alpha("dodgerblue",0.5), pch=19)


subset_foo <-dec_sea$Longitude[which(dec_sea$floor_time==time_points[i])]

# missing some ports (found by finding points that are stationary in movie)
Nahcotta <- c(-124.0302, 46.5)
SanFrancisco <- c(-122.4201, 37.80847)
Astoria2 <- c(-123.7585,46.2)	# think already have Astoria, but another port just outside of town
SanPedro<- c(-118.2652, 33.73503)
Ventura <- c(-119.2652, 34.2435)
Oxnard <- c(-119.2285, 34.17006)
MarinadelRey <- c(-118.4469, 33.97175)


# not in a port, in a neighborhood. VMS not turned off?
IslaVista <- c(-119.7986, 34.43672)	
SimiValley <- c(-118.7986, 34.24689)
Camarillo <- c(-118.9952, 34.22683)
FountainValley <- c(-117.9568, 33.72006)
Oxnard_land <- c(-119.1585, 34.23333)
Santa_Maria <- c(-120.4418, 34.94858)
Nipomo <- c(-120.4701, 35.04181)



points(-122.4201, 37.80847,col="red")
