rm(list=ls())
library(maptools)
library(sp)
library(maps)
library(mapdata)
library(scales)
library(marmap)
library(scales)

# load data

setwd('/Users/efuller/Documents/Harvesting_Strategies/PaperSeeds/EmpiricalAnalysis/NOAA/Data/VMS_Related/VMS/Code/For_posterity')

if(!exists("sorted")) {load("sorted.Rda")}

ports <- read.csv("/Users/efuller/Documents/Harvesting_Strategies/PaperSeeds/EmpiricalAnalysis/NOAA/Data/VMS_RElated/VMS/Code/For_posterity/wc_fishing_communities.csv",header=FALSE)
colnames(ports) <- c("Ports","Longitude","Latitude")

# add in extra ports that are not part of dataset, but identified by time fishermen spend at them 

# missing some ports (found by finding points that are stationary in movie)
Nahcotta <- c(-124.0302, 46.5)
SanFrancisco <- c(-122.4201, 37.80847)
Astoria2 <- c(-123.7585,46.2)	# think already have Astoria, but another port just outside of town
SanPedro<- c(-118.2652, 33.73503)
Ventura <- c(-119.2652, 34.2435)
Oxnard <- c(-119.2285, 34.17006)
MarinadelRey <- c(-118.4469, 33.97175)
Petaluma  <- c(-122.5034,38.11667)


# not in a port, in a neighborhood. VMS not turned off?
IslaVista <- c(-119.7986, 34.43672)	
SimiValley <- c(-118.7986, 34.24689)
Camarillo <- c(-118.9952, 34.22683)
FountainValley <- c(-117.9568, 33.72006)
Oxnard_land <- c(-119.1585, 34.23333)
SantaMaria <- c(-120.4418, 34.94858)
Nipomo <- c(-120.4701, 35.04181)

extras <- rbind(Nahcotta, SanFrancisco, Astoria2, SanPedro, Ventura, Oxnard, MarinadelRey, Petaluma, IslaVista, SimiValley, Camarillo, FountainValley, Oxnard_land, SantaMaria, Nipomo)

extra_ports <- data.frame(Ports = rownames(extras), Longitude = extras[,1], Latitude = extras[,2])
 row.names(extra_ports) <- NULL

ports <- rbind(ports, extra_ports)

west_coast <- readRDS("westcoastbath.rds")

# associate depth with ports

ports$depth <- rep(NA,length=nrow(ports))

# turn both port, westcoast, and sorted into sp objects
coordinates(ports) <- c("Longitude","Latitude")
coordinates(west_coast) <- c("longitude","latitude")

for(i in 1:nrow(ports)){
	ports$depth[i] <- west_coast$depth[which.min(spDistsN1(west_coast,ports[i,],longlat=TRUE))]
}

# make list for each port. Find VMS data points within a certain threshold distance from these ports. Bertrand et al. (2005) [find Levy walks in VMS peruvian anchovetta data] use a threshold of 2 nautical miles. According to The Google 2 nautical miles is equivalent to 3.704 km. When longlat=TRUE in spDists the units are km. 

coordinates(sorted) <- c("Longitude","Latitude")


dn <- vector(mode = "list", length=nrow(ports))	# preallocate list
threshold <- 3.704

for (i in 1:nrow(ports)){
	dn[[i]] <- which(spDists(sorted, ports[i,], longlat=TRUE) <= threshold)
	print(i)
}

names(dn) <- ports$Ports # each list is named for the port it's about

sorted$status <- rep(NA,nrow(sorted))

for(i in 1:length(dn)){
	if(length(dn[[i]]) > 0) sorted$status[dn[[i]]] <- names(dn)[i]
	print(i)
}


atsea <- sorted[which(is.na(sorted$status)),]
saveRDS(atsea,file="atsea.rds")

