library(ggplot2)
library(ggthemes)
library(ggmap)
library(plyr)
library(sp)
require(lubridate)

# load data
    VMS <- read.csv("/home/efuller/NOAA/Data_Analysis/Code/processed_data/VMS_wDups.csv")
	# local stand in
	VMS <- readRDS("/Users/efuller/Documents/Projects/Harvesting_Strategies/EmpiricalAnalysis/NOAA/Data_Analysis/Code/processed_data/VMSdata.RDS")
	
# how many fishermen?
    length(unique(VMS$Vessel_Name))
# how long?
	# add year
	VMS$Year <- year(VMS$Date_Time)
	range(VMS$Year)
	
	# plot
	numVesDecs <- ddply(VMS, .(year(Date_Time), Declarations), summarize, numVessels = length(unique(Vessel_Name)) )
	names(numVesDecs)[1] <- "Year"
	ggplot(numVesDecs, aes (x = Year, y = numVessels, fill = factor(Declarations))) + geom_bar(stat = "identity", position = "stack") + guides(guide_legend(nrow = 10))
	
# would like to see the number of vessels associated with each port
portsdec <- ddply(VMS, "status", summarise, NumShips = length(unique(Vessel_Name)), NumDecs = length(unique(Declarations)))
portsdec <- portsdec[-which(is.na(portsdec$status)),]
portsdec <- portsdec[order(-portsdec$NumShips),]
ggplot(portsdec, aes(x = status, y = NumShips)) + geom_bar(stat = "identity")

# most ports have a few boats (<100), a few have many more than that. 
	ggplot(ports, aes(x = NumShips)) + geom_bar()

# see top 10 ports
	ggplot(ports[1:10,], aes(x = status, y = NumShips)) + geom_bar(stat = "identity")

# what about the number of declarations associated with each port?
	# maybe a little bimodal: many with 1 declaration, some more with 15-25 declarations
	ggplot(portsdec, aes(x = NumDecs)) + geom_bar()

# which declarations have most boats?
	dec.boats <- ddply(VMS,"Declarations", summarise, NumShips = length(unique(Vessel_Name)))
dec.boats <- dec.boats[-which(is.na(dec.boats$Declarations)),]
dec.boats <- dec.boats[order(-dec.boats$NumShips),]

# many declarations have only one boat in them (remember binned across years). Few have +100
	ggplot(dec.boats, aes(x = NumShips)) + geom_bar()

	ggplot(dec.boats, aes(x = factor(Declarations), y = NumShips)) + geom_bar(stat="identity")
