# preprocessing data for use in hidden markov model
rm(list=ls())
library(ggplot2)
library(ggmap)
library(ggthemes)
library(maps)
library(mapdata)


setwd('~/Documents/Harvesting_Strategies/EmpiricalAnalysis/NOAA/Data_Analysis/Code/processed_data')
if(!exists("VMSdata")){VMSdata <- readRDS("processedVMS.RDS")}
VMSdata <- as.data.frame(VMSdata)

# split out limited entry midwater trawl, Pacific whiting shorebased IFQ fishery - declaration 221
  whiting <- subset(VMSdata, Declarations == 221)

# Look at it by individual
  # number of boats
  length(unique(whiting$Vessel_Name))

  # split one individual away at random - "Capt Jack"
  Pegasus <- subset(whiting, Vessel_Name == "Pegasus")

  # make sure ordered by date
  Pegasus <- Pegasus[order(Pegasus$Date_Time),]

  Pegasus <- Pegasus[-which(Pegasus$onland==1),]
  Pegasus <- Pegasus[which(is.na(Pegasus$status)),]

  # plot with land mass
  states <- map_data("state")
  usamap <- ggplot(states, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", colour="black") + coord_map(xlim = c(-130, -114), ylim = c(30, 49))

  ggplot(Pegasus[1:300,], aes(x = Longitude, y = Latitude)) + geom_line() + geom_point(aes(color=factor(status))) + coord_map(xlim = c(-130, -123), ylim = c(41.4, 48)) + geom_polygon(data=states, aes(x=long, y=lat, group=group), fill="white", colour="black")

# split out limited entry midwater trawl, highly migratory species line gear - declaration 266
migratory <- subset(VMSdata, Declarations == 266)

# Look at it by individual
# number of boats
length(unique(migratory$Vessel_Name))

# split one individual away at random - "Capt Jack"
Judy <- subset(migratory, Vessel_Name == "Judy")

# make sure ordered by date
Judy <- Judy[order(Judy$Date_Time),]
Judy <- Judy[-which(Judy$onland==1),]
Judy <- Judy[which(is.na(Judy$status)),]

# plot with land mass
states <- map_data("state")
usamap <- ggplot(states, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", colour="black") + coord_map(xlim = c(-130, -114), ylim = c(30, 49))

ggplot(Judy, aes(x = Longitude, y = Latitude)) + geom_line() + geom_point(aes(color=factor(status))) + coord_map(xlim = c(-124.41, -124), ylim = c(44.25, 44.65)) + geom_polygon(data=states, aes(x=long, y=lat, group=group), fill="white", colour="black")

foo <- Judy[1:100,]
foo <- foo[-which(foo$onland==1),]

ggplot(foo, aes(x = reorder(Longitude, Date_Time), y = reorder(Latitude,Date_Time))) + geom_line() + geom_point(aes(color=factor(onland))) + coord_map(xlim = c(-124.41, -124), ylim = c(44.25, 44.65)) + geom_polygon(data=states, aes(x=long, y=lat, group=group), fill="white", colour="black")

# also crab
CaptJack <- subset(VMSdata,Vessel_Name=="Capt Jack")
CaptJack <- CaptJack[-which(CaptJack$onland==1),]
CaptJack <- CaptJack[which(is.na(CaptJack$status)),]


## plotting HMM states
Vessel <- Pegasus

plot(Vessel$Longitude, Vessel$Latitude, pch=19,col=hmm.states,type="o",cex=0.25)
map('worldHires', xlim = range(Vessel$Longitude)+c(-.5,.5), ylim = range(Vessel$Latitude)+c(-.5,.5) , add = T,col="darkgrey",fill=TRUE,border=FALSE)
