# identifying ports that are not on the NOAA generated list of 'fishing communities' (wc_fishing_communities.csv) by plotting how much time is spent in each grid cell total

rm(list=ls())
library(fields)
library(sp)
library(scales)
library(maps)
library(mapdata)

setwd('/Users/efuller/Documents/Harvesting_Strategies/PaperSeeds/EmpiricalAnalysis/NOAA/Data/Fishermen/VMS_Related/VMS/Code/For_posterity')
if(!exists("atsea")) {atsea <- readRDS("atsea_updated.RDS")}

gridz <- table(atsea$longrid, atsea$latgrid)
gridz <- as.matrix(gridz)

image.plot(as.numeric(row.names(gridz)), as.numeric(colnames(gridz)),gridz, xlab="Longitude", ylab="Latitude")

# looks like 5 points that have more than 20,000 boat hours (that's more than 2 years of time). These are good candidates as locations of ports. 

Finding the grid cells that are responsible 

topfive <- which(gridz>20000)

# this gives the cell number, know that the table counts down by rows. So write a function to give me the row and column number


findtable <- function(x,data){
	col <-(floor(x[1]/nrow(data)))
	row <- x - col*nrow(data)
	lat <- colnames(data)[col+1]
	lon <- row.names(data)[row]
	return(list(row,(col+1),lat,lon, data[row,(col+1)]))
	}

summary <- data.frame(latgrid=rep(NA,5),longrid=rep(NA,5),boat.hours=rep(NA,5))

for(i in 1:5){summary[i,] <- findtable(topfive[i],gridz)[3:5]}

# plot points in these five locations on top of google map satellite images, points colored by boat name. 

topfive.1 <- which(atsea$latgrid==summary$latgrid[1] & atsea$longrid==summary$longrid[1])

topfive.2 <- which(atsea$latgrid==summary$latgrid[2] & atsea$longrid==summary$longrid[2])

topfive.3 <- which(atsea$latgrid==summary$latgrid[3] & atsea$longrid==summary$longrid[3])

topfive.4 <- which(atsea$latgrid==summary$latgrid[3] & atsea$longrid==summary$longrid[4])

topfive.5 <- which(atsea$latgrid==summary$latgrid[5] & atsea$longrid==summary$longrid[5])


# Going for the Google API to try to use their maps.
library(ggmap)

#point A
data = data.frame(x = atsea$Longitude[topfive.1], y = atsea$Latitude[topfive.1],name=atsea$Vessel_Name[topfive.1]) 


PointA = get_map(location = c(lon = as.numeric(summary$longrid[1]), lat = as.numeric(summary$latgrid[1])), maptype = 'satellite', zoom = 13)

PointAMAP = ggmap(PointA)

inset = get_map(location = c(lon = as.numeric(summary$longrid[1]), lat = as.numeric(summary$latgrid[1])), maptype = 'terrain', zoom = 12)

insetMAP =ggmap(inset, extent="device")

g = ggplotGrob(insetMAP + geom_point(data = data, aes(x = x, y = y), colour = "red"))

PointAMAP + geom_point(data = data, aes(x = x, y = y, colour = name)) + inset(grob = g, xmin = -117.915, xmax = -117.87, ymin = 33.626, ymax = 33.6696)

ggsave(file = "PointA.png")

# point B

data = data.frame(x = atsea$Longitude[topfive.2], y = atsea$Latitude[topfive.2],name=atsea$Vessel_Name[topfive.2]) 


PointB = get_map(location = c(lon = as.numeric(summary$longrid[2]), lat = as.numeric(summary$latgrid[2])), maptype = 'satellite', zoom = 13)

PointBMAP = ggmap(PointB)

inset = get_map(location = c(lon = as.numeric(summary$longrid[2]), lat = as.numeric(summary$latgrid[2])), maptype = 'terrain', zoom = 12)

insetMAP =ggmap(inset, extent="device")

g = ggplotGrob(insetMAP + geom_point(data = data, aes(x = x, y = y), colour = "red"))

PointBMAP + geom_point(data = data, aes(x = x, y = y, colour = name)) + inset(grob = g, xmin = -119, xmax = -118.97, ymin = 34.1789, ymax = 34.205)


ggsave(file = "PointB.png")

# point C

data = data.frame(x = atsea$Longitude[topfive.3], y = atsea$Latitude[topfive.3],name=atsea$Vessel_Name[topfive.3]) 


PointC = get_map(location = c(lon = as.numeric(summary$longrid[3]), lat = as.numeric(summary$latgrid[3])), maptype = 'satellite', zoom = 13)

PointCMAP = ggmap(PointC)

inset = get_map(location = c(lon = as.numeric(summary$longrid[3]), lat = as.numeric(summary$latgrid[3])), maptype = 'terrain', zoom = 11)

insetMAP =ggmap(inset, extent="device")

g = ggplotGrob(insetMAP + geom_point(data = data, aes(x = x, y = y), colour = "red"))

PointCMAP + geom_point(data = data, aes(x = x, y = y, colour = name)) + inset(grob = g, xmin = -122.050, xmax = -122.020, ymin = 36.9685, ymax = 37.0175)


ggsave(file = "PointCMAP.png")

# point D
data = data.frame(x = atsea$Longitude[topfive.4], y = atsea$Latitude[topfive.4],name=atsea$Vessel_Name[topfive.4]) 
# some reason the longrid and latgrid are incorrect for this group. Or all are clustered in one corner and not captured by google map. Instead taking mean of both lat and lon to get center of map. 

PointD = get_map(location = c(lon = mean(data$x), lat = mean(data$y)), maptype = 'satellite', zoom = 13)

PointDMAP = ggmap(PointD)

inset = get_map(location = c(lon = mean(data$x), lat = mean(data$y)), maptype = 'terrain', zoom = 11)

insetMAP =ggmap(inset, extent="device")

g = ggplotGrob(insetMAP + geom_point(data = data, aes(x = x, y = y), colour = "red"))

PointDMAP + geom_point(data = data, aes(x = x, y = y, colour = name)) + theme(legend.position="none")


PointDMAP + geom_point(data = data, aes(x = x, y = y, colour = name)) + inset(grob = g, xmin = -122.250, xmax = -122.227, ymin = 36.98, ymax = 37.01) + theme(legend.position="none")

ggsave(file = "PointDMAP.png")