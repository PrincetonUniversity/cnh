# get bathymetry data, save it as an .Rda file

rm(list=ls())
library(maptools)
library(sp)
library(maps)
library(mapdata)
library(scales)
library(marmap)

getNOAA.bathy(lon1 = -133, lon2 = -117, lat1 = 32, lat2 = 50, resolution = 1) -> west_coast

# making vectorized
west_coast.sorted <- data.frame(longitude=rep(as.numeric(row.names(west_coast)),dim(west_coast)[2]),latitude=rep(as.numeric(colnames(west_coast)),each=dim(west_coast)[1]),depth=c(west_coast))

west <- as.matrix(west_coast.sorted)

# save file
saveRDS(west_coast.sorted,"westcoastbath.rds")