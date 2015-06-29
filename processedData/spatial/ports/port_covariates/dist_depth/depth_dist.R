# distance to deep water
library(sp); library(rgdal); library(rgeos)

# load depth contours
depth <- readOGR("/Users/efuller/1/CNH/processedData/spatial/EFH/depth/", "Physiographic_Strata")
all_ports <- read.csv("/Users/efuller/1/CNH/processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE)

port_locs <- all_ports[-which(is.na(all_ports$lon)),]

coordinates(port_locs) <- ~lon+lat
proj4string(port_locs) <- CRS("+init=epsg:4269")

port_locs <- spTransform(port_locs, CRS(proj4string(depth)))

# calculate minimum cartesian distances between upper slope and lower slope
up_slope <- subset(depth, PhysType == "upper slope")
lower_slope <- subset(depth, PhysType == "lower slope")

port_locs@data$dist_upper_slope.m2 <- NA
port_locs@data$dist_lower_slope.m2 <- NA

for(i in 1:nrow(port_locs)){
  port_locs@data$dist_upper_slope.m2[i] <- gDistance(port_locs[i,], up_slope)
  port_locs@data$dist_lower_slope.m2[i] <- gDistance(port_locs[i,], lower_slope)
}

depth_ports <- as.data.frame(port_locs@data)
depth_ports$Name <- NULL
depth_ports$Agency <- NULL
depth_ports$state <- NULL

write.csv(depth_ports, "/Users/efuller/1/CNH/processedData/spatial/ports/port_covariates/dist_depth.csv")
