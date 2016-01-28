# save as RDS file, easier?
library(raster)
ports <- read.csv("/Users/efuller/1/CNH/processedData/spatial/ports/all_ports.csv")
ports <- ports[-which(is.na(ports$lon)),]

library(rgeos)
library(sp)
coordinates(ports) <- ~lon+lat

proj4string(ports) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

library(rgdal)
mpas <- readOGR("/Users/efuller/1/CNH/processedData/spatial/EFH/management/","MPAs_restrict_prohib_commercial_fiishing")

ports <- spTransform(ports, proj4string(mpas))

port_polys <- gBuffer(ports, width = 100000, byid = TRUE,quadsegs = 1000)

# find coastline
load("/Users/efuller/1/CNH/processedData/spatial/2_coastline.Rdata")

proj4string(WC) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
WC <- spTransform(WC, proj4string(port_polys))

plot(WC, add = T)

# remove any land-part of the polygons
at_seas <- gDifference(port_polys, WC, byid = TRUE)

# find area of each polygon
ports@data$poly_area = gArea(at_seas, byid=TRUE)

# find overlap for each polygon with MPA. Percent overlay. 
mpa_area_covered <- gIntersection(at_seas, mpas, byid = TRUE)
mpa_area <- gArea(mpa_area_covered, byid = TRUE)

ids <- rep(NA, length(mpa_area_covered))
for(i in 1:length(mpa_area_covered)){
  ids[i] <- as.numeric(unlist(strsplit(mpa_area_covered@polygons[[i]]@ID," "))[1])
}

mpa_areas <- data.frame(id = ids)
mpa_areas$area_covered = mpa_area

ports@data$num_link_mpa <- seq(1:72)

library(plyr)
mpa_areas_agg <- ddply(mpa_areas, .(id), summarize, total_mpa_area_covered = sum(area_covered))
mpa_areas_agg$real_id <- seq(1:72)

ports@data <- merge(ports@data, mpa_areas_agg, by.x = "num_link_mpa", by.y = "real_id",all.x=TRUE, all.y=FALSE)
any(ports@data$total_mpa_area_covered > ports@data$poly_area) # should be FALSE

# percent MPA cover
ports@data$mpa_cover <- ports@data$total_mpa_area_covered/ports@data$poly_area

ports@data$id <- NULL

# load old ports data and merge
old_ports <- read.csv("/Users/efuller/1/CNH/processedData/spatial/ports/all_ports.csv")

new_ports <- merge(old_ports, ports,all.x=TRUE)

# save port with MPA percentage cover
write.csv(new_ports, "/Users/efuller/1/CNH/processedData/spatial/ports/all_ports_mpa.csv",row.names=FALSE)
