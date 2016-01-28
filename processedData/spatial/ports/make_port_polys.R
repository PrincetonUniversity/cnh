# find buffered polygons from port lat/lon. Function takes radius argument, 
# run with variable. Start with 100 km

rm(list=ls())
all_ports <- read.csv(
  "/Users/efuller/1/CNH/processedData/spatial/ports/all_ports.csv",
  stringsAsFactors = FALSE)

library(rgeos); library(sp); library(maptools)
# subset to only pcid that have lat/lon
  port_locs <- all_ports[-which(is.na(all_ports$lon)),]
  # remove row names
  row.names(port_locs)
  coordinates(port_locs) <- ~ lon+lat
  rownames(port_locs@data) <- NULL
# give lat/lon formally 
# assume NAD83 since it's a federal agency
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf
proj4string(port_locs) <- CRS("+init=epsg:4269")

# project EFH projection for habitat layers
library(raster)
hab <- raster("/Users/efuller/1/CNH/processedData/spatial/EFH/substrate/coastal_habitat_type1.tif")

port_locs <- spTransform(port_locs, proj4string(hab))

# load coastline to cut up polygon
load("/Users/efuller/1/CNH/processedData/spatial/2_coastline.Rdata")

proj4string(WC) <- CRS("+init=epsg:4269")
WC <- spTransform(WC, proj4string(hab))


buf_port <- function(radius){
  for(i in 1:nrow(port_locs)){
    port_buf <- gBuffer(port_locs[i,], width = radius * 1000, quadsegs = 10000)
    # overlay coast and remove overalp
    port_dif <- gDifference(port_buf, WC)
    # save to port_polys directory
    port_data <- port_locs@data[i,]
    rownames(port_data) <- NULL
    port_df <- SpatialPolygonsDataFrame(port_dif, cbind(port_data,
                                              radius.km = radius, 
                                              area.km2 = gArea(port_dif)/1e6)
                                        )
    writeOGR(port_df, ".", 
             paste0("/Users/efuller/1/CNH/processedData/spatial/ports/port_polys/",
                    port_locs@data$Pcid[i],"r",radius), 
             driver="ESRI Shapefile")
  }
}

buf_port(radius = 100)