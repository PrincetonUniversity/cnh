# Make coastline shapefile to use with VMS data
library(maptools); library(rgeos); library(raster);library(rgdal)
#----

# loading the high-resolution coastline takes a long time. avoid if possible
if(file.exists("/Users/efuller/1/CNH/Analysis/VMS/writing/code/2_coastline.Rdata")){
  message("nice, nothing to do here")
}else{
  polys <- readShapePoly("/Users/efuller/1/CNH/Data/WCspatial/GSHHS_shp/h/GSHHS_h_L1.shp", force_ring = T)
  cut <- as.data.frame(list(
    x = c(-130, -116, -116, -130, -130),
    y = c(50, 50, 32, 32, 50)))
#   Alternative, can click on the points manually
#       locator(4) # coordinates for cutting polygon
#       plot(polys) # takes forever, so shortcutting above
#       cut <- locator(4)
#       cut <- as.data.frame(cut)
#   	  # close polygon, put last entry at the end
#       cut <- rbind(cut,head(cut,1))

  B1 <- Polygon(cut)
  Bs1 <- Polygons(list(B1),ID="west_coast")
  BSP <- SpatialPolygons(list(Bs1))
  WC <- gIntersection(BSP,polys)
#   plot(WC)
	save(WC,file="/Users/efuller/1/CNH/Analysis/VMS/writing/code/2_coastline.Rdata") # name is WC
}

load("/Users/efuller/1/CNH/Analysis/VMS/writing/code/2_coastline.Rdata") # is now called WC
proj4string(WC) <- CRS("+proj=longlat +datum=WGS84")

# Mask VMS data
#----
VMS <- readRDS("/Users/efuller/1/CNH/Analysis/VMS/writing/code/1_VMS_basic_clean.RDS")
coordinates(VMS) <- c("Longitude","Latitude")

## now try the filtering for behavior of "port" and on "water" again
proj4string(VMS) <- proj4string(WC)
VMS@data$onland <- as.vector(gContains(WC, VMS, byid=TRUE)) # TRUE values are on land
  # need as.vector() because gContains returns a vector (albeit 1 column, many rows -- but messes things up)

saveRDS(VMS, file="/Users/efuller/1/CNH/Analysis/VMS/writing/code/2_VMS_masked.RDS")
