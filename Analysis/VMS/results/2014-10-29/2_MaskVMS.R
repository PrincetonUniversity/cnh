# Make coastline shapefile to use with VMS data
library(maptools); library(rgeos); library(raster);library(rgdal)
#----

# loading the high-resolution coastline takes a long time. avoid if possible
if(file.exists("results/2014-10-29/coastline.Rdata")){
  message("nice, nothing to do here")
}else{
  polys <- readShapePoly("../../Data/WCspatial/GSHHS_shp/h/GSHHS_h_L1.shp", force_ring = T)
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
	save(WC,file="results/2014-10-29/2_coastline.Rdata") # name is WC
}

load("results/2014-10-29/2_coastline.Rdata") # is now called WC
proj4string(WC) <- CRS("+proj=longlat +datum=WGS84")
# important, CRS("+proj=longlat + dataum=WGS84") is a geographic coordinate system, not a projected one. 
# see here: http://stackoverflow.com/questions/25411251/buffer-geospatial-points-in-r-with-gbuffer
# need to transform to a projected coordinate system. Choose Albers equal area centered on PNW
WC_aea <- spTransform(WC, CRS("+proj=aea +lat_1=34 +lat_2=47 +lat_0=43 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "))
# is NAD83 Pacific Northwest Albers
# http://www.spatialreference.org/ref/sr-org/7257/

# buffer to filter out VMS data within this buffer as "onland"
bpoly <- gBuffer(WC_aea, width=5556)
# in meters, but is equivalent to 3 nautical miles

# plotting to see
plot(bpoly, col="steelblue",bor="steelblue")
plot(WC_aea, bor=F, col="grey",add=T)

# transform bpoly back so that it can line up with VMS data
bpoly <- spTransform(bpoly, CRS(projection(WC)))

# save masked coastline and equal-area projection
save(WC_aea, file="results/2014-10-29/2_WC_aea.Rdata")
save(bpoly, file="results/2014-10-29/2_bpoly.Rdata")

# Mask VMS data
#----
load("results/2014-10-29/2_bpoly.Rdata")
load("results/2014-10-29/2_WC_aea.Rdata")

VMS <- readRDS("results/2014-10-29/1_VMS_basic_clean.RDS")
coordinates(VMS) <- c("Longitude","Latitude")

## now try the filtering for behavior of "port" and on "water" again
proj4string(VMS) <- proj4string(WC)
VMS@data$closeToShore <- as.vector(gContains(bpoly,VMS,byid=TRUE)) # now TRUE values are within 3 nautical miles of shore
VMS@data$onland <- as.vector(gContains(WC, VMS, byid=TRUE)) # TRUE values are on land
  # need as.vector() because gContains returns a vector (albeit 1 column, many rows -- but messes things up)

# not sure why column names don't work, rename [edit: because it's saving it as a matrix]
colnames(VMS@data$onland) <- "onland"
colnames(VMS@data$closeToShore) <- "closeToShore"

saveRDS(VMS, file="results/2014-10-29/2_VMS_masked.RDS")
# plot(VMS[which(VMS$behav==TRUE),],pch=19,cex=0.15)
# plot(WC,add=TRUE,col=rgb(0.5,0.5,0.5,0.15))
# plot(bpoly,add=TRUE, col=rgb(0.5,0.5,0.5,0.15))
