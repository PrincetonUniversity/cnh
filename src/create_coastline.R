# Make coastline shapefile to use with VMS data
library(maptools); library(rgeos); library(raster);library(rgdal)

# loading the high-resolution coastline takes a long time. avoid if possible ----
if(file.exists("/Users/efuller/1/CNH/processedData/spatial/2_coastline.Rdata")){
  message("nice, nothing to do here")
}else{
  polys <- readShapePoly("/Users/efuller/1/CNH/rawData/WCspatial/GSHHS_shp/h/GSHHS_h_L1.shp", force_ring = T)
  cut <- as.data.frame(list(
    x = c(-130, -116, -116, -130, -130),
    y = c(50, 50, 32, 32, 50)))
  #   Alternative, can click on the points manually
  #       locator(4) # coordinates for cutting polygon
  #       plot(polys) # takes forever, so shortcutting above
  #       cut <- locator(4)
  #       cut <- as.data.frame(cut)
  #       # close polygon, put last entry at the end
  #       cut <- rbind(cut,head(cut,1))
  
  B1 <- Polygon(cut)
  Bs1 <- Polygons(list(B1),ID="west_coast")
  BSP <- SpatialPolygons(list(Bs1))
  WC <- gIntersection(BSP,polys)
  #   plot(WC)
  save(WC,file="/Users/efuller/1/CNH/processedData/spatial/2_coastline.Rdata") # name is WC
}
