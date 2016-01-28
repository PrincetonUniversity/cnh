library(raster)
habitat <- raster("/Users/efuller/1/CNH/processedData/spatial/EFH/substrate/coastal_habitat_type1.tif")
habitat <- as.integer(habitat[])

water_polys <- readRDS("/Users/efuller/Desktop/foo.RDS")

p <- lapply(water_polys@polygons , slot , "Polygons")
poly1 <- p[[1]][which.max(lapply(p[[1]], function(x) slot(x, "area")))][[1]]@coords
poly1 <- as.data.frame(poly1)
coordinates(poly1)<- ~x+y
sp_bar <- SpatialPolygons(list(Polygons(list(Polygon(poly1)), ID=1)))
proj4string(sp_bar) <- proj4string(water_polys)

port_area <- readRDS("/Users/efuller/Desktop/port_area.RDS")
plot(port_area[[1]]$buffer,col="dodgerblue",bor="white")
plot(port_area[[1]]$port,add=T,col="darkorange",cex=2,lwd=5,pch=1)

# now want to clip to get coastal raster extent
library(raster)
coast <- raster("/Users/efuller/1/CNH/processedData/spatial/EFH/substrate/coastal_habitat_type1.tif")

bar = crop(x = coast, y = extent(port_area[[1]]$buffer))
clip2 <- mask(bar,port_area[[1]]$buffer)

sub_types <- table(extract(bar, port_area[[1]]$buffer, method = "simple", progress="window")) 

sub_types <- table(getValues(bar))

# contours
library(sp)
library(rgdal)
library(raster)
library(maptools)
contour <- readShapePoly("/Users/efuller/1/CNH/processedData/spatial/EFH/depth/Physiographic_Strata.shp")
proj4string(contour) <- proj4string(water_polys)

consarea <- readShapePoly("/Users/efuller/1/CNH/processedData/spatial/EFH/management/efh_consarea_polygons.shp")

par(bg = "steelblue")
plot(subset(contour, PhysType == "lower slope" & SubRegion == "central"), col = "darkorange",bor = "darkorange",axes = T)
plot(subset(contour, PhysType == "upper slope" & SubRegion == "central"), col = "orange", add = T, bor = "orange")
plot(subset(contour, PhysType == "shelf" & SubRegion == "central"), col = "goldenrod1", add = T, bor = "goldenrod1")

plot(consarea, add = T, col = "white", bor = "white")

plot(port_area[[1]]$buffer, axes = T, ylim = c(3500000, 5500000))

for(i in 1:length(port_area)){
  plot(port_area[[i]]$buffer,add = T)
}



# saving as spatial polygons data frame
IDs <- sapply(slot(port_area[[5]]$buffer, "polygons"), function(x) slot(x, "ID"))
df <- data.frame(name = rep(0, length(IDs)), row.names=IDs)
AST_df <- SpatialPolygonsDataFrame(port_area[[5]]$buffer, df)
writeOGR(AST_df, ".", "port_radius", driver = "ESRI Shapefile")

# cropping and saving habitat
bar = crop(x = coast, y = extent(port_area[[5]]$buffer))
writeRaster(x = bar, filename = "habitat.tif", format = "GTiff", overwrite = T)

