# interpolate number of VMS relocations to trawl points. 

# make 1km grids of fishing intensity (number of points). Then take trawl points and do nearest neighboor to connect grid value to trawl, and then linear weighting of neighboring grid cells. 

# using practice VMS data, load one vessel 

vms <- read.csv("/Volumes/untitled/CNH/VMS_cleaning/results/2014-03-02/VMS_woDups.csv", stringsAsFactors=F)

ll <- subset(vms, Vessel_Name=="Lady Law" | Vessel_Name=="Katie Ann" | Vessel_Name=="Jessie Mae")

library(sp); library(raster)
coordinates(ll)<- ~Longitude+Latitude
proj4string(ll) <- CRS("+init=epsg:4326")

rast <- raster()
extent(rast) <- extent(ll) # this might be unnecessary
ncol(rast) <- 10 # this is one way of assigning cell size / resolution
nrow(rast) <- 30

r2 <- rasterize(coordinates(ll), rast, fun=function(x,...)length(x))
plot(r2)
map('state',add=T)


### now doing it for all VMS, still need to figure out the proper resolution to make sure I'm getting km. 

coordinates(vms)<- ~Longitude+Latitude

proj4string(vms) <- CRS("+init=epsg:4326")

rast <- raster()
extent(rast) <- extent(vms) # this might be unnecessary
ncol(rast) <- 150 # this is one way of assigning cell size / resolution
nrow(rast) <- 250

r2 <- rasterize(coordinates(vms), rast, fun=function(x,...)length(x))

# see this stackoverflow answer for map code: 
# http://stackoverflow.com/questions/17214469/r-crop-raster-data-and-set-axis-limits

library(maps, maptools, mapdata)
ext <- as.vector(extent(r2))

boundaries <- map('worldHires', xlim = ext[1:2], ylim = ext[3:4], plot = FALSE)
boundaries <- map2SpatialLines(boundaries, proj4string=CRS(projection(r2)))

rgb.palette <- colorRampPalette(c("snow1","snow2","snow3","seagreen","orange","firebrick"),
                                space = "rgb")

ssplot(r2, col.regions = rgb.palette, colorkey = list(height=0.3), sp.layout = list('sp.lines', boundaries, lwd=0.5))

#plot(log(r2),col=colorRampPalette(c("steelblue","wheat"))(255),colNA="steelblue",asp=1)
#map('state',add=T,col="grey",fill=T,bor="white",asp=1)


# now add points
trawls <- read.csv("/Users/efuller/Dropbox/Jen/Data/Malin_TrawlData/2003-2010FishCatchHaulsOk.csv",stringsAsFactors=F)

library(dplyr); library(scales)
trawls_df <- select(trawls, BestLat, BestLong, TrawlId)
trawls_df <- trawls_df[!duplicated(trawls_df),]

coordinates(trawls_df) <- ~BestLong+BestLat
proj4string(trawls_df) <- proj4string(vms)
plot(trawls_df,add=T,pch=19, cex=.25, col=alpha("sienna1",.15),asp=1)

# extract points to trawl data
FI_simple <- extract(r2, coordinates(trawls_df))
FI_bilinear <- extract(r2, coordinates(trawls_df), method = 'bilinear')


# add to trawl data, write csv

trawls_df$fishing_intensity <- FI_simple
trawls_df$fishing_bilinear <- FI_bilinear
tdf <- as.data.frame(trawls_df)
require(ggplot2)
states <- map_data("state")
ggplot(states, aes(x=long, y = lat, group=group)) + geom_polygon(fill="white", colour="black")
ggplot(tdf, aes(x = x, y=y, colour=fishing_intensity)) + geom_point()

ggplot(tdf, aes(x = x, y=y, colour=fishing_bilinear)) + geom_point(size=1) + geom_polygon(data=map_data("state",region=c("washington","oregon","california")), aes(x=long, y=lat, group=group), fill="grey", colour="white") + theme_bw() + coord_equal()

write.csv(tdf, "/Volumes/untitled/CNH/Analysis/Diversity/2014-08-17/tdf.csv")

# think it needs to be: 1, decide on a projection, spTransform lat/lon to that projection, then assign CRS that goes with the projection. 

#1 decide on a projection. Because we're interpolating based on distance, want a projection that minimizes distortion in distance. but wherever I center will have no distortion in distance, but other points around edge will. see here: http://support.esri.com/en/knowledgebase/GISDictionary/term/equidistant%20projection. 

# Based on NCEAS page, it matters what the CRS was when the measurements were taken. it's likely that WGS84 was that.. I hope. I guess I'll use that to project points. 

## 

