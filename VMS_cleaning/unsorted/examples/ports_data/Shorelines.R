# Reading NOAA GIS shapefiles of coastlines into R
library(maptools)

setwd("/Users/efuller/Documents/Harvesting_Strategies/PaperSeeds/EmpiricalAnalysis/NOAA/Data/Biophysical/CoastLines/gshhg-shp-2.2.2/GSHHS_shp/i")

# read in shapefile

# want intermediate resolution ('i') and just the boundary between ocean and land ('L1'). This is the biggest one I can do without R crashing
x <- readShapeSpatial("GSHHS_i_L1.shp",force_ring=TRUE)

# based on trial and error, found that N.America is the polygon with id==2

NAmerica <- x[x$id==2,]		# N. america

# load in VMS data

setwd("~/Documents/Harvesting_Strategies/PaperSeeds/EmpiricalAnalysis/NOAA/Data/Fishermen/VMS_Related/VMS/Code/For_posterity")

atsea <- readRDS("atsea.rds")

filtersea <- over(atsea, NAmerica)
onland <- filtersea[!is.na(filtersea$area),]

# onland provides rowname for atsea data which falls on land. will label in atsea$status 'onland'

atsea$status[which(row.names(atsea) %in% row.names(onland))]="onland"

# now test with plot
plot(NAmerica,xlim=c(-120,-115),ylim=c(34,48))
points(atsea[which(atsea$status=="onland"),],pch=19,col="red",cex=0.1)

reallywater <- atsea[which(is.na(atsea$status)),]

saveRDS(reallywater,"atsea_updated.RDS")