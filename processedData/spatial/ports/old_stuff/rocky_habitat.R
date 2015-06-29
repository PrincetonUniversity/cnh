library(rgdal)
library(rgeos)
library(sp)
library(maps)
library(raster)

# find rocky habitat within a certain radius of port (no longer needed) ----

# which ports get the most of these landings?
tickets <- readRDS(
  "/Users/efuller/1/CNH/processedData/catch/fisheries_participation_profiles/tickets_plus.RDS")

hkl2_land <- unique(tickets[which(tickets$metier=="HKL_2"),c("pcid","trip_id")])

sort(table(hkl2_land$pcid),decreasing = T)


# looks like port orford, gold beach, crescent city, brookings, tilamook, pacific city
# use VMS vessels that participate in hkl_2 to find this, because that's black rockfish and basis of extremely low-variability fishing strategy

# load VMS 
vms <- readRDS("/Users/efuller/1/CNH/processedData/spatial/vms/intermediate/04_with_trip/VMS_catch.RDS")

hkl_2 <- vms[which(vms$metier1=="HKL_2" | vms$metier2 == "HKL_2"),]
plot(hkl_2$longitude, hkl_2$latitude, asp = 1)
map('state',add=T)

# to figure out relevant distance, look at which port each of these trips are landing at
hkl_2 <- merge(hkl_2, hkl2_land, by.x = "trip_id1", by.y = "trip_id")

library(RColorBrewer)
paint <- brewer.pal(n = 12, name = "Set3")
par(bg = "grey10", mai = rep(0,4))
plot(hkl_2$longitude, hkl_2$latitude, asp = 1, cex = .5, pch = 3, col = paint[factor(hkl_2$pcid)])
map('state',add=T, col = "grey30", fill = T, bor="grey60")
legend(-129, 43, legend = unique(hkl_2$pcid), col = paint, pch = 3,lwd=5, cex = .5, bg = "grey90")

# choosing a buffer

hkl2_ports <- subset(all_ports, Pcid %in% unique(hkl_2$pcid))
points(hkl2_ports$lon, hkl2_ports$lat, col = "darkorange",pch = 19)

coordinates(hkl2_ports) <- ~lon + lat
plot(gBuffer(hkl2_ports, width = 1), add = T, bor = "white")

# think this looks good. technically I probably need to project all these measures. For now will stick with 1 km
# load data ----
all_ports <- read.csv("/Users/efuller/1/CNH/processedData/spatial/ports/all_ports.csv")

# sew together habitat rasters if not already done
if(!file.exists("/Users/efuller/1/CNH/processedData/spatial/habitat/coastal_habitat_type1.tif")){
  WA = raster("Desktop/Emma1/WA Habitat Type1.tif")
  OR = raster("Desktop/Emma1/OR Habitat Type1.tif")
  CA = raster("Desktop/Emma1/CA Habitat Type1.tif")
  
  coast_habitat <- merge(WA, OR, CA, tolerance = 1)
  writeRaster(coast_habitat, filename = "Desktop/Emma1/coastal_habitat_type1.tif", format = "GTiff")
}else{
  coast_habitat <- raster("/Users/efuller/1/CNH/processedData/spatial/habitat/coastal_habitat_type1.tif")
}

# find habitat layers ----
# using "rockfy reefs" HAPC as categorized by the EFH. Type 1 from habitat maps
# need to find some meta-data values range 1-4. My guess is that 1 is probable hard, 2 is probable mixed, 3 is probable soft, 4 is inferred rock.

# now get ports to be in same projection
  # remove ports without lon/lats
  all_ports <- all_ports[!is.na(all_ports$lon),]
  coordinates(all_ports) <- ~lon+lat
  
  # give it a CRS
  proj4string(all_ports) <- CRS("+proj=longlat +datum=WGS84")

  # project ports
  proj_ports <- spTransform(x = all_ports, CRS(proj4string(coast_habitat)))
  # save data
  saveRDS(proj_ports, "/Users/efuller/1/CNH/processedData/spatial/habitat/proj_ports.RDS")
  proj_ports <- readRDS("/Users/efuller/1/CNH/processedData/spatial/habitat/port_habitats/proj_ports.RDS")
  load('/Users/efuller/1/CNH/rawData/WCspatial/coastline.Rdata')
  proj4string(WC) <- CRS("+proj=longlat +datum=WGS84")
  WC <- spTransform(WC, CRS(proj4string(proj_ports)))
  
# I think the memory requirements are too big, have to do them one by one
  pcids <- unique(proj_ports@data$Pcid)
# list of ports and polygons
port_area <- vector("list", length = length(pcids))
names(port_area) <- pcids
  for(i in 1:nrow(proj_ports@data)){
    sub <- subset(proj_ports, Pcid == pcids[i])
    sub_poly <- gBuffer(sub, width = 150000) # in meters, I think. so this is 150km
    # then use coastline to cut off anything to the east (most important in Puget Sound)
  
    foo <- gDifference(sub_poly, WC)
    
    port_area[[i]]$port <- sub
  port_area[[i]]$buffer <- sub_poly
  }


 # find bathymetry ----

# distance to nearest rocky habitat
# amount of rocky habitat

# find distance to 100 fathoms. 