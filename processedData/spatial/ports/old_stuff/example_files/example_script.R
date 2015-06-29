library(rgdal)
library(raster)
# read polygon
port_radius <- readOGR(".", "port_radius")

# read reaster
habitat <- raster("habitat.tif")

plot(habitat)
plot(port_radius, add = T, bor = "darkorange", lwd = 3)

# extract habitat types inside 
hab_types <- extract(habitat, port_radius, method = "simple", progress = "text")

# takes forever!