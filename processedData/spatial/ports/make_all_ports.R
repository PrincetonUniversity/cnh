# make a port dataframe with latitude/longitude, number of processors observed in catch data

# load data ----
tickets <- readRDS("/Users/efuller/1/CNH/processedData/catch/fisheries_participation_profiles/tickets_plus.RDS")
ports <- read.csv("/Users/efuller/1/CNH/rawData/WCspatial/wc_fishing_communities.csv",col.names=c("port","lon","lat"), stringsAsFactors = FALSE)
pcid <- read.csv("/Users/efuller/1/CNH/rawData/WCspatial/pcid.csv", stringsAsFactors = FALSE)

# format ports - get lat/lons ----
# reformat names
  pcid$Name <- tolower(pcid$Name)
  ports$port <- tolower(ports$port)
  # princeton misspelled 
  ports$port[grep("princetion",ports$port)] <- "princeton (half moon bay)"
# find only ports that are in ticket data
  represented_ports <- unique(tickets$pcid)
  all_ports <- subset(pcid,Pcid %in% represented_ports ) # find ports only in tickets
# seaside-gearhart is weird, deal with whitespace
  all_ports$Name[grep("gearhart",all_ports$Name)] <- "seaside-gearhart" # reverse. gah
# remove the washington version of long beach, is a community but not a landed port
  ports <- ports[-51,]
  all_ports <- merge(all_ports, ports, by.x="Name",by.y="port",all.x=TRUE) # add latitudes
# some pcids are "/", find those and take mean between latitudes
  slashed <- grep("/",all_ports$Name)

for(i in slashed){
  # unlist the two names
    two_names <- unlist(strsplit(all_ports$Name[i],"/"))
  # gets rid of leading/trailing whitespace
    two_names <- gsub("^\\s+|\\s+$", "", two_names) 
  # find the lats listed in original fishing communities
    lat1 <- ports$lat[grep(two_names[1],ports$port)]
    lat2 <- ports$lat[grep(two_names[2],ports$port)]
    lon1 <- ports$lon[grep(two_names[1],ports$port)]
    lon2 <- ports$lon[grep(two_names[1],ports$port)]
  # take means of those
  lat <- mean(c(lat1,lat2))
  lon <- mean(c(lon1,lon2))
  # assign to proper port
  all_ports$lat[i] <- lat
  all_ports$lon[i] <- lon
}

# missing latitudes for following ports
  misses <- all_ports[which(is.na(all_ports$lat)),]
# those that don't have "other" in title should be included
  to_find <- misses[-grep("other",misses$Name),]
# search google maps with these names
  library(ggmap); library(maps)
# add states using agency so google maps doesn't get confused
  add_agency <- function(adgency){
    state = ifelse(adgency=="CDFG","CA",ifelse(adgency=="ODFW", "OR",ifelse(adgency=="WDFW","WA","NA")))
    to_find$Name[which(to_find$Agency==adgency)] <- paste0(to_find$Name[which(to_find$Agency==adgency)],", ",state)
    return(to_find)
  }
  to_find <- add_agency("CDFG")
  to_find <- add_agency("ODFW")
  to_find <- add_agency("WDFW")
# use google maps API to find lat/lons
  to_find[,c("lon","lat")] <- geocode(to_find$Name)
# check
#   with(to_find, plot(lon,lat,asp=1,col="dodgerblue",lwd=2,cex=.75))
#   map('state',add=T)
# looks good!

# assign to dataframe
put_in <- function(c.name){
  all_ports[,c.name][ind] <- to_find[,c.name][i]
  return(all_ports)
}

for(i in 1:nrow(to_find)){
  ind <- which(all_ports$Pcid==to_find$Pcid[i])
  all_ports <- put_in("lon")
  all_ports <- put_in("lat")
}

# add states to each port ----
all_ports$state <- ifelse(all_ports$Agency=="CDFG", "CA",
                   ifelse(all_ports$Agency=="ODFW", "OR",
                   ifelse(all_ports$Agency=="WDFW", "WA","NA")))

write.csv(all_ports,"/Users/efuller/1/CNH/processedData/spatial/ports/all_ports.csv",row.names = FALSE)


# add habitat types to each port ----
habitat <- read.csv("/Users/efuller/1/CNH/processedData/spatial/ports/PortsAndLanduseTypes.csv",stringsAsFactors = FALSE)
habitat <- habitat[,c("GRIDCODE","Pcid", "Shape_Length","Shape_Area")]

# caclulate habitat diversity/evenness for each port
library(vegan)
hab_div <- ddply(subset(habitat, GRIDCODE!=4), .(Pcid), summarize, habitat_simp = diversity(Shape_Area, index = "simpson")) 
# drop type 4

# convert to wide format
library(reshape2)
melt_habt <- melt(habitat, id.vars = c("Pcid","GRIDCODE"), measure.vars = "Shape_Area")
cast_habt <- dcast(melt_habt, Pcid ~ GRIDCODE, fun.aggregate = sum)
colnames(cast_habt)[2:ncol(cast_habt)] <- paste0("h", 1:4)

# calculate percents for each
cast_habt$per.h1 <- cast_habt$h1/rowSums(cast_habt[,2:5])
cast_habt$per.h2 <- cast_habt$h2/rowSums(cast_habt[,2:5])
cast_habt$per.h3 <- cast_habt$h3/rowSums(cast_habt[,2:5])
cast_habt$per.h4 <- cast_habt$h4/rowSums(cast_habt[,2:5])
cast_habt <- merge(cast_habt, hab_div)

all_ports <- merge(all_ports, cast_habt, by = "Pcid",all.x = TRUE)

# missing tacoma because doesn't have any overlap in 100km to EFH habitat layers

# add depth to upper and lower slope ----
lower <- read.csv("/Users/efuller/1/CNH/processedData/spatial/ports/lower_slope_dist.csv", stringsAsFactors = FALSE)
lower <- lower[,c("Pcid","NEAR_DIST")]
colnames(lower) <- c("Pcid","dist_lower_slope")

upper <- read.csv("/Users/efuller/1/CNH/processedData/spatial/ports/upper_slope_dist.csv", stringsAsFactors = FALSE)
upper <- upper[,c("Pcid","NEAR_DIST")]
colnames(upper) <- c("Pcid","dist_upper_slope")

dist_slopes <- merge(lower, upper, by = "Pcid")
dist_slopes$dist_lower_slope[which(dist_slopes$dist_lower_slope==-1)] <- NA
dist_slopes$dist_upper_slope[which(dist_slopes$dist_upper_slope==-1)] <- NA

all_ports <- merge(all_ports, dist_slopes, all.x =TRUE)

# write to csv ----
write.csv(all_ports, "/Users/efuller/1/CNH/processedData/spatial/ports/all_ports.csv", row.names=FALSE)
