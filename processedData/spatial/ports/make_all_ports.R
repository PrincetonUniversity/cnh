# make a port dataframe with latitude/longitude, number of processors observed in catch data

# load data ----
tickets <- readRDS("/Users/efuller/1/CNH/processedData/catch/fisheries_participation_profiles/tickets_plus.RDS")
ports <- read.csv("/Users/efuller/1/CNH/rawData/WCspatial/wc_fishing_communities.csv",col.names=c("port","lon","lat"), stringsAsFactors = FALSE)
pcid <- read.csv("/Users/efuller/1/CNH/rawData/WCspatial/pcid.csv", stringsAsFactors = FALSE)

# format ports - get lat/lons ----
# reformat names
  pcid$Name <- tolower(pcid$Name)
  ports$port <- tolower(ports$port)
# find only ports that are in ticket data
  represented_ports <- unique(tickets$pcid)
  all_ports <- subset(pcid,Pcid %in% represented_ports ) # find ports only in tickets
# seaside-gearhart is weird, deal with whitespace
  all_ports$Name[grep("gearhart",all_ports$Name)] <- "seaside-gearhart" # reverse. gah
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
  with(to_find, plot(lon,lat,asp=1,col="dodgerblue",lwd=2,cex=.75))
  map('state',add=T)
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

# find number of processors at each port ----
library(plyr)
num_procs <- ddply(tickets, .(pcid, year), summarize, num_procs = length(unique(processorid)))
num_procs_all <- ddply(tickets, .(pcid), summarize, num_procs = length(unique(processorid)))

# there's a lot of turnover from year to year. hard to know. will leave this out for the moment. 

# write to csv ----
write.csv(all_ports, "/Users/efuller/1/CNH/processedData/spatial/ports/all_ports.csv", row.names=FALSE)
