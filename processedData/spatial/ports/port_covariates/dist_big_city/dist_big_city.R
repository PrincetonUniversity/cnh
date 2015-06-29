
rm(list=ls())
dist_big_city <- function(min_pop = 100000){
  # set up and load data
  all_ports <- read.csv("/Users/efuller/1/CNH/processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE)

    # find nearest city of > min_pop for each port
    # Incorporated Places and Minor Civil Divisions Datasets: 
    # Subcounty Resident Population Estimates: 
    # April 1, 2010 to July 1, 2013 - WA, OR, CA
    # http://www.census.gov/popest/data/cities/totals/2013/SUB-EST2013.html

  # get data
  library(RCurl)
  wa_csv <- getURL(
    "http://www.census.gov/popest/data/cities/totals/2013/files/SUB-EST2013_53.csv")
  wa_Pop <- read.csv(textConnection(wa_csv))
  or_csv <- getURL(
    "http://www.census.gov/popest/data/cities/totals/2013/files/SUB-EST2013_41.csv")
  or_Pop <- read.csv(textConnection(or_csv))
  ca_csv <- getURL(
    "http://www.census.gov/popest/data/cities/totals/2013/files/SUB-EST2013_6.csv")
  ca_Pop <- read.csv(textConnection(ca_csv))
  
  state_pop <- rbind(wa_Pop, or_Pop, ca_Pop)

  # just want cities, remove counties
  city_pop <- state_pop[-grep("County",state_pop$NAME),]
  city_pop <- subset(city_pop, PLACE != 0) # remove states

# keep only cities that are more than min_pop
  big_cities <- subset(city_pop, POPESTIMATE2010 > min_pop, 
                       select = c("NAME","STNAME","CENSUS2010POP"))
  colnames(big_cities) <- tolower(colnames(big_cities))

  # get columns for lat/lon
  big_cities$lon <- NA
  big_cities$lat <- NA

  for(i in 1:nrow(big_cities)){
    big_cities[,c("lon","lat")][i,] <- 
      geocode(paste0(big_cities$name[i],", ",big_cities$stname[i]))
  }

  # plot to check
  # with(big_cities, plot(lon, lat, asp = 1, col= "white"))
  # map('state',add=T)
  # text(big_cities$lon, big_cities$lat, big_cities$name, cex = .25)
  
  # for each port, find closest city, along with name

  all_ports$km.big.city <- NA
  all_ports$min.city <- NA

library(sp)
for(j in 1:nrow(all_ports)){
  if(is.na(all_ports$lon[j])) next # some don't have lat/lon because are "other" ports
  # find city with minimum distance
  city_id <- which.min(spDistsN1(as.matrix(big_cities[,c("lon","lat")]), 
                                 pt = as.matrix(all_ports[,c("lon","lat")][j,]), 
                                 longlat = TRUE))
  # assign city name
  all_ports$min.city[j] <- paste(big_cities$name[city_id], 
                                 big_cities$stname[city_id], sep = ", ")
  # assign distance
  all_ports$km.big.city[j] <- min(spDistsN1(as.matrix(big_cities[,c("lon","lat")]), 
                                            pt = as.matrix(all_ports[,c("lon","lat")][j,]), 
                                            longlat = TRUE))
}

# remove the 'city' from the names
all_ports$min.city <- gsub(pattern = " city","", all_ports$min.city)
write.csv(all_ports[,c("Pcid","km.big.city","min.city")], 
          paste0("/Users/efuller/1/CNH/processedData/spatial/ports/dist_big_city_pop",as.integer(min_pop),".csv"),
          row.names = FALSE)
}

dist_big_city()
dist_big_city(min_pop = 50000)
