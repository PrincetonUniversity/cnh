# making maps of sablefish
library(RColorBrewer); library(sp); library(scales); library(rgeos); library(raster)

vms <- read.csv("/Users/efuller/1/CNH/VMS_cleaning/results/2014-03-02/VMS_woDups.csv", stringsAsFactors = F)
lb2013 <- read.csv("/Users/efuller/1/CNH/Data/Catch/LBK_2013_woc_samhouri.csv", stringsAsFactors = F, skip = 2)
lb2013 <- lb2013[-1,]
colnames(lb2013)[1] <- "trip.id"
lb2012 <- read.csv("/Users/efuller/1/CNH/Data/Catch/LBK_2012_woc_samhouri.csv", stringsAsFactors = F, skip = 2)
colnames(lb2012)[1] <- "trip.id"
lb2011 <- read.csv("/Users/efuller/1/CNH/Data/Catch/LBK_2011_woc_samhouri.csv", stringsAsFactors = F, skip = 2)
colnames(lb2011)[1] <- "trip.id"
lb2010 <- read.csv("/Users/efuller/1/CNH/Data/Catch/LBK_2010_woc_samhouri.csv", stringsAsFactors = F, skip = 2)
colnames(lb2010)[1] <- "trip.id"
lb2009 <- read.csv("/Users/efuller/1/CNH/Data/Catch/LBK_2009_woc_samhouri.csv", stringsAsFactors = F, skip = 2)
colnames(lb2009)[1] <- "trip.id"

lb <- rbind(lb2013, lb2012, lb2011, lb2010, lb2009)

# find trips which are a trawl and have sablefish. 

sablefish <- subset(lb,spid == "SABL", select = c("trip.id", "ddate","dtime", "rdate", "rtime", "veid", "ftid"))

# only need one of these trip entries
sablefish <- sablefish[!duplicated(sablefish[,"trip.id"]),]

sable_vessels <- subset(vms, Doc_Number %in% sablefish$veid)
length(unique(sable_vessels$Doc_Number)) < length(unique(sablefish$veid)) 
# TRUE: missing some

sable_left <- subset(sablefish, veid %in% unique(sable_vessels$Doc_Number))

reformatLogbookTime <- function (sable_left) {
  # add zeros before and after if missing digits
  # if 3 characters, means prepend a 0
  # if 2 characters and is not == 30 then append 00. If == 30, then prepend 00
  # if 1 character, append 00 and prepend 0
  
  dnew <- ifelse(nchar(sable_left$dtime) == 3, paste0('0', sable_left$dtime), sable_left$dtime)
  dnew <- ifelse(dnew == "30", paste0('00',dnew), dnew)
  dnew <- ifelse(nchar(dnew) == 2, paste0(dnew, "00"), dnew)
  dnew <- ifelse(nchar(dnew) == 1, paste0("0",dnew, "00"), dnew)

  return(dnew)
}

sable_left$dtime <- reformatLogbookTime(sable_left)
sable_left$rtime <- reformatLogbookTime(sable_left)

# format date and time
sable_left$departure <- paste(sable_left$ddate, sable_left$dtime, sep = " ")
sable_left$return <- paste(sable_left$rdate, sable_left$rtime, sep = " ")

sable_left$departure <- as.POSIXct(sable_left$departure, format = "%d-%b-%y %H%M", tz = "US/Pacific")
sable_left$return <- as.POSIXct(sable_left$return, format = "%d-%b-%y %H%M", tz = "US/Pacific")

# some NA trips, because times don't make sense. If don't, just use 9pm on that day. 

na_inds <- which(is.na(sable_left$departure))
sable_left$departure[na_inds] <- as.POSIXct(sable_left$ddate[na_inds], format = "%d-%b-%y")
sable_left$return[na_inds] <- as.POSIXct(sable_left$rdate[na_inds], format = "%d-%b-%y")

# check to make sure no more NAs
any(is.na(sable_left$departure)); any(is.na(sable_left$return))
# both false, good

sable_left$ddate <- NULL; sable_left$dtime <- NULL
sable_left$rdate <- NULL; sable_left$rtime <- NULL

# check: all return times after departures?
any(sable_left$return < sable_left$departure, na.rm = T)
# should be FALSE

# for each trip, put a 1 next to VMS data
sable_vessels$sable_trip <- NA

# make date-time class for VMS
sable_vessels$DateTime <- as.POSIXct(sable_vessels$Date_Time, format = "%Y-%m-%d %H:%M", tz = "US/Pacific")

for(i in 1:nrow(sable_left)){
  vessel_ind <- which(sable_vessels$Doc_Number == sable_left$veid[i] & 
                        sable_vessels$DateTime >= sable_left$departure[i] & 
                        sable_vessels$DateTime <= sable_left$return[i]) 
  # which VMS corresponds to vessel of interest
  
  sable_vessels$sable_trip[vessel_ind] <- 1
}

# just sablefish "fishing" points
just_sable <- subset(sable_vessels, sable_trip==1)

# split into spring and fall
just_sable$trawl_season <- NA

just_sable$trawl_season <- ifelse(just_sable$month %in% c(5,6,7), "spring", just_sable$trawl_season)
just_sable$trawl_season <- ifelse(just_sable$month %in% c(8,9,10), "fall", just_sable$trawl_season)

trawl_sable <- subset(just_sable, !is.na(trawl_season))

# remove any datapoints with speed > 40 and 0 (assume they're in port)
trawl_sable <- subset(trawl_sable, Avg_Speed < 40 & Avg_Speed != 0)

plot(density(trawl_sable$Avg_Speed), xlab = "speed (knots)", lwd = 2, main = "speed distribution")
# 2 peaks, assume lower peak is fishing. Anything lower than 5 knots, assume fishing
abline(v = 5, lwd = 3, lty = 3)

fishing_points <- subset(trawl_sable, Avg_Speed <= 5)

saveRDS(fishing_points, file="/Users/efuller/1/CNH/Analysis/VMS/2014-09-29/fishing_points.RDS")
saveRDS(trawl_sable, file ="/Users/efuller/1/CNH/Analysis/VMS/2014-09-29/trawl_sable.RDS" )

### plotting

# par(mfrow=c(2,5), mai = c(0,0,.3,0))
# years = 2009:2013
# season = c("spring","fall")
# paint <- colorRampPalette(colors=c("steelblue3","wheat2","wheat","wheat1","whitesmoke" ,"white"))
# for(j in 1:2){
#   for(i in 1:5){
#     df <- subset(fishing_points, trawl_season == season[j] & year == years[i])
#     r2 <- rasterize(coordinates(df), rast, fun=function(x,...)length(x))
#     plot(r2, main = paste(season[j], years[i], sep = " "), axes = F, bty = "n", legend = FALSE, col = paint(255), colNA = "steelblue", extent = "device")
#     map('worldHires',add=T, fill = TRUE, col=  "grey", bor=FALSE)
#   }
# }


# # plotting by season
#   spring <- subset(fishing_points, trawl_season == "spring")
#   fall <- subset(fishing_points, trawl_season == "fall" )
#   plot(spring$Longitude, spring$Latitude, asp = 1, cex = .1, col=alpha("black", .15))
#   points(fall$Longitude, fall$Latitude, cex = .1, col=alpha("steelblue", .15))
# 
# 
# # plotting raw fishing data, all
#   plot(fishing_points$Longitude, 
#        fishing_points$Latitude, asp = 1, cex = .15, col = alpha('black', .15))
#   map('state', add = T)
# 
# # fancier ssplot-ing
# library(sp); library(raster)
# coordinates(fishing_points)<- ~Longitude+Latitude
# proj4string(fishing_points) <- CRS("+init=epsg:4326")
# 
# rast <- raster()
# extent(rast) <- extent(fishing_points) # this might be unnecessary
# ncol(rast) <- 20 # this is one way of assigning cell size / resolution
# nrow(rast) <- 100
# 
# r2 <- rasterize(coordinates(fishing_points), rast, fun=function(x,...)length(x))
# plot(r2)
# map('state',add=T)
# 
# 
# # see this stackoverflow answer for map code: 
# # http://stackoverflow.com/questions/17214469/r-crop-raster-data-and-set-axis-limits
# 
# library(maptools);library(mapdata); library(lattice)
# ext <- as.vector(extent(r2))
# 
# boundaries <- map('worldHires', xlim = ext[1:2], ylim = ext[3:4], plot = FALSE)
# boundaries <- map2SpatialLines(boundaries, proj4string=CRS(projection(r2)))
# 
# rgb.palette <- colorRampPalette(c("snow1","snow2","snow3","seagreen","orange","firebrick"),space = "rgb")
# 
# spplot(r2, col.regions = rgb.palette, colorkey = list(height=0.3), sp.layout = list('sp.lines', boundaries, lwd=0.5))
# 
# 
# 
