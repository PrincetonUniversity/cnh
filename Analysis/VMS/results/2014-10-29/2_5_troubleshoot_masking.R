# search for some good example vessels that have trajectories that go on land. 

# think San Francisco will be good, since it has such a big bay(?).
# SF is at ~37.8 degrees Lat. Find which vessels are between 37 and 38

library(sp); library(maps)
VMS <- readRDS("/Users/efuller/1/CNH/Analysis/VMS/results/2014-10-29/2_VMS_masked.RDS")

# subset df
sf_points <- VMS[which(coordinates(VMS)[,2] > 37 & coordinates(VMS)[,2] < 38),]
ves_names <- unique(sf_points@data$Ship_Number)
counts <- sort(table(sf_points@data$Ship_Number),decreasing=T)

# will do vessel with most VMS points around there

v1 <- VMS[which(VMS@data$Ship_Number==names(counts)[1]),]
plot(coordinates(v1), type='o', pch=19, cex= .15, xlim = c(-123, -122), ylim=c(37,38))
map('state',add=T)

# there's a big jump from SF to monterary. Is that accompanied by a time jump too?

v1@data$Date_Time <- strptime(v1@data$Date_Time, format="%Y-%m-%d %H:%M", tz = "US/Pacific")
v1 <- v1[order(v1@data$Date_Time),] # this doesn't seem to work totally
is.ordered(v1@data$Date_Time) # FALSE, but why?


dtime <- diff(v1@data$Date_Time)/3600
plot(dtime,type='h')
which(dtime>1000)

# look around that index 
v1@data[14858:14865,]
#yep there is a difference of months there.

# and a huge jump in lat lon
coordinates(v1)[14858:14865,]

# plotting that
plot(coordinates(v1[14858:14865,]), cex = .15)
map('state', add = T)

# means can just use that vessel in SF though. 

sf_v1 <- v1[(which(dtime>1000)+1): nrow(v1),]
plot(sf_v1)

# there is still some which are in monteray. find those
which(coordinates(sf_v1)[,2]<37)

# look at context
sf_v1@data[45115:45118,]

#difference of months, but things are not ordered. meh. make it a move object

library(move)
# remove any NA timestamps
v1 <- v1[!is.na(v1@data$Date_Time),]
# stupid duplicate times, remove those
v1 <- v1[!duplicated(v1@data$Date_Time),]
mov1 <- move(x=coordinates(v1)[,1], y = coordinates(v1)[,2], time = as.POSIXct(v1@data$Date_Time), data = v1@data, proj=CRS(projection(v1)))

v1@data$onland <- as.numeric(v1@data$onland)

sfmov1 <- mov1[(which(distance(mov1)>4e05)+1):nrow(mov1),]


land_dex <- v1@data$onland[(which(distance(mov1)>4e05)+1):nrow(mov1)]
shore_dex <- v1@data$closeToShore[(which(distance(mov1)>4e05)+1):nrow(mov1)]

# if i assume both objects are same order, I can just use data from v1

land_paint <- ifelse(land_dex==1, "red",ifelse(shore_dex==TRUE, "orange","grey"))
plot(sfmov1, type='o',cex=.15, lwd=.5, col="white")
plot(bpoly, col="steelblue",bor=F,add=T)
plot(WC, col="wheat",add=T,bor="white")
points(sfmov1, type='p',cex=.5, lwd=.5, col=land_paint)

save(sfmov1, land_paint, file="results/2014-10-29/sfmov1.Rdata")
