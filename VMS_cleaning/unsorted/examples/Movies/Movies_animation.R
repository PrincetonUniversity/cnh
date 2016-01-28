# movies using animation package to sew together all inside R

rm(list=ls())
library(sp)
library(maptools)
library(animation)
library(zoo)
library(RColorBrewer)library(maptools)
library(maps)
library(mapdata)
library(scales)
library(fields)
library(lubridate)

sea <- readRDS("atsea.rds")

dec = 999 # which declaration to make a movie for

dec_sea <- sea[which(sea$Declarations==dec),]	# subsetting

dec_sea$floor_time <- floor_date(dec_sea$Date_Time, "hour") # makes a column for what hour it is when the VMS pings

time_points <- sort(unique(dec_sea$floor_time))
time_points <- as.POSIXlt(time_points)

dec_sea <- as.data.frame(dec_sea)

# Animation and options

# set delay between frames when replaying 
ani.options(interval = 0.1, ani.width = 400, ani.height = 650)

# begin animation loop
saveGIF({

par(mar=c(5,3,2,2)+0.1)

for (i in 1:200){
plot(c(min(dec_sea$Longitude), max(dec_sea$Longitude)), c(min(dec_sea$Latitude), max(dec_sea$Latitude)), type="n",bty="n", xlab="Longitude", ylab="Latitude", main=time_points[i])
map('worldHires', xlim = range(dec_sea$Longitude), ylim = range(dec_sea$Latitude) , add = T,col="darkgrey",fill=TRUE,border=FALSE)
points(dec_sea$Longitude[which(dec_sea$floor_time==time_points[i])], dec_sea$Latitude[which(dec_sea$floor_time==time_points[i])],bty="n",xlab="Longitude",ylab="Latitude",col= alpha("dodgerblue",0.5), pch=19)
points(dec_sea$Longitude[which(dec_sea$floor_time==time_points[i-1])],dec_sea$Latitude[which(dec_sea$floor_time==time_points[i-1])],bty="n",xlab="Longitude",ylab="Latitude",col=alpha("dodgerblue",0.5),pch=19,cex=0.8)
points(dec_sea$Longitude[which(dec_sea$floor_time==time_points[i-2])],dec_sea$Latitude[which(dec_sea$floor_time==time_points[i-2])],bty="n",xlab="Longitude",ylab="Latitude",col=alpha("dodgerblue",0.5),pch=19,cex=0.6)
points(dec_sea$Longitude[which(dec_sea$floor_time==time_points[i-3])],dec_sea$Latitude[which(dec_sea$floor_time==time_points[i-3])],bty="n",xlab="Longitude",ylab="Latitude",col=alpha("dodgerblue",0.5),pch=19,cex=0.4)
points(dec_sea$Longitude[which(dec_sea$floor_time==time_points[i-4])],dec_sea$Latitude[which(dec_sea$floor_time==time_points[i-4])],bty="n",xlab="Longitude",ylab="Latitude",col=alpha("dodgerblue",0.5),pch=19,cex=0.2)
points(dec_sea$Longitude[which(dec_sea$floor_time==time_points[i-5])],dec_sea$Latitude[which(dec_sea$floor_time==time_points[i-5])],bty="n",xlab="Longitude",ylab="Latitude",col=alpha("dodgerblue",0.5),pch=19,cex=0.01)

}
})
