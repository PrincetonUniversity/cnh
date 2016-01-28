
rm(list=ls())
library(maptools)
library(sp)
library(maps)
library(mapdata)
library(scales)
library(fields)
library(lubridate)

setwd('/Users/efuller/Dropbox/Projects/Harvesting_Strategies/PaperSeeds/EmpiricalAnalysis/NOAA/Data/VMS_Related/VMS/Code')

if(!exists("atsea")) {load("atsea.Rda")}

# sort VMS data by declaration
list.declarations <- unique(atsea$Declarations)

# make list of names
for(i in 1:length(list.declarations)){
	name <- paste("trip",list.declarations[i],sep="_")
	assign(name,atsea[which(atsea$Declarations==list.declarations[i]),])
	print(paste(list.declarations[i],"done!",sep=" "))
}

# try dungenous crab first, dec 261

trip_261$floor_time <- floor_date(trip_261$Date_Time,"hour")	# makes a column for what hour it is when the VMS pings
time_points <- sort(unique(trip_261$floor_time))

for(i in 1:length(500)){
	png(file=paste("Movies/Plots/trip_261",time_points[i],"png",sep="."),width=500,height=800)
	par(mar=c(5,3,2,2)+0.1)
	plot(c(min(trip_261$Longitude), max(trip_261$Longitude)), c(min(trip_261$Latitude), max(trip_261$Latitude)), type="n",bty="n", xlab="Longitude", ylab="Latitude", main=time_points[i])
	map('worldHires', xlim = range(trip_261$Longitude), ylim = range(trip_261$Latitude) , add = T,col="darkgrey",fill=TRUE,border=FALSE)
	points(trip_261$Longitude[which(trip_261$floor_time==time_points[i])], trip_261$Latitude[which(trip_261$floor_time==time_points[i])],bty="n",xlab="Longitude",ylab="Latitude",col= alpha("dodgerblue",0.5), pch=19)
points(trip_261$Longitude[which(trip_261$floor_time==time_points[i-1])],trip_261$Latitude[which(trip_261$floor_time==time_points[i-1])],bty="n",xlab="Longitude",ylab="Latitude",col=alpha("dodgerblue",0.5),pch=19,cex=0.8)
points(trip_261$Longitude[which(trip_261$floor_time==time_points[i-2])],trip_261$Latitude[which(trip_261$floor_time==time_points[i-2])],bty="n",xlab="Longitude",ylab="Latitude",col=alpha("dodgerblue",0.5),pch=19,cex=0.6)
points(trip_261$Longitude[which(trip_261$floor_time==time_points[i-3])],trip_261$Latitude[which(trip_261$floor_time==time_points[i-3])],bty="n",xlab="Longitude",ylab="Latitude",col=alpha("dodgerblue",0.5),pch=19,cex=0.4)
points(trip_261$Longitude[which(trip_261$floor_time==time_points[i-4])],trip_261$Latitude[which(trip_261$floor_time==time_points[i-4])],bty="n",xlab="Longitude",ylab="Latitude",col=alpha("dodgerblue",0.5),pch=19,cex=0.2)
points(trip_261$Longitude[which(trip_261$floor_time==time_points[i-5])],trip_261$Latitude[which(trip_261$floor_time==time_points[i-5])],bty="n",xlab="Longitude",ylab="Latitude",col=alpha("dodgerblue",0.5),pch=19,cex=0.01)
dev.off()
i
print((i/length(time_points))*100)
}


