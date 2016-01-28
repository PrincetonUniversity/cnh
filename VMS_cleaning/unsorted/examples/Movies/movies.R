# Making a movies of fishing by declaration, saving long strings of plots
rm(list=ls())

setwd('/Users/efuller/Dropbox/Projects/Harvesting_Strategies/PaperSeeds/EmpiricalAnalysis/NOAA/Data/VMS_Related/VMS/Code')
library(sp)
library(scales)
library(maps)
library(mapdata)
library(fields)
library(lubridate)
if(!exists("VMS")) {load("VMS.Rda")}

# sort VMS data by declaration
list.declarations <- unique(VMS$Declarations)

# make list of names
for(i in 1:length(list.declarations)){
	name <- paste("trip",list.declarations[i],sep="_")
	assign(name,VMS[which(VMS$Declarations==list.declarations[i]),])
	print(paste(list.declarations[i],"done!",sep=" "))
}

# need to save these data frames so I don't have to generate them again. 

trip_211$floor_time <- floor_date(trip_211$Date_Time,"hour") # makes a column for what hour it is when the VMS pings
time_points <- sort(unique(trip_211$floor_time))



for(i in 401:length(time_points)){
	png(file=paste("Movies/Plots/trip_211",time_points[i],"png",sep="."),width=500,height=800)
	par(mar=c(5,3,2,2)+0.1)
	plot(c(min(trip_211$Longitude), max(trip_211$Longitude)), c(min(trip_211$Latitude), max(trip_211$Latitude)), type="n",bty="n", xlab="Longitude", ylab="Latitude", main=time_points[i])
	map('worldHires', xlim = range(trip_211$Longitude), ylim = range(trip_211$Latitude) , add = T,col="darkgrey",fill=TRUE,border=FALSE)
	points(trip_211$Longitude[which(trip_211$floor_time==time_points[i])], trip_211$Latitude[which(trip_211$floor_time==time_points[i])],bty="n",xlab="Longitude",ylab="Latitude",col= alpha("dodgerblue",0.5), pch=19)
points(trip_211$Longitude[which(trip_211$floor_time==time_points[i-1])],trip_211$Latitude[which(trip_211$floor_time==time_points[i-1])],bty="n",xlab="Longitude",ylab="Latitude",col=alpha("dodgerblue",0.5),pch=19,cex=0.8)
points(trip_211$Longitude[which(trip_211$floor_time==time_points[i-2])],trip_211$Latitude[which(trip_211$floor_time==time_points[i-2])],bty="n",xlab="Longitude",ylab="Latitude",col=alpha("dodgerblue",0.5),pch=19,cex=0.6)
points(trip_211$Longitude[which(trip_211$floor_time==time_points[i-3])],trip_211$Latitude[which(trip_211$floor_time==time_points[i-3])],bty="n",xlab="Longitude",ylab="Latitude",col=alpha("dodgerblue",0.5),pch=19,cex=0.4)
points(trip_211$Longitude[which(trip_211$floor_time==time_points[i-4])],trip_211$Latitude[which(trip_211$floor_time==time_points[i-4])],bty="n",xlab="Longitude",ylab="Latitude",col=alpha("dodgerblue",0.5),pch=19,cex=0.2)
points(trip_211$Longitude[which(trip_211$floor_time==time_points[i-5])],trip_211$Latitude[which(trip_211$floor_time==time_points[i-5])],bty="n",xlab="Longitude",ylab="Latitude",col=alpha("dodgerblue",0.5),pch=19,cex=0.01)
dev.off()
i
print((i/400)*100)
}


