#rm(list=ls())
# summarizing declarations, plotting trip-trajectories

library(maps)
library(mapdata)
if(!exists("sorted")) {load("sorted.Rda")}
#if(!exists("VMS")) {load("VMS.Rda")}
library(scales)
library(sp)
library(ggplot2)
library(RColorBrewer)
library(plyr)
library(reshape)

Sum_Decs <- data.frame(Declaration=unique(sorted$Declarations)[-2], Description=unique(sorted$Description))

years <- seq(109,113)
Ships <- data.frame(Year = seq(2009,2013))
time <- rep(0,length(years))

colors <- c("Reds","Greens","Blues","Purples","Greys")
colors <- rep(colors, dim(Sum_Decs)[1])
for (i in 1:dim(Sum_Decs)[1]){
	
	Trips <- sorted[which(sorted$Declarations==Sum_Decs$Declaration[i]),]
	
	for (j in 1:length(years)){
		time[j] = length(unique(Trips$Ship_Number[which(Trips$Date_Time$year==years[j])]))
		}
	Ships$foo = time
	colnames(Ships)[i+1] = paste(as.character(Sum_Decs$Declaration[i]))

	Trips_names <- unique(Trips$Ship_Number)

pdf(file=paste("Plots/","Trips","_",Sum_Decs$Declaration[i],".pdf",sep=""),width=9,height=7)


	map("worldHires","USA",xlim=range(Trips$Longitude),ylim=range(Trips$Latitude),fill=TRUE,col="grey",border=FALSE)
	
	cols <- brewer.pal(9,colors[i])
	cols <- rep(cols, length(Trips_names))
	
	for (k in 1:length(Trips_names)){
	lines(Trips$Longitude[which(Trips$Ship_Number==Trips_names[k])],Trips$Latitude[which(Trips$Ship_Number==Trips_names[k])],pch=19,type="o",cex=0.5,col=alpha(cols[k],0.5))
}

dev.off()

print(paste(floor(i/dim(Sum_Decs)[1]*100),"%",sep=""))

}

pdf(file=paste("Plots/","Fleet","_","Change",".pdf",sep=""),width=9,height=7)

# change in fleet size over time
ships.long <- melt(Ships,id="Year",measure=colnames(Ships)[-1])
ggplot(ships.long,aes(Year,value,color=variable))+geom_line()+theme_minimal()+ guides(col = guide_legend(nrow = 8,title="Declaration"))
dev.off()

Ship_dif <- as.data.frame(apply(Ships,2,diff))
Ship_dif$Year <- Ships$Year[-1]

pdf(file=paste("Plots/","Fleet","_","Rel","_","Change",".pdf",sep=""),width=9,height=7)

# relative difference btwn years
dif.long <- melt(Ship_dif,id="Year",measure=colnames(Ship_dif[-1]))
ggplot(dif.long,aes(Year,value,color=variable))+geom_line()+theme_minimal()+ guides(col = guide_legend(nrow = 8,title="Declaration"))

dev.off()
