# identifying ports: two approaches

# 1. Try clustering some more but carefully calculate turning difference (absolute) and distance. Also calculate second difference in distance and turning angle. 

# 2. look at distribution of grid cells. Grid cell where they spend most time is likely a port. 

# 3. Third approach would be to get a really detailed land polygon mask from NOAA (USGS?). Then any datapoint that is east of the polygon is identified as being on land. That at least will help a bit with identification. Detailed shoreline polygons from NOAA: http://www.ngdc.noaa.gov/mgg/shorelines/

# clustering
if(!exists("VMS")) {load("VMS.Rda")}
if(!exists("sorted")) {load("sorted.Rda")}

# Look at subset of different fisheries: Limited entry groundfish non-trawl, shorebased IFQ (221); limited entry trawl, shorebased IFQ, not including demersal trawl (230); Open access longline gear for groundfish (233); Non-groundfish trawl for pink shrimp (241)

S_IFQ <- subset(sorted, sorted$Declarations==221)

map("worldHires","USA",xlim=range(S_IFQ$Longitude),ylim=range(S_IFQ$Latitude),fill=TRUE,col="grey",border=FALSE)
Boats=unique(S_IFQ$Ship_Number)
for(i in 1:length(Boats)){
lines(S_IFQ$Longitude[S_IFQ$Ship_Number==Boats[i]],S_IFQ$Latitude[S_IFQ$Ship_Number==Boats[i]],type="o",pch=19,col=alpha(i,0.25),cex=0.1)
}

# with these I think that fishermen are going out of the EEZ and coming in much later. Because I'm plotting all these points for 5 years there are big steps as exemplified by the plot below. Need to think about how to deal with that. 

map("worldHires","USA",xlim=range(S_IFQ$Longitude),ylim=range(S_IFQ$Latitude),fill=TRUE,col="grey",border=FALSE)

lines(S_IFQ$Longitude[S_IFQ$Ship_Number==Boats[3]],S_IFQ$Latitude[S_IFQ$Ship_Number==Boats[3]],type="o",pch=19,col=alpha(i,0.25),cex=0.1)



Shrimp <- subset(sorted,sorted$Declarations==241)

# just look at one shrimp tracks by boat
map("worldHires","USA",xlim=range(Shrimp$Longitude),ylim=range(Shrimp$Latitude),fill=TRUE,col="grey",border=FALSE)
Boats=unique(Shrimp$Ship_Number)
for(i in 1:length(Boats)){
lines(Shrimp$Longitude[Shrimp$Ship_Number==Boats[i]],Shrimp$Latitude[Shrimp$Ship_Number==Boats[i]],type="o",pch=19,col=alpha(i,0.25),cex=0.1)
}

# checking some weird tracks

Jaka_B <- subset(Shrimp,Ship_Number=="X00392")

# this boat seems to have some mis-firings while in port 
map("worldHires","USA",xlim=range(Jaka_B$Longitude),ylim=c(42.5,46.25),fill=TRUE,col="grey",border=FALSE)
lines(Jaka_B$Longitude[825:2050],Jaka_B$Latitude[825:2050],type="o",pch=19,col=alpha("blue",.5))

# does seem like this only happens from port (since all the lines radiate out)

