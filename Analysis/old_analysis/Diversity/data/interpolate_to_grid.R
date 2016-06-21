# load trawl data, mess with colnames
trawl <- read.csv("Desktop/trawldat.csv", stringsAsFactors=F, header=FALSE)
colnames(trawl) <- trawl[1,]
trawl <- trawl[-1,]

# load fish catch data, mess with colnames
fish <- read.csv("Desktop/fishdat.csv",stringsAsFactors=F, header=F)
colnames(fish) <- fish[1,]
fish <- fish[-1,]

require(plyr); require(dplyr)

# merge the lat/lon with haul info
to_merge <- subset(trawl, select=c("Trawl Id","Best Longitude (dd)", "Best Latitude (dd)"))

fish <- merge(fish, to_merge, by="Trawl Id")

# make unique gridID for each lat/lon cell
colnames(fish) <- c("TrawlID","Species","Weight","Ind_Weight","Lon","Lat")
fish$Lon <- as.numeric(fish$Lon)
fish$Lat <- as.numeric(fish$Lat)
fish$X <- floor(fish$Lon)
fish$Y <- floor(fish$Lat)
fish$Cell <- fish$X + 360 * fish$Y

# look at number of species by cell

by_cell <- ddply(fish, .(Cell), summarize, num_sp=length(unique(Species)))

# connect number of species to lat/lon
to_merge <- select(fish, Cell, X, Y )
to_merge <- to_merge[!duplicated(to_merge),]

by_cell <- merge(by_cell, to_merge, by="Cell",all.x=TRUE, all.y=F)

# make color scale for diversity
require(RColorBrewer)
paint = colorRampPalette(brewer.pal(9,"YlGnBu"))(length(seq(1,max(by_cell$num_sp)+1)))

# plot -- might need to play with the cex value to get a grid that doesn't have white space. kinda hacky
plot(by_cell$X, by_cell$Y, col=paint[(by_cell$num_sp)],asp=1,pch=15,cex=6, xlab="Longitude", ylab="Latitude", bty="n")
map('state',add=T, fill=T,bor="grey", col="dark grey")
legend("bottomleft",legend=c("min diversity","max diversity"), fill=c(paint[1],paint[length(paint)]),bor=paint[75], bty="n")

# meta code for making a series of plots

par(mfrow=c(2,3))

for(i in 1:(num_plots)){
	df <- subset(all_data, year==year[i])
	plot(df$X, df$Y, ...)
}