# checking crab catches. Where and when are these crab catches landed. What are they made up of species-wise?

rm(list=ls())

# load data
load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/cluster_sol_objectives_asw.Rdata")
load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/propTable_tickets.Rdata")
load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets//datSetup_datProp.Rdata")

########### looking at crab, cluster 1 #########
cp <- prop_table
cp$cluster <- cluster_ind[,"cluster"]
target_mgmt = "CRAB"

bar <- subset(cp, cluster==1, select=-cluster)
column <- which(names(bar)==target_mgmt)

# get port, gear, and date info to merge

dem <- select(tickets, ftid, veid, year, grgroup, grid, tdate,pcid)
dem <- dem[!duplicated(dem),]

# reduce to trips just in crab cp

dem <- subset(dem, ftid %in% bar$ftid)
#look for duplicates of of ftid
dem[which(duplicated(dem$ftid) | duplicated(dem$ftid, fromLast = TRUE)),] # these carry multiple gear

# will add a second column for second gear

dem$grgroup2 <- rep(NA, nrow(dem))
dem$grid2 <- rep(NA, nrow(dem))

# mark duplicates

dem$dup <- rep(0, nrow(dem))
dem$dup[duplicated(dem$ftid)] <- 1

# find fish tickets that are duplicated

dup_ftid <- dem$ftid[dem$dup==1]

# take grgroup from 
dem$grgroup2[which(dem$dup==0 & dem$ftid %in% dup_ftid)] <- dem$grgroup[which(dem$dup==1 & dem$ftid %in% dup_ftid)]
dem$grid2[which(dem$dup==0 & dem$ftid %in% dup_ftid)] <- dem$grid[which(dem$dup==1 & dem$ftid %in% dup_ftid)]

# remove dups
dem <- subset(dem, dup!=1)

# merge with bar

bar <- merge(bar, dem, by="ftid")
bar$tdate <- as.Date(bar$tdate, format="%d-%b-%y")
bar$pure <- rep(0, nrow(bar))
bar$pure[bar$CRAB==1] <- 1
bar$pure <- factor(bar$pure)

# where do these trips take place
where_caught = table(bar$pure, bar$pcid)
barplot(where_caught)
barplot(where_caught, las=2) # port orford mostly. 

# when
bydate_nt <- ddply(bar, .(tdate, pure), summarize, num_trips=length(ftid))

foo <- subset(bydate_nt, pure==1)
plot(foo$tdate, foo$num_trips, type='h',col="dodgerblue")
df <- subset(bydate_nt, pure==0)
lines(df$tdate, df$num_trips, col="red",type="h") # doesn't seem to be a temporal signature. 

# what gear
bygear <- table(bar$pure, bar$grgroup)
barplot(bygear, ylim=c(0,200)) # if hook and line or troll, then explains. sometimes with trawl and pot you get it. I'm surprised by the number of trawl trips that get crab. Maybe going out to collect pots...? Or trawls just bring in a lot of crab?

########### looking at gf, cluster  ####

cp <- prop_table
cp$cluster <- cluster_ind[,"cluster"]
target_mgmt = "GRND"

bar <- subset(cp, cluster==8, select=-cluster)
column <- which(names(bar)==target_mgmt)

# get port, gear, and date info to merge

dem <- select(tickets, ftid, veid, year, grgroup, grid, tdate,pcid)
dem <- dem[!duplicated(dem),]

# reduce to trips just in crab cp

dem <- subset(dem, ftid %in% bar$ftid)
#look for duplicates of of ftid
dem[which(duplicated(dem$ftid) | duplicated(dem$ftid, fromLast = TRUE)),] # these carry multiple gear

# will add a second column for second gear

dem$grgroup2 <- rep(NA, nrow(dem))
dem$grid2 <- rep(NA, nrow(dem))

# mark duplicates

dem$dup <- rep(0, nrow(dem))
dem$dup[duplicated(dem$ftid)] <- 1

# find fish tickets that are duplicated

dup_ftid <- dem$ftid[dem$dup==1]

# take grgroup from 
dem$grgroup2[which(dem$dup==0 & dem$ftid %in% dup_ftid)] <- dem$grgroup[which(dem$dup==1 & dem$ftid %in% dup_ftid)]
dem$grid2[which(dem$dup==0 & dem$ftid %in% dup_ftid)] <- dem$grid[which(dem$dup==1 & dem$ftid %in% dup_ftid)]

# remove dups
dem <- subset(dem, dup!=1)

# merge with bar

bar <- merge(bar, dem, by="ftid")
bar$tdate <- as.Date(bar$tdate, format="%d-%b-%y")
bar$pure <- rep(0, nrow(bar))
bar$pure[bar$GRND==1] <- 1
bar$pure <- factor(bar$pure)

# where do these trips take place
where_caught = table(bar$pure, bar$pcid)
barplot(where_caught)
barplot(where_caught, las=2) # some places more than others. mostly bigger places: astoria, newport, coos bay 

# when
bydate_nt <- ddply(bar, .(tdate, pure), summarize, num_trips=length(ftid))

foo <- subset(bydate_nt, pure==1)
plot(foo$tdate, foo$num_trips, type='h',col="dodgerblue")
df <- subset(bydate_nt, pure==0)
lines(df$tdate, df$num_trips, col="red",type="h") # doesn't seem to be a temporal signature. seems to go up with the number of catches. 

# what gear
bygear <- table(bar$pure, bar$grgroup)
barplot(bygear) # if hook and line or troll, then explains. sometimes with trawl and pot you get it. I'm surprised by the number of trawl trips that get crab. Maybe going out to collect pots...? Or trawls just bring in a lot of crab?

# probably has a lot to do with having two types of gear reported for a trip. 

# subset to vessels that have more than one type of gear
two_gear <- subset(bar, !is.na(grgroup2))
table(two_gear$pure)

barplot(table(two_gear$pure)) # bringing two gears seems to do it. 

# what proportion of the unpure trips have two gears
unpure <- subset(bar, pure==0)
table(unpure$grgroup2)

# total length of multiple gear types (guessing it's 33), so carrying two types of gear is more likely that you'll have a non-uniform trip return, but not sufficient. So where do the multiple trips happen from?

table(two_gear$pure, two_gear$pcid)


# just looking at single gear unpure catches
sg_unpure <- subset(unpure, is.na(grgroup2))
barplot(table(sg_unpure$pcid))

# make table of ports, add lat/lon to them
sg_byport <- table(sg_unpure$pcid)
sg_byport <- melt(sg_byport, value.name = "num_trips", varnames = "pcid")

# load lat/lon info
geo_port <- read.csv("/Volumes/NOAA_Data/CNH/Data/VMS/port_IDs.csv",stringsAsFactors=FALSE,header=TRUE)

sg_geo <- merge(sg_byport, geo_port, by="pcid")

# some are duplicated, so search numtrips and pcid to remove duplicates. 
sg_geo <- sg_geo[!duplicated(sg_geo[,c("pcid","num_trips")]),]

# make a map and plot ports with point the size of number of unpure trips. 
require(rworldmap)
newmap <- getMap(resolution="high")

# finding ranges to plot in
lons <- range(sg_geo$lon)
lats <- range(sg_geo$lat)

plot(newmap, xlim=lons, ylim=lats, asp=1, col="grey",bor=F)

polygon(c(-min(sg_geo$lon)^2,-min(sg_geo$lon)^2,max(sg_geo$lon)^2,max(sg_geo$lon)^2),c(-min(sg_geo$lat)^2,max(sg_geo$lat)^2,max(sg_geo$lat)^2,-min(sg_geo$lat)^2), col="darkblue")

plot(newmap, xlim=lons, ylim=lats, asp=1, col="grey",bor=F)

# try to make pie charts for each port by the number of pure and unpure trips landed

sg_new_geo <- ddply(bar, .(pcid, pure),c("nrow"))
sg_cast <- dcast(sg_new_geo, pcid ~ pure)
names(sg_cast) <- c("pcid","unpure","pure")
sg_cast <- merge(sg_cast, geo_port, by="pcid")
sg_cast <- sg_cast[!duplicated(sg_cast[,c("pcid","unpure")]),]
sg_cast[is.na(sg_cast)] <-0
colourPalette <- brewer.pal(3,'Spectral')
mapPies(sg_cast, 
        nameX="lon",
        nameY="lat",
        nameZs=c("pure","unpure"),
        xlim=lons,
        ylim=lats,
        bor=F,
        add=T,
        symbolSize=2,
        zColours=colourPalette[c(3,1)],
        addCatLegend=F)

legend(x=lons[2]-1.5 ,y=lats[1]+2 ,legend=c("gf only trips","mixed gf trips"),fill=colourPalette[c(3,1)],bor=F,bty="n",y.intersp = .5,cex=.5)
