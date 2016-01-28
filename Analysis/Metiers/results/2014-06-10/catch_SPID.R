# looking at catch by SPID

FTL <- read.csv("/Volumes/NOAA_Data/CNH/Data/Catch/FTL_2009-2013_2014-03-21.csv",as.is=TRUE)

# which species caught most?
require(plyr)
catch <- ddply(FTL, .(SPID),summarize, caught = sum(LANDED_WT))
catch <- catch[order(catch$caught,decreasing=TRUE),]

# species of unspecified rockfish have are: NUSR, NUSF, NUSP, UPOP, URCK

which(catch$SPID %in% c("NUSR","NUSF","NUSP","UPOP","URCK"))

paint = rep("grey",nrow(catch))
paint[which(catch$SPID %in% c("NUSR","NUSF","NUSP","UPOP","URCK"))] <- "indianred"

barplot(catch$caught[1:100],names.arg=catch$SPID[1:100],las=2,col=paint,bor=FALSE,log="y",ylab="log(total lbs)")

# how many different trips per species
trips <- ddply(FTL, .(SPID),summarize, num_trips = length(unique(FTID)))
trips <- trips[order(trips$num_trips,decreasing=TRUE),]

barplot(trips$num_trips, names.arg=catch$SPID,las=2,cex.names=.75)

trips_catch <- merge(catch,trips, by="SPID")
plot(trips_catch$num_trips,trips_catch$caught,pch=19,cex=0.5)
text(trips_catch$num_trips,trips_catch$caught,trips_catch$SPID,col="indianred",cex=0.5)

# zoom in
plot(trips_catch$num_trips,trips_catch$caught,pch=19,cex=0.5,ylim=c(0,7e7),xlim=c(0,20000))
text(trips_catch$num_trips,trips_catch$caught,trips_catch$SPID,col="indianred",cex=0.5)

# number of vessels 

vessels <- ddply(FTL, .(SPID), summarize, num_ves = length(unique(VEID)))

vtc <- merge(trips_catch,vessels,by="SPID")

require(lattice)
splom(vtc[,2:4], data=vtc)

# let's color these by species groups
spid <- read.csv("/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-06-10/input_data/spid.csv",stringsAsFactors=F)
require(dplyr)
mgmt <- select(spid, SPID,mgmt_grp)
vtc_wm <- merge(vtc,mgmt,by="SPID")
super.sym <- trellis.par.get("superpose.symbol")

splom(vtc[,2:4],groups=vtc$mgmt_grp,panel=panel.superpose,key=list(title="mgmt_grp",columns=4, points=list(pch=super.sym$pch[1:8], col=super.sym$col[1:8]),text=list(unique(vtc_wm$mgmt_grp))))

require(ggplot2)
plot(vtc_wm[,2:4],col=as.numeric(factor(vtc_wm$mgmt_grp)),pch=19)

gf <- subset(vtc_wm,mgmt_grp=="GRND")
plot(gf[,2:4],pch=19)
text(gf[,2:4],gf$SPID)

# now removing pacific whiting because outlier in amount caught
gf_nw <- subset(gf, SPID!="PWHT")
plot(gf_nw[,2:4],pch=19)

# so looks like there are some big flat fish types (dover, arrowtooth, petrale) and roundfish (sablefish),

# is yellowtail not part of the slope/shelf complex?

# let's look at non-gf
n_gf <- subset(vtc_wm,mgmt_grp!="GRND")

n_gf <- n_gf[with(n_gf, order(-n_gf$num_ves,-n_gf$caught,-n_gf$num_trips)),]
barplot(n_gf$caught[1:20], names.arg=n_gf$SPID[1:20], col=as.numeric(factor(n_gf$mgmt_grp)),las=2,bor=F)

gf <- gf[with(gf, order(-gf$num_ves, -gf$caught, -gf$num_trips)),]
barplot(gf$caught, names.arg=gf$SPID,las=2,ylim=c(0,7e07),bor=F)
