# finding gf trawl trips, getting speed distributions from trips
#     1. Finding IDs of vessels that land gf trips
#     2. Find VMS for those vessels
#     3. Make move stack of all vessels
#     3.5 remove outlier speeds, points which have Avg_speed > 30
#     4. Find landing dates for each vessel
#     5. Parse VMS into trip segments, additional ID for catch profile type
#     6. Find distributions of speed (both instantaneous and inferred) for each
#        catch profile type

# load data
require(plyr);  require(mapdata); require(move);require(dplyr); require(move);

VMS <- read.csv(
  "/Volumes/NOAA_Data/CNH/VMS_cleaning/results/2014-03-02/VMS_woDups.csv",
  stringsAsFactors=F)

# load ticket data
load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/cluster_sol_objectives_asw.Rdata")
load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/propTable_tickets.Rdata")
load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/tickets.Rdata")

# merge ftid with cluster
ref_ftid <- subset(prop_table, select=ftid)
ref_ftid$cluster <- cluster_ind[,"cluster"]

# merge cluster with fish ticket
tickets <- merge(tickets, ref_ftid, by="ftid")

#########################################################
# 1. Finding IDs of vessels that land salmon/tuna trips #
# tuna, salmon, cluster == 2 | cluster == 7
# or Shrimp, when subsetting to cluster==6

# exploratory, what gears are catching cluster 7/2?
total_trips <- subset(tickets, cluster==6, select=c(grid, ftid, grgroup))
total_trip <- total_trips[!duplicated(total_trips),]
barplot(round(sort(table(total_trip$grgroup))/sum(table(total_trip$grgroup))*100,3),
        las=2, bor=F, 
        main="proportion of shrimp landed by gear from 2009-2013")
# go with troll (TRL), but should also look at midwater trawlers for tuna/salmon
# go with shrimp trawl (TWS), but should check out what this MSC business is. Probably all the non-pink shrimp stuff

any_trip <- unique(subset(tickets, cluster==6 & grgroup=="TWS",select=veid)$veid)

#################################
# 2. Find VMS for these vessels #

# subset veids for all vessels which land gf trips
any_VMS <- subset(VMS, Doc_Number %in% any_trip)

length(unique(any_VMS$Doc_Number))# how many vessels in VMS
length(any_trip) # how many vessels landed gf with trawl, missing most

#################################
# 3. Make move_stack out of these vessels #
# order by vessel ID and time

any_VMS$Date_Time <- as.POSIXct(any_VMS$Date_Time, 
                               format="%Y-%m-%d %H:%M", 
                               tz="US/Pacific")

any_VMS <- any_VMS[order(any_VMS$Doc_Number, any_VMS$Date_Time),]
# remove NA time stamps
any_VMS <- any_VMS[!is.na(any_VMS$Date_Time),]

# remove any Avg_Speed > 30
any_VMS <- subset(any_VMS, Avg_Speed < 30)

#########################################
# 4. Find landing dates for each vessel #

vms_tickets <- subset(tickets, veid %in% unique(any_VMS$Doc_Number))
vms_tickets$tdate <- as.POSIXct(vms_tickets$tdate, format="%d-%b-%y", tz="US/Pacific")
unique_clusters <- function(data){
  all <- unique(data[,c("tdate","cluster","ftid")])
  all <- all[order(all$tdate),]
  return(all)
}
landings <- ddply(vms_tickets, .(veid), unique_clusters) # list of landing dates for each vessel

#########################################################################
# 5. Parse VMS into trip segments, additional ID for catch profile type #

# try using ddply to appy findInterval to landings trips. want to subset both landings and VMS by veid, find interval on tdate, and apply cluster and ftid. 

combine_VMS <- function(df){
# function to feed into ddply to match trips to cluster, ftid and generate a tripID
# also transform into move object to calculate speed to find any outliers and flag those
  veid.x=unique(df$Doc_Number)
  sub_landings <- subset(landings,veid==veid.x)
  df$tripID <- rep(NA,nrow(df))
  df$tripID[is.na(df$status)] <- findInterval(df$Date_Time[is.na(df$status)], sub_landings$tdate) + 1 # start indexing at 1
  sub_landings$tripID <- 1:nrow(sub_landings)
  sub_merge <- merge(df,sub_landings[,c("cluster","tripID","ftid")],by="tripID",all.x=TRUE,all.y=FALSE, sort=F)
  sub_merge <- sub_merge[order(sub_merge$Date_Time),]
  sub_move <- move(x=sub_merge$Longitude, y=sub_merge$Latitude, time=sub_merge$Date_Time, data=sub_merge, proj=CRS("+proj=longlat +ellps=WGS84"), animal=sub_merge$Doc_Number)
  too_fast <- which(speed(sub_move) > 16)
  sub_merge$too_fast <- rep(0,nrow(sub_merge))
  sub_merge$too_fast[too_fast] <- 1
  #sub_merge <- sub_merge[-which(speed(sub_move)>16),]
  return(sub_merge)
}

clean_VMS <- ddply(any_VMS, .(Doc_Number), combine_VMS, .progress='text')

# need to drop the too_fast data points (can't drop them in the ddply function, breaks for some reason)
clean_VMS <- subset(clean_VMS, too_fast!=1)

#############################################################################
# 6. Find distributions of speed (both instantaneous and inferred) for each #

# try for first vessel, want to subset by non_zero trip IDs and only when the type is 8
# should just subset, for only when trip ID is 8 then. then can burst based on tripID if I need to

# should make a function that takes the gf_filter, add the trip type, and save as a move object again. that's bursted for trip ID. Then can subset on trip type. Ideally link back with ftid 

pdf(file="/Volumes/NOAA_Data/CNH/Analysis/VMS/2014-07-07/maps_of_trips_shrimp.pdf",width=8,height=8)
for(i in 1:length(unique(clean_VMS$Doc_Number))){
v1 <- subset(clean_VMS, Doc_Number == unique(clean_VMS$Doc_Number)[i])

possible_trips <- unique(v1$cluster)
plot(v1$Longitude,v1$Latitude, col="white",asp=1)
v1_hmsp <- subset(v1, cluster==2)
lines(v1_hmsp$Longitude, v1_hmsp$Latitude, pch=20, cex=.25,type='o',lwd=0.1, col="cyan",asp=1)
v1_trips <- subset(v1, cluster==7)
lines(v1_trips$Longitude,v1_trips$Latitude,pch=20,lwd=0.1,type='o',cex=.25,asp=1)
map('state',add=T)
v1_crab <- subset(v1, cluster==1)
lines(v1_crab$Longitude, v1_crab$Latitude, pch=20, cex=.25, type='o',lwd=.1,col="indianred")
v1_gf <- subset(v1, cluster==8)
lines(v1_gf$Longitude, v1_gf$Latitude, pch=20, cex=.25, type='o',lwd=.1,col="chartreuse4")
v1_shrimp <- subset(v1,cluster==6)
lines(v1_shrimp$Longitude, v1_shrimp$Latitude, pch=20, cex=.25, type='o',lwd=.1,col="deeppink")
v1_shell <- subset(v1, cluster==4)
lines(v1_shell$Longitude, v1_shell$Latitude, pch=20, cex=.25, type='o',lwd=.1, col="grey")
v1_other <- subset(v1, cluster==3)
lines(v1_other$Longitude, v1_other$Latitude, pch=20, cex=0.25, type='o',lwd=.1, col="darkorchid4")
v1_cpel <- subset(v1, cluster==5)
lines(v1_cpel$Longitude, v1_cpel$Latitude, pch=20, cex=0.25, type='o',lwd=.1, col="dodgerblue")
cat(i," ")
}
dev.off()



# estimate mixture model for shrimp speed distribution
# Saving Data 
cp6 <- clean_VMS
save(cp6,file="/Volumes/NOAA_Data/CNH/Analysis/VMS/2014-07-07/cp6_tripIDVMS.Rdata")
load("/Volumes/NOAA_Data/CNH/Analysis/VMS/2014-07-07/cp6_tripIDVMS.Rdata")

# speed distribution
shrimp <- subset(cp6, cluster==6)
hist(shrimp$Avg_Speed, breaks=50,freq=F)
plot(density(shrimp$Avg_Speed))

require(mixtools)
# assumes variance same for both
shrimp1 <- normalmixEM(shrimp$Avg_Speed, lambda=c(.7,.3), mu=c(2, 12), sigma=c(2,2))
plot(shrimp1, which=2, main2="Shrimp Trawling Speeds",breaks=30)
lines(density(shrimp$Avg_Speed))

# load salmon/tuna - careful, writes over clean_VMS
load("/Volumes/NOAA_Data/CNH/Analysis/VMS/2014-07-07/cp2_7_tripIDVMS.Rdata")
samn <- subset(clean_VMS, cluster == 2)
tuna <- subset(clean_VMS, cluster == 7)

samn1 <- normalmixEM(samn$Avg_Speed, lambda=.5, mu=c(0,7), sigma=c(2,2))
tuna1 <- normalmixEM(tuna$Avg_Speed, lambda=.5, mu=c(2,7), sigma=c(2,2))
samn2 <- spEMsymloc(samn$Avg_Speed, mu0 = c(0,7))
  
plot(samn1, which=2, main2="Salmon Trolling Speeds", breaks=50,lwd2=3)
summary(samn1)

plot(tuna1, density=TRUE)
summary(tuna1)

# problem, estimated distributions not the same as actual. cant' figure out why

plot(density(tuna$Avg_Speed),col="indianred",bty="n",lwd=3,main="Distribution of Speeds",xlab="knots")
lines(density(shrimp$Avg_Speed),col="slate blue",lwd=3)
lines(density(samn$Avg_Speed),col="goldenrod1",lwd=3)
legend("topright",col=c("indianred","slate blue","goldenrod1"), legend=c("tuna troll", "shrimp trawl", "salmon troll"),lwd=3,bty="n")
