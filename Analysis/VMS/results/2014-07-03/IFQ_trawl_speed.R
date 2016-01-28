# getting speed distributions for vessels that are known to be in groundfish fishery. 
# steps
# 1. find vessels which have a declaration for IFQ trawl, get vessel IDs
# 2. merge to find which trips they have, make sure all are present, and get clustering IDs
# 3. verify that vessels only landed in one vessel cluster.
# 4. Investigate any vessels which have trips in more than one vessel cluster. 

# load VMS data
require(plyr); require(dplyr); require(mapdata); require(move)

VMS <- read.csv("/Volumes/NOAA_Data/CNH/VMS_cleaning/results/2014-03-02/VMS_woDups.csv",stringsAsFactors=F)

# declarations to look at include
# 230: limited entry bottom trawl: shorebased IFQ, not including demersal trawl
# 231: limited entry demersal trawl, shorebased IFQ
# 220: limited entry midwater trawl, non_whiting shorebased IFQ

table(unique(IFQ[,c("Ship_Number","Declarations")])$Declarations)
# mostly have bottom trawl, but 4 midwater trawls and 1 shorebased whiting. 

IFQ <- subset(VMS, Declarations %in% c(230, 231, 220))
IFQ_veids <- unique(IFQ$Doc_Number)
length(IFQ_veids) # 43 vessels

rm(VMS) # take out of working memory

# load ticket data
load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/cluster_sol_objectives_asw.Rdata")
load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/propTable_tickets.Rdata")
load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/tickets.Rdata")

# merge ftid with cluster
ref_ftid <- select(prop_table, ftid)
ref_ftid$cluster <- cluster_ind[,"cluster"]

# merge cluster with fish ticket
tickets <- merge(tickets, ref_ftid, by="ftid")

IFQ_tickets <- subset(tickets, veid %in% IFQ_veids)

# all vessels present?
length(unique(IFQ_tickets$veid)) == length(IFQ_veids) # should be TRUE for all accounted

# missing 20. Which?

found <- which(IFQ_veids %in% unique(IFQ_tickets$veid))
missing <- IFQ_veids[-found]

# vessels I have VMS for but no tickets
missing_VMS <- subset(IFQ, Doc_Number %in% missing)

# these are probably california boats. plot to check
map('state', xlim=range(missing_VMS$Longitude), ylim=range(missing_VMS$Latitude))
points(missing_VMS$Longitude, missing_VMS$Latitude, pch=19, cex=.05)

# definitely a lot of california activity, but also go fairly far up into OR. will look at catch distribuitons in OR and hope they're representative of CA... :( 

# for each vessel, how many trips in each cluster?

count_cp <- ddply(IFQ_tickets, .(veid), summarize, num_cp=length(unique(cluster)))
# varied, histogram of number of cps present
hist(count_cp$num_cp, bor="dark grey",col="grey",breaks=8)

# which catch profiles?
# ref
# 1: CRAB
# 2: HMSP
# 3: OTHR
# 4: SHLL
# 5: CPEL
# 6: SRMP
# 7: SAMN
# 8: GRND 
  # restrict down to just unique veid/cp pairs
  cp_ref <- select(IFQ_tickets, veid, cluster)
  cp_ref <- cp_ref[!duplicated(cp_ref),]

 # vessels that only participated in on cp
  cp1 <- subset(count_cp, num_cp==1)
  subset(cp_ref, veid %in% cp1$veid) # possible that more landings for these vessels are in CA so don't see all fisheries.
      # one only participates in crab, other in HMSP

  cp2 <- subset(count_cp, num_cp==2)
  subset(cp_ref, veid %in% cp2$veid)
    # one does HMSP and GRND
    # three do GRND and CRAB
    # one does GRND and SRMP
    # one does GRND and SAMN
  
  cp3 <- subset(count_cp, num_cp==3)
subset(cp_ref, veid %in% cp3$veid)[order(subset(cp_ref, veid %in% cp3$veid)$veid),]

#### looking at one vessel -- Columnbian Star

cstar <- subset(IFQ, Vessel_Name=="Columbian Star")
cstar$Date_Time <- as.POSIXct(cstar$Date_Time, format="%Y-%m-%d %H:%M", tz="US/Pacific")
cstar <- cstar[order(cstar$Date_Time),]
mc <- move(x=cstar$Longitude, y=cstar$Latitude, time=cstar$Date_Time,proj=CRS("+proj=longlat +ellps=WGS84"), data=cstar, animal=cstar$Ship_Number)

mc_b <- move::burst(x=mc, f=head(mc$onland,-1))
plot(mc_b, type='o',lwd=.25,pch=19,cex=.25)
# add a map 
map('state',add=T,fill=T,col="grey",bor=F)
points(mc_b,type="o",lwd=.25,pch=19,cex=.25)

plot(midPoint(coordinates(mc_b)[-n.locs(mc_b), ], coordinates(mc_b)[-1,]), col=mc_b@burstId, pch=20)
plotBursts(mc_b, breaks=3, add=F, pch=19)
map('state',add=T,fill=T,col=alpha("grey",0.5),bor=F)

# where are cstar tickets?

cstar_tick <- subset(IFQ_tickets, veid=="578282")
cstar_tick$tdate <- as.POSIXct(cstar_tick$tdate, format="%d-%b-%y", tz="US/Pacific")

landings <- unique(cstar_tick$tdate)
t1 <- mc[mc$Date_Time < landings[1],]
t1_sub <- t1[is.na(t1$status),]
plot(t1, type="o",pch=20)
plot(t1_sub, type="o",pch=20,col="grey",xlim=c(-124.9, -123.8))
map('state',add=T)
t2 <- mc[mc$Date_Time > landings[1] & mc$Date_Time < landings[2],]
t2_sub <- t2[is.na(t2$status),]
points(t2_sub,type="o",pch=20,col="orange")
lines(t2,type="o",pch=20,col="red")
t3 <- mc[mc$Date_Time > landings[2] & mc$Date_Time < landings[3] & is.na(mc$status),]
lines(t3, type="o",pch=20, col="blue")
t4 <- mc[mc$Date_Time > landings[3] & mc$Date_Time < landings[4] & is.na(mc$status),]
lines(t4, type="o",pch=20, col="green")

trip_number = rep(NA,nrow(mc))
trip_number[which(mc$Date_Time < landings[1] & is.na(mc$status))] <- 1

for(i in 2:length(landings)){
  trip_number[which(mc$Date_Time > landings[i-1] & mc$Date_Time < landings[i] & is.na(mc$status))] <- i 
}

mc_trip <- move::burst(x=mc, f=head(trip_number,-1))

r_var <- spTransform(t1_sub,, CRSobj = "+proj=aeqd", center=T )
dBMvar_t1 <- dynBGB(r_var, locErr=9, raster=10, ext=2.15, windowSize = 31, timeStep=6, margin=13)
plot(dBMvar_t1, col=hsv(sqrt(1:700/1000)))
lines(r_var)