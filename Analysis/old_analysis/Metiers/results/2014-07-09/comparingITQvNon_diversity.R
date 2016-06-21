FTL <- read.csv("/Volumes/NOAA_Data/CNH/Data/Catch/FTL_2009-2013_2014-03-21.csv",stringsAsFactors=F)

require(plyr); require(reshape2); require(vegan)

# vessels which landed IFQ 
IFQ <- subset(FTL, IFQ_LANDING=="Y", select=c(VEID, FTID, LIST_OF_PERMIDS,GRID))
IFQ <- IFQ[!duplicated(IFQ),]

barplot(sort(table(IFQ$VEID),decreasing=T),bor=F)

# using existance of IFQ permit as indicator of IFQ participation. Vessels after IFQs should be more diverse. 

# need to decide about how to measure participation in a fishery. typically in lbs or $. want to say it's trips. but trips don't matter ecologically unless they're bringing in more catch. although there is some time cost for fishermen. so maybe worth looking at if lbs and $ don't yield anything.

# subset vessels into two classes: those that participate in the IFQ fishery and those that don't. 

tickets <- readRDS("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/tickets.Rda")

# create dummy variable for IFQ
tickets$IFQ <- rep(0, nrow(tickets))

# find vessels that participated in IFQ landings
pv <- unique(subset(FTL, IFQ_LANDING=="Y")$VEID)

# populate dummy variable
tickets$IFQ[tickets$veid %in% pv] <- 1

# find out lbs per year per mgmt group for IFQ and non IFQ

IFQ <- subset(tickets, veid %in% pv)
nonIFQ <- subset(tickets, !(veid %in% pv))

IFQ_lbs <- ddply(IFQ, .(year, mgmt_grp), summarize, total_caught = sum(landed_wt))
nonIFQ_lbs <- ddply(nonIFQ, .(year, mgmt_grp), summarize, total_caught = sum(landed_wt))

IFQby_mgmt <- dcast(IFQ_lbs, year~mgmt_grp)
IFQby_mgmt <- as.matrix(IFQby_mgmt)
row.names(IFQby_mgmt) <- IFQby_mgmt[,1]
IFQby_mgmt <- IFQby_mgmt[,-1]
IFQ_f <- diversity(IFQby_mgmt)
plot(IFQ_f, type='o', ylim=c(0,2))

nonIFQby_mgmt <- dcast(nonIFQ_lbs, year~mgmt_grp)
nonIFQby_mgmt <- as.matrix(nonIFQby_mgmt)
row.names(nonIFQby_mgmt) <- nonIFQby_mgmt[,1]
nonIFQby_mgmt <- nonIFQby_mgmt[,-1]
nonIFQ_f <- diversity(nonIFQby_mgmt)
lines(nonIFQ_f, type='o',col="red")

# the diversity is much higher in non IFQ because i'm looking at the aggregate catch diversity. not average by vessel. should look at that. 
# means getting diversity index for each vessel and then taking the average.


IFQ_vessel_lbs <- ddply(IFQ, .(veid,year, mgmt_grp), summarize,total_catch=sum(landed_wt))
IFQ_vessels <- split(IFQ_vessel_lbs, IFQ_vessel_lbs$veid)

IFQv_cast <- lapply(IFQ_vessels, dcast, year~mgmt_grp)

# sure there's a faster way to do this. but running through each entry in list. checking that all mgmt_grps present, adding in columsn of 0s if not, and replacing NAs with 0s
mgmt_grps <- unique(IFQ$mgmt_grp)
for(i in 1:length(IFQv_cast)){
  IFQv_cast[[i]][is.na(IFQv_cast[[i]])] <- 0
  if(length(grep(mgmt_grps[1],names(IFQv_cast[[i]])))==0){
    IFQv_cast[[i]]$CRAB <- rep(0,nrow(IFQv_cast[[i]]))
  }
  if(length(grep(mgmt_grps[2],names(IFQv_cast[[i]])))==0){
    IFQv_cast[[i]]$SRMP <- rep(0,nrow(IFQv_cast[[i]]))
  }
  if(length(grep(mgmt_grps[3],names(IFQv_cast[[i]])))==0){
    IFQv_cast[[i]]$GRND <- rep(0,nrow(IFQv_cast[[i]]))
  }
  if(length(grep(mgmt_grps[4],names(IFQv_cast[[i]])))==0){
    IFQv_cast[[i]]$OTHR <- rep(0,nrow(IFQv_cast[[i]]))
  }
  if(length(grep(mgmt_grps[5],names(IFQv_cast[[i]])))==0){
    IFQv_cast[[i]]$SHLL <- rep(0,nrow(IFQv_cast[[i]]))
  }
  if(length(grep(mgmt_grps[6],names(IFQv_cast[[i]])))==0){
    IFQv_cast[[i]]$SAMN <- rep(0,nrow(IFQv_cast[[i]]))
  }
  if(length(grep(mgmt_grps[7],names(IFQv_cast[[i]])))==0){
    IFQv_cast[[i]]$CPEL <- rep(0,nrow(IFQv_cast[[i]]))
  }
  if(length(grep(mgmt_grps[8],names(IFQv_cast[[i]])))==0){
    IFQv_cast[[i]]$HMSP <- rep(0,nrow(IFQv_cast[[i]]))
  }
}

# measure diversity

diversity_IFQ <- sapply(IFQv_cast, diversity)
mean_IFQ <- sapply(diversity_IFQ, mean)
plot(diversity_IFQ[[1]],col="white",ylim=c(0,1.5))
for(i in 1:length(diversity_IFQ)){
  lines(diversity_IFQ[[i]],lwd=.5, col=alpha("black",.5),pch=20)
}

# nonIFQ

nonIFQ_vessel_lbs <- ddply(nonIFQ, .(veid,year, mgmt_grp), summarize,total_catch=sum(landed_wt))
nonIFQ_vessels <- split(nonIFQ_vessel_lbs, nonIFQ_vessel_lbs$veid)

nonIFQv_cast <- lapply(nonIFQ_vessels, dcast, year~mgmt_grp)

# sure there's a faster way to do this. but running through each entry in list. checking that all mgmt_grps present, adding in columsn of 0s if not, and replacing NAs with 0s
mgmt_grps <- unique(IFQ$mgmt_grp)
for(i in 1:length(nonIFQv_cast)){
  nonIFQv_cast[[i]][is.na(nonIFQv_cast[[i]])] <- 0
  if(length(grep(mgmt_grps[1],names(nonIFQv_cast[[i]])))==0){
    nonIFQv_cast[[i]]$CRAB <- rep(0,nrow(nonIFQv_cast[[i]]))
    

  }
  if(length(grep(mgmt_grps[2],names(nonIFQv_cast[[i]])))==0){
    nonIFQv_cast[[i]]$SRMP <- rep(0,nrow(nonIFQv_cast[[i]]))
  }
  if(length(grep(mgmt_grps[3],names(nonIFQv_cast[[i]])))==0){
    nonIFQv_cast[[i]]$GRND <- rep(0,nrow(nonIFQv_cast[[i]]))
  }
  if(length(grep(mgmt_grps[4],names(nonIFQv_cast[[i]])))==0){
    nonIFQv_cast[[i]]$OTHR <- rep(0,nrow(nonIFQv_cast[[i]]))
  }
  if(length(grep(mgmt_grps[5],names(nonIFQv_cast[[i]])))==0){
    nonIFQv_cast[[i]]$SHLL <- rep(0,nrow(nonIFQv_cast[[i]]))
  }
  if(length(grep(mgmt_grps[6],names(nonIFQv_cast[[i]])))==0){
    nonIFQv_cast[[i]]$SAMN <- rep(0,nrow(nonIFQv_cast[[i]]))
  }
  if(length(grep(mgmt_grps[7],names(nonIFQv_cast[[i]])))==0){
    nonIFQv_cast[[i]]$CPEL <- rep(0,nrow(nonIFQv_cast[[i]]))
  }
  if(length(grep(mgmt_grps[8],names(nonIFQv_cast[[i]])))==0){
    nonIFQv_cast[[i]]$HMSP <- rep(0,nrow(nonIFQv_cast[[i]]))
  }
}

diversity_nonIFQ <- sapply(nonIFQv_cast, diversity)
plot(diversity_nonIFQ[[1]],col="white",ylim=c(0,1.57),xlim=c(1,5))
for(i in 1:length(diversity_nonIFQ)){
  lines(diversity_nonIFQ[[i]],lwd=.5, col=alpha("red",.5),pch=20)
}


IFQ_together <- dcast(melt(lapply(diversity_IFQ, as.list)), L2 ~ L1) # columns are veid, rows are years
IFQ_together <- t(IFQ_together)
IFQ_together<-IFQ_together[-1,]
colnames(IFQ_together) <- paste("yr",2009:2013,sep="")
boxplot(IFQ_together)

nonIFQ_together <- dcast(melt(lapply(diversity_nonIFQ, as.list)), L2 ~ L1) # columns are veid, rows are years
nonIFQ_together <- t(nonIFQ_together)
nonIFQ_together<-nonIFQ_together[-c(1,2,3),]
colnames(nonIFQ_together) <- paste("yr",2009:2013,sep="")
boxplot(nonIFQ_together)
boxplot(IFQ_together, add=T)
par(mfrow=c(1,2))
boxplot(IFQ_together)
boxplot(nonIFQ_together)
