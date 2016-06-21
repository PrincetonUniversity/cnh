# re-processing FTL for catch composition analyses
# methods: 
#   1. will group rockfish into northern slope, northern shelf, and northern nearshore group. 
#   2. look at filtering out rare species 

# paths
home <- "/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-06-11/"
library(plyr); library(dplyr); library(data.table)

# read in data
FTL <- read.csv("/Volumes/NOAA_Data/CNH/Data/Catch/FTL_2009-2013_2014-03-21.csv",stringsAsFactors=F)
colnames(FTL) <- tolower(colnames(FTL))

# define rockfish market categories
spid <- read.csv(paste(home,"spid.csv",sep=""),stringsAsFactors=F)

# now going to add darkblotched into slope, yelloweye, canary, widow, shortbelly into shelf, and black into nearshore. These are all species of rockfish that the PacFin species codes do not give a rockfish assemblage for. But the maps of rockfish areas do list these species as these particular assemblages. 

shelf <- subset(spid,complex3=="NSLF" | complex4=="SSLF" | SPID == "YEY1" | SPID == "CNR1" | SPID == "WDW1" | SPID == "SBL1",select=c(SPID))
slope <- subset(spid,complex3=="NSLP" | complex4=="SSLP" | SPID == "DBR1",select=c(SPID))
nearshore <- subset(spid,complex3=="NSHR" | complex4=="SSRD" | complex4=="SSRT" | SPID == "BLK1",select=SPID)

# check to make sure these are unique groups, should all be false
any(shelf$SPID %in% slope$SPID)
any(shelf$SPID %in% nearshore$SPID)
any(slope$SPID %in% nearshore$SPID)

tickets <- select(FTL, ftid, veid, year, spid, landed_wt, ppp, grgroup, grid, tdate)

tickets$market_cat <- tickets$spid
tickets$market_cat[tickets$spid %in% shelf$SPID] <- "shelf"
tickets$market_cat[tickets$spid %in% slope$SPID] <- "slope"
tickets$market_cat[tickets$spid %in% nearshore$SPID] <- "nearshore"

# real effect is showing up for nearshore, bunch of species now grouped together
unique(subset(tickets,market_cat=="nearshore",select="spid"))

# no real effect for shelf, and just darkblotched and some unspecified rockfish for slope
unique(subset(tickets,market_cat=="slope",select="spid"))
unique(subset(tickets,market_cat=="shelf",select="spid"))
  # aside: interested to know how much the vermillion and tiger rockfish made up relative to unspecified shelf rockfish
    shelf_ids <- unique(subset(tickets,market_cat=="shelf",select="spid"))
    shelf_catch <- subset(FTL, spid %in% shelf_ids$spid, select=c("spid","landed_wt"))
    shelf_species <- ddply(shelf_catch, .(spid), summarize, total_catch= sum(landed_wt))
    shelf_species$prop <- shelf_species$total_catch/sum(shelf_species$total_catch)
    barplot(shelf_species$prop,names.arg=shelf_species$spid,bor=F,las=2)
      # yeah, not that much contributes. but that's still ok. Yellowtail dominates. 

# now look at amount caught per trip
by_trip <- data.table(tickets)
setkey(by_trip, ftid)
catch <- by_trip[, sum(landed_wt), by=c("ftid","market_cat")]

# transform: rows are trip, column are market category that was landed
total_catch <- dcast.data.table(catch, ftid ~ market_cat, fun=sum)

# in order to remove trips, need to find each trips major species
prop_species <- prop.table(as.matrix(total_catch[,!"ftid",with=FALSE]),1)
major_species <- apply(prop_species, 1, which.max)
which_species <- colnames(prop_species)[major_species]

# add back to data_frame
total_catch[,major_species:= which_species]

# remove clams, echinoderms, oysters, abalone
to_remove <- unique(subset(spid,mgmt_grp=="SHLL" | complex=="ECHN",select="SPID"))

# remove trips in which major species was in to_remove
filtered_catch <- subset(total_catch,!major_species %in% to_remove$SPID)

# remove columns that are in the to_remove species IDs
filtered_catch <- filtered_catch[,!which(colnames(filtered_catch) %in% to_remove$SPID), with=FALSE]

# look at species caught by market category
# lbs caught by market category
by_species <- colSums(filtered_catch[,2:(ncol(filtered_catch)-1),with=FALSE])
by_species <- sort(by_species,decreasing=T)
barplot(by_species,bor=F,las=2,ylim=c(0,2e6))
barplot(by_species,bor=F,las=2,log="y")
prop_species <- by_species/sum(by_species)
barplot(prop_species,las=2,bor=F)
cumsum(prop_species)
plot(cumsum(prop_species),type="o",pch=19,cex=0.5)

# remove whiting
nwh <- tail(by_species,-1)
prop_nwh <- nwh/sum(nwh)
plot(cumsum(prop_nwh),type="o",pch=19,cex=0.5)
barplot(prop_nwh,las=2,bor=F,col="dark blue")

# major species, gives the number of trips that had each species as the major species. Thinking of trying to restrict to species which have more than 50 trips in which they were the major species caught

filtered_species <- sort(table(filtered_catch$major_species),decreasing=T)

to_keep <- c(names(filtered_species[filtered_species>50]),"ftid","major_species")

# remove trips which do not have these species as major
f_50 <- subset(filtered_catch, major_species %in% to_keep)
f_50 <- f_50[,which(colnames(f_50) %in% to_keep), with=FALSE]

# look at fish tickets not included, what ports are they from, what's in their catch? how many different vessels?

# want to know what's not in f_50$ftid
missing_ftid <- setdiff(total_catch[,"ftid",with=FALSE],f_50[,"ftid",with=FALSE])
missing_ftid <- as.data.frame(missing_ftid[,"ftid",with=FALSE])

dropped <- subset(tickets, ftid %in% missing_ftid$ftid)

# how many different vessels
length(unique(dropped$veid)); length(unique(tickets$veid))

# what about market categories?
unique(dropped$market_cat)
dropped_spid<-sort(table(dropped$market_cat),decreasing=T)

kept_spid<-sort(table(tickets$market_cat),decreasing=T)
kept_spid <- kept_spid[which(names(kept_spid) %in% names(dropped_spid))]
kept_spid <- as.data.frame(kept_spid);
kept_spid$spid <- rownames(kept_spid)

dropped_spid <- as.data.frame(dropped_spid)
dropped_spid$spid <- rownames(dropped_spid)

mspid <- merge(dropped_spid,kept_spid,by="spid")
mspid$prop <- mspid$dropped_spid/mspid$kept_spid
mspid <- mspid[order(mspid$prop,decreasing=T),]
barplot(mspid$prop[mspid$prop<1],las=2,bor=F,col="dark red",main="proportion of reported catch dropped")
# can see that this pulls most of the trips that contain some of the more non-target species, but keeps most of the trips of highly targeted species. 

# what about ports, get ports from FTL

ports <- select(FTL, ftid, pcid)

dropped <- merge(dropped, ports, by="ftid")
by_port <- sort(table(dropped$pcid),decreasing=T)
barplot(by_port,las=2,bor=F,col="dark orange")

# loose most from Willapa bay, which makes sense as they do oysters and clams. but let's look relative to the number of ports landed at in all tickets. 
tickets <- merge(tickets, ports, by="ftid")
kept_port<-sort(table(tickets$pcid),decreasing=T)
kept_port <- kept_port[which(names(kept_port)%in%names(by_port))]
kept_port <- as.data.frame(kept_port);
kept_port$pcid <- rownames(kept_port)

dropped_port <- as.data.frame(by_port)
dropped_port$pcid <- rownames(dropped_port)

mpcid <- merge(dropped_port,kept_port,by="pcid")
mpcid$prop <- mpcid$by_port/mpcid$kept_port
mpcid <- mpcid[order(mpcid$prop,decreasing=T),]
barplot(mpcid$prop,las=2,bor=F,col="tomato1",main="proportion of trips dropped from each port")

# which looks like I'm dropping a lot of washington ports, but those seem like they're mostly shellfish ports anyway. Although shelton was surprising to me.. should investigate that further. these are valuable plots to get a feel for whether I'm systematically disregarding some ports. Certainly it doesn't seem like it's random (would expect a relatively flat proportion). Also it seemed like a lot of astoria trips, but reltaive to the amount, it seems actually pretty low. but the highest from oregon. might be worth comign back to? maybe it's the shellfish? will have to check. 

# still a litle confused about the rockfish. For example, the black rockfish should be in the nearshore group but are not listed that way. same with canaray (they're shelf), and same with widow (also shelf) and yellowtail (shelf). I think it might be because these are species of special concern, but for my purposes (to be consistent), shouldn't they be grouped with the shelf groups they're a part of?

# also looking at the species that are kept
spid[spid$SPID %in% names(f_50),]

# not sure about some of these unspecified species that are kept. in particular OSRM, BSRM, GSRM, OCTP, RCRB, SPRW, UHAG, USKT

# function for general info about trips

vess_comp <- function(species){
  
  # finding trips that have majority of this catch
  species_only <- subset(f_50, major_species==species)
  
  cat(nrow(species_only)," trips caught only ",species,"\n")
  
  # what else is caught in these trips
  s_tot <- colSums(species_only[,!c("ftid","major_species"),with=F]) 
  
  cat("these targeting ",species," also caught:\n", names(s_tot[s_tot>0]),"\n", s_tot[s_tot>0], "\n")
  
  
  species_some <- subset(f_50, species>0)
  
  cat("an additional ",nrow(species_some)-nrow(species_only), " trips which have caught ",species, "along with other stuff\n")
  
  species_ftid <- as.data.frame(species_only[,"ftid",with=F])
  species_vid<-unique(subset(tickets, ftid %in% species_ftid$ftid,select=veid)) #  12 vessels
  
  cat(nrow(species_vid)," different vessels with majority catch of",species,"\n")
  
  species_trips <- subset(tickets, ftid %in% species_ftid$ftid, select=c(ftid, veid, year, grgroup,tdate,pcid))
  species_trips <- species_trips[!duplicated(species_trips),]
  # hm, looks like boats from unspecified washington ports, shelton. 
  
  cat("ports that trips landed majority catches of ",species, "are: ",unique(species_trips$pcid),"\n")
  
  # also it's only the sand/ghost shrimp
  cat("market codes for ", species, "are ",unique(subset(FTL, spid==species,select=category)$category),"\n")
  
  species_ftl <- subset(tickets, veid %in% species_vid$veid,select=ftid)
  species_other <- subset(f_50, ftid %in% species_ftl$ftid,select = major_species)
  barplot(table(species_other$major_species),las=2,bor=F,col="tomato1",main="major species of other trips vessels made") # looks like they're involved in other fisheries. i'm going to leave it. 
}

# looking at albacore
species_only <- subset(f_50, major_species=="ALBC")

# what else is caught in these trips
s_tot <- colSums(species_only[,!c("ftid","major_species"),with=F]) 


# how are albacore catches getting anchovy?

AN <- subset(species_only, NANC>0,select=c("ftid","ALBC","NANC"))
plot(AN$ALBC,AN$NANC,pch=19,cex=0.75,col=alpha("black",.5),ylab="lbs Anchovy",xlab="lbs Albacore")
# maybe a break here
abline(h=3500,lty=2)

# what's the gear being used?
gAN <- subset(FTL, ftid %in% AN$ftid, select=c(ftid, grid, spid, landed_wt,pcid,removal_type,year))

gear_only <- gAN[!duplicated(gAN$ftid),]
barplot(table(gear_only$grid),bor=F) 

# most are troll, but some are ONT. I bet the ONT are ones where anchovy catch is high. but each trip is using two types of gear. So any albacore trip that has both anchovy and tuna, using two types of gear. 

table(gAN$grid) # almost all of these trips are also reporting "ONT" (other net), 

table(gAN$pcid) # and everyone is coming from WPT, Westport, WA. That's crazy. I wouldn't have guessed that anchovy would be landed in Washington. But I guess they're landed as bait there. http://www.ifish.net/board/showthread.php?t=295315

# but also, ALBC is a recreational fishery for sure, but what's the reporting requirements. Do they fill out fish tickets? More info for Washington tuna: http://wdfw.wa.gov/fishing/tuna/recreational.html 

# but looks like all of these are characterized as commercial (C). But is that what charters have to do? But obviously a commercial tuna fishery off the coast. I think you'd have to have a commercial license to land tuna at a processor. And that's where the fish tickets take place. But anyone else, wouldn't. But does that mean that if you sell tuna off your boat, commercially, that it's not captured by the fish ticket data?

table(gAN$removal_type)

## looking at salmon: first which ones are in the fish tickets
sal_name <- subset(spid, mgmt_grp=="SAMN",select="SPID")
sal_catch <- subset(species_tot, spid %in% sal_name$SPID) # all of them are in there. 

barplot(sal_catch$total,bor=F,names.arg=sal_catch$spid,main="Salmon catch by market category")

### chinook
CK_only <- subset(f_50, major_species=="CHNK")

# what else is caught in these trips
s_tot <- colSums(CK_only[,!c("ftid","major_species"),with=F]) 

# what else are in these catches?
barplot(tail(sort(s_tot[s_tot>0],decreasing=T),-1),bor=F,las=2)

# what's the gear being used?
gCK <- subset(FTL, ftid %in% CK_only$ftid, select=c(ftid, grid, spid, landed_wt,pcid,removal_type,year))

barplot(sort(table(gCK$grid),decreasing=T),bor=F,las=2)

# what's the gear being used?
gAN <- subset(FTL, ftid %in% AN$ftid, select=c(ftid, grid, spid, landed_wt,pcid,removal_type,year)) # not super helpful 

# what are the trips that bring in rockfish?
CK_rock <- subset(CK_only, nearshore > 0 | shelf > 0 | slope > 0)

barplot(tail(sort(colSums(CK_rock[,!c("ftid","major_species"),with=F]),decreasing=T),-1),las=2,bor=F)

# what's the gear of these?
gCKr <- subset(FTL, ftid %in% CK_rock$ftid,select=c(ftid, grid, spid, landed_wt,pcid,removal_type,year))

barplot(table(gCKr$grid)) # hm, not useful.

# what's the distribution of catch for shelf rockfish by trip

rock <- as.data.frame(CK_rock[,"shelf",with=F])

hist(rock$shelf[rock$shelf>0],bor=F,col="darkslategrey",breaks=100,xlab="distribution of landed weight of shelf rockfish",main="")

# what are the trips that are bringing in a ton of rockfish?
CKrft <- as.data.frame(CK_rock[which(rock$shelf>400),"ftid",with=F])

rCKt <- subset(FTL, ftid %in% CKrft$ftid,select=c(ftid, grid, spid, landed_wt,pcid,removal_type,year))

table(rCKt$grid)
table(rCKt$spid)

subset(rCKt,spid=="NUSF")
subset(rCKt,spid=="WDW1")

# there are definitely trolling trips that are bringing in widow rockfish, canary, unspecified shelf and yellowtail. Why?

# looking at DCRB

vess_comp("OSRM") # keep
vess_comp("BSRM") # drop > seperate from rest of groundfish fisheries
vess_comp("GSRM") # drop > seperate from rest of grounfish fisheries
vess_comp("OCTP") # keep
vess_comp("RCRB") # maybe drop?
vess_comp("SPRW") # keep
vess_comp("UHAG") # keep
vess_comp("USKT") # keep


# adding in BSRM and GSRM to the list of dropped species

to_remove <- unique(subset(spid,mgmt_grp=="SHLL" | complex=="ECHN" | SPID=="BSRM" | SPID=="GSRM",select="SPID"))

# remove trips in which major species was in to_remove
filtered_catch <- subset(total_catch,!major_species %in% to_remove$SPID)

# remove columns that are in the to_remove species IDs
filtered_catch <- filtered_catch[,!which(colnames(filtered_catch) %in% to_remove$SPID), with=FALSE]

filtered_species <- sort(table(filtered_catch$major_species),decreasing=T)

to_keep <- c(names(filtered_species[filtered_species>50]),"ftid","major_species")

# remove trips which do not have these species as major
f_50 <- subset(filtered_catch, major_species %in% to_keep)
f_50 <- f_50[,which(colnames(f_50) %in% to_keep), with=FALSE]

# find proportion table

prop_table <- as.data.frame(f_50)
prop_table[,2:(ncol(prop_table)-1)] <- prop_table[,2:(ncol(prop_table)-1)]/rowSums(prop_table[,2:(ncol(prop_table)-1)])

# do pca
pca_prop <- prcomp(prop_table[,2:(ncol(prop_table)-1)],scale=T)
npc <- length(which(summary(pca_prop)[[6]][3,]<= .81))

# retain principal components which retain > 80% of variation
dat_setup <- pca_prop$x[,1:npc]

# randomize rows to prevent any bias from order of fish tickets
  # set up row index, so can translate back to original order
row <- seq(1:nrow(dat_setup))
dat_setup <- cbind(dat_setup,row)
dat_setup <- dat_setup[sample(nrow(dat_setup)),]
dat_prop <- dat_setup[,1:(ncol(dat_setup)-1)]

# set cluster info/data structures
require(cluster)
max.clusts = 30
samples = 100
sampsize = 1000
clust.dat.prop <- vector("list",length = max.clusts)

# clustering
set.seed(2)
for(i in 1:max.clusts){
  clust.dat.prop[[i]] <- clara(dat_prop, i, stand=TRUE, samples = samples, sampsize=sampsize, keep.data = FALSE, pamLike=TRUE, rngR = TRUE)
  cat(i,"...")
}

objectives <- vector(length = max.clusts)
for (i in 1:max.clusts) { objectives[i] <- clust.dat.prop[[i]]$objective }

asw <- vector(length = max.clusts)
for (i in 2:max.clusts) { asw[i] <- clust.dat.prop[[i]]$silinfo$avg.width  }
max_asw <- max(asw)
nodes <- which(asw==max(asw))

clust_prop <- list(data_transform="prop",cluster_sol = nodes, objectives = objectives, asw = asw, clustering = clust.dat.prop[[nodes]]$clustering, sampsize=sampsize, samples=samples, npc=npc)

png(paste(home,"plots/","Fig2.png",sep=""),height=4,width=5,units="in",res=300)
par(mfrow=c(1,2),omi=c(0,0,0,0))
plot(clust_prop$objectives, type="o",pch=19, cex=0.35,bty="n", main="Objective function",col="tomato1")
plot(clust_prop$asw, type="o", pch=19, cex=0.35, bty="n", main="Silhouette Width",col="turquoise")
points(c(19,24,27),c(asw[c(19,24,27)]),pch=19,col="dark blue",cex=.5)

dev.off()

png(paste(home,"plots/","Fig3.png",sep=""),height=8,width=12,units="in",res=300)
plot_clusters(19)
dev.off()

png(paste(home,"plots/","Fig4.png",sep=""),height=8,width=12,units="in",res=300)
plot_clusters(24)
dev.off()

png(paste(home,"plots/","Fig5.png",sep=""),height=8,width=12,units="in",res=300)
plot_clusters(27)
dev.off()

plot(diff(clust_prop$asw),type="o",pch=19,cex=0.35, bty="n")
abline(h=0,lty=2,lwd=2,col="grey")

# seems like 19, 24, 27 are possible
plot(asw,type="o")
points(19,asw[19],pch=19)
points(24,asw[24],pch=19)
points(27,asw[27],pch=19)

# next steps: look at different clusters there, then think about pulling out different single-species fisheries. tuna, salmon, crab, shrimp, hake. we know those are single species fisheries. 

# what are the catches for each set of mediods?

plot_clusters <- function(sol){
  
  meds <- dat_setup[clust.dat.prop[[sol]]$i.med,"row"]
  to_plot <- prop_table[meds,2:(ncol(prop_table)-1)]
  to_plot$cluster <- 1:nrow(to_plot)
  
  one_df <- melt(to_plot,id="cluster")
  one_df <- one_df[one_df$value>0,]
  colnames(one_df) <- c("cluster","species","catch_proportion")
  
  ggplot(one_df, aes(x=species,y=catch_proportion,fill=species)) + geom_bar(stat="identity") + facet_wrap(~ cluster,scales="free_x") + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + guides(fill=FALSE)
}

plot_clusters(19)
plot_clusters(24)
plot_clusters(27)

# conclusions: going from 19-24, add the nearshore/shelf (cluster 18), 3 additional groundfish multispecies groups, and a nearshore group (cluster 5).

barplot(table(clust.dat.prop[[19]]$clustering),main="19 clusters")
barplot(table(clust.dat.prop[[24]]$clustering),main="24 clusters")
barplot(table(clust.dat.prop[[27]]$clustering),main="24 clusters")

# maping trips to clusters
clust_map <- data.frame(c19 = clust.dat.prop[[19]]$clustering, c24 = clust.dat.prop[[24]]$clustering, c27 = clust.dat.prop[[27]]$clustering)

clust_map$trip = seq(1, nrow(clust_map))

require(scales)
sub_map <- clust_map[1:10000,]
plot(seq(1,3),sub_map[1,],type='l',ylim=c(0,27),col=alpha("black",0.15),pch=19)
for(i in 1:nrow(sub_map)){
  lines(seq(1,3),sub_map[i,],col=alpha("black",0.5),type="l",pch=19)
}

# what about looking at variance across clustering solutions? Some will consistently be assigned to the same cluster, whereas others won't. The absolute number of clustering shouldn't matter... just the difference. Like a 0 if the numbers are the same, a 1 if they're different. 

foo <- function(vals){
  bar <-ifelse(diff(vals)!=0,1,0)
}

bar <- apply(clust_map[,1:2],1,foo)
foobar <- apply(clust_map[,2:3],1,foo)
barplot(table(bar)/length(bar))
barplot(table(foobar)/length(foobar))

index=which(foobar==1)

plot(seq(1,3),clust_map[index[1],],type='l',ylim=c(0,27),col=alpha("black",0.15),pch=19)
for(i in 1:length(index)){
lines(seq(1,3),clust_map[index[i],],type='l',col=alpha("black",0.15))
}

# think I should drop these species trips, so much bigger than everything else

png(paste(home,"plots/","Fig6.png",sep=""),height=8,width=6,units="in",res=300)
par(mfrow=c(3,1))
barplot(species_tot$total,names.arg=species_tot$spid,las=2,bor=F,col="darkslategrey",ylab="lbs",cex.axis=1,cex.names=0.5)

barplot(species_tot[which(species_tot$total>1e08),2],names.arg=species_tot[which(species_tot$total>1e08),1],bor=F,col="indian red",ylab="lbs")

barplot(species_tot[which(species_tot$total<1e08),2],names.arg=species_tot[which(species_tot$total<1e08),1],las=2,col="deepskyblue",bor=F,cex.names=0.5,ylab="lbs")
dev.off()


# now going to change darkblotched into slope, yelloweye, canary, widow, shortbelly into shelf, and black into nearshore. These are all species of rockfish that the PacFin species codes do not give a rockfish assemblage for. But the maps of rockfish areas do list these species as these particular assemblages. 


# what happens if I cluster over the raw data?
raw_prop <-prop_table[,2:41]
raw_clust <- vector("list",length = max.clusts)

# clustering
set.seed(2)
for(i in 1:max.clusts){
  raw_clust[[i]] <- clara(raw_prop, i, stand=TRUE, samples = samples, sampsize=sampsize, keep.data = FALSE, pamLike=TRUE, rngR = TRUE)
  cat(i,"...")
}

objectives_raw <- vector(length = max.clusts)
for (i in 1:max.clusts) { objectives_raw[i] <- raw_clust[[i]]$objective }

asw_raw <- vector(length = max.clusts)
for (i in 2:max.clusts) { asw_raw[i] <- raw_clust[[i]]$silinfo$avg.width  }
max_asw <- max(asw)
nodes <- which(asw==max(asw))

# PCA is way better
png(paste(home,"plots/","Fig1.png",sep=""),height=7,width=5,units="in",res=300)
par(mfrow=c(2,1),oma=c(0,0,0,0))
plot(clust_prop$objectives, type="o",pch=19, cex=1,bty="n", main="Objective function",col="tomato1",ylim=c(2,20),xlab="Number of Clusters",ylab="Objective Function")
lines(objectives_raw,type="o",pch=19,col="turquoise2")
legend("topright",legend=c("PCA applied prior","no PCA"),col=c("tomato1","turquoise2"),bty="n",pch=19,pt.cex=1,pt.lwd=1,lwd=1)

plot(clust_prop$asw, type="o", pch=19, cex=1, bty="n", main="Silhouette Width",col="tomato1",xlab="Number of Clusters",ylab="Average Silhouette Width")
lines(asw_raw,type="o",pch=19,col="turquoise2")
legend("topleft",legend=c("PCA applied prior","no PCA"),col=c("tomato1","turquoise2"),bty="n",pch=19,pt.cex=1,pt.lwd=1,lwd=1)
dev.off()
