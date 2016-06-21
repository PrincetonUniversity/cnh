# author: Emma
# date: 2014-03-22
# goal: to find catch profiles of fish ticket data -- subsetting just to TRAWL GEAR

require(cluster)
require(ggplot2)
require(dplyr)
require(reshape2)
require(plyr)
require(scales)
require(qgraph)
require(data.table)

#----------------------------------------------------------------
# Load Data

# Load data
FTL <- read.csv("Data/Catch/FTL_2009-2013_2014-03-21.csv",as.is=TRUE)
names(FTL) <- tolower(names(FTL))


# which species are caught by which gears?
barplot(table(FTL$spid, FTL$grid))

subFTL <-FTL

# need to make up a unique tripID
dates <- paste(subFTL$year,subFTL$month, subFTL$day,sep="-")
subFTL$tripID <- paste(dates,subFTL$veid,sep="_")
# NOTE: veid the best option? dates are day that the catch is delievered. Logbooks may help with this. 

# Format data (columns species, rows TRIPID)
FTL_df <- select(subFTL,tripID,spid,landed_wt)

# calculate total catch by trip (tripID)
  totals <- ddply(FTL_df, .(tripID), summarize, total=sum(landed_wt))

# calculate by species total catch for each trip
  catch <- ddply(FTL_df, .(tripID, spid), summarize, catch = sum(landed_wt))
  catch <- filter(catch, catch > 0)
# any NAs?
any(is.na(catch))   #FALSE, we're good

# for each entry for species caught, need to find total proportion of catch (divide by catchRET$total)
total_catch <- dcast(catch, tripID ~ spid, fill = 0, drop=TRUE)

# combine totals and species composition 
cluster <- merge(total_catch, totals, by="tripID")
cluster_df <- tbl_df(cluster)

# find "unspecified species", things that start with a "U"
unspecified <- unique(FTL$spid)[grep("^U",unique(FTL$spid))]
cluster_pre <- cluster_df[,-which(names(cluster_df) %in% unspecified)]

# find now empty rows
cluster_new <- cluster_pre[-which(rowSums(cluster_pre[2:(ncol(cluster_pre)-1)])==0),]

dim(cluster_df) - dim(cluster_new)

# construct table
start_ind = 2
end_ind = ncol(cluster_new)-1
freq <- apply(cluster_new[,start_ind:end_ind], 2, function(x) length(which(x > 0))) 
# returns indexes for species which are found fewer than 20 times

# remove those species that are in fewer than 20 trips
cluster_int <- cluster_new[,-which(names(cluster_new) %in% names(freq)[which(freq<200)])]

# remove rows that have no catch from any target species
cluster_sub <- cluster_int[-which(rowSums(cluster_int[2:(ncol(cluster_int)-1)])==0),]
dim(cluster_df)-dim(cluster_sub)
# lost 572 trips and 55 species
# lost 2003 trips and 60 species

#----------------------------------------------------------------
# Apply PCA: optional, not convinced this helps clustering substantially. 
pca.dat <- cluster_sub[2:(ncol(cluster_sub)-1)]
# Log quantities as PCA indata
pca.dat_log <- pca.dat+0.1
pca.dat_log <- log(pca.dat_log) 
pca.log <- prcomp(pca.dat_log, scale = TRUE) 
pca.scoresdat_log <- pca.log$x

# first 33 components give you > 80%
data.clust <- pca.scoresdat_log[,1:33]
plot(pca.log,npcs=(ncol(cluster_sub)-2))
abline(v=33.7,lwd=10,lty=2,col="maroon")
#----------------------------------------------------------------
# Clustering
## Using CLARA for clustering, it's a partioning around mediods approach. But takes random samples of the data. 

# preallocate a list
#data.clust <- cluster_sub[2:(ncol(cluster_sub)-1)]
max.clusters = 50
clusts.dat_log <- vector("list", max.clusters) 

for(i in 1:max.clusters){
  clusts.dat_log[[i]] <- clara(data.clust, i, metric = "euclidean", stand = TRUE, samples = 20, sampsize = 100, keep.data = FALSE, medoids.x = FALSE)
  print(i)
}

# Objective function and ASW
objectives <- vector(length = max.clusters)
for (i in 1:max.clusters) { objectives[i] <- clusts.dat_log[[i]]$objective }

asw <- vector(length = max.clusters)
for (i in 2:max.clusters) { asw[i] <- clusts.dat_log[[i]]$silinfo$avg.width  }

# Finding a jump in the objective function
plot(diff(objectives[1:62]), type = "o", pch = 19, ylab = "jump in Objective function", xlab = "Number of clusters", main = "Reduction in Unexplained variation by adding one cluster", bty="n"); 
abline(h = 0, lty = 2, lwd=2, col="red")
lines(rollmean(diff(objectives[1:62]),15),lwd=2,col="red")

# Objective function
plot(objectives[1:62], type = "o", xlab = "Number of clusters", ylab = "Objective function",pch=19,bty="n")

#ASW
plot(asw[1:62], type = "o", xlab = "Number of clusters", ylab = "Average silhoutte width",bty="n", pch=19)


## Based on results, going with the 29 cluster solution
comps <- clara(data.clust, 29, metric = "euclidean", stand = TRUE, samples = 20, sampsize = 100, keep.data = TRUE, medoids.x = TRUE)

# add catch profiles (cluster number) to original dataset
  cluster_sub$catch_profile <- comps$clustering
  profile_sub <- subset(cluster_sub,select=c(tripID,catch_profile))
  
# link to FTL
  ftl_clust <- merge(subFTL,profile_sub,by="tripID")
  save(ftl_clust,file="Analysis/Metiers/results/2014-04-14/ftl_clust29.Rdata")

  catch <- merge(catch, profile_sub, by="tripID")
  totals <- merge(totals,profile_sub, by="tripID")
  
# Which species define which clusters?
  # by occurance (i.e. these species were present most often
      freq_caught <- table(catch$catch_profile, catch$spid)
      bar <- prop.table(freq_caught,1)
      sum_freq <- melt(bar)
      # let's take the top 10 species
      names(sum_freq) <- c("profile", "spid","times")
      world <- ddply(sum_freq, .(profile), transform, rank = rank(-times, ties.method="first"))
      top_ten_freq <- subset(world, rank < 10)
      
    ggplot(top_ten_freq, aes(x=factor(spid),y=times)) + geom_bar(stat="identity", aes(fill=factor(spid))) + facet_wrap(~profile, scale="free_x") + theme(legend.position="none")

# now subset just to groundfish profiles
groundfish <- catch[which(catch$catch_profile %in% c(3, 7,8, 11, 12, 13, 21, 22, 23, 24, 25, 29)),]

gf_caught <- table(groundfish$catch_profile, groundfish$spid)

bar <- prop.table(gf_caught,1)
sum_freq <- melt(bar)
# let's take the top 10 species
names(sum_freq) <- c("profile", "spid","times")
world <- ddply(sum_freq, .(profile), transform, rank = rank(-times, ties.method="first"))
top_ten_freq <- subset(world, rank < 10)

ggplot(top_ten_freq, aes(x=factor(spid),y=times)) + geom_bar(stat="identity", aes(fill=factor(spid))) + facet_wrap(~profile, scale="free_x") + theme(legend.position="none")

######## Now look at these catch profiles over time

ts_FTL <- select(subFTL, tripID, year, month, day)
ts_FTL <- merge(ts_FTL, profile_sub,by="tripID")
ts_FTL <- ts_FTL[-which(duplicated(ts_FTL)),]
ts_FTL$date <- paste(ts_FTL$year, ts_FTL$month, ts_FTL$day,sep="-")
ts_FTL$date <- as.POSIXct(ts_FTL$date,"%Y-%m-%d")
ts_FTL <- ts_FTL[-which(duplicated(ts_FTL)),]


foo <- table(ts_FTL$date,ts_FTL$catch_profile)
bar <- melt(foo)
names(bar) <- c("date", "profile", "count")


# number of trips by profile
ggplot(bar,aes(x=strptime(date, "%Y-%m-%d"),y=count,group=profile)) +geom_line(aes(col=factor(profile))) + scale_x_datetime(limits=c(as.POSIXct(min(ts_FTL$date),format="%Y:%m:%d"),as.POSIXct(max(ts_FTL$date),format="%Y:%m:%d")),breaks=date_breaks("1 year"), labels = date_format("%Y")) + labs(x="Year",y="Number of trips")

## vessel composition (3363 vessels), matrix for profiles, elements are the number of vessels that have at least 

int <- table(profile_sub$catch_profile,profile_sub$tripID)

# for each pair of profile numbers
  # how many vessels have at least one trip in both profiles?

# subset vessels by profile pairs, then take length

# matrix
# add veid to profile_sub
  veid <- subset(subFTL, select=c("veid","tripID"))
  profile_sub <- merge(profile_sub,veid,by="tripID")

network <- matrix(nrow=29,ncol=29)
for(i in 1:length(unique(profile_sub$catch_profile))){
    set1 <- unique(profile_sub$veid[profile_sub$catch_profile==i])
  for(j in 1:length(unique(profile_sub$catch_profile))){
    set2 <- unique(profile_sub$veid[profile_sub$catch_profile==j])
    network[i,j] = length(Reduce(intersect, list(set1, set2)))
    print(j)
  }
  print(i)
}
sizes <- diag(network)
diag(network) <- rep(0,29)
# g <- graph.adjacency(network, weighted=TRUE, mode="undirected")
# plot(g, vertex.size = log(diag(network)), layout=layout.fruchterman.reingold, width=g)
qgraph(network, vsize=log(sizes), layout="spring", posCol="white", curveAll = FALSE, esize=30,colFactor=.25,bg="black")


# Just looking at trips from pinkshrimp
shrimp <- subset(profile_sub, catch_profile==15)
sblf <- profile_sub[which(profile_sub$catch_profile %in% c(3, 12, 13, 25, 29)),]

# now match to VMS
  # unique sablefish boats
    vesselIDs <- as.character(unique(sblf$veid))
    foo <- vesselIDs[1:3]

slbf_VMS <- read.csv(pipe(system(paste('grep ',foo, '/Volumes/NOAA_Data/CNH/VMS_cleaning/results/2014-03-02/VMS_woDups.csv'))))

