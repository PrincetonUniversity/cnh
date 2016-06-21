# This script sorts the catch data into catch per management group (as delineated in PacFin's database) and clusters
require(plyr); require(dplyr); require(data.table); require(reshape2); require(ggplot2); require(qgraph); require(RColorBrewer)
CNH <- "/Volumes/NOAA_Data/CNH/"

FTL <- read.csv(paste(CNH,"Data/Catch/FTL_2009-2013_2014-03-21.csv",sep=""),stringsAsFactors=F)
spid <- read.csv(paste(CNH,"Analysis/Metiers/results/2014-04-24/spid.csv",sep=""),stringsAsFactors=F)

colnames(FTL) <- tolower(colnames(FTL))

# what are the different management groups?
mgmt_grp <- dlply(spid, .(mgmt_grp))

# remove the NA ones
mgmt_grp <- mgmt_grp[2:9]

# add management group variable to fish tickets

m.vec <- rep(NA,nrow(FTL))

for(i in 1:length(mgmt_grp)){
  m.vec[FTL$spid %in% mgmt_grp[[i]]$SPID] = names(mgmt_grp[i])
}

tickets <- select(FTL, ftid, veid, year, spid, landed_wt, ppp, grgroup, grid, tdate)
tickets$mgmt_grp <- m.vec

# now look at amount caught per trip
by_trip <- data.table(tickets)
setkey(by_trip, ftid)
catch <- by_trip[, sum(landed_wt), by=c("ftid","mgmt_grp")]

total_catch <- dcast.data.table(catch, ftid ~ mgmt_grp, fun=sum)

# find proportion table
prop_table <- as.data.frame(total_catch)
prop_table[,2:ncol(prop_table)] <- prop_table[,2:ncol(prop_table)]/rowSums(prop_table[,2:ncol(prop_table)])

# do pca
pca_prop <- prcomp(prop_table[,2:ncol(prop_table)],scale=T)
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

#png(paste(home,"plots/","Fig2.png",sep=""),height=4,width=5,units="in",res=300)
par(mfrow=c(1,2),omi=c(0,0,0,0))
plot(clust_prop$objectives, type="o",pch=19, cex=0.35,bty="n", main="Objective function",col="tomato1")
points(8,objectives[8],lwd=5, col="tomato4")
plot(clust_prop$asw, type="o", pch=19, cex=0.35, bty="n", main="Silhouette Width",col="turquoise")
points(8,asw[8],lwd=5,col="turquoise4")
#dev.off()

plot_clusters <- function(sol){
  
  meds <- dat_setup[clust.dat.prop[[sol]]$i.med,"row"]
  to_plot <- prop_table[meds,2:ncol(prop_table)]
  to_plot$cluster <- 1:nrow(to_plot)
  
  one_df <- melt(to_plot,id="cluster")
  one_df <- one_df[one_df$value>0,]
  colnames(one_df) <- c("cluster","species","catch_proportion")
  
  ggplot(one_df, aes(x=species,y=catch_proportion,fill=species)) + geom_bar(stat="identity") + facet_wrap(~ cluster,scales="free_x") + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + guides(fill=FALSE)
}

plot_clusters(6)
barplot(table(clust.dat.prop[[12]]$clustering))

plot_clusters(14)
barplot(table(clust.dat.prop[[8]]$clustering))

# choose 8 clusters because that's the point at which it looks like we're not improving the fit much more. 

# how does number of trips vary over the year?
nodes = 8
clust_prop <- list(data_transform="prop",cluster_sol = nodes, objectives = objectives, asw = asw, clustering = clust.dat.prop[[nodes]]$clustering, sampsize=sampsize, samples=samples, npc=npc)

# map clustering to data
ind <- cbind(clust_prop$clustering,dat_setup[,"row"])

prop_table$c8[ind[,2]] <- ind[,1]

mc <- select(prop_table,ftid,c8)
tickets <- merge(tickets, mc, by="ftid")

# look at number of trips by date
date = as.POSIXct(tickets$tdate, format="%d-%b-%y",tz="US/Pacific")
tickets$date = date

by_date <- ddply(tickets, .(date,c8), summarize, number_trips = length(unique(ftid)))

ggplot(by_date, aes(x=date,y=number_trips, group=c8, col=factor(c8, labels=c("Shellfish","Shrimp","Crab","Other","Salmon","Groundfish","Tuna","Coastal Pelagics")))) + geom_line() + facet_wrap(~c8) + labs(fill="Catch Profile") + scale_colour_discrete(name = "Catch profile")

# look at proportion of vessels that participated in more than one fishery

num_cl <- ddply(tickets, .(veid),summarize, num_clust = length(unique(c8)))
barplot(table(num_cl$num_clust))
# most vessels participated in only one fishery, but a number of them participated in more than 1. Luckly the 'vessel' that participated in 7 is actually "UNKNOWN". Should make network plots for each of these: i.e. the fisheries vessels participate if they participate in only 1, 2, 3, etc. 

# for vessels participating in 1 cluster
plot_network <- function(num_clust_part){
c1 <- subset(num_cl, num_clust==num_clust_part)
c1_trips <- subset(tickets, veid %in% c1$veid,select=c(ftid,veid,c8))
c1_trips <- c1_trips[!duplicated(c1_trips),]

network <- matrix(nrow=nodes,ncol=nodes)

for(i in 1:length(unique(c1_trips$c8))){
  # for catch profile i
  set1 <- unique(c1_trips$veid[c1_trips$c8==i])
  # where are the unique vessel ids
  for(j in 1:length(unique(c1_trips$c8))){
    # for catch profile j
    set2 <- unique(c1_trips$veid[c1_trips$c8==j])
    # what are the unique vessel ids
    network[i,j] = length(Reduce(intersect, list(set1, set2)))
    # element is the number of vessel IDs in common
  }
}


sizes <- diag(network)
diag(network) <- rep(0,nodes)
g <- graph.adjacency(network, weighted=TRUE, mode="undirected")
# plot(g, vertex.size = log(diag(network)), layout=layout.fruchterman.reingold, width=g)

paint <- colorRampPalette(brewer.pal(8,"Accent"))(nodes)

qgraph(network, vsize=log(sizes), layout="spring", posCol="black", curveAll = FALSE, esize=30,colFactor=.25,bg="white",color=paint,borders=FALSE, title=paste(nrow(c1), "vessels",sep=" "))

}

legend("topleft",legend=unique(mgmt_key$mgmt_grp),fill=unique(paint[mgmt_key$mgmt_grp]),bty="n",bor=FALSE,ncol=4)

# I bet that gear largely explains this. In that trawlers are not switching from groundfish to albacore/salmon. but fixed gear people are. 

plot_network(1)
plot_network(2)
plot_network(3)
plot_network(4)
plot_network(5)
plot_network(6)
