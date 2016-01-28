# subsetting, doing the PCA and clustering groundfish 

# load management clustering
load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/cluster_sol_objectives_asw.Rdata")
load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/propTable_tickets.Rdata")

# combine prop_table with clustering solution
prop_table$cluster <- cluster_ind[,"cluster"]

# subset to groundfish
gf_prop <- subset(prop_table, cluster == 3)

# subset tickets to only fish tickets here
gf_tickets <- subset(tickets, ftid %in% gf_prop$ftid)

# build up proportion table based on species ID. 

by_trip <- data.table(gf_tickets)
setkey(by_trip, ftid)
catch <- by_trip[, sum(landed_wt), by=c("ftid","spid")]

total_catch <- dcast.data.table(catch, ftid ~ spid, fun=sum)

# find proportion table
prop_table <- as.data.frame(total_catch)

# replace lbs with proportion of trip 
prop_table[,2:ncol(prop_table)] <- prop_table[,2:ncol(prop_table)]/rowSums(prop_table[,2:ncol(prop_table)])

pca_prop <- prcomp(prop_table[,2:ncol(prop_table)],scale=T)
npc <- length(which(summary(pca_prop)[[6]][3,]< .79))

# check to makes sure npc is greater than .8
while(summary(pca_prop)[[6]][3,][npc]<.8) npc = npc + 1

# retain principal components which retain > 80% of variation
dat_setup <- pca_prop$x[,1:npc]

# prepare for cluster analysis

# randomize rows to prevent any bias from order of fish tickets
# set up row index, so can translate back to original order
row <- seq(1:nrow(dat_setup))
dat_setup <- cbind(dat_setup,row)
dat_setup <- dat_setup[sample(nrow(dat_setup)),]
dat_prop <- dat_setup[,1:(ncol(dat_setup)-1)]

# set cluster info/data structures
require(cluster)
max.clusts = 12
samples = 50
sampsize = 500
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


species_total <- colSums(prop_table[,2:ncol(prop_table)])
species_total <- sort(species_total, decreasing=T)
barplot(species_total)
