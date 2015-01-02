# cluster PCA results into fisheries
# input: dat_prop, dat_setup. If not present, run mgmt_grps.R to generate
# output: 

# load data
rm(list=ls())
load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/datSetup_datProp.Rdata")

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
  #cat(i,"...")
}

objectives <- vector(length = max.clusts)
for (i in 1:max.clusts) { objectives[i] <- clust.dat.prop[[i]]$objective }

asw <- vector(length = max.clusts)
for (i in 2:max.clusts) { asw[i] <- clust.dat.prop[[i]]$silinfo$avg.width  }

# save solution with 8 clusters
cluster_sol <- clust.dat.prop[[8]]

# re-arranging the vector to undo the randomization of rows
cluster_ind <- dat_setup
cluster_ind <- cbind(cluster_ind, cluster=cluster_sol$clustering)
cluster_ind <- cluster_ind[,c("row","cluster")]
cluster_ind <- cluster_ind[order(cluster_ind[,"row"]),]
save(cluster_sol, cluster_ind, objectives, asw, file="/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/cluster_sol_objectives_asw.Rdata")

