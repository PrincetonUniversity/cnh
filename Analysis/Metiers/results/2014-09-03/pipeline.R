# plan: seperate by vessel, use pvclust to find approximately unbiased p-values. Then for clusters that are deemed significant, determine indicator species. Compare to lists of species that are commonly assumed to be targeted by vessels as proximate measure of validity 

#----pvclust----
library(pvclust)
result <- pvclust(t(v1.hel), method.hclust="ward", method.dist = "euclidean", nboot=100)
plot(result, cex=.15, cex.pv = 0.15)
pvrect(result, alpha=.95, max.only=T)

result2 <- pvclust(t(v2.hel), method.hclust="ward", method.dist="euclidean", nboot=1000)
plot(result2, cex=.15, cex.pv=0.15)
pvrect(result2, alpha = .95, max.only=T)

# finding that it lumps a large number of species together because often there's a strong break between crap and groundfish/rockfish/salmon. This is because the compositions are not single species, so harder to break apart consistently. 

# wondering if dropping either unspecified species from catches or classifying by management group again would help. This would make rockfish one category, salmon another, etc. This would make the composition of catches more uniform and deal with the issue that not all rockfish are identified to species level. However it means that I can't really do indicator species. Although I guess I can, it's just that the species is a management group. Which I suppose is still useful (i.e. it will be crap, or whiting, or rockfish). 

#----aggregate species to management group, then try clustering ----
# problem is that what has groups and want doesn't isn't uniform. DCRB doesn't have a complex, but is managed as "crab". Where tuna has a complex. rockfish have a complex, but are aggregated to groundfish in the management group. halibut don't have a complex, and their management is "other". 

# will use species complex to aggregate. if species don't have a complex, will use species ID. This is probably because they are caught seperately. So the prawns and shrimp don't have a complex, but all are managed as shrimp. but salmon, for example, are not assumed to be in a complex, but are managed collectively as 'salmon'. And you see them often caught together. and pacific whiting are part of a "round fish" complex. But they are often caught by themselves. 

# feel like many of the round fish are likely to be fairly distinct, and vessels will be able to target them seperately. sablefish are also a roundfish but seperate in pots. 

# will try 

spid <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/data/spid.csv", stringsAsFactors=F)
v2 <- subset(ftl, veid=="578930")
v2_vars <- select(v2, ftid, spid, landed_wt)

v2_complex <- rep(NA, nrow(v2_melt))
for(i in 1:nrow(v2_vars)){
  complex_id <- spid$complex[which(spid$SPID==v2_vars$spid[i])]
  v2_complex[i] <- ifelse(is.na(complex_id), v2_vars$spid[i], complex_id)
}

v2_vars$complex <- v2_complex
v2_vars <- select(v2_vars, - spid)

v2_melt <- melt(v2_vars, id=c("ftid", "complex"), measure = "landed_wt")
v2_cast <- dcast(v2_melt, ftid ~ complex, fun.aggregate=sum)
v2.hel <- decostand(v2_cast[,2:ncol(v2_cast)], "hel")
v2.dend_ward <- hclust(dist(v2.hel), method="ward.D")
plot(v2.dend_ward, cex=.15)

result3 <- pvclust(t(v2.hel), method.hclust="complete", method.dist="euclidean", nboot=100)
plot(result3, cex=.15, cex.pv = .15)
pvrect(result3, alpha=.9, max.only=T)

# the complete method seems to be working better, but there are still many splits of small and big catches. I think volume probably shoudn't matter. pacific hake should still come out seperately because the proportion of non roundfish should be vanishingly small. 

v2.chord <- decostand(v2_cast[, 2:ncol(v2_cast)], method = "normalize")
result4 <- pvclust(t(v2.chord), method.hclust="complete", method.dist="euclidean", nboot=10000)
plot(result4, cex=.15, cex.pv=.15)
pvrect(result4, alpha=.95)

# this is better, captures crab, salmon, whiting, and rockfish/flatfish. I bet that the centroids for salmon, whiting, crab are going to be easy to match. But the rockfish/flatfish centroids are going to be harder to match. 

# UPDATE: increasing from nboot from 100 to 1000 allows seperation of the flatfish and rockfish groups significantly. I should up it to 10,000 just to make sure. meh. 

# there still seems to be some big splits that are missed that are informative. For example flatfish make up > 50% of the catch, or rockfish > 50% that are still lumped together. Although it does a good job pulling out what I'm guessing to be sablefish (80% roundfish, 20% rockfish). What I need to do is figure out an automated method.

# may be that I'll just have to cut the dendogram the way I'd want and calculate statistics to see if any match. 

# I also need to decide what happens if there are only a few trips for a given strategy. i.e. 2. and what an "outlier" for pvclust means. 

# for all of this, I still think that calculating the mean proportion in entire dataset for species ID versus within cluster is a helpful way to look at whether the clusters are getting meaningfully different groups. I suppose you chould do that for each point of the diagram... And make a plot. Not sure though, how this would look. 

#-----Practice script to generate significant clusters and save centroids-----

# practice script to generate hierarchical clusters, find significant clusters, calculate indSpecies and cluster resulting centroids

 library(reshape2); library(vegan); library(pvclust); library(labdsv);library(dplyr);

# load data
# loading all data to see where these trips are coming from
ftl <- read.csv("/Users/efuller/1/CNH/Data/Catch/FTL_2009-2013_w-o-c_samhouri.csv", stringsAsFactors=F)
spid <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/data/spid.csv", stringsAsFactors=F)

# will try for thee individuals
clusterTrips <- function(v_veid, len_boot){
  cat("subsetting fish tickets, transforming to chord...")
  v1 <- subset(ftl, veid==v_veid)
  v1_vars <- dplyr::select(v1, ftid, spid, landed_wt)
  
  v1_complex <- rep(NA, nrow(v1_vars))
  for(i in 1:nrow(v1_vars)){
    complex_id <- spid$complex[which(spid$SPID==v1_vars$spid[i])]
    v1_complex[i] <- ifelse(is.na(complex_id), v1_vars$spid[i], complex_id)
  }
  
  v1_vars$complex <- v1_complex
  v1_vars <- dplyr::select(v1_vars, - spid)
  
  v1_melt <- melt(v1_vars, id=c("ftid", "complex"), measure="landed_wt")
  v1_cast <- dcast(v1_melt, ftid ~ complex, fun.aggregate = sum)
  v1.chord <- decostand(v1_cast[,2:ncol(v1_cast)], method = "normalize")
  rownames(v1.chord) <- v1_cast$ftid
  cat("calculating pvclust, number of bootstraps = ", len_boot,"...")
  
  v1.pvclust <- pvclust(t(v1.chord), method.hclust = "complete", method.dist = "euclidean", nboot = len_boot)
  
  # find significant groups
  clust_list <- pvpick(v1.pvclust, alpha = .95)$clusters
  
  grp <- rep(NA, nrow(v1_cast))
  
  for(i in 1:length(clust_list)){
    grp[which(v1_cast$ftid %in% clust_list[[i]])] <- i
  }
  
  # remove any NAs
  ref_trips <- v1_cast[-which(is.na(grp)),]
  grp <- grp[-which(is.na(grp))]
  ref_trips$grp <- grp
  
  # note: this doesn't work for all because not all species are in all groups. 
  
  # calculate indicator species
#   iva <- indval(spe, grp)
#   
#   gr <- iva$maxcls[iva$pval <= 0.05]
#   iv <- iva$indcls[iva$pval <= 0.05]
#   pv <- iva$pval[iva$pval <= 0.05]
#   fr <- apply(spe > 0, 2, sum)[iva$pval <= 0.05]
#   fidg <- data.frame(group=gr, indval=iv, pvalue=pv, freq=fr)
#   fidg <- fidg[order(fidg$group, -fidg$indval),]
  
  # calculate centroid with sd 
  
  num_clust <- sort(unique(grp))
  centroid <- matrix(ncol=(ncol(ref_trips)-2), nrow=max(num_clust))
  colnames(centroid) <- tail(head(colnames(ref_trips),-1),-1)
  
  for(i in num_clust){
    tab <- subset(ref_trips, grp==i, c(-grp,-ftid))
    centroid[i,] <- t(as.matrix(colSums(tab)/nrow(tab)))
  }
  rownames(centroid) <- paste("v",v_veid,"_c",num_clust,sep="")
  
for(i in 1:nrow(centroid)){
  barplot(centroid[i,], bor=F, las=2)
}

  v1_info <- list(centroid = centroid,  pvclust = v1.pvclust, ref_trips = ref_trips)
  
  return(v1_info)
}

v1_veid <- "8956"
v2_veid <- "578930"
v3_veid <- "1800000"

v1_trips <- clusterTrips(v1_veid, 100)
v2_trips <- clusterTrips(v2_veid, 100)
v3_trips <- clusterTrips(v3_veid, 100)

#-----cluster resulting centroids
# re-melt centroids 

v1_cent <- melt(v1_trips$centroid)
v2_cent <- melt(v2_trips$centroid)
v3_cent <- melt(v2_trips$centroid)

centroid_all <- rbind(v1_cent, v2_cent, v3_cent)
centroid_table <- dcast(centroid_all, Var1 ~ Var2, fun.aggregate = sum)
centroid_chord <- decostand(centroid_table[,2:ncol(centroid_table)], method="normalize")
rownames(centroid_chord) <- centroid_table$Var1

centroid_tree <- hclust(dist(centroid_chord), method = "complete")
plot(centroid_tree)
pv_tree <- pvclust(t(centroid_chord), method.hclust="complete",method.dist="euclidean",nboot=10000)
plot(pv_tree)
pvrect(pv_tree)
par(mfrow=c(2,3))

#----random permutations
# idea is that if I randomly shuffle the rows and columns of a trip table, what's the proportion of species-types in each significant pvclust relative the rest of the dataset?

reference_trips <- v1_cast[,2:ncol(v1_cast)]
rand_trips <- matrix(runif(ncol(reference_trips)*nrow(reference_trips), min=0, max = max(reference_trips)), ncol=ncol(reference_trips))
colnames(rand_trips) <- colnames(reference_trips)


rand_chord <- decostand(rand_trips, method="normalize")
plot(hclust(dist(rand_chord), method="complete"))

pv_rand <- pvclust(t(rand_chord), method.hclust="complete", method.dist="euclidean",nboot=1000)

plot(pv_rand, cex=.3, cex.pv = .3)
pvrect(pv_rand)

# calculate species proportions 

# calculate silhouette width for groups

# calculate connectance

# calculate isolation

#----doubley ordered community table

or <- vegemite(round(round(log(v1_cast[,2:ncol(v1_cast)]+1))/10), v1.pvclust$hclust)
# need to figure out a better way to scale this

spe <- v1_cast[,2:ncol(v1_cast)]
heatmap(t(spe[rev(or$species)]), Rowv=NA, Colv=as.dendrogram(v1.pvclust$hclust), col=c("white",colorRampPalette(brewer.pal(9,"Greens"))(100)), scale="none", margin=c(4,4), ylab="Species (weighted average of trips)", xlab="trips")
