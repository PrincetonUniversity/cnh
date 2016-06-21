# author: Emma
# date: 2014-03-22
# goal: to find catch profiles of fish ticket data

require(cluster)
require(ggplot2)
require(dplyr)
require(reshape2)

#----------------------------------------------------------------
# Load Data

# Load data
  FTL <- read.csv("Data/Catch/FTL_2009-2013_2014-03-21.csv")
  names(FTL) <- tolower(names(FTL))

# need to make up a unique tripID
  dates <- paste(FTL$year,FTL$month, FTL$day,sep="-")
  FTL$tripID <- paste(dates,FTL$veid,sep="_")
  # NOTE: veid the best option? dates are day that the catch is delievered. Logbooks may help with this. 

# Format data (columns species, rows TRIPID)
  FTL_df <- select(FTL,tripID,spid,landed_wt)
  
  # calculate total catch by trip (ftid)
  totals <- FTL_df %.%
    group_by(tripID) %.%
    summarise(total = sum(landed_wt)) %.%
    arrange(tripID)
  
  # calculate by species total catch for each trip
  catch <- FTL_df %.%
    group_by(tripID,spid) %.%
    summarise(catch = sum(landed_wt)) %.%
    filter(catch > 0) %.%
    arrange(tripID)

    # any NAs?
    any(is.na(catch))   #FALSE, we're good

  # for each entry for species caught, need to find total proportion of catch (divide by catchRET$total)
    total_catch <- dcast(catch, tripID ~ spid, fill = 0, drop=TRUE)
  
    # combine totals and species composition 
      cluster <- merge(total_catch, totals, by="tripID")
      cluster_df <- tbl_df(cluster)

  # construct table
    start_ind = 2
    end_ind = ncol(cluster_df)-1
    freq <- apply(cluster_df[,start_ind:end_ind], 2, function(x) length(which(x > 0))) 
    # returns indexes for species which are found fewer than 20 times
  
  # remove those species that are in fewer than 20 trips
    cluster_int <- cluster_df[,-which(names(cluster_df) %in% names(freq)[which(freq<20)])]
    # removing 24 species
  
  # remove rows that have no catch from any target species
    cluster_sub <- cluster_int[-which(rowSums(cluster_int[2:(ncol(cluster_int)-1)])==0),]
    dim(cluster_df)-dim(cluster_sub)
    # lost 18 trips and 24 species

#----------------------------------------------------------------
# Apply PCA: optional, not convinced this helps clustering substantially. 
pca.dat <- cluster_sub[2:(ncol(cluster_sub)-1)]
# Log quantities as PCA indata
pca.dat_log <- pca.dat+0.1
pca.dat_log <- log(pca.dat_log) 
pca.log <- prcomp(pca.dat_log, scale = TRUE) 
pca.scoresdat_log <- pca.log$x

# first 56 components give you > 80%
data.clust <- pca.scoresdat_log[,1:56]

#----------------------------------------------------------------
# Clustering
## Using CLARA for clustering, it's a partioning around mediods approach. But takes random samples of the data. 
  
  # preallocate a list
    #data.clust <- cluster_sub[2:(ncol(cluster_sub)-1)]
    max.clusters = 55
    clusts.dat_log <- vector("list", max.clusters) 
  
for(i in 1:max.clusters){
      clusts.dat_log[[i]] <- clara(data.clust, i, metric = "euclidean", stand = TRUE, samples = 100,keep.data = FALSE,medoids.x = FALSE)
      print(i)
    }

    # Objective function and ASW
      objectives <- vector(length = max.clusters)
      for (i in 1:max.clusters) { objectives[i] <- clusts.dat_log[[i]]$objective }
      
      asw <- vector(length = max.clusters)
      for (i in 2:max.clusters) { asw[i] <- clusts.dat_log[[i]]$silinfo$avg.width  }
      
    # Finding a jump in the objective function
      plot(1:(max.clusters-1), objectives[2:max.clusters]-objectives[1:(max.clusters-1)], type = "o", pch = 19, ylab = "jump in Objective function", xlab = "Number of clusters", main = "Reduction in Unexplained variation by adding one cluster"); 
      abline(h = 0, lty = 2)
      abline(v = seq(2,max.clusters,2), col = "lightgrey", lty = 2)
    
    # Objective function
    plot(1:max.clusters, objectives, type = "o", xlab = "Number of clusters", ylab = "Objective function")
    abline(v = seq(2,max.clusters,2), col = "lightgrey", lty = 2)

    #ASW
      lot(2:max.clusters, asw, type = "o", xlab = "Number of clusters", ylab = "Average silhoutte width")
      abline(v = seq(2,20,2), col = "lightgrey", lty = 2)



## Trying hierarchical aglomerative clustering
data.clust <- cluster_sub[2:(ncol(cluster_sub)-1)]
d <- dist(data.clust, method="euclidean")
