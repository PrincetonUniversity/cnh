# author: Emma Fuller
# date: 2014-04-20
# modified: 2014-04-21
# goal: make datasets from fish-ticket to do catch clustering and effort clustering
# output: pca_data.Rdata in CNH/Analysis/Metiers/results/2014-04-20/

# major choices: 
#   + threshold to determine "rare" species. Right now exclude species present in fewer than 200 trips. 
#   + that I remove 'unspecified' and 'other' species IDs

setwd("/Volumes/NOAA_Data/CNH/")

require(cluster)
require(ggplot2)
require(dplyr)
require(reshape2)
require(plyr)
require(scales)
require(qgraph)
require(data.table)

## function works? yes!
source("/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-04-20/FTL_tripTable.R")

FTL <- data.table(read.csv("Data/Catch/FTL_2009-2013_2014-03-21.csv",as.is=TRUE))
price_tripTable <- FTL_tripTable(FTL,type="price",times=200)
lb_tripTable <- FTL_tripTable(FTL,type="lbs",times=200)
log_tripTable <- FTL_tripTable(FTL, type="log",times=200)
proportion_tripTable <- FTL_tripTable(FTL, type="proportion",times=200)

save(price_tripTable, file="Analysis/Metiers/results/2014-04-20/price_tripTable.Rdata")
save(lb_tripTable, file="Analysis/Metiers/results/2014-04-20/lb_tripTable.Rdata")
save(log_tripTable, file="Analysis/Metiers/results/2014-04-20/log_tripTable.Rdata")
save(proportion_tripTable, file="Analysis/Metiers/results/2014-04-20/proportion_tripTable.Rdata") 



# Below is rough draft of the function FTL_tripTable source-d above
#----------------------------------------------------------------
# Load Data

# Load data
  FTL <- data.table(read.csv("Data/Catch/FTL_2009-2013_2014-03-21.csv",as.is=TRUE))
  setnames(FTL,names(FTL),tolower(names(FTL)))

# need to make up a unique tripID
  dates <- paste(FTL$year,FTL$month, FTL$day,sep="-")
  FTL$tripID <- paste(dates,FTL$veid,sep="_")
# NOTE: veid the best option? dates are day that the catch is delievered. Logbooks may help with this. 

#-----------------------------

# Format data (columns species, rows TRIPID)
  FTL_df <- select(FTL,tripID,spid,landed_wt)
# calculate total catch by trip (tripID)
  totals <- FTL_df[, sum(landed_wt), by=tripID]

# calculate by species total catch for each trip
  catch <- FTL_df[, sum(landed_wt), by=c("tripID", "spid")]

# transform: rows are trip, columns are species that were caught 
  total_catch <- dcast.data.table(catch, tripID ~ spid, fun=sum)

# keep connection with tripID by making tripID rowname
  setkey(total_catch, tripID)

# find "unspecified species", things that start with a "U" [see http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/sp.txt]
  unspecified <- unique(FTL$spid)[grep("^U",unique(FTL$spid))]

# find "other species," things that start with "O" (mostly)
  other <- unique(FTL$spid)[grep("^O", unique(FTL$spid))]
  # but not OLV1, OLVE, OTCR, OWFS (these are actual species codes that begin with "O")
    other <- other[-which(other=="OLVE" | other=="OLV1" | other=="OTCR" | other=="OWFS")]
# merge 
     to_remove <- c(unspecified, other)

# remove any column that matches the names in to_remove
      cluster_pre <- total_catch[,!(names(total_catch) %in% to_remove), with=FALSE]

# find now empty rows
  cluster_new <- subset(cluster_pre, rowSums(cluster_pre[,!"tripID",with=FALSE]) != 0)

  dim(total_catch) - dim(cluster_new)

# construct table
  freq <- apply(cluster_new[,!"tripID", with=FALSE], 2,function(x) length(which(x > 0))) #same for prop
  # returns number of trips each species found in 

# remove those species that are in fewer than 200 trips
  cluster_int <- cluster_new[,!(names(cluster_new) %in% names(freq)[which(freq<200)]),with=FALSE]

# remove rows that have no catch from any target species
  cluster_sub <- subset(cluster_int, rowSums(cluster_int[,!"tripID",with = FALSE]) !=0)

dim(total_catch)-dim(cluster_sub)
# lost 2073 trips and 62 species

pca_data <- cluster_sub

# make a proportion dataset, along with an unchanged one
  prop_cluster <- sweep(pca_data[,!"tripID", with=FALSE], 1, rowSums(pca_data[,!"tripID", with=FALSE]), "/")
  prop_cluster <- as.data.table(prop_cluster)
  prop_cluster$tripID <- pca_data$tripID
  setkey(prop_cluster, tripID)

save(pca_data, file="Analysis/Metiers/results/2014-04-20/pca_data.Rdata")

prop.pca_data <- prop_cluster
save(prop.pca_data, file="Analysis/Metiers/results/2014-04-20/prop_pca_data.Rdata")

#-----------------
# make pca_data but instead of weight, do price
# take pca_data value, multiply it by that price

# Format data (columns species, rows TRIPID)
FTL_df <- select(FTL,tripID,spid,landed_wt,ppp)
FTL_df$value = FTL_df$ppp * FTL_df$landed_wt

# calculate by species total catch for each trip
catch <- select(FTL_df, tripID, spid, value)
catch <- filter(catch, value > 0)
catch <- as.data.table(catch)

# transform: rows are trip, columns are species that were caught 
  total_catch <- dcast.data.table(catch, tripID ~ spid,fun=sum)
  setkey(total_catch, tripID)

# find "unspecified species", things that start with a "U" [see http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/sp.txt]
  unspecified <- unique(FTL$spid)[grep("^U",unique(FTL$spid))]
# find "other species," things that start with "O" (mostly)
  other <- unique(FTL$spid)[grep("^O", unique(FTL$spid))]
# but not OLV1, OLVE, OTCR, OWFS (these are actual species codes that begin with "O")
  other <- other[-which(other=="OLVE" | other=="OLV1" | other=="OTCR" | other=="OWFS")]
# merge 
  to_remove <- c(unspecified, other)
# remove any column that matches the names in to_remove
  cluster_pre <- total_catch[,!(names(total_catch) %in% to_remove), with=FALSE]
  
# find now empty rows
  cluster_new <- subset(cluster_pre, rowSums(cluster_pre[,!"tripID", with=FALSE]) != 0)
  dim(total_catch) - dim(cluster_new)

# construct table
  freq <- apply(cluster_new[,!"tripID", with=FALSE], 2,function(x) length(which(x > 0)))

  # returns number of trips each species found in 

# remove those species that are in fewer than 200 trips
  cluster_int <- cluster_new[,!(names(cluster_new) %in% names(freq)[which(freq<200)]),with=FALSE]

# remove rows that have no catch from any target species
cluster_sub <- subset(cluster_int, rowSums(cluster_int[,!"tripID", with=FALSE]) !=0)
dim(total_catch)-dim(cluster_sub)
# lost 2080 trips and 58 species

price.pca_data <- cluster_sub
save(price.pca_data, file="Analysis/Metiers/results/2014-04-20/price_pca_data.Rdata")

# save FTL with useful data for later clustering

FTL.ref <- select(FTL,tripID,veid, grid, month)
FTL.ref <- FTL.ref[!duplicated(FTL.ref)]
save(FTL.ref, file="Analysis/Metiers/results/2014-04-20/FTL_ref.Rdata")
