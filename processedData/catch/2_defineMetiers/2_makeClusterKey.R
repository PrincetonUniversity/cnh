# ind(key,clusters) definition lists
library(data.table, lib.loc="/tigress/efuller/R_packages")

args <- commandArgs(trailingOnly = TRUE)


gear <- args[1]
year <- args[2]

# load .clu file
clusters <- read.table(paste0("/tigress/efuller/raw_infoMap/",gear,year,".clu"), skip = 1)
key <- read.table(paste0("/tigress/efuller/raw_infoMap/",gear,year,"key.txt"),sep=",",skip=1)
key <- cbind(key,clusters)
names(key) <- c("trip_id","node","cluster")

write.csv(key, paste0("/tigress/efuller/raw_infoMap/",gear,year,"cluster_key.txt"),row.names=FALSE, quote = FALSE)

