# make definition lists
library(data.table)

gear <- "MSC"
year <- 2010

# load .clu file
clusters <- fread(paste0("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-20/define/",gear,year,".clu"))
key <- read.table(paste0("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-20/define/",gear,year,"key.txt"),sep=",",skip=1)
key <- cbind(key,clusters)
names(key) <- c("ftid","node","cluster")

write.csv(key, paste0("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-20/define/",gear,year,"cluster_key.txt"),row.names=FALSE, quote = FALSE)
