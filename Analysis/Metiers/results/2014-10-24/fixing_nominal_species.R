# load PacFin species codes
filtered_ftl <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-10-08/filtered_ftl.RDS")
library(dplyr)

# load species ids
spid <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/data/spid.csv", stringsAsFactors=F)

spid <- spid[which(spid$X==1),]
species <- spid$common_name
species <- gsub("NOM. ", "", species)
spid$common <- species

# build species key
nominal <- spid[grep("NOM.", spid$common_name),]
nominal <- select(nominal, SPID, common)
colnames(nominal) <- c("nominal", "common")
reg <- spid[-grep("NOM.",spid$common_name),]
reg <- select(reg, SPID, common)
colnames(reg) <- c("reg", "common")

species.key <- merge(reg, nominal, by = "common", all.x = TRUE, all.y = TRUE)

# make loop through species key for those with nominal spids
inds <- which(!is.na(species.key$nominal))
new_df <- filtered_ftl
new_df$modified <- NA
for(i in inds){
  new_df$modified[which(new_df$spid==species.key$nominal[i])] <- species.key$reg[i]
  cat(i)
}

new_df$modified <- ifelse(is.na(new_df$modified), new_df$spid, new_df$modified)
saveRDS(new_df, file="/Users/efuller/1/CNH/Analysis/Metiers/results/2014-10-24/filtered_ftl_new.RDS")
