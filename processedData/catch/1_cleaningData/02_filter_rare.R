filter_rare <- function(data){
  library(dplyr)
  # details: drop nominal distinction in market categories, drop dredge gear
  
# merge nominal and species market categories ----
  # load species ids
  spid <- read.csv("/Users/efuller/1/CNH/processedData/catch/spid.csv", 
                   stringsAsFactors=F)
  
  # some problems - make common name identical across species
    spid$common_name[which(spid$SPID=="CHL1")] <- "NOM. CALIFORNIA HALIBUT"
    # RCK1 will only have nominal, because chilipeper + boccacio. Only occurs once in dataset
    spid$common_name[which(spid$SPID=="SQR1")] <- "NOM. SQUARESPOT ROCKFISH"
    # mis-spelling of vermillion
    spid$common_name[which(spid$SPID=="VRM1")] <- "NOM. VERMILION ROCKFISH"
  
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

  data$modified <- NA
  for(i in inds){
    data$modified[which(data$spid==species.key$nominal[i])] <- species.key$reg[i]
    #cat(i," ")
  }
  
  # for rest then can import spid to modified column
  data$modified <- ifelse(is.na(data$modified), data$spid, data$modified)
  
  # make new ftl dataset that drop dredges
  filtered_ftl <- subset(data, grgroup != "DRG")

# remove any trips which have more than one type of gear on it per trip ----
  dub_gears <- unique(filtered_ftl[,c("trip_id","grgroup")])
  rm_ves <- unique(dub_gears$trip_id[which(duplicated(dub_gears$trip_id))])
  no_dups <- subset(filtered_ftl, !(trip_id %in% rm_ves))
#  saveRDS(no_dups, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/filtered_ftl.RDS")
#----
return(no_dups)
}