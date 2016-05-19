filter_nominal <- function(data){
  library(dplyr)
  # details: drop nominal distinction in market categories (makes it so that each row has a single species, pretending that all catches are clean)
  # legacy, no longer active part of the code, to drop dredge gear
  
# merge nominal and species market categories ----
  # load species ids
  spid <- read.csv("processedData/catch/spid.csv", 
                   stringsAsFactors=F)
  
  # some problems - make common name identical across species
    spid$common_name[which(spid$SPID=="CHL1")] <- "NOM. CALIFORNIA HALIBUT"
    # RCK1 will only have nominal, because chilipeper + boccacio. Only occurs once in dataset
    spid$common_name[which(spid$SPID=="SQR1")] <- "NOM. SQUARESPOT ROCKFISH"
    # mis-spelling of vermillion
    spid$common_name[which(spid$SPID=="VRM1")] <- "NOM. VERMILION ROCKFISH"
  
  # drop to species level records only  
  spid <- spid[which(spid$X==1),]
  
  # reassign nominal name as common name
  species <- spid$common_name
  species <- gsub("NOM. ", "", species)
  spid$common <- species
  
  # build species key to map nominal species names to a single common name used throughout the database
  nominal <- spid[grep("NOM.", spid$common_name),]
  nominal <- select(nominal, SPID, common)
  colnames(nominal) <- c("nominal", "common")
  reg <- spid[-grep("NOM.",spid$common_name),]
  reg <- dplyr::select(reg, SPID, common)
  colnames(reg) <- c("reg", "common")
  
  species.key <- merge(reg, nominal, by = "common", all.x = TRUE, all.y = TRUE)
  
  # make loop through species key for those with nominal spids
  inds <- which(!is.na(species.key$nominal))

  data$modified <- NA
  for(i in inds){
    data$modified[which(data$spid==species.key$nominal[i])] <- species.key$reg[i]
    #cat(i," ")
  }
  
  # for spid without nominal counterparts we can import spid directly to modified column
  data$modified <- ifelse(is.na(data$modified), data$spid, data$modified)
  
  # make new ftl dataset that drop dredges. dredges catch geoducks, clams, lingcod, sea cukes
  # filtered_ftl <- subset(data, grgroup != "DRG")

# remove any trips which have more than one type of gear on it per trip ----
  # this could be an issue except it drops <0.01% of trips annually
  dub_gears <- unique(data[,c("trip_id","grgroup")])
  rm_ves <- unique(dub_gears$trip_id[which(duplicated(dub_gears$trip_id))])
  no_dups <- subset(data, !(trip_id %in% rm_ves))
#----
return(no_dups)
}