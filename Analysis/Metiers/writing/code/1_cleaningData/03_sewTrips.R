# comparing classifications of major species versus clusters. 
sewTrips <- function(){
# load data ----
filtered_ftl <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/filtered_ftl.RDS")

files <- list.files("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/2_defineMetiers/") # load 2010 predicted metiers
# files that were found via clustering and classifying are in slightly different formats

# clustered trips 
  # get file names
  pred_files <- files[grep("2010cluster_key.txt",files)]
  # load files
  predicteds <- do.call(rbind, lapply(paste0("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/2_defineMetiers/",pred_files), read.csv))
  # add 2010 to trip_id to make it match rest
  predicteds$ftid <- paste0(predicteds$ftid, 2010)
  # get rid of infoMap column
  predicteds$node <- NULL
  colnames(predicteds) <- c("trip_id", "metier")
# classified trips
  class_files <- files[grep("2010p",files)]
  classifieds <- do.call(rbind, lapply(paste0("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/2_defineMetiers/", class_files), readRDS))
  colnames(classifieds) <- c("trip_id", "metier")

  metiers <- rbind(classifieds, predicteds)

# merge metiers ----
  tickets <- merge(filtered_ftl, metiers, by = "trip_id")
  length(unique(tickets$trip_id)) == length(unique(metiers$trip_id))

  tickets$metier <- paste(tickets$grgroup, tickets$metier, sep="_")

  saveRDS(tickets, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/tickets.RDS")
#----
return(tickets)
}