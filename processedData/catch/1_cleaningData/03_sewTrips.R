# comparing classifications of major species versus clusters. 
sewTrips <- function(base_year){
# load data ----
filtered_ftl <- readRDS("/Users/efuller/1/CNH/processedData/catch/1_cleaningData/filtered_ftl.RDS")

files <- list.files("/Users/efuller/1/CNH/processedData/catch/2_defineMetiers/") # load 2010 predicted metiers
# files that were found via clustering and classifying are in slightly different formats

# clustered trips 
  # get file names
if(base_year == 2010){
  pred_files <- files[grep("2010cluster_key.txt",files)]
}else{
  pred_files <- files[grep("2012cluster_key.txt",files)]
  }
  # load files
  predicteds <- do.call(rbind, lapply(paste0("/Users/efuller/1/CNH/processedData/catch/2_defineMetiers/",pred_files), read.csv,stringsAsFactors = FALSE))
  # get rid of infoMap column
  predicteds$node <- NULL
  colnames(predicteds) <- c("trip_id", "metier")
# classified trips
  class_files <- files[grep(paste0(base_year,"p"),files)]
  classifieds <- do.call(rbind, lapply(paste0("/Users/efuller/1/CNH/processedData/catch/2_defineMetiers/", class_files), readRDS))
  colnames(classifieds) <- c("trip_id", "metier")
  classifieds$trip_id <- as.character(classifieds$trip_id)
  classifieds$metier <- as.integer(as.character(classifieds$metier))
  
  metiers <- rbind(classifieds, predicteds)

# merge metiers ----
  tickets <- merge(filtered_ftl, metiers, by = "trip_id")
  length(unique(tickets$trip_id)) == length(unique(metiers$trip_id)) # should be TRUE

  tickets$metier <- paste(tickets$grgroup, tickets$metier, sep="_")

  saveRDS(tickets, paste0("/Users/efuller/1/CNH/processedData/catch/1_cleaningData/tickets_",base_year,".RDS"))
#----
return(tickets)
}