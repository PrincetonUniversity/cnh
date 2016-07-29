# match VMS to metiers

# load data ----
tickets <- readRDS("processedData/catch/1_cleaningData/tickets.RDS") # /Users/efuller/Desktop/CNH/processedData/catch/1_cleaningData
names(tickets)

landings <- unique(tickets[,c("trip_id","drvid","tdate","metier.2010")])
landings$tdate <- as.POSIXlt(landings$tdate, format = "%d-%b-%y", tz = "Etc/GMT-8")
rm(tickets)

# for each vessel track, check if it's in landings data. if not, next
#(the below setup places better with the saving file path below)
fp <- "processedData/spatial/vms/intermediate/02_cleaned/"
fish_tracks <- dir(fp)
fish_tracks <- paste0(fp, fish_tracks)

# find out if VMS is in landing tickets ----
for(i in 1:length(fish_tracks)){
  
  track <- readRDS(fish_tracks[i])
  if(nrow(track)==0) next
  # if there's no track 
  # (which happens because didn't properly filter for vessels that never leave 
  # land upstream in 02_cleaning_data.R)
  #docnum <- unique(track$docnum)
  docnum <- unique(track$docnum)
  
  if(length(docnum)>1) break # shouldn't be the case. because split on doc.num earlier
  
  # if the landing gear is there at all
  if(docnum %in% landings$drvid){ 
    single_landing <- landings[(which(landings$drvid == docnum)),]
    
    # check if there's any overlap between dates of VMS and landings
    if(any(track$datetime > min(single_landing$tdate) & 
           track$datetime < max(single_landing$tdate))){
      
      saveRDS(track, paste0(
        "processedData/spatial/vms/intermediate/03_overlapMetier/v_",
        unique(track$doc.num),".RDS")) }
  }
}