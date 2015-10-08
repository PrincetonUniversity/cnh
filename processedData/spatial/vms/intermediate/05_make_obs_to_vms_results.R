#' # Matching VMS to landings
#' If the matching is working correctly we expect
#' 
#' + Observer trip durations should be longer than VMS durations
#' + no more than 24 hr difference between start or end dates
#' + sectors align with metiers (bipartite graph?)
#' + trips that don't have observers are either sectors that are not fully observed (i.e. TWL_1 or TWL_4), or prior to 2011
#' 
#' For training and validating models we want to filter to
#' 
#' + VMS data for which we have observer and fish ticket coverage
#' + Make sure those trips don't have VMS gaps > 3 hrs
#' 
#' For segmenting we have to filter to 
#' 
#' + VMS for which we have fish ticket data 
#' 
#' For all data we want to 
#' 
#' + remove points that are not on the west coast (using lat/lon filters) 
#' + remove trips that have > 3 hr gaps
#' + calculate the coverage by calculating the percentage of landings we have left in the VMS that are also in fish ticket data, do this by number of trips, biomass landed, revenue made, and percentage of the fleet (number of boats)
#' 


user = "emma"
if(user == "emma"){
  infile.dir <- "/Users/efuller/1/CNH/processedData/spatial/vms/intermediate/05_make_obs_to_vms/"
  infiles <- dir(infile.dir)
  obs <- read.csv("/Users/efuller/1/CNH/rawData/Observers/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv",stringsAsFactors = FALSE)
}

match_list <- list()
for(b in 1:length(infiles)){
  v <- read.csv(paste0(infile.dir, infiles[b]),stringsAsFactors = FALSE)
  
  weird_names <- grep("data.vms",colnames(v))
  colnames(v)[weird_names] <-  gsub("data.vms.","",colnames(v)[weird_names])
  
  # make date-time correct
  v$date.time <- as.POSIXct(v$date.time, format = "%Y-%m-%d %H:%M:%S", 
                            tz = "Etc/GMT-8")
  
  # make all trip_ids characters
  col_i <- grep("trip_id",colnames(v))
  v[,col_i] <- sapply(v[,col_i],as.character)
  
  # if any observed overlap - validate using observer
    # check to make sure all good values
    if(any(is.na(v$observed))) warning("weird values in observed vector")
    
  if(any(v$observed!=0)){
    # make the fish ticket and observer fish ticket the same by appending year
    obs_id <- which(v$observed!=0)
    v$observed[obs_id] <- paste0(v$observed[obs_id], format(v$date.time[obs_id], "%Y"))
    
    # how many matches
    # intersect gives the number of unique observed that are found in trip_id1
    w.matches = intersect(v$observed, v$trip_id1)
    n.matches = length(w.matches)
    n.matches/length(unique(v$trip_id1)) # but small percentage
    
    # how many observed with no trip_ids? - 12 different. 
    no.matches <-unique(v$observed[which(v$observed!=0 & is.na(v$trip_id1))])
    
    # observed can have colons. Great. 
    
    real_no_matches <- no.matches[which(!(no.matches %in% w.matches))] # nope these are truely missing, but can be true. 
    
    # what fisheries were these?
    no_match_no_year <- substr(no.matches, 1, nchar(no.matches)-4)
    unique(obs$FISHERY[which(obs$FISH_TICKETS %in% no_match_no_year)])
    unique(obs$sector[which(obs$FISH_TICKETS %in% no_match_no_year)])
    # is pink shrimp
    
    # for those that matched, were the durations the same?
    
      # for trips that have observed match, how long in VMS time? 
    match_results <- data.frame(trip_id = w.matches, stringsAsFactors = FALSE)
    match_results$vms_start <- NA; match_results$vms_end <- NA; 
    match_results$vms_dur <- NA; match_results$obs_start <- NA;
    match_results$obs_end <- NA; match_results$obs_dur <- NA;
    match_results$obs_sector <- NA; match_results$obs_fishery <- NA;
    match_results$metier.2010 <- NA; match_results$drvid = gsub("v","",gsub(".csv","",infiles[b]))
    
    for(i in 1:length(w.matches)){
      vms_ind <- which(v$trip_id1 %in% w.matches[i])
      match_results$vms_start[i] <- as.character(v$date.time[vms_ind[1]])
      match_results$vms_end[i] <- as.character(v$date.time[tail(vms_ind,1)])
      match_results$metier.2010[i] <- unique(v$metier.2010[vms_ind])
      
      obs_ind <- which(obs$FISH_TICKETS==substr(w.matches[i],1,nchar(w.matches[i])-4))
      match_results$obs_start[i] <- unique(obs$D_DATE[obs_ind])
      match_results$obs_end[i] <- unique(obs$R_DATE[obs_ind])
      
      match_results$obs_sector <- unique(obs$sector[obs_ind])
      match_results$obs_fishery <- unique(obs$FISHERY[obs_ind])
    }
    
    match_results$vms_start <- as.POSIXct(match_results$vms_start, tz = "Etc/GMT-8")
    match_results$vms_end <- as.POSIXct(match_results$vms_end, tz = "Etc/GMT-8")
    match_results$obs_start <- as.POSIXct(match_results$obs_start, format = "%m/%d/%Y %I:%M:%S %p", tz = "Etc/GMT-8")
    match_results$obs_end <- as.POSIXct(match_results$obs_end, format = "%m/%d/%Y %I:%M:%S %p", tz = "Etc/GMT-8")
    
    match_results$vms_dur <- match_results$vms_end - match_results$vms_start
    match_results$obs_dur <- match_results$obs_end - match_results$obs_start
    
    match_list[[b]] <- match_results
  }else{
    match_results <- data.frame(trip_id = NA, stringsAsFactors = FALSE)
    match_results$vms_start <- NA; match_results$vms_end <- NA; 
    match_results$vms_dur <- NA; match_results$obs_start <- NA;
    match_results$obs_end <- NA; match_results$obs_dur <- NA;
    match_results$obs_sector <- NA; match_results$obs_fishery <- NA;
    match_results$metier.2010 <- NA; match_results$drvid = gsub("v","",gsub(".csv","",infiles[b]))
    
    match_list[[b]] <- match_results
  }
}

plot(match_results$obs_dur, match_results$vms_dur)
abline(a = 0, b = 1)

