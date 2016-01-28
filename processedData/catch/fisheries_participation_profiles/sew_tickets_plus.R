# sewing results of cluster fisheries participation

  tickets <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/tickets.RDS")
  
  particp_profiles <- read.csv(
    "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/fisheries_participation_profiles/link_list_cluster_key.txt"
    ,stringsAsFactors=FALSE) 
  particp_profiles$year <- NA
  particp_profiles$drvid <- NA
  for(i in 1:nrow(particp_profiles)){
    particp_profiles$year[i] = as.integer(unlist(strsplit(particp_profiles$ftid[i],split = "_"))[2])
    particp_profiles$drvid[i] = unlist(strsplit(particp_profiles$ftid[i],split = "_"))[1]
  }
  
  particp_profiles$ftid <- NULL
  particp_profiles$node <- NULL
  
  # merge participation profiles ----
  new_tickets <- merge(tickets, particp_profiles, by = c("year","drvid"))
  
  saveRDS(new_tickets, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/fisheries_participation_profiles/tickets_plus.RDS")
