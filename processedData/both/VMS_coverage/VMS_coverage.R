# determine the percentage of each metier landed. This is the maximum coverage I could have of each fishery. Might regionally have more depending on where they land

# load VMS data ----
VMS <- readRDS("/Users/efuller/1/CNH/processedData/spatial/vms/intermediate/04_with_trip/VMS_catch.RDS")

# load landings data ----
tickets <- readRDS("/Users/efuller/1/CNH/processedData/catch/fisheries_participation_profiles/tickets_plus.RDS")

# check metier for coverage ----
# go through each metier, find total number of trip_ids. Then find number which are also in VMS. Report absolute numbers and percentages 

metiers <- data.frame(metier = unique(tickets$metier), stringsAsFactors = FALSE)
metiers$total_trips <- NA
metiers$VMS_trips <- NA
metiers$percent_cover <- NA

VMS_trip.ids <- unique(c(VMS$trip_id1, VMS$trip_id2))
for(i in 1:nrow(metiers)){
  trips <- unique(subset(tickets, metier == metiers$metier[i])$trip_id)
  metiers$total_trips[i] <- length(trips)
  metiers$VMS_trips[i] <- length(which(trips %in% VMS_trip.ids))
  metiers$percent_cover[i] <- metiers$VMS_trips[i]/metiers$total_trips[i]
  if(i %% 10 == 0) cat(i," finished\n")
}

write.csv(metiers, "/Users/efuller/1/CNH/processedData/both/VMS_coverage.csv",row.names=FALSE)
