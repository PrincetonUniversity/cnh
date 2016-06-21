# function to find what the majority species is for each trip, remove those that have a majority in <remove_species>
# date: 2014-05-01
# author: emma

major_species <- function(tripTable, remove_species){
# remove these rows
  # make data.table of just catch composition
  major_trip <- tripTable[,"tripID", with = FALSE]
  # find proportion each species makes up 
  prop_species <- prop.table(as.matrix(tripTable[,!"tripID", with = FALSE]), 1)
  # find maximum species for each trip (each row)
  major_species <- apply(prop_species, 1, which.max)
  # get names of major species by row
  which_species <- colnames(prop_species)[major_species]
  # add new column to ID the maximum of each trip
  major_trip <- major_trip[, "major_species":=which_species]
  # find tripID for trips that have a majority catch of a species that matches those in <remove_species>
  trips_remove <- subset(major_trip, major_species %in% remove_species)
  trips_remove <-  as.data.frame(trips_remove)
  # remove tripIDs identified as majority catches
  tripTable <- as.data.frame(tripTable)
  filter_table <- subset(tripTable, !(tripID %in% trips_remove$tripID))

# remove these columns  
  # filter 2: remove columns with any of these species from clustering
  filter_table <- filter_table[,-which(names(filter_table) %in% remove_species)]

# check no rows now have zero catch
  filter_table <- as.data.table(filter_table)
  if(any(rowSums(filter_table[,!"tripID",with=FALSE])==0)==TRUE){message("Warning, check row sums of tripTable, some rows are zero")}

return(filter_table)
}