# date: 2014-04-14
# author: Emma
# goal: cluster vessels based on yearly catch using proportion of species

  # make data.frame: rows vessel, columns average proportion of catch of species x (averaged over year, across years)
    #   + make table for each vessel, rows are trips, columsn are proportion of each species type
    #   + average rows

# load fishticket data
  FTL <- read.csv("Data/Catch/FTL_2009-2013_2014-03-21.csv", as.is=TRUE)
  names(FTL) <- tolower(names(FTL))
  
  # make unique tripID
    dates <- paste(subFTL$year,subFTL$month, subFTL$day,sep="-")
    subFTL$tripID <- paste(dates,subFTL$veid,sep="_")

  # format df
  FTL_df <- select(subFTL,tripID,spid,landed_wt)
  
  # calculate total catch by trip (tripID)
  totals <- FTL_df %.%
    group_by(tripID) %.%
    summarise(total = sum(as.numeric(landed_wt))) %.%
    arrange(tripID)
  
  # not working, why? Alternate way
  totals <- ddply(FTL_df, .(tripID), summarize, total=sum(landed_wt))
  
  # calculate by species total catch for each trip
  catch <- FTL_df %.%
    group_by(tripID,spid) %.%
    summarise(catch = sum(as.numeric(landed_wt))) %.%
    filter(catch > 0) %.%
    arrange(tripID)
  
  # also not working, why? Alernate way
  catch <- ddply(FTL_df, .(tripID, spid), summarize, catch = sum(landed_wt))
  catch <- filter(catch, catch > 0)
  # any NAs?
  any(is.na(catch))   #FALSE, we're good
  
  # for each entry for species caught, need to find total proportion of catch (divide by catchRET$total)
  total_catch <- dcast(catch, tripID ~ spid, fill = 0, drop=TRUE)
  
  # combine totals and species composition 
  cluster <- merge(total_catch, totals, by="tripID")
  cluster_df <- tbl_df(cluster)
  
  # find "unspecified species", things that start with a "U"
  unspecified <- unique(FTL$spid)[grep("^U",unique(FTL$spid))]
  cluster_pre <- cluster_df[,-which(names(cluster_df) %in% unspecified)]
  
  # find now empty rows
  cluster_new <- cluster_pre[-which(rowSums(cluster_pre[2:(ncol(cluster_pre)-1)])==0),]
  
  dim(cluster_df) - dim(cluster_new)
  
  # construct table
  start_ind = 2
  end_ind = ncol(cluster_new)-1
  freq <- apply(cluster_new[,start_ind:end_ind], 2, function(x) length(which(x > 0))) 
  # returns indexes for species which are found fewer than 20 times
  
  # remove those species that are in fewer than 20 trips
  cluster_int <- cluster_new[,-which(names(cluster_new) %in% names(freq)[which(freq<200)])]
  
  # remove rows that have no catch from any target species
  cluster_sub <- cluster_int[-which(rowSums(cluster_int[2:(ncol(cluster_int)-1)])==0),]
  dim(cluster_df)-dim(cluster_sub)
  # lost 572 trips and 55 species 