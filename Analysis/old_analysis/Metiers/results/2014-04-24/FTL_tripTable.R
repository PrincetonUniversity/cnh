#author: emma 
# date: 2014-04-21
# goal: a function that takes the FTL data and outputs a trip table, see function for help
FTL_tripTable <- function(FTL, type="lbs", times = 200){
  # description: rows are columns with unique tripID, columns are species. Entries in table can either be total price, lbs, log(lbs), or proportion of the catch. This depends on what type is. 
  require(data.table)
  require(reshape2)
  require(dplyr)
  
  # make data.table object
  FTL <- data.table(FTL)
  setnames(FTL,names(FTL),tolower(names(FTL)))
  
  # make unique trip ID (concatenate date, veid), link that with data.table via setkey()
  dates <- paste(FTL$year,FTL$month, FTL$day,sep="-")
  FTL$tripID <- paste(dates,FTL$veid,sep="_")
  setkey(FTL, tripID)
  
  # select columns of interest
  FTL_df <- select(FTL, tripID, spid, landed_wt, ppp)
  
  # if price, instead of pounds, it's the price per lb * amount of lbs
  if(type=="price"){
   
    FTL_df$value = FTL_df$ppp * FTL_df$landed_wt
    
    # calculate by species total catch for each trip
    catch <- select(FTL_df, tripID, spid, value)
    catch <- filter(catch, value > 0)
    catch <- as.data.table(catch)
    
  }else{
  # calculate by species total catch for each trip
  catch <- FTL_df[, sum(landed_wt), by=c("tripID", "spid")]
  setnames(catch, "V1", "value")
  }
  
  # transform: rows are trip, columns are species that were caught 
  total_catch <- dcast.data.table(catch, tripID ~ spid, fun=sum)
  
  # find "unspecified species", things that start with a "U" [see http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/sp.txt]
  unspecified <- unique(FTL$spid)[grep("^U",unique(FTL$spid))]
  
  # find "other species," things that start with "O" (mostly)
  other <- unique(FTL$spid)[grep("^O", unique(FTL$spid))]
  # but not OLV1, OLVE, OTCR, OWFS (these are actual species codes that begin with "O")
  other <- other[-which(other=="OLVE" | other=="OLV1" | other=="OTCR" | other=="OWFS")]
  # add other unspecifieds that don't have same pattern
  more <- c("MSC2", "MISC", "NUSF", "NUSP", "POP2")
  # merge 
  to_remove <- c(unspecified, other,more)
  
  # remove any column that matches the names in to_remove
  cluster_pre <- total_catch[,!(names(total_catch) %in% to_remove), with=FALSE]
  
  # find now empty rows
  cluster_new <- subset(cluster_pre, rowSums(cluster_pre[,!"tripID",with=FALSE]) != 0)
  
  # find number of times each species present (# of !=0 in each column)
  freq <- apply(cluster_new[,!"tripID", with=FALSE], 2,function(x) length(which(x > 0))) #same for prop
  
  # remove those species that are in fewer than <times> trips (200 is default)
  cluster_int <- cluster_new[,!(names(cluster_new) %in% names(freq)[which(freq<times)]),with=FALSE]
  
  # remove rows that have no catch from any target species
  cluster_sub <- subset(cluster_int, rowSums(cluster_int[,!"tripID",with = FALSE]) !=0)
  
  if(type=="proportion"){
    prop_cluster <- sweep(cluster_sub[,!"tripID", with=FALSE], 1, rowSums(cluster_sub[,!"tripID", with=FALSE]), "/")
    prop_cluster <- as.data.table(prop_cluster)
    prop_cluster$tripID <- cluster_sub$tripID
    setkey(prop_cluster, tripID)
    return(prop_cluster)
  }else if(type=="lbs" | type=="price"){
    return(cluster_sub)
  }else if(type=="log"){
    # log
    cluster_log <- cluster_sub[,!"tripID", with=FALSE] + 0.1
    cluster_log$tripID <- cluster_sub$tripID
    setkey(cluster_log, tripID)
    cols <- names(cluster_log[,!"tripID",with=FALSE])
    pca.dat_log <- cluster_log[ ,lapply(.SD, log), .SDcols=cols]
    pca.dat_log$tripID <- cluster_log$tripID
    setkey(pca.dat_log, tripID)
    return(pca.dat_log)
  }else{
    message("'type' needs to be 'price', 'proportion', 'log', or 'lbs'")
  }
  
}