# Date: 2014-04-28
# Author: Emma
# Goal: take FTL data and return datasets appropriate for clustering analysis. This includes
#   + removing trips in which the majority of catch is shellfish (in the "SHLL" complex according to PacFIN)
#   + removing unspecified/other species
#   + Will include vessel ID, year, and tripID
             
source("/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-04-28/new_plan/input_data/major_species.R")

FTL_cp <- function(FTL, type="lbs",times=0,spid_remove,cmplx_remove,mgmt_remove){
# FTL is a fish ticket dataset from PacFin (rows are trip/species entries)
# type = what conversion:
#     + price = price per pound times landed_wt (ppp*landed_wt)
#     + proportion = proportion of species in total catch
#     + lbs = pounds of species in catch
#     + log(lb) = log(pounds of species in catch)
#     + *_remove = data from PacFin website here: http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/sp.txt, list of species that are unspecified
  require(data.table)
  require(reshape2)
  require(dplyr)
  require(RColorBrewer)
  
  FTL <- data.table(FTL)
  setnames(FTL,names(FTL),tolower(names(FTL)))
  
  # make unique trip ID (concatenate date, veid), link that with data.table via setkey()
    dates <- paste(FTL$year,FTL$month, FTL$day,sep="-")
    FTL$tripID <- paste(dates,FTL$veid,sep="_")
    setkey(FTL, tripID)
  
  # select columns of interest
    FTL_df <- select(FTL, veid, year, tripID, spid, landed_wt, ppp,grgroup, grid)
  
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
  
  # filter: 
  # find "unspecified species"
    unspecified <- spid_remove$spid
  # shellfish 
    shell <- subset(species_data,mgmt_grp=="SHLL",select="SPID")
  # other complexes
    cmplx <- subset(species_data, complex %in% cmplx_remove$cmplx, select="SPID") 
  # combine
    to_remove <- unique(c(unspecified, shell$SPID, cmplx$SPID))
  
  # filter species: retain only major, common species. 
    filter_total <- major_species(total_catch, to_remove) 
  
    # remove rare species if times > 0
      if(times>0){
        # define: find number of times each species present (# of !=0 in each column)
        freq <- apply(filter_total[,!"tripID", with=FALSE], 2,function(x) length(which(x > 0))) #same for prop
        rare_remove <- names(freq)[which(freq<times)]
        
        # filter
        cluster_sub <- major_species(filter_total, rare_remove) 

        # check: remove rows that have no catch from any target species
          cluster_sub <- subset(cluster_sub, rowSums(cluster_sub[,!"tripID",with = FALSE]) !=0)
        }else{cluster_sub <- filter_total}
  
  if(type=="proportion"){
    prop_cluster <- sweep(cluster_sub[,!"tripID", with=FALSE], 1, rowSums(cluster_sub[,!"tripID", with=FALSE]), "/")
    prop_cluster <- as.data.table(prop_cluster)
    prop_cluster$tripID <- cluster_sub$tripID
    setkey(prop_cluster, tripID)
    finished_tripTable <- prop_cluster
  }else if(type=="lbs" | type=="price"){
    finished_tripTable <- cluster_sub
  }else if(type=="log"){
    # log
    cluster_log <- cluster_sub[,!"tripID", with=FALSE] + 0.1
    cluster_log$tripID <- cluster_sub$tripID
    setkey(cluster_log, tripID)
    cols <- names(cluster_log[,!"tripID",with=FALSE])
    pca.dat_log <- cluster_log[ ,lapply(.SD, log), .SDcols=cols]
    pca.dat_log$tripID <- cluster_log$tripID
    setkey(pca.dat_log, tripID)
    finished_tripTable <- pca.dat_log
  }else{message("'type' needs to be 'price', 'proportion', 'log', or 'lbs'")}  

  # make reference table that matches year, veid, with tripID of retained trips
    trips_retained <- as.data.frame(finished_tripTable[,"tripID",with=FALSE])
    ref_table <- as.data.frame(FTL_df[,c("veid","year","tripID","grgroup","grid"),with=FALSE])
    ref_table <- ref_table[-which(duplicated(ref_table)),]
    ref_table <- merge(trips_retained, ref_table, by="tripID")

  return(list(finished_tripTable, ref_table))
}