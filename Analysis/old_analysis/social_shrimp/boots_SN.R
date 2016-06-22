# reshuffle time stamps for fishing events within each vessel track, recalc social metrics, save. 
library(sp)
library(dplyr)
library(tidyr)
# load data ----
fp <- "processedData/spatial/vms/intermediate/06_predict_fishing/vms_classified.RDS"
vms_dat <- readRDS(fp)
rownames(vms_dat) <- NULL

# define helper functions ----
# define function to get relevant VMS points
relevant_vms <- function(vessel_name, time_list = ts_list){
  ves <- vessel_name
  # all times where the vessel is present
  ves_list <- time_list[which(sapply(time_list, function(x) return(any(x$doc.num == ves))))]
  
  # any that have another doc.num that's not vessel of interest
  ves_social <- ves_list[which(sapply(ves_list, function(x) any(x$doc.num!=ves)))];
  rm(ves_list)
  
  # for these, how many are fishing
  ves_social_fishing <- ves_social[which(sapply(ves_social,function(x){
    sub <- subset(x, doc.num==ves & 
                    predicted_fishing == 1)
    if(nrow(sub)>0){ 
      return(TRUE)
    }else{
      return(FALSE)
    }
  }))]
  rm(ves_social)
  
  # for each boat, want to know total hrs fishing next to any vessel, and
  # pairwise total hrs for each partner vessel
  fishing_dists <- function(x){
    #x <- subset(x, predicted_fishing==1) # drop this for the moment
    if(nrow(x)<2) stop("subsetting failed - no data")
    coordinates(x) <- ~longitude+latitude
    proj4string(x) <-   CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    ves_interest <- subset(x, doc.num==ves)[1,] # just take the first entry if 2
    rest_ves <- subset(x, doc.num!=ves)
    if(nrow(rest_ves)==0) stop("subsetting failed - no other vessels")
    pwdist <- spDistsN1(matrix(coordinates(rest_ves),ncol=2),
                        matrix(coordinates(ves_interest),ncol=2),
                        longlat=TRUE)
    doc.num = rest_ves@data$doc.num
    
    return(data.frame(doc.num = doc.num,
                      pairwise_km = pwdist,
                      vessel = rep(ves, length(pwdist)),
                      trip_id = ves_interest$trip_id1,
                      adj_time = ves_interest$adj_time,
                      stringsAsFactors = FALSE))
  }
  ves_fishing_dists <- lapply(ves_social_fishing,
                              fishing_dists)
  # combine to dataframe to sort
  all_dists <- do.call(rbind, ves_fishing_dists)
  all_dists <- as.data.frame(all_dists)
  return(all_dists)
}

# permute = TRUE shuffles wihithin vessel, year, predicted behavior
calculate_social_networks <- function(vms_dat, permute){
  # set up VMS and possibly permute time vectors ----
  if(permute){
    vms <- vms_dat %>%
      mutate(adj_time = format(round(date.time, units="hours"), 
                               format="%Y-%m-%d %H:%M:%S"),
             adj_time = as.POSIXct(adj_time,
                                   format = "%Y-%m-%d %H:%M:%S", 
                                   tz = "Etc/GMT-8"),
             year = as.numeric(format(adj_time, "%Y"))) %>%
      group_by(doc.num, predicted_fishing, year) %>%
      mutate(shuffle_order = sample(1:n()), 
             adj_time = adj_time[shuffle_order]) %>%
      dplyr::select(-shuffle_order, -year)
    }else{
    vms <- vms_dat %>%
      mutate(adj_time = format(round(date.time, units="hours"), 
                               format="%Y-%m-%d %H:%M:%S"),
             adj_time = as.POSIXct(adj_time,
                                   format = "%Y-%m-%d %H:%M:%S", 
                                   tz = "Etc/GMT-8"))
}

  vms <- as.data.frame(vms)
  ts_list <- split(vms, vms$adj_time)

  # make networks based on amount of time fishing within 0.5 - 9 km (prep data structures) ----
  vs <- unique(vms$doc.num)
  
  # set up social matrix
  social_mat <- matrix(0, ncol=length(vs), nrow = length(vs))
  colnames(social_mat) <- vs
  rownames(social_mat) <- vs
  
  # make list of social matrices for all vessels
  social_mat_list <- lapply(1:10, function(x) social_mat)
  names(social_mat_list) <- c(0.5,1:9)
  
  # keep track of total social hours
  total_social_hrs <- data.frame(doc.num = rep(vs,each=10),
                                 km_cutoff = rep(c(.5,1:9), length.out=620),
                                 social_hrs = rep(NA, 620),
                                 stringsAsFactors = FALSE)

  # keep track of all social points and pairwise dists
  social_list <- list()
  
  # flip through vessels to find social VMS 
  for(i in 1:length(vs)){
    # find pairwise distances for vessel
    all_dists <- relevant_vms(vessel_name = vs[i], time_list = ts_list)
    
    # fill in social matrices
    if(nrow(all_dists)==0){
      next
    }else{
      row_ind <- which(rownames(social_mat)==vs[i])
      min_dists <- as.numeric(names(social_mat_list))
      for(j in 1:length(min_dists)){
        social_hrs <- with(subset(all_dists, pairwise_km<min_dists[j]), table(doc.num))
        col_inds <- names(social_hrs)
        social_mat_list[[j]][row_ind, col_inds] <- social_mat_list[[j]][row_ind, col_inds] + social_hrs
      }
    }
    
    # fill total social hours for vessel 
    for(j in c(.5,1:9)){
      relevant_dist  <- subset(all_dists, pairwise_km<j)
      idx = which(total_social_hrs$doc.num==vs[i] & total_social_hrs$km_cutoff == j)
      total_social_hrs$social_hrs[idx] <- length(unique(relevant_dist$adj_time))
    } 
    
    # label VMS points as social or solitary
    # relevant dist is the version w subset < 9km
    social_list[[i]] <- relevant_dist %>% 
      group_by(vessel, trip_id, adj_time, doc.num) %>%
      summarize(pairwise_km = min(pairwise_km)) %>%
      spread(key=doc.num, value=pairwise_km) %>%
      rename(doc.num = vessel)
    
    names(social_list)[i] <- vs[i]
  }

  file.name = paste0("social_stats",sample(1:100000,1),".RDS")
  if(permute) file.name = paste0("permuated_",file.name)
  saveRDS(list(total_social_hrs = total_social_hrs, 
               social_mat_list = social_mat_list, 
               social_list= social_list),
          file=paste0("Analysis/social_shrimp/boots_SN/",file.name))
}

# calculate 100 bs ----
for(j in 1:100){
  calculate_social_networks(vms_dat = vms_dat, permute = TRUE)
}