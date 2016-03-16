# creating feature space that's clean
library(dplyr)
library(adehabitatLT)
library(sp)
library(rgdal)

# load vms data ----

# window = 24, fishery = "Pink Shrimp"
load_vms <- function(window, fishery, downscale = TRUE){
  # using trip_tot_dat dataframe to find VMS files with observed pink shrimp
  # using window to choose the matching time window to metier data
  
  # load trip_tot_dat to find vms with <fishery> ids
  fp <- paste0("processedData/spatial/vms/intermediate/05_make_obs_to_vms/trip_total_tw",
               window,"hr.csv")
  trip_tot_dat <- read.csv(fp, stringsAsFactors = FALSE) %>%
    filter(sector==fishery) %>%
    dplyr::select(vessel.ID) %>%
    distinct()
  
  fp <- paste0("processedData/spatial/vms/intermediate/05_make_obs_to_vms/tw_",
               window,"hr/",trip_tot_dat$vessel.ID,".csv")
  
  dat_list <- lapply(fp, function(x) read.csv(x, stringsAsFactors = FALSE))
  dat_long <- do.call(rbind, dat_list)
  
  # only keep points that are at sea (i.e. only_trips!=0) 
  # and drop lots of columns that are unnecessary
  vms_files <-  dat_long %>%
    filter(!(is.na(fishing)),only_trips!=0) %>%
    dplyr::select(-contains("lbs"), -contains("rev"), -contains("trip_id"), 
                  trip_id1, -onland, -time, -distance, only_trips,-optional, 
                  -n.trips,-declarations, -agg_id, -vessel.name, 
                  -alt.vessel.name, -tdate,-ship.number) %>%
    mutate(date.time = as.POSIXct(date.time, format="%Y-%m-%d %H:%M:%S", 
                                  tz = "Etc/GMT-8"), 
           metier.2010 = as.character(metier.2010),
           trip_id1 = as.character(trip_id1),
           burst.id = paste(only_trips, doc.num,sep="_")) %>%
    arrange(doc.num, date.time)
  
  # downsample hourly to make samples representative
  hrly_samps <- vms_files %>%
    mutate(date = as.Date(date.time, tz="Etc/GMT-8"), 
           hour = format(date.time, "%H", tz = "Etc/GMT-8")) %>%
    group_by(doc.num, date, hour) %>%
    mutate(adj_date.time = unique(date.time)[1]) %>%
    mutate(keep = ifelse(adj_date.time==date.time, 1, 0)) %>%
    filter(keep == 1) %>%
    ungroup() %>%
    dplyr::select(-adj_date.time, -keep, -hour, -date, -only_trips)
  
  if(downscale){
    return(hrly_samps)
  }else{
    return(vms_files)
  }
}

vms_files <- load_vms(window = 24, fishery = "Pink Shrimp", downscale = TRUE)

# filter observed data ----

# split into list and work by trip
  vms_list <- split(vms_files, vms_files$burst.id)
  for(i in 1:length(vms_list)){
    if(nrow(vms_list[[i]]) < 3){
      vms_list[[i]] <- NA
    }else{
      vms_list[[i]] <- vms_list[[i]][order(vms_list[[i]]$date.time),]
      
      # calculate dt in hours
      calc_dt <- function(trip_df, lag_limit = 4){
        t = trip_df$date.time[1:(nrow(trip_df)-1)]
        t1 = trip_df$date.time[2:nrow(trip_df)]
        trip_df$dt <- c(abs(difftime(t, t1, units='hours')),NA)
        return(trip_df)
      }
      vms_list[[i]] <- calc_dt(vms_list[[i]], lag_limit = 4)
      
      # calculate distance in km between each point
      calc_dist <- function(trip_df){
        trip_df <- as.data.frame(trip_df)
        coordinates(trip_df) <- ~longitude+latitude
        proj4string(trip_df) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        trip_df$distance_km <- NA
        for(j in 1:(nrow(trip_df)-1)){
          trip_df$distance_km[j] <- spDistsN1(matrix(coordinates(trip_df)[j,], ncol=2),
                                               matrix(coordinates(trip_df)[j+1,], ncol=2),
                                               longlat = TRUE)
        }
        return(as.data.frame(trip_df))
      }
      vms_list[[i]] <- calc_dist(vms_list[[i]])
      
      # calculate speed (kmph), drop points that are > 40kmph
      calc_speed <- function(trip_df, speed_limit = 40){
        trip_df$speed.kmph <- trip_df$distance_km/trip_df$dt
        
        while(any(trip_df$speed.kmph>speed_limit, na.rm=T)){
          # take first point
          too_high <- which(trip_df$speed.kmph>40)[1]
          trip_df <- trip_df[-too_high,]
          
          # recalc dt
          trip_df <- calc_dt(trip_df, lag_limit = 2)
          
          # recalc speed
          trip_df <- calc_dist(trip_df)
          
          trip_df$speed.kmph <- trip_df$distance_km/trip_df$dt
        }
        return(trip_df)
      }
      vms_list[[i]] <- calc_speed(vms_list[[i]], speed_limit = 40)
      
      # calculate turning angles and R2n - don't love this
      calc_turningAngles <- function(trip_df){
        coordinates(trip_df) <- ~longitude+latitude
        proj4string(trip_df) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        new_proj = "+proj=eqdc +lat_1=33.040181240749774 
    +lat_2=45.838330604411105 +lon_0=-125.068359375"
        vms_proj <- spTransform(trip_df, new_proj)
        path <- as.ltraj(xy = coordinates(vms_proj), 
                         date = vms_proj$date.time, 
                         id = names(vms_list)[i])
        trip_df$abs.angle <- path[[1]]$abs.angle
        trip_df$rel.angle <- path[[1]]$rel.angle
        trip_df$R2n <- path[[1]]$R2n
        
        return(as.data.frame(trip_df))
      }
      vms_list[[i]] <- calc_turningAngles(vms_list[[i]])
      
      # calculate rolling versions, also displacement
      rollfun<-function(sub){
        sub<-sub[order(sub$date.time),]
        
        # set up functions to go through time windows 
        make_time_windows <- function(window.size, data = sub){
          # window.size in hours
          
          # function to get data in list form for each sliding window of window.size
          win.size <- 60*60*window.size # convert to seconds
          
          # make a list for values in each time window
          time_dat <- data.frame(start = data$date.time)
          time_dat$end <- data$date.time + win.size
          
          time_list <- list()
          
          # have colvars to retain
          colvars <- c("abs.angle","rel.angle","R2n", "dist_coast",
                       "avg.speed","avg.direction","distance_km","speed.kmph")
          
          for(i in 1:nrow(data)){
            # find data in possible time interval
            time_list[[i]] <- data[which(data$date.time >= time_dat$start[i] & 
                                           data$date.time<= time_dat$end[i]),colvars]
            
            if(nrow(time_list[[i]])==0){
              time_list[[i]] <- as.data.frame(t(
                data.frame(data = rep(NA, length(colvars)), row.names = colvars)
              ))
            }
          }
          
          # lapply to list entry to get basic stats by column for window size
          basic_stats <- function(x){
            if(all(is.na(x))){
              mn <- NA; md <- NA; sd <- NA; sum <- NA; max <- NA
            }else{
              mn <- mean(x, na.rm = T)
              md <- median(x, na.rm=T)
              sd <- sd(x, na.rm=T)
              sum <- sum(x, na.rm = T)
              max <- max(x, na.rm = T)
            }
            
            return(list(mn, md, sd, sum, max))
          }
          
          # list entry for each data point, each entry is a list of 6 lists 
          # (one for each measurement, R2n, dist, etc.)
          # and in each list is a list of 5, one for each of the functions in basic_stats()
          stats_3 <- lapply(time_list, function(x) apply(x, 2,basic_stats))
          
          # so to unlist, want nrows = nrow(data), 
          # there will be nfunctions * data types (5*length(colvars)) columns
          df_stats <- matrix(unlist(stats_3), 
                             nrow = nrow(data), byrow=TRUE, ncol = 5*length(colvars))
          
          # rename based on contents
          colnames(df_stats) <- paste(rep(colvars, each=5), 
                                      rep(c("mean","median","sd",
                                            "sum","max"), length(colvars)), 
                                      window.size,sep="_")
          
          return(df_stats)
        }
        
        # get all combinations of parameters  
        time_windows = c(2, 3, 4, 5, 6, 7)
        
        # run function
        hourly_rolls <-  mapply(make_time_windows, window.size = time_windows, SIMPLIFY = FALSE)
        
        outdata <- do.call(cbind, hourly_rolls) # put all data together
        outdata<-cbind(sub, outdata) # add to original data
        
        # do displacement
        coordinates(outdata)<- ~longitude+latitude
        displacement_window <- function(window.size, outdata2 = outdata){
          # make a list for values in each time window
          win.size = 60*60*window.size
          time_dat <- data.frame(start = outdata2$date.time)
          time_dat$end <- outdata2$date.time + win.size
          
          time_list <- list()
          
          # have colvars to retain
          colvars = c("longitude","latitude")
          for(i in 1:(nrow(outdata2@data)-1)){
            # find data in possible time interval
            time_list[[i]] <- coordinates(outdata2)[which(outdata2@data$date.time >= time_dat$start[i] & 
                                                            outdata2@data$date.time<= time_dat$end[i]),colvars,drop=FALSE]
            # if nothing is returned, return NA and go to next iteration
            if(nrow(time_list[[i]])<=1) {
              time_list[[i]] <- NA
            } else {
              # take first and last coordinates
              time_list[[i]] <- spDistsN1(time_list[[i]][1,,drop=FALSE],
                                          time_list[[i]][nrow(time_list[[i]]),,drop=FALSE],
                                          longlat = TRUE) 
            }
          }
          
          return(unlist(time_list))
          
        }
        displacement_rolls <- mapply(displacement_window, window.size = time_windows, SIMPLIFY = FALSE)
        
        displacement_df <- do.call(cbind, displacement_rolls)
        colnames(displacement_df) <- paste("displacement", time_windows, sep="_")
        displacement_df <- rbind(displacement_df,rep(NA, ncol(displacement_df)))
        
        outdata <- as.data.frame(outdata)
        outdata <- cbind(outdata, displacement_df)
        
        return(outdata)
      }
      vms_list[[i]] <- rollfun(sub=vms_list[[i]])
    }
  }

# collect back into single dataset
  vms_list <- vms_list[-which(is.na(vms_list))]
  vms_all <- do.call(rbind, vms_list)
  

# split into location data and features and true ----
  loc_vars <- c("doc.num", "date.time", "metier.2010","obs_dup","longitude",
               "latitude","fish_tickets","trip_id1","burst.id")
  loc_data <- vms_all[,loc_vars]
  
  feature_vars <- colnames(vms_all)[-which(colnames(vms_all) %in% c(loc_vars,"fishing"))]
  feature_data <- vms_all[,feature_vars]
  
  true_data <- vms_all$fishing
  

# save processed data ----
  saveRDS(list(loc_data = loc_data, 
               feature_data = feature_data, 
               true_data = true_data),
          file="processedData/spatial/vms/intermediate/06_predict_fishing/06_obs_training_dat.RDS")