# load training data and train RF
library(dplyr)
library(adehabitatLT)
library(maps)
library(sp)
library(rgdal)

# load vms data
# window = 24, fishery = "Pink Shrimp"
load_vms <- function(window, fishery){
  # using trip_tot_dat dataframe to find VMS files with observed pink shrimp
  # using window to choose the matching time window to metier data
  
  # load trip_tot_dat to find vms with <fishery> ids
  fp <- paste0("processedData/spatial/vms/intermediate/05_make_obs_to_vms/trip_total_tw",
               window,"hr.csv")
  trip_tot_dat <- read.csv(fp, stringsAsFactors = FALSE) %>%
    filter(sector==fishery) %>%
    dplyr::select(vessel.ID) %>%
    distinct()
  
  # load VMS and subset to observed points ----
  fp <- paste0("processedData/spatial/vms/intermediate/05_make_obs_to_vms/tw_",
               window,"hr/",trip_tot_dat$vessel.ID,".csv")
  
  dat_list <- lapply(fp, function(x) read.csv(x, stringsAsFactors = FALSE))
  dat_long <- do.call(rbind, dat_list)
  vms_files <-  dat_long %>%
    filter(!(is.na(fishing)),only_trips!=0) %>%
    dplyr::select(-contains("lbs"), -contains("rev"), -contains("trip_id"), 
                  trip_id1, -onland, -time, -distance, only_trips,-optional, 
                  -n.trips,-declarations, -agg_id, -vessel.name, 
                  -alt.vessel.name, -tdate,-ship.number, - only_trips) %>%
    mutate(date.time = as.POSIXct(date.time, format="%Y-%m-%d %H:%M:%S", 
                                  tz = "Etc/GMT-8"), 
           metier.2010 = as.character(metier.2010),
           trip_id1 = as.character(trip_id1)) %>%
    arrange(doc.num, date.time)
    
    # will generate warnings, is ok. it's the fishing vector. 
    # only really care if 0 or not. not (of any flavor) means fishing
    
}


# generate movement statistics ----
  
  # need to make an ID that is only_trips + doc.num to get unique trips 
  # other groupings may aggregate multiple trips. 
  # shouldn't happen because observed, but does look like it's happening 
  vms_files$trip_id1 <- as.character(vms_files$trip_id1)

  # drop trips that have fewer than 50 relocs
    # n.locs <- table(vms_files$burst.id)
    # vms_files <- subset(vms_files, burst_id %in% names(n.locs)[which(n.locs>100)])
  # make spatial
  vms_files <- as.data.frame(vms_files)
  coordinates(vms_files) <- ~longitude+latitude
  proj4string(vms_files) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  # probably should project, ltraj just does euclidean distance, 
  # now distance in meters
  # projection came from projectionwizard, is 
  # Equidistant conic PROJ.4 - distance correct along meridians
  #   Latitude of origin: 39º 26' N
  #   Standard parallel 1: 33º 02' N
  #   Standard parallel 2: 45º 50' N
  #   Central meridian: 125º 04' W
  new_proj = "+proj=eqdc +lat_1=33.040181240749774 
  +lat_2=45.838330604411105 +lon_0=-125.068359375"
  vms_proj <- spTransform(vms_files, new_proj)
  
  # jitter lat/lon
  vms_proj$adj_long <- jitter(coordinates(vms_proj)[,1],amount = 500)
  vms_proj$adj_lat <- jitter(coordinates(vms_proj)[,2], amount = 500)
  
  # add coastline for ref
  load("processedData/spatial/2_coastline.Rdata")
  proj4string(WC) <- proj4string(vms_files)
  WC <- spTransform(WC, proj4string(vms_proj))
  
  dat_lt <- as.ltraj(xy = vms_proj@data[,c("adj_long","adj_lat")], date = vms_proj$date.time, 
                     id = vms_proj$burst.id)
  dat <- ld(dat_lt)
  
  dat$id <- as.character(dat$id)
  
  # add observed and fishing data to dat
  full_dat <- left_join(dat, dplyr::select(as.data.frame(vms_files), burst.id, 
                                      date.time, fishing, dist_coast, 
                                      avg.speed, avg.direction),
                   by = c("id" = "burst.id","date"="date.time"))
  
# make sure training data is clean ----
  
  # need to have trips that have at least 6 relocs
  n.locs <- table(dat$burst)
  n_big <- names(n.locs)[which(n.locs>10)]
  
  # look at speeds
  full_dat$speed_kmph = (full_dat$dist/1000)/(full_dat$dt/3600)
  ggplot(full_dat, aes(x = speed_kmph)) + geom_histogram() +
    theme_minimal() + scale_x_sqrt() + scale_y_sqrt()
  
  # what's going on with speeds > 40kmph?
  fast_trips <- unique(full_dat$id[which(full_dat$speed_kmph> 40)])
  # about 5% of trips have this
  length(fast_trips)/length(unique(full_dat$id)) 
  
  # inspecting this one, looks like a very small dt where boat doesn't move for a second
  drp_ids <- which(full_dat$dx==0 & full_dat$dy==0 & full_dat$dt==60)
  
  # drop these
  if(length(drp_ids)>0){
  filtered_dat <- full_dat[-drp_ids,]
  }else{
    filtered_dat <- full_dat
  }
  # drop also anything with dt == 180
  drp_ids <- which(filtered_dat$dt<=180)
  if(length(drp_ids)>0){
    filtered_dat <- filtered_dat[-drp_ids,]
  }
  # redo ld
  filter_ld <- as.ltraj(xy=filtered_dat[,c("x","y")], date = filtered_dat$date,
                        id = filtered_dat$id)
  
  fld <- ld(filter_ld)
  # check dt again
  length(which(fld$dt>4*60*60)) # 0, good

  # recalc speeds
  fld$speed_kmph <- (fld$dist/1000)/(fld$dt/3600)
  ggplot(fld, aes(x = speed_kmph)) + geom_histogram() +
    theme_minimal() + scale_x_sqrt() + scale_y_sqrt()
  # still some fast ones
  fast_trips <- unique(fld$id[which(fld$speed_kmph> 40)])
  # about <1% of trips have this, 1. good with me
  length(fast_trips)/length(unique(fld$id)) 
  
  # what are the NAs for dt and dist?
  # are at end of trajectory
  
  fld <- left_join(fld, dplyr::select(as.data.frame(vms_files), burst.id, 
                                           date.time, fishing, dist_coast, 
                                      avg.speed, avg.direction),
                        by = c("id" = "burst.id","date"="date.time")) %>%
    mutate(id = as.character(id))
  
  # calculate displacement for 3 points (not hours!). assigned to forward time point
  displacement_window <- function(window.size, outdata2 = outdata){
    # make a list for values in each time window
    coordinates(outdata2)<- ~x+y
    win.size = 60*60*window.size
    time_dat <- data.frame(start = outdata2$date)
    time_dat$end <- outdata2$date + win.size
    
    time_list <- list()
    
    # have colvars to retain
    colvars = c("x","y")
    for(i in 1:(nrow(outdata2@data)-1)){
      # find data in possible time interval
      time_list[[i]] <- coordinates(outdata2)[which(outdata2@data$date >= time_dat$start[i] & 
                                                      outdata2@data$date<= time_dat$end[i]),colvars,drop=FALSE]
      # if nothing is returned, return NA and go to next iteration
      if(nrow(time_list[[i]])<=1) {
        time_list[[i]] <- NA
      } else {
        # take first and last coordinates
        time_list[[i]] <- spDistsN1(time_list[[i]][1,,drop=FALSE],
                                    time_list[[i]][nrow(time_list[[i]]),,drop=FALSE]) 
      }
    }
    
    return(unlist(time_list))
    
  }
  
  fld_list <- split(fld, fld$id)
  # drop small ones
  fld_list <- fld_list[-which(sapply(fld_list, nrow)<10)]
  
  wdispl <- lapply(fld_list, function(x) c(NA,displacement_window(window.size = 3, outdata2 = x)))
  wdispl6 <- lapply(fld_list, function(x) c(NA,displacement_window(window.size = 6, outdata2 = x)))
  wdispl2 <- lapply(fld_list, function(x) c(NA,displacement_window(window.size = 2, outdata2 = x)))
  
  disps <- mapply(cbind, wdispl, wdispl6, SIMPLIFY = F)
  disps <- mapply(cbind, disps, wdispl2, SIMPLIFY = F)
  foobar <- mapply(cbind, fld_list, disps, SIMPLIFY = F)
  for(i in 1:length(foobar)){
    colnames(foobar[[i]]) <- c(colnames(fld),"displacement3","displacement6", "displacement2")
  }
  foobar <- do.call(rbind,foobar)
    
  complete_dat <- foobar %>%
    filter(id %in% n_big) %>%
    dplyr::select(rel.angle, abs.angle, R2n, dist, speed_kmph, fishing, id,
                  x, y, date, dist_coast, avg.speed, displacement3, 
                  displacement6, displacement2) %>%
    group_by(id) %>%
    mutate(mean_speed = mean(speed_kmph,na.rm=T), 
           spd_norm_mean = speed_kmph-mean_speed, 
           median_speed = median(speed_kmph, na.rm = T),
           spd_norm_median = speed_kmph - median_speed,
           persistence_velocity = cos(abs.angle)*speed_kmph)
  
  speed_deltas <- function(speed_col, df){
    df <- as.data.frame(df)
    speed_idx = which(colnames(df)==speed_col)
    new_colf <- paste("d",speed_col,"forward",sep="_")
    new_colb <- paste("d",speed_col,"backward",sep="_")
  
    speed_back <- cbind(df[,speed_idx], c(df[2:nrow(df), speed_idx], NA))
    speed_forward <- cbind(df[,speed_idx], c(NA, df[1:nrow(df)-1, speed_idx]))
    df[,c(new_colb)] <- speed_forward[,1] - speed_forward[,2]
    df[,c(new_colf)] <- speed_back[,1] - speed_back[,2]
    return(df)
  }
  
  # add speed_kmph
  complete_dat <- speed_deltas("speed_kmph", complete_dat)
  complete_dat <- speed_deltas("spd_norm_median", complete_dat)
  complete_dat <- speed_deltas("spd_norm_mean", complete_dat)
  complete_dat <- speed_deltas("rel.angle", complete_dat)
  complete_dat <- speed_deltas("abs.angle", complete_dat)
  
  complete_dat <- complete_dat[complete.cases(complete_dat),]
  
  # all but two trips have uncertain points. drop y = 3 points
  complete_filter <- subset(complete_dat, fishing!=3)
  
# split up and prepare for RF
  training_dat <- dplyr::select(as.data.frame(complete_filter), -fishing, -id, -date)
  
  # normalize
  training_dat_norm <- apply(training_dat,2,function(x) (x - min(x))/diff(range(x)))
  
  # make response variable
  y = factor(complete_filter$fishing)
  
  
# subset to 60/40 and run RF - subset by whole trips though
  all_ids <- unique(complete_filter$id)
  train_n = floor(length(all_ids)*.6)
  trainingids <- sample(all_ids, size = train_n)
  testids <- all_ids[-which(all_ids %in% trainingids)]
  
  train_i <- which(complete_filter$id %in% trainingids)
  test_i <- which(complete_filter$id %in% testids)

  library(randomForest)
  library(rfUtilities)
  tuning <- rf.modelSel(xdata = training_dat_norm[train_i,], ydata = y[train_i])
  
  mtrys <- tuneRF(x = training_dat_norm[train_i, tuning$selvars], 
                  y = y[train_i])
  
  rf1 <- randomForest(x = training_dat_norm[train_i, tuning$selvars], 
                      y = y[train_i],mtry = 8)
# predict out to withheld data
  
 pred_rf1 <-   predict(rf1, training_dat_norm[test_i,tuning$selvars], type = "prob")
 colnames(pred_rf1) <- paste0("p",colnames(pred_rf1))
 pred_rf1 <- as.data.frame(pred_rf1)
 pred_rf1$true <- y[test_i]
 pred_rf1$pred <- ifelse(pred_rf1$p1>.7, 1, 0)
 
 table(pred_rf1$true, pred_rf1$pred,deparse.level = 2)
 
 complete_filter$predicted <- NA
 complete_filter$predicted[test_i] <- pred_rf1$pred
 
 # plot time series
 ids <- unique(complete_filter$id[test_i])
  
  i = 7
  par(mfrow=c(2,1),mai=rep(0,4))
  with(subset(complete_filter, id==ids[i]),
       plot(x, y, asp=1,typ='o',cex=.5,col=as.factor(fishing), bty="n",axes=F))
  plot(WC,add=T,col='wheat')
  with(subset(complete_filter, id==ids[i]),
       plot(x, y, asp=1,typ='o',cex=.5,col=as.factor(predicted), bty="n",axes=F))
  plot(WC,add=T,col='wheat')
  
  par(mfrow=c(1,1))
  with(subset(complete_filter, id==ids[i]),
       plot(date, fishing*2,typ='o',
            cex=.5, bty="n",axes=T))
  with(subset(complete_filter, id==ids[i]),
       lines(date, predicted*1, asp=1,typ='o',cex=.5,
             col=as.factor(predicted!=fishing), pch=19))

 with(complete_dat[test_i,], plot(x, y,asp=1,cex=.15, col = pred_rf1$pred+1))
 with(complete_dat[test_i,][which(pred_rf1$pred!=pred_rf1$true & pred_rf1$pred==0),], points(x, y,asp=1,cex=.5, col = "steelblue"))
 with(complete_dat[test_i,][which(pred_rf1$pred!=pred_rf1$true & pred_rf1$pred!=0),], points(x, y,asp=1,cex=.5, col = "green"))
 
 
 ggplot(complete_dat, aes(x=speed_kmph)) + geom_histogram(aes(fill=as.factor(fishing)))
 ggplot(complete_dat, aes(x=speed_kmph)) + geom_density(aes(fill=as.factor(fishing)),alpha=.75)
 
 ggplot(complete_dat[test_i,], aes(x=speed_kmph)) + geom_histogram(aes(fill=as.factor(fishing)))
 ggplot(complete_dat[test_i,], aes(x=speed_kmph)) + geom_density(aes(fill=as.factor(fishing)))
 
 ggplot(complete_dat[test_i,][which(pred_rf1$pred!=pred_rf1$true),], 
        aes(x=speed_kmph)) + geom_histogram(aes(fill=as.factor(fishing)))
 ggplot(complete_dat[test_i,][which(pred_rf1$pred!=pred_rf1$true),], 
        aes(x=speed_kmph)) + geom_density(aes(fill=as.factor(fishing)))
 
 ggplot(complete_dat[test_i,][which(pred_rf1$pred==pred_rf1$true),], 
        aes(x=speed_kmph)) + geom_histogram(aes(fill=as.factor(fishing)))
 ggplot(complete_dat[test_i,][which(pred_rf1$pred==pred_rf1$true),], 
        aes(x=speed_kmph)) + geom_density(aes(fill=as.factor(fishing)),alpha=.8)
 
 ggplot(complete_dat[test_i,][which(pred_rf1$pred==pred_rf1$true),], aes(x=avg.speed)) + geom_histogram(aes(fill=as.factor(fishing)))
 ggplot(complete_dat[test_i,][which(pred_rf1$pred!=pred_rf1$true),], aes(x=avg.speed)) + geom_histogram(aes(fill=as.factor(fishing)))
 ggplot(complete_dat[test_i,][which(pred_rf1$pred==pred_rf1$true),], aes(x=dist_coast)) + geom_density(aes(fill=as.factor(fishing)),alpha=.8)
 ggplot(complete_dat[test_i,][which(pred_rf1$pred!=pred_rf1$true),], aes(x=dist_coast)) + geom_density(aes(fill=as.factor(fishing)),alpha=.8)
 
 ggplot(complete_dat[test_i,][which(pred_rf1$pred==pred_rf1$true),], aes(x=persistence_velocity)) + geom_histogram(aes(fill=as.factor(fishing)))
 ggplot(complete_dat[test_i,][which(pred_rf1$pred==pred_rf1$true),], aes(x=persistence_velocity)) + geom_density(aes(fill=as.factor(fishing)),adjust=2)
 
 ggplot(training_dat_norm[test_i,][which(pred_rf1$pred==pred_rf1$true),], aes(x=persistence_velocity)) + geom_density(aes(fill=as.factor(fishing)),adjust=2)
 # looking critically at features: do they seperate, what does a time series of
 # these things look like?
 
 # subset 1 single trip, speed and turning angles
 t1 <- subset(complete_dat, id == testids[20])
 plot(x=t1$date, y=t1$speed_kmph,type='h',col = t1$fishing+1)
 plot(x=t1$date, y=t1$spd_nrom, type='h',col=t1$fishing+1)
 plot(x=t1$date, y=t1$dist_coast,type='h',col = t1$fishing+1)
 plot(x=t1$speed_kmph, y=t1$dist_coast,type='p',col = t1$fishing+1)
 plot(x=t1$date, y=t1$dist,type='h',col = t1$fishing+1)

 
 plot(t1$x, t1$y, col = t1$fishing+1, cex = .5,pch=19,asp=1)
 lines(t1$x, t1$y, col='grey',lwd=.5)
 plot(WC, add = T, col='grey20',bor="white")
 ggplot(t1, aes(x = x, y = y)) + geom_path() + 
   geom_point(aes(shape=as.factor(fishing), color = predicted)) + 
   coord_equal() + theme_minimal()
 
 plot(t1$date, t1$fishing*2,type='h')
 points(t1$date, t1$predicted, col='blue',type='h')
