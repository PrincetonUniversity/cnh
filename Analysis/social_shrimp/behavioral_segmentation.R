# load training data and train RF
library(dplyr)
library(ggplot2)
library(broom)
library(adehabitatLT)
library(maps)
library(sp)
library(rgdal)

# look in trip_tot_dat to find vessel IDs that have observed shrimp trips ----
  # choose time window
  window = 24
  
fp <- paste0("processedData/spatial/vms/intermediate/05_make_obs_to_vms/trip_total_tw",
             window,"hr.csv")
  trip_tot_dat <- read.csv(fp) %>%
    filter(sector=="Pink Shrimp") %>%
    dplyr::select(vessel.ID) %>%
    distinct()

# load VMS and subset to observed points ----
  fp <- paste0("processedData/spatial/vms/intermediate/05_make_obs_to_vms/tw_",
               window,"hr/",trip_tot_dat$vessel.ID,".csv")
  vms_files <- lapply(fp, read.csv)
  vms_files <- do.call(rbind, vms_files)
  
  vms_files <- vms_files %>%
    filter(!(is.na(observed))) # 
  
# generate movement statistics ----
  
  # need to make an ID that is only_trips + doc.num to get unique trips 
  # other groupings may aggregate multiple trips. 
  # shouldn't happen because observed, but does look like it's happening 
  vms_files$date.time <- as.POSIXct(vms_files$date.time, tz = "Etc/GMT-8")
  vms_files$trip_id1 <- as.character(vms_files$trip_id1)
  vms_files <- vms_files[order(vms_files$doc.num, vms_files$date.time),]
  # to make burst ids need to drop 0s
  vms_files <- subset(vms_files, only_trips!=0)
  vms_files$burst_id <- paste(vms_files$only_trips, vms_files$doc.num, sep="_")
  
  # test to make sure all trips correspond to one burst_id
    all_ids <- 1:length(unique(vms_files$trip_id1))
    n_burst = function(x){
      return(length(unique(
        subset(vms_files,
               trip_id1==unique(vms_files$trip_id1)[x])$burst_id)))
    }

    count_bursts = sapply(all_ids, n_burst)
    which(count_bursts>1)
    prob_trips <- unique(vms_files$trip_id1)[which(count_bursts>1)]
  
    states <- map_data('state',region=c("Washington","Oregon"))
    ggplot(subset(as.data.frame(vms_files), trip_id1==prob_trips[2]), 
           aes(x = longitude, y = latitude, group = burst_id)) + 
      xlim(c(-125,-123)) + ylim(c(45.8,49)) +
      geom_path(aes(color=burst_id)) + coord_equal() + 
      geom_polygon(data = states, aes(x = long, y= lat,group=group)) +
      theme_minimal() 
    
  # make spatial
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
  
  # add coastline for ref
  load("processedData/spatial/2_coastline.Rdata")
  proj4string(WC) <- proj4string(vms_files)
  WC <- spTransform(WC, proj4string(vms_proj))
  
  dat_lt <- as.ltraj(coordinates(vms_proj), date = vms_proj$date.time, 
                     id = vms_proj$burst_id)
  dat <- ld(dat_lt)
  
  dat$id <- as.character(dat$id)
  
  # add observed and fishing data to dat
  full_dat <- left_join(dat, dplyr::select(as.data.frame(vms_files), burst_id, 
                                      date.time, fishing, observed, dist_coast),
                   by = c("id" = "burst_id","date"="date.time"))
  
# make sure training data is clean ----
  # are there gaps in trips > 2 hours?
  length(which(dat$dt>4*60*60))
  ggplot(dat, aes(x = dt)) + geom_histogram() + 
    theme_minimal() + scale_x_log10() + scale_y_sqrt()
  
  # subset to those trips
  miss_trips <- unique(dat$id[which(dat$dt> 4*60*60)])
  # about a 15% have this
  length(miss_trips)/length(unique(dat$id))
  
  head(miss_trips)
  head(which(dat$dt>4*60*60))
  
  library(broom)
  wc_g <- tidy(WC, region = "id")
  ggplot(subset(full_dat, id == miss_trips[8]), aes(x, y, group = id)) + 
    geom_path(aes(color=dt/3600), size = .5)  + 
    coord_equal() + theme_minimal() + 
    geom_polygon(data = wc_g, aes(x = long, y = lat, group = group)) + 
    ylim(c(4500000, 5000000))
  
  # going to drop these trips with > 4 hours diff
  full_dat <- subset(full_dat, !(id %in% miss_trips))
  ggplot(full_dat, aes(x = dt/3600)) + geom_histogram() + 
    theme_minimal() + scale_x_log10() + scale_y_sqrt()
  
  # look at speeds
  full_dat$speed_kmph = (full_dat$dist/1000)/(full_dat$dt/3600)
  ggplot(full_dat, aes(x = speed_kmph)) + geom_histogram() +
    theme_minimal() + scale_x_sqrt() + scale_y_sqrt()
  
  # what's going on with speeds > 40kmph?
  fast_trips <- unique(full_dat$id[which(full_dat$speed_kmph> 40)])
  # about 5% of trips have this
  length(fast_trips)/length(unique(full_dat$id)) 
  
  t1 <- subset(full_dat, id == fast_trips[1])
  ggplot(t1, aes(x, y, group = id)) + 
    geom_path(aes(color=speed_kmph), size = .5)  + 
    coord_equal() + theme_minimal() + 
    geom_polygon(data = wc_g, aes(x = long, y = lat, group = group))
  
  # inspecting this one, looks like a very small dt where boat doesn't move for a second
  drp_ids <- which(full_dat$dx==0 & full_dat$dy==0 & full_dat$dt==60)
  
  # drop these
  filtered_dat <- full_dat[-drp_ids,]
  
  # drop also anything with dt == 180
  drp_ids <- which(filtered_dat$dt<=180)
  filtered_dat <- filtered_dat[-drp_ids,]
  
  
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
  
  # t1 <- subset(fld, id == fast_trips[1])
  # ggplot(t1, aes(x, y, group = id)) + 
  #   geom_path(aes(color=speed_kmph), size = .5)  + 
  #   coord_equal() + theme_minimal() + 
  #   geom_polygon(data = wc_g, aes(x = long, y = lat, group = group))
  
  # what are the NAs for dt and dist?
  # are at end of trajectory
  
  fld <- left_join(fld, dplyr::select(as.data.frame(vms_files), burst_id, 
                                           date.time, fishing, observed, dist_coast),
                        by = c("id" = "burst_id","date"="date.time"))
  
  # need to have trips that have at least 6 relocs
  n.locs <- table(fld$id)
  n_big <- names(n.locs)[which(n.locs>6)]
  
  complete_dat <- fld %>%
    filter(id %in% n_big) %>%
    dplyr::select(rel.angle, abs.angle, R2n, dist, speed_kmph, fishing, id,
                  x, y, date, dist_coast) %>%
    mutate(fishing = as.numeric(fishing)) %>% # add new feature
    group_by(id) %>%
    mutate(mean_speed = mean(speed_kmph,na.rm=T), 
           spd_norm_mean = speed_kmph-mean_speed, 
           median_speed = median(speed_kmph, na.rm = T),
           spd_norm_median = speed_kmph - median_speed)
  
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
  
  complete_dat$fishing <- as.numeric(!(is.na(complete_dat$fishing)))
  
  complete_dat <- complete_dat[complete.cases(complete_dat),]
  
# split up and prepare for RF
  training_dat <- dplyr::select(as.data.frame(complete_dat), -fishing, -id, -x, -y, -date)
  
  # normalize
  training_dat_norm <- apply(training_dat,2,function(x) (x - min(x))/diff(range(x)))

  
  # make response variable
  y = factor(complete_dat$fishing)
  
# subset to 60/40 and run RF - subset by whole trips though
  all_ids <- unique(complete_dat$id)
  train_n = floor(length(all_ids)*.6)
  trainingids <- sample(all_ids, size = train_n)
  testids <- all_ids[-which(all_ids %in% trainingids)]
  
  train_i <- which(complete_dat$id %in% trainingids)
  test_i <- which(complete_dat$id %in% testids)

  library(randomForest)
  library(rfUtilities)
  tuning <- rf.modelSel(xdata = training_dat_norm[train_i,], ydata = y[train_i])
  
  rf1 <- randomForest(x = training_dat_norm[train_i, tuning$selvars], 
                      y = y[train_i],mtry = 2)
  
# predict out to withheld data
  
 pred_rf1 <-   predict(rf1, training_dat[test_i,tuning$selvars], type = "prob")
 colnames(pred_rf1) <- paste0("p",colnames(pred_rf1))
 pred_rf1 <- as.data.frame(pred_rf1)
 pred_rf1$true <- y[test_i]
 pred_rf1$pred <- ifelse(pred_rf1$p1>.15, 1, 0)
 
 table(pred_rf1$true, pred_rf1$pred,deparse.level = 2)
 
 complete_dat$predicted <- NA
 complete_dat$predicted[test_i] <- pred_rf1$p1
 # looking critically at features: do they seperate, what does a time series of
 # these things look like?
 
 # subset 1 single trip, speed and turning angles
 t1 <- subset(complete_dat, id == testids[20])
 plot(t1$date, t1$speed_kmph,type='h',col = t1$fishing+1)
 plot(t1$date, t1$spd_nrom, type='h',col=t1$fishing+1)
 plot(t1$date, t1$dist_coast,type='h',col = t1$fishing+1)
 plot(t1$speed_kmph, t1$dist_coast,type='p',col = t1$fishing+1)
 plot(t1$date, t1$dist,type='h',col = t1$fishing+1)
 
 plot(t1$x, t1$y, col = t1$fishing+1, cex = .5,pch=19,asp=1)
 lines(t1$x, t1$y, col='grey',lwd=.5)
 plot(WC, add = T, col='grey20',bor="white")
 ggplot(t1, aes(x = x, y = y)) + geom_path() + 
   geom_point(aes(shape=as.factor(fishing), color = predicted)) + 
   coord_equal() + theme_minimal()
 
 plot(t1$date, t1$fishing*2,type='h')
 points(t1$date, t1$predicted, col='blue',type='h')
 
# additional feature ideas ---
# displacement of 3, 4, 5, 6, hours. Can't go much higher because will drop trips. 