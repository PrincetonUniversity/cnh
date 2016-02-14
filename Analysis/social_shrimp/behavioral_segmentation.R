# load training data and train RF
library(dplyr)
library(ggplot2)
library(broom)
library(adehabitatLT)
library(maps)
library(sp)

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
    filter(!(is.na(observed)))
  
# generate movement statistics ----
  
  # need to make an ID that is only_trips + doc.num to get unique trips 
  # other groupings may aggregate multiple trips. 
  # shouldn't happen because observed, but does look like it's happening 
  vms_files$date.time <- as.POSIXct(vms_files$date.time, tz = "Etc/GMT-8")
  vms_files$trip_id1 <- as.character(vms_files$trip_id1)
  vms_files <- vms_files[order(vms_files$doc.num, vms_files$date.time),]
  vms_files$burst_id <- paste(vms_files$only_trips, vms_files$doc.num, sep="_")
  
  # test to make sure all trips correspond to one burst_id
    # all_ids <- 1:length(unique(vms_files$trip_id1))
    # n_burst = function(x){
    #   return(length(unique(
    #     subset(vms_files,
    #            trip_id1==unique(vms_files$trip_id1)[x])$burst_id)))
    # }
    # 
    # count_bursts = sapply(all_ids, n_burst)
    # which(count_bursts>1)
  prob_trips <- unique(vms_files$trip_id1)[which(count_bursts>1)]

  with(subset(vms_files, trip_id1==prob_trips[1]),
       plot(longitude, latitude, asp = 1, type='o',cex=.5,
            col = as.factor(burst_id)))
  map('state',add=T, col='darkolivegreen4', fill=T, bor = "grey")
  
  with(subset(vms_files, trip_id1==unique(vms_files$trip_id1)[]),
       plot(longitude, latitude, asp = 1, type='o',cex=.5))
  map('state',add=T, col='darkolivegreen4', fill=T, bor = "grey")
  
  
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
  
  vms_proj <- spTransform(vms_files, 
                          CRS("+proj=eqdc +lat_1=33.040181240749774 +
                              lat_2=45.838330604411105 +lon_0=-125.068359375"))
  # add coastline for ref
  load("processedData/spatial/2_coastline.Rdata")
  proj4string(WC) <- proj4string(vms_files)
  WC <- spTransform(WC, proj4string(vms_proj))
  
  vms_proj$date.time <- as.POSIXct(vms_proj$date.time, tz = "Etc/GMT-8")
  vms_proj$trip_id1 <- as.character(vms_proj$trip_id1)
  dat_lt <- as.ltraj(coordinates(vms_proj), date = vms_proj$date.time, 
                     id = vms_proj$trip_id1)
  dat <- ld(dat_lt)
  
  dat$id <- as.character(dat$id)
  # add observed and fishing data to dat
  dat <- left_join(dat, dplyr::select(as.data.frame(vms_files), trip_id1, 
                                      date.time, fishing, observed),
                   by = c("id" = "trip_id1","date"="date.time"))
  # add speed
  dat$speed <- dat$dist/dat$dt
  
# make sure training data is clean ----
  # are there gaps in trips > 2 hours?
  length(which(dat$dt>2*60*60))
  
  # subset to those trips
  miss_trips <- unique(dat$id[which(dat$dt> 2*60*60)])
  # about a fifth have this
  length(miss_trips)/length(unique(dat$id))
  
  library(broom)
  wc_g <- tidy(WC, region = "id")
  ggplot(subset(dat, id %in% miss_trips), aes(x, y, group = id)) + 
    geom_point(aes(color=id), size = .05) + theme(legend.position="None") + 
    coord_equal() + 
    geom_polygon(data = wc_g, aes(x = long, y = lat, group = group)) 

  plot(dat$x, dat$y, asp = 1, col = ifelse(dat$dt>2*60*60, "red","grey"), 
       cex = .15)
  
  # what are the NAs for dt and dist?
  
  # are there any huge jumps?
  
  complete_dat <- dat %>%
    dplyr::select(rel.angle, abs.angle, R2n, dist, speed, fishing) 
  
  complete_dat$fishing <- as.numeric(is.na(complete_dat$fishing))
  
  complete_dat <- complete_dat[complete.cases(complete_dat),]
  training_dat <- dplyr::select(complete_dat, -fishing)
  
  # make response variable
  y = factor(complete_dat$fishing)
  
# subset to 60/40 and run RF
  trainingids <- sample(1:nrow(complete_dat), size = floor(nrow(complete_dat)*.6))
  testids <- 1:nrow(complete_dat)
  testids <- testids[-which(testids %in% trainingids)]
  
  library(randomForest)
  library(rfUtilities)
  tuning <- rf.modelSel(xdata = training_dat[trainingids,], ydata = y[trainingids])
  
  rf1 <- randomForest(x = training_dat[trainingids, tuning$selvars], y = y[trainingids],mtry = 2)
  
# predict out to withheld data
  
 pred_rf1 <-   predict(rf1, training_dat[testids,tuning$selvars], type = "prob")
 colnames(pred_rf1) <- paste0("p",colnames(pred_rf1))
 pred_rf1 <- as.data.frame(pred_rf1)
 pred_rf1$true <- y[testids]
 pred_rf1$pred <- ifelse(pred_rf1$p1>.1, 1, 0)
 