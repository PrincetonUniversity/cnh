# load training data and train RF
library(dplyr)

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
  library(adehabitatLT)
  
  # make spatial
  coordinates(vms_files) <- ~longitude+latitude
  proj4string(vms_files) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  # probably should project
  
  vms_files$date.time <- as.POSIXct(vms_files$date.time, tz = "Etc/GMT-8")
  vms_files$trip_id1 <- as.character(vms_files$trip_id1)
  dat_lt <- as.ltraj(coordinates(vms_files), date = vms_files$date.time, 
                     id = vms_files$trip_id1)
  
  dat <- ld(dat_lt)
  dat$id <- as.character(dat$id)
  # add observed and fishing data to dat
  dat <- left_join(dat, dplyr::select(as.data.frame(vms_files), trip_id1, date.time, fishing, observed),by = c("id" = "trip_id1","date"="date.time"))
  dat$speed <- dat$dist/dat$dt
  
# make sure training data is clean ----
  # are there gaps in trips > 2 hours?
  
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
 