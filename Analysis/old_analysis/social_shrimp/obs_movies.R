# making some movies

library(dplyr)
library(ggplot2)
library(broom)
library(adehabitatLT)
library(maps)
library(sp)
library(rgdal)
library(scales)

# look in trip_tot_dat to find vessel IDs that have observed shrimp trips ----
# choose time window
window = 24

# get shrimp vessel IDs
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

vms_files$date.time <- as.character(vms_files$date.time)
vms_files$date.time <- as.POSIXct(vms_files$date.time, 
                                format = "%Y-%m-%d %H:%M:%S", 
                                tz = "Etc/GMT-8")
vms_files <- vms_files %>%  mutate(vessel.name = as.character(vessel.name))

# make movies ----

make_pdfs <- function(vessel_name){
  library(maps)
  library(scales)
  ves <- gsub(" ","_", vessel_name)
  ves <- gsub("[.]","_", ves)
  fn = paste0("Analysis/social_shrimp/obs_movies/", ves, ".pdf")
  pdf(file=fn)
  vms_files$metier.2010 <- as.character(vms_files$metier.2010)
  sub_dat <- subset(vms_files, vessel.name==vessel_name & metier.2010=="TWS_1"
                    & !is.na(fishing))
  sub_dat$date.time <- as.POSIXct(as.character(sub_dat$date.time), 
                                  format = "%Y-%m-%d %H:%M:%S", 
                                  tz = "Etc/GMT-8")
  sub_dat <- sub_dat[order(sub_dat$date.time),]
  sub_dat$paint <- "red"
  sub_dat$paint[which(sub_dat$fishing=="0")] <- "grey"
  sub_dat$paint[which(is.na(sub_dat$fishing))] <- "black"
  sub_dat$cex <- .2
  sub_dat$cex[which(sub_dat$avg.speed<5 & sub_dat$fishing=="0")] <- .5
  xlim = range(sub_dat$longitude)
  ylim = range(sub_dat$latitude)
  for(i in 10:nrow(sub_dat)){
    rowz = ifelse(nrow(sub_dat)<10, nrow(sub_dat),9)
    subs <- sub_dat[(i-rowz):i,]
    subs$alphas = seq(.1,1, length.out = nrow(subs))
    with(subs, plot(longitude, latitude, asp = 1, xlim = xlim, ylim = ylim,
                      type='l',pch=19, cex=.2, 
                      col = alpha('grey',.25),
                    main = subs$date.time[i]))
    with(subs, points(longitude, latitude, asp = 1,
                    pch=19, cex=subs$cex, 
                    col = alpha(paint,
                    alpha=subs$alphas)))
    # with(subs, points(c(set_long[10],up_long[10]), c(set_lat[10], up_lat[10]), 
    #                   cex=.75, pch = 3, col='steelblue'))
    map('state',add=T)
  }
  dev.off()
}

vs <- unique(vms_files$vessel.name)
for(j in 1:length(vs)){
  make_pdfs(vs[j])
}

# make a movie of all vessels together

# add density surface for patches
library(MASS)
h <- kde2d(x = vms_files$longitude,vms_files$latitude, n = c(50,200))
make_together_pdf<- function(vms_files){
 
  fn = paste0("Analysis/social_shrimp/obs_movies/shrimp_vessels.pdf")
  pdf(file=fn)
  
  # round vessels to nearest hour
  vms_files$adj_time <- format(round(vms_files$date.time, units="hours"), 
                               format="%Y-%m-%d %H:%M:%S")
  vms_files$adj_time <- as.POSIXct(vms_files$adj_time, 
                                   format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-8")
  vms_all <- vms_files %>% 
    dplyr::select(vessel.name, latitude, longitude, adj_time, fishing, 
                  set_long, set_lat, up_long, up_lat) %>%
    distinct(vessel.name, adj_time)
  
  time_list <- split(vms_all, vms_all$adj_time)
  
  xlim = range(vms_files$longitude)
  ylim = range(vms_files$latitude)
  
  for(i in 10:length(time_list)){
    # slice time list
    subs <- do.call(rbind, time_list[(i-9):i])
    subs$vessel.name= as.character(subs$vessel.name)
    
    # make alpha matching to datetime
    alpha_df <- data.frame(adj_time = unique(subs$adj_time),
                           alphas = seq(.1,1, length.out = length(unique(subs$adj_time))))
    subs <- merge(subs, alpha_df)
    
    # find all vessels
    all_ves <- unique(subs$vessel.name)
    # plot each vessels' trajectory
    for(j in 1:length(all_ves)){
      sub_vs <- subset(subs, vessel.name == all_ves[j])
      if(j == 1){
        with(sub_vs, plot(longitude, latitude, asp = 1, xlim = xlim, ylim = ylim,
                        type='l', cex=.2, 
                        col = alpha('grey',.25),
                        main = tail(subs$adj_time,1)))
      }else{
        with(sub_vs, lines(longitude, latitude, asp = 1, xlim = xlim, ylim = ylim,
                        cex=.2, col = alpha('grey',.25)))
        
      }
      
      with(sub_vs, points(longitude, latitude, asp = 1, xlim = xlim, ylim = ylim,
                        pch=19, cex=.25, 
                        col = alpha(as.numeric(!is.na(fishing))+1,
                                    alpha=sub_vs$alphas)))
      with(sub_vs, points(c(set_long,up_long), c(set_lat, up_lat), 
                          cex=.75, pch = 3, col='steelblue'))
    }
    map('state',add=T)
    contour(h,asp=1, levels = 0.85, drawlabels = FALSE,
            col = "steelblue",add = TRUE)
  }
  dev.off()
}
make_together_pdf(vms_files)

