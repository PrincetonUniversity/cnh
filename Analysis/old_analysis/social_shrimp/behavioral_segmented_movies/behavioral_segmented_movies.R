# making some movies using behaviorally segmented vms data

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
vms <- readRDS("processedData/spatial/vms/intermediate/06_predict_fishing/vms_classified.RDS")

# make movies ----

make_pdfs <- function(doc_num){
  sub_dat <- subset(vms, doc.num==doc_num)
  sub_dat$date.time <- as.POSIXct(as.character(sub_dat$date.time), 
                                  format = "%Y-%m-%d %H:%M:%S", 
                                  tz = "Etc/GMT-8")
  sub_dat <- sub_dat[order(sub_dat$date.time),]
  sub_dat$paint <- "red"
  sub_dat$paint[which(sub_dat$predicted_fishing==0)] <- "grey"
  sub_dat$cex <- .2
  xlim = range(sub_dat$longitude)
  ylim = range(sub_dat$latitude)
  
  pdf(paste0("Analysis/social_shrimp/behavioral_segmented_movies/",doc_num,".pdf"), 
      width = 8, height = 8)
  par(bg="black", mai=rep(0,4))
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
    map('state',add=T, fill=TRUE, col='grey20')
  }
  dev.off()
}

vs <- unique(vms_files$doc.num)
for(j in 1:length(vs)){
  make_pdfs(vs[j])
}

# make a movie of all vessels together

# add density surface for patches
library(MASS)
h <- kde2d(x = vms$longitude,vms$latitude, n = c(50,200))
make_together_pdf<- function(vms){
  
  fn = "Analysis/social_shrimp/behavioral_segmented_movies/shrimp_vessels.pdf"
  pdf(file=fn)
  
  # round vessels to nearest hour
  vms$adj_time <- format(round(vms$date.time, units="hours"),
                         format="%Y-%m-%d %H:%M:%S")
  vms$adj_time <- as.POSIXct(vms$adj_time,
                             format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-8")
  vms_all <- vms %>% 
    dplyr::select(doc.num, latitude, longitude, adj_time, predicted_fishing) %>%
    distinct(doc.num, adj_time) # drops multiple points in an hour
  
  time_list <- split(vms_all, vms_all$adj_time)
  
  xlim = range(vms$longitude)
  ylim = range(vms$latitude)
  
  for(i in 10:length(time_list)){
    # slice time list
    subs <- do.call(rbind, time_list[(i-9):i])
    subs$doc.num= as.character(subs$doc.num)
    
    # make alpha matching to datetime
    alpha_df <- data.frame(adj_time = unique(subs$adj_time),
                           alphas = seq(.1,1, length.out = length(unique(subs$adj_time))))
    subs <- merge(subs, alpha_df)
    subs$paint <- ifelse(subs$predicted_fishing==0, "black","red")
    # find all vessels
    all_ves <- unique(subs$doc.num)
    # plot each vessels' trajectory
    for(j in 1:length(all_ves)){
      sub_vs <- subset(subs, doc.num == all_ves[j])
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
                          pch=19, cex=.25, col = alpha(paint,alpha=alphas)))
    }
    map('state',add=T)
    contour(h,asp=1, levels = 0.85, drawlabels = FALSE,
            col = "steelblue",add = TRUE)
  }
  dev.off()
}
make_together_pdf(vms)

