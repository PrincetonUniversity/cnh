# defining shrimp fishing patches
library(dplyr)
library(sp)
library(ggplot2)
library(maps)

# load VMS data ----
shrimps <- readRDS("processedData/spatial/vms/intermediate/06_predict_fishing/vms_classified.RDS") %>% 
  filter(metier.2010 == "TWS_1") %>%
  mutate(trip_id1 = as.character(trip_id1))

# defining fishing grounds based on fishing locations ----
  # build patches, return as spatial polygon dataframes in lat/lon with ID field
  define_fishing_patches <- function(fishing_coordinates){
    library(spatstat)
    library(maptools)
    library(raster)
    library(rgeos)
    
    # just take fishing points
    shrimps_fishing <- fishing_coordinates %>%
      filter(predicted_fishing==1)
    # assign lat/lon 
    coordinates(shrimps_fishing) <- ~longitude+latitude
    proj4string(shrimps_fishing) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    # project for density calculations and rasters
    shrimps_fishing <- spTransform(shrimps_fishing, CRS("+proj=aea +lat_0=44.25 +lon_0=-124"))
    
    # build density-medoids and assign all fishing points to nearest neighbors
    density_polygons <- function(fishing_locs = shrimps_fishing, 
                                 adjust_dens = .15, contour_levels=.45e-05){
      sSp <- as(SpatialPoints(fishing_locs), "ppp")  # convert points to pp class
      Dens <- density(sSp,adjust=adjust_dens)  # create density object
      
      Dsg <- as(Dens, "SpatialGridDataFrame")  # convert to spatial grid class
      Dim <- as.image.SpatialGridDataFrame(Dsg)  # convert again to an image
      Dcl <- contourLines(Dim, levels=contour_levels )  # create contour object - change 8 for more/fewer levels
      SLDF <- ContourLines2SLDF(Dcl)  # convert to SpatialLinesDataFrame
      
      Polyclust <- gPolygonize(SLDF)
      pid <- sapply(slot(Polyclust, "polygons"), function(x) slot(x, "ID")) 
      p.df <- data.frame( ID=1:length(Polyclust), row.names = pid)   
      p <- SpatialPolygonsDataFrame(Polyclust, p.df)
      proj4string(p) <- proj4string(fishing_locs)
      
      return(p)
    }
    p <- density_polygons()
    shrimps_fishing@data$patch_id <- apply(gDistance(p,shrimps_fishing,byid=TRUE),
                                           1, which.min)
    
    # for each set of classified points, build raster of 2km resolution
    r <- raster(shrimps_fishing,res=2000) 
    raster_patch <- rasterize(shrimps_fishing, r, field = "patch_id")
    poly_patch <- rasterToPolygons(raster_patch, dissolve=T)
    colnames(poly_patch@data) <- "IDs"
    
    drop_holes <- function(shapes){
      NZp <- slot(shapes, "polygons")
      holes <- lapply(NZp, function(x) sapply(slot(x, "Polygons"), slot, "hole"))
      res <- lapply(1:length(NZp), function(i) slot(NZp[[i]], "Polygons")[!holes[[i]]])
      IDs <- row.names(poly_patch)
      NZfill <- SpatialPolygons(lapply(1:length(res), function(i) 
        Polygons(res[[i]], ID=IDs[i])), proj4string = CRS(proj4string(shapes)))
      poly_patch_fill <- SpatialPolygonsDataFrame(NZfill, data = data.frame(IDs=unique(shrimps_fishing@data$patch_id)))
      return(poly_patch_fill)
    }
    poly_patch_fill <- drop_holes(poly_patch)
    # drop any internal holes and reproject to latlon
    
    # buffer to make sure all points stay within, some fall back out due to projections?
    buffer_poly <- gBuffer(poly_patch_fill, width = 2100, byid=TRUE,joinStyle = "MITRE",capStyle = "FLAT")
    buffer_fill <- drop_holes(buffer_poly)
    
    patches_latlon <- spTransform(buffer_fill, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    
    return(patches_latlon)
  }

  patches <- define_fishing_patches(fishing_coordinates = shrimps)

# now re-overlay all points including fishing. To ensure that IDs are consistently numbered
  coordinates(shrimps) <- ~longitude+latitude
  proj4string(shrimps) <- proj4string(patches)
  shrimps$patch <- over(shrimps, patches)[,1]
  
  # check to make sure all fishing points are on a patch
  table(shrimps$predicted_fishing, !is.na(shrimps$patch))
  
  shrimps <- as.data.frame(shrimps, stringsAsFactors = FALSE)

# find occupation status of patch for each trip ----
shrimps$adj_time <- format(round(shrimps$date.time, units="hours"), 
                       format="%Y-%m-%d %H:%M:%S")
shrimps$adj_time <- as.POSIXct(shrimps$adj_time, 
                           format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-8")

unique_occurances <- shrimps %>%
  filter(!is.na(patch)) %>%
  dplyr::select(adj_time, doc.num, patch) %>%
  distinct()

# counting number of vessels on each patch at each time
big_table <- as.data.frame(table(unique_occurances$adj_time, unique_occurances$patch),
                           stringsAsFactors = FALSE)
colnames(big_table) <- c("adj_time","patch","n_vessels") 
big_table$patch <- as.numeric(big_table$patch)
big_table$adj_time <- as.POSIXct(big_table$adj_time, format="%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-8")

# for each vessel, how often on a patch fishing when occupied?
fishing_points <- shrimps %>%
  filter(predicted_fishing==1) %>%
  dplyr::select(adj_time, patch, doc.num, trip_id1) %>%
  distinct() %>%
  group_by(doc.num) %>%
  left_join(big_table) %>%
  mutate(n_vessels = ifelse(n_vessels==0, 0, n_vessels-1))
  
fishing_occupation <- fishing_points %>%
  group_by(doc.num, trip_id1) %>%
  summarize(percent_occupied = length(which(n_vessels > 0))/length(n_vessels))

# sidebar, let's add median latitude to color
median_lat <- shrimps %>%
  filter(predicted_fishing==1) %>%
  group_by(doc.num) %>%
  summarize(median_lat = median(latitude))

fishing_occupation %>% left_join(median_lat) %>%
ggplot(aes(x=reorder(factor(doc.num), percent_occupied, median), y = percent_occupied,
           fill=median_lat)) + 
  geom_boxplot() + 
  ylab("percent of time vessel spent in occupied patches") +
  xlab("vessel ID") + theme_minimal() + scale_fill_continuous(low="indianred",high="steelblue")

# reshuffling based on trips part 1: possible patches ----
# so for each trip, find maximum distance out.
# then draw buffer around that, and look for intersection of different patches. 

boots <- shrimps %>%
  group_by(doc.num, trip_id1) %>%
  arrange(adj_time) %>%
  summarize(max_dist = max(spDistsN1(pts = cbind(longitude, latitude),
                       pt = cbind(longitude[1],latitude[1]),
                       longlat = TRUE)), 
            start_lat = latitude[1], 
            start_lon = longitude[1],
            start_time = head(adj_time,1),
            end_time = tail(adj_time,1))
# calculates a unit circle 
find_possible_patches <- function(max_dist, start_lon, start_lat, trip_df, 
                                  trip_id, plot=FALSE){
  max_dist <- max_dist/111.32 # convert to decimal degrees 
  
  # build a circle from center point with radius = max dist traveled
  t <- seq(0,2*pi,length=100) 
  coords <-  SpatialPoints(t(rbind( start_lon+sin(t)*max_dist, 
                                    start_lat+cos(t)*max_dist)), 
                           proj4string = CRS(proj4string(patches)))
  sp = SpatialPolygons( list(  Polygons(list(Polygon(coords)), 1)), 
                        proj4string = CRS(proj4string(coords)))

  possible_patches <- unique(unlist(over(sp, patches,returnList = TRUE)))
  if(any(is.na(possible_patches))){possible_patches <- possible_patches[-which(is.na(possible_patches))]}
  poss_df <- data.frame(trip_id1 = rep(trip_id, length(possible_patches)), 
                        possible_patches = possible_patches, stringsAsFactors = FALSE)
  if(plot){
    plot(coords, asp=1,cex=.01)
    plot(patches,add=T,col='grey90',bor='grey50')
    points(coords,pch=19, col='grey30')
    map('state',add=T,col='grey50',fill=T,bor=F)
    with(subset(shrimps, trip_id1==trip_id), points(longitude, latitude,type='o',pch=19, cex= .5))
  }
  return(poss_df)
}
poss_patch <- vector('list',nrow(boots))
for(i in 1:nrow(boots)){
  poss_patch[[i]] <- find_possible_patches(
  max_dist = boots$max_dist[i], start_lon = boots$start_lon[i], 
  start_lat = boots$start_lat[i], trip_df = boots[i,], trip_id = boots$trip_id1[i])
}  

hist(sapply(poss_patch, nrow),col='grey')
which(sapply(poss_patch,nrow)==0)
# one cross zero patches
i = 2236
find_possible_patches(
  max_dist = boots$max_dist[i], start_lon = boots$start_lon[i], 
  start_lat = boots$start_lat[i], trip_df = boots[i,], 
  trip_id = boots$trip_id1[i], plot = TRUE)

possible_patches <- do.call(rbind, poss_patch)
possible_patches$trip_id1 <- as.character(possible_patches$trip_id1)
# this drops the one trip that didn't have any overlap (and this is true, 
# consists of a single trip)

# reshuffling part 2: reshuffling -----
# for each of the patches that are selected, subset time present during trip
# reshuffle 0/1s across all patches

# find trip adj_time and patches from big table
# shuffle n_vessels row across both
# match to shrimps adj_time, doc.num and trip_id1
trip_boots <- vector("list",nrow(boots))
# make parallel
library(parallel)
cl <- makeCluster(3)
clusterEvalQ(cl, library(dplyr))
clusterEvalQ(cl, library(sp))
clusterExport(cl,c("shrimps","boots","possible_patches","big_table") )

trip_bootstraps <- function(fishing_locs = shrimps, boots_df = boots, possible_patches_df = possible_patches, big_table_df = big_table, i){
  t1_trip <- fishing_locs %>%
    dplyr::select(adj_time, predicted_fishing, trip_id1, doc.num) %>%
    filter(doc.num==boots_df$doc.num[i], trip_id1 == boots_df$trip_id1[i]) %>%
    distinct()
  
  ppatch <- possible_patches_df$possible_patches[which(
    possible_patches_df$trip_id1 == boots_df$trip_id1[i])]
  
  t1_lookup_df <- subset(big_table_df, adj_time >= boots_df$start_time[i] & 
                 adj_time<= boots_df$end_time[i] & 
                 patch %in% ppatch) %>%
    mutate(n_vessels = ifelse(n_vessels==0,0, n_vessels-1))
  
  # this is the randomizing step
  boot_patch_occupency <- function(I=i, fishing_locs_df = fishing_locs, big_table_lookup=big_table_df, boots_lookup=boots_df, ppatch_df=ppatch, t1_lookup = t1_lookup_df){
    t1_rand <- t1_lookup %>%
      mutate(n_vessels = n_vessels[sample(1:length(n_vessels), length(n_vessels), replace=FALSE)],
             doc.num = boots_lookup$doc.num[I], trip_id1 = boots_lookup$trip_id1[I]) %>%
      inner_join(dplyr::select(fishing_locs_df, adj_time, doc.num, trip_id1, predicted_fishing),
                 by = c("adj_time","doc.num","trip_id1")) %>%
      filter(predicted_fishing == 1) %>%
      summarize(percent_occupied = length(which(n_vessels>0))/length(n_vessels))
    return(t1_rand$percent_occupied)
  }
  boot_vec <- vector()
 for(j in 1:100){
    boot_vec[j] <- boot_patch_occupency()
  }
  return(boot_vec)
}

clusterExport(cl, "trip_bootstraps")

system.time(all_boots <- parLapply(cl, 1:nrow(boots), function(x) trip_bootstraps(i=x)))

stopCluster(cl)
  
 
saveRDS(all_boots, "Analysis/social_shrimp/boots_SN/all_boots100.RDS")
all_boots <- readRDS("Analysis/social_shrimp/boots_SN/all_boots100.RDS")

# looking at vessel level sociality: different than random? ----
boots_together <- as.data.frame(do.call(rbind, all_boots))
colnames(boots_together) <- paste0("strap",1:ncol(boots_together))
boots_together$trip_id1 <- boots$trip_id1
boots_together <- left_join(boots, boots_together, by = "trip_id1") %>%
  right_join(fishing_occupation) %>%
  ungroup()
for(i in 1:nrow(boots_together)){
  diff_vec <- boots_together$percent_occupied[i] - 
                    as.numeric(as.data.frame(dplyr::select(boots_together, contains("strap"))[i,],drop=TRUE))
  boots_together$delta_social[i] <- median(diff_vec)
  if(length(unique(diff_vec))==1){boots_together$pvalue[i] <- 0}else{
    boots_together$pvalue[i] <- t.test(diff_vec)$p.value
  }
}
boots_together <- dplyr::select(boots_together, -starts_with('strap'))

# fishing_occupation drops trips where there was no predicted fishing points

  ggplot(boots_together, aes(x = delta_social)) + geom_histogram() + theme_minimal()

  boots_together %>% 
  left_join(median_lat, by="doc.num") %>%
  ggplot(aes(x=reorder(factor(doc.num), delta_social, mean), y = delta_social,
             fill=median_lat)) + 
  geom_boxplot() + 
  ylab("difference in social from random") +
  xlab("vessel ID") + theme_minimal() + scale_fill_continuous(low="indianred",high="steelblue")
  
# load catch to start comparing revenue, lbs, and effort to sociability ----
  catch <- readRDS("processedData/catch/1_cleaningData/tickets.RDS")
  trip_stats <- catch %>%
    filter(metier.2010 == "TWS_1") %>%
    group_by(drvid, metier.2010, pcid, trip_id, year,grid) %>%
    summarize(revenue = sum(adj_revenue, na.rm = T), lbs = sum(landed_wt, na.rm = T)) %>%
    right_join(boots_together, by = c("trip_id"="trip_id1"))
  
  ggplot(trip_stats, aes(x = delta_social, y = lbs)) + geom_point() + facet_wrap(~year, ncol=1)
  ggplot(trip_stats, aes(x = delta_social, y = revenue)) + geom_point() + facet_wrap(~year, ncol=1)
  
  library(arm)
  # revenue
  lm1 <- lm(sqrt(revenue) ~ delta_social, trip_stats)
  lm2 <- lm(sqrt(revenue) ~ delta_social + drvid, trip_stats)
  lm3 <- lm(sqrt(revenue) ~ delta_social + year, trip_stats)
  lm4 <- lm(sqrt(revenue) ~ delta_social + year + drvid, trip_stats)
  lm5 <- lm(sqrt(revenue) ~ year + drvid, trip_stats)
  lm6 <- lm(sqrt(revenue) ~ year + drvid + delta_social + grid, trip_stats)
  
  
  AIC(lm1)-AIC(lm2) # > 0, 2 is better
  AIC(lm2)-AIC(lm3) # < 0, 2 is better
  AIC(lm4)-AIC(lm2) # < 0, 4 is better
  AIC(lm4)-AIC(lm5) # < 0, 4 is better
  AIC(lm6)-AIC(lm4) # < 0, 6 is better
  
  # best model is year, vessel ID and how social the trip was, plus type of rig
  
  # look at lbs
  lm1 <- lm(lbs ~ delta_social, trip_stats)
  lm2 <- lm(lbs ~ delta_social + drvid, trip_stats)
  lm3 <- lm(lbs ~ delta_social + year, trip_stats)
  lm4 <- lm(lbs ~ delta_social + year + drvid, trip_stats)
  lm5 <- lm(lbs ~ year + drvid, trip_stats)
  lm6 <- lm(lbs ~ delta_social + year + drvid + grid, trip_stats)
  
  AIC(lm1)-AIC(lm2) # > 0, 2 is better
  AIC(lm2)-AIC(lm3) # < 0, 2 is better
  AIC(lm4)-AIC(lm2) # < 0, 4 is better
  AIC(lm4)-AIC(lm5) # < 0, 4 is better
  AIC(lm6)-AIC(lm4) # < 0, 6 is better
  
  # use VMS to look at effort 
  trip_effort <- shrimps %>%
    group_by(trip_id1) %>% 
    summarize(median_fishing_lat = median(latitude[which(predicted_fishing==1)]),
              duration_hrs = as.numeric(difftime(tail(date.time,1), 
                                                 head(date.time,1), units = 'hours')),
              percent_fishing = sum(predicted_fishing)/length(date.time)) %>%
    right_join(trip_stats, by =c('trip_id1'="trip_id")) %>%
    ungroup() %>%
    mutate(bin_fishing_lat = cut(trip_effort$median_fishing_lat,seq(40,48,1)),
           sig_social = ifelse(pvalue < 0.05 & delta_social > 0, "sig_social",
                               ifelse(pvalue<0.05 & delta_social , "sig_anti", 
                                      "random")))
    
  
  ggplot(trip_effort, aes(x = delta_social, y = duration_hrs)) + geom_point()
  ggplot(trip_effort, aes(x = bin_fishing_lat, y = delta_social)) + 
    geom_boxplot() + theme_minimal()
  # similar pattern to revenue/lbs with latitude
  
  ggplot(trip_effort, aes(x = bin_fishing_lat, y = lbs)) + geom_boxplot() +
    theme_minimal()
  ggplot(trip_effort, aes(x = bin_fishing_lat, y = revenue)) + geom_boxplot() +
    theme_minimal()
  
  ggplot(trip_effort, aes(x = lbs, y = revenue, color = delta_social)) + geom_point()
  
  # look at lbs/revenue as function of how long trip was
  
  ggplot(subset(trip_effort, duration_hrs > 50), aes(x = delta_social, y = lbs/duration_hrs)) + 
    geom_point() + facet_wrap(~year, ncol=1, scale='free_y')
  ggplot(subset(trip_effort, duration_hrs > 50), aes(x = delta_social, y = revenue/duration_hrs)) + 
    geom_point(aes(col=median_fishing_lat)) + facet_wrap(~year, ncol=1, scale='free_y') + scale_color_continuous(low="indianred",high="steelblue")
  
  # just look at duration period
  ggplot(subset(trip_effort, duration_hrs > 50), aes( x =delta_social, y = duration_hrs)) +
    geom_point(aes(col=median_fishing_lat)) + scale_color_continuous(low="indianred",high="steelblue")
  
  
  # revenue
  lm1 <- lm(sqrt(revenue) ~ delta_social, subset(trip_effort, duration_hrs >50))
  lm2 <- lm(sqrt(revenue) ~ delta_social + drvid, subset(trip_effort, duration_hrs >50))
  lm3 <- lm(sqrt(revenue) ~ delta_social + year, subset(trip_effort, duration_hrs >50))
  lm4 <- lm(sqrt(revenue) ~ delta_social + year + drvid, subset(trip_effort, duration_hrs >50))
  lm5 <- lm(sqrt(revenue) ~ year + drvid, subset(trip_effort, duration_hrs >50))
  
  AIC(lm1)-AIC(lm2) # > 0, 2 is better
  AIC(lm2)-AIC(lm3) # < 0, 2 is better
  AIC(lm4)-AIC(lm2) # < 0, 4 is better
  AIC(lm4)-AIC(lm5) # < 0, 4 is better
  
  # what about adding median_lat
  
  lm6 <- lm(sqrt(revenue) ~ bin_fishing_lat, subset(trip_effort, duration_hrs>50))
  lm7 <- lm(sqrt(revenue) ~ bin_fishing_lat + year, subset(trip_effort, duration_hrs>50))
  lm8 <- lm(sqrt(revenue) ~ bin_fishing_lat + year + delta_social, subset(trip_effort, duration_hrs>50))
  lm9 <- lm(sqrt(revenue) ~ bin_fishing_lat + year + drvid + delta_social, subset(trip_effort, duration_hrs>50))
  lm10 <- lm(sqrt(revenue) ~ bin_fishing_lat + year + drvid + delta_social + delta_social*bin_fishing_lat, subset(trip_effort, duration_hrs>50))
  
  AIC(lm6)-AIC(lm4) # > 0, 4 is better
  AIC(lm7)-AIC(lm4) # > 0, 4 is better
  AIC(lm8)-AIC(lm4) # > 0, 4 is better
  AIC(lm9)-AIC(lm4) # < 0, 9 is better
  AIC(lm9)-AIC(lm10) # > 0, 10 is better
  
  
  vessel_averages <- trip_effort %>% filter(duration_hrs>50) %>% group_by(doc.num) %>%
    summarize(average_social = median(delta_social), average_revenue = median(revenue), 
              sd_social = sd(delta_social), sd_revenue = sd(revenue),
              average_lbs = median(lbs), sd_lbs=sd(lbs), 
              revenue_hr = median(revenue/duration_hrs), lbs_hr = median(lbs/duration_hrs), 
              sd_revenue_hr = sd(revenue/duration_hrs), sd_lbs_hr = sd(lbs/duration_hrs),
              median_lat = median(median_fishing_lat), 
              average_percent_occupied = median(percent_occupied), 
              sd_percent_occupied = sd(percent_occupied))
  
    ggplot(vessel_averages, aes(x = average_social, y = average_revenue, color = median_lat)) + 
      geom_point() +  geom_errorbar(aes(ymin = average_revenue - sd_revenue, 
                                        ymax = average_revenue+sd_revenue), 
                                    alpha = .25) + 
      geom_errorbarh(aes(xmin = average_social -sd_social,
                         xmax = average_social + sd_social), alpha = .25) + 
    theme_minimal() + scale_color_continuous(high="steelblue", low='indianred')
    cor.test(vessel_averages$average_social, vessel_averages$average_revenue, method = "spearman")
  
  ggplot(vessel_averages, aes(x = average_social, y = average_lbs, color=median_lat)) + 
    geom_point() + geom_errorbar(aes(ymin = average_lbs - sd_lbs, 
                                     ymax = average_lbs+sd_lbs), alpha = .25) + 
    geom_errorbarh(aes(xmin = average_social -sd_social, 
                       xmax = average_social + sd_social), alpha = .25) + 
    theme_minimal()  + scale_color_continuous(high="steelblue", low='indianred')
  cor.test(vessel_averages$average_social, vessel_averages$average_lbs, method = "spearman")
  
  ggplot(vessel_averages, aes(x = average_social, y = revenue_hr, color = median_lat)) + 
    geom_point() + geom_errorbar(aes(ymin = revenue_hr - sd_revenue_hr, 
                                     ymax = revenue_hr+sd_revenue_hr), alpha = .25) + 
    geom_errorbarh(aes(xmin = average_social -sd_social, 
                       xmax = average_social + sd_social), alpha = .25) + 
    theme_minimal() + scale_color_continuous(high="steelblue", low='indianred')
    cor.test(vessel_averages$average_social, vessel_averages$revenue_hr, method = "spearman")
    
    ggplot(vessel_averages, aes(x = average_social, y = lbs_hr, color=median_lat)) + 
      geom_point() + geom_errorbar(aes(ymin = lbs_hr - sd_lbs_hr, 
                                       ymax = lbs_hr + sd_lbs_hr), alpha = .25) + 
      geom_errorbarh(aes(xmin = average_social -sd_social, 
                         xmax = average_social + sd_social), alpha = .25) + 
      theme_minimal() + scale_color_continuous(high="steelblue", low='indianred')
    cor.test(vessel_averages$average_social, vessel_averages$lbs_hr, method = "spearman")
    
    # interestingly, boats that looked like fish in busy places are not that much more social then you'd expect
    ggplot(vessel_averages, aes(x = average_social, y = average_percent_occupied, color = median_lat)) + 
      geom_point() +  geom_errorbar(aes(ymin = average_percent_occupied - sd_percent_occupied, 
                                        ymax = average_percent_occupied + sd_percent_occupied), 
                                    alpha = .25) + 
      geom_errorbarh(aes(xmin = average_social -sd_social,
                         xmax = average_social + sd_social), alpha = .25) + 
      theme_minimal() + scale_color_continuous(high="steelblue", low='indianred')
    cor.test(vessel_averages$average_social, vessel_averages$average_revenue, method = "spearman")
    
    # interestingly, boats that looked like fish in busy places are not that much more social then you'd expect
    ggplot(vessel_averages, aes(x = average_percent_occupied, y = average_revenue, color = median_lat)) + 
      geom_point() +  geom_errorbarh(aes(xmin = average_percent_occupied - sd_percent_occupied, 
                                        xmax = average_percent_occupied + sd_percent_occupied), 
                                    alpha = .25) + 
      geom_errorbar(aes(ymin = average_revenue - sd_revenue, 
                        ymax = average_revenue+sd_revenue), 
                    alpha = .25) +
      theme_minimal() + scale_color_continuous(high="steelblue", low='indianred')
    cor.test(vessel_averages$average_social, vessel_averages$average_revenue, method = "spearman")
    
    # assigning homeports
   major_ports <-  trip_effort %>% group_by(doc.num, pcid) %>%
      summarize(revenue = sum(revenue), lbs = sum(lbs)) %>%
      group_by(doc.num) %>%
      mutate(percent_rev = revenue/sum(revenue),
             percent_lbs = lbs/sum(lbs),
             major_port_revenue = pcid[which.max(revenue)]) %>% # lbs/revenue the same
     left_join(vessel_averages)
   
   ggplot(major_ports, aes(x = major_port_revenue, y = average_social)) + geom_boxplot()

   # load social vulnerability
   sv <- read.csv("/Users/efuller/Downloads/social_vulnerability.csv",stringsAsFactors = FALSE)
   colnames(sv) <- c("pcid","description","geoID2","geoname","fishing_dependence","social_vulnerability")
   sv <- sv %>%
     group_by(pcid, geoname) %>%
     summarize(fishing_dependence = mean(fishing_dependence), 
               social_vulnerability = mean(social_vulnerability))
   
   major_ports <- left_join(major_ports, sv, by = c("major_port_revenue"="pcid"))
   
   major_ports %>% group_by(doc.num) %>%
     summarize(average_social = unique(average_social), 
               average_percent_occupied = unique(average_percent_occupied),
               social_vulnerability = weighted.mean(social_vulnerability, percent_rev),
               fishing_dependence = weighted.mean(fishing_dependence, percent_lbs),
               median_lat = unique(median_lat)) %>%
   ggplot(aes(x = social_vulnerability, 
              y = average_social, color = median_lat)) + geom_point(size=3) +
     scale_color_continuous(low="indianred",high='steelblue') + theme_minimal()
   
   major_ports %>% group_by(doc.num) %>%
     summarize(average_social = unique(average_social), 
               average_percent_occupied = unique(average_percent_occupied),
               social_vulnerability = weighted.mean(social_vulnerability, percent_rev),
               fishing_dependence = weighted.mean(fishing_dependence, percent_lbs),
               median_lat = unique(median_lat)) %>%
     ggplot(aes(x = fishing_dependence, 
                y = average_social, color = median_lat)) + geom_point(size=3) +
     scale_color_continuous(low="indianred",high='steelblue') + theme_minimal()
   