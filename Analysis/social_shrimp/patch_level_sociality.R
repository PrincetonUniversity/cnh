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
  
 



