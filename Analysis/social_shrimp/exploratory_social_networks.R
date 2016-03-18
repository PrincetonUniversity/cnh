vms <- readRDS("processedData/spatial/vms/intermediate/06_predict_fishing/vms_classified.RDS")

# look at distribution of pairwise distances when boats are out on the water together ----

# prep data

vms$adj_time <- format(round(vms$date.time, units="hours"),
                       format="%Y-%m-%d %H:%M:%S")
vms$adj_time <- as.POSIXct(vms$adj_time,
                           format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-8")
time_list <- split(vms_all, vms_all$adj_time)

# all distances (doesn't distinguish between same boat twice in an hour)
mult_boats <- which(sapply(time_list, nrow)>1)
get_dists <- function(x){
  coordinates(x) <- ~longitude+latitude
  proj4string(x) <-   CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  pwdist <- spDists(x,longlat=TRUE)
  diag(pwdist) <- NA
  dist_vec <- as.vector(pwdist)[1:nrow(pwdist)] # sym matrix only want half of it
  dist_vec <- dist_vec[-which(is.na(dist_vec))]
  return(dist_vec)
}

pairwise_dists <- lapply(time_list[mult_boats],get_dists )
hist(unlist(pairwise_dists),breaks=500)

# only fishing distances
get_social_dists <- function(x){
  x <- subset(x, predicted_fishing==1)
  if(nrow(x)<2){
    return(NA)
  }else{
  coordinates(x) <- ~longitude+latitude
  proj4string(x) <-   CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  pwdist <- spDists(x,longlat=TRUE)
  diag(pwdist) <- NA
  dist_vec <- as.vector(pwdist)[1:nrow(pwdist)]
  dist_vec <- dist_vec[-which(is.na(dist_vec))]
  return(dist_vec)
  }
}
pairwise_social_dists <- lapply(time_list[mult_boats], get_social_dists)
hist(unlist(pairwise_social_dists),breaks=500)

social_dists <- unlist(pairwise_social_dists)
social_dists <- social_dists[-which(is.na(social_dists))]

hist(social_dists[social_dists<6],breaks=100,col='grey')

# make networks based on amount of time fishing within 9 km ----
vs <- unique(vms$doc.num)
# set up social matrix
social_mat <- matrix(0, ncol=length(vs), nrow = length(vs))
colnames(social_mat) <- vs
rownames(social_mat) <- vs
social_mat_9 <- social_mat
social_mat_8 <- social_mat
social_mat_7 <- social_mat
social_mat_6 <- social_mat
social_mat_5 <- social_mat
social_mat_4 <- social_mat
social_mat_3 <- social_mat
social_mat_2 <- social_mat
social_mat_1 <- social_mat
social_mat_0 <- social_mat

# keep track of total social hours
total_social_hrs <- data.frame(doc.num = rep(vs,each=10),
                               km_cutoff = rep(c(.5,1:9), length.out=610),
                               social_hrs = rep(NA, 610),
                               stringsAsFactors = FALSE)

# shouldn't have to worry about multiple boats in one hour
#any(sapply(time_list, function(x) any(table(x$doc.num)>1))) 
# FALSE = no doc.num shows up more than once in an hour

for(i in 1:length(vs)){
  ves <- vs[i]
  # possible times
  ves_list <- time_list[which(sapply(time_list, function(x) return(any(x$doc.num == ves))))]
  
  # any have > 1 row
  ves_social <- ves_list[which(sapply(ves_list, nrow)>1)]
  
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
  
  # for each boat, want to know total hours fishing, and pairwise total hours for each other vessel
  # can get total hours fishing from data frame. So just make matrix with entries are hours fishing
  # with pairwise vessel
  
  fishing_dists <- function(x){
    x <- subset(x, predicted_fishing==1)
    if(nrow(x)<2){
      return(NA)
    }else{
      coordinates(x) <- ~longitude+latitude
      proj4string(x) <-   CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      ves_interest <- subset(x, doc.num==ves)
      rest_ves <- subset(x, doc.num!=ves)
      pwdist <- spDistsN1(matrix(coordinates(rest_ves),ncol=2),
                          matrix(coordinates(ves_interest),ncol=2),longlat=TRUE)
      return(data.frame(doc.num = rest_ves@data$doc.num,
                        pairwise_km = pwdist,
                        vessel = rep(ves, length(pwdist)),
                        stringsAsFactors = FALSE))
    }
  }
  ves_fishing_dists <- lapply(ves_social_fishing,
                              fishing_dists)
  if(any(is.na(ves_fishing_dists))){
    ves_fishing_dists <- ves_fishing_dists[-which(is.na(ves_fishing_dists))]
  }
  
  all_dists <- do.call(rbind, ves_fishing_dists)
  all_dists$social_9 <- ifelse(all_dists$pairwise_km <9, 1, 0)
  all_dists$social_8 <- ifelse(all_dists$pairwise_km <8, 1, 0)
  all_dists$social_7 <- ifelse(all_dists$pairwise_km <7, 1, 0)
  all_dists$social_6 <- ifelse(all_dists$pairwise_km <6, 1, 0)
  all_dists$social_5 <- ifelse(all_dists$pairwise_km <5, 1, 0)
  all_dists$social_4 <- ifelse(all_dists$pairwise_km <4, 1, 0)
  all_dists$social_3 <- ifelse(all_dists$pairwise_km <3, 1, 0)
  all_dists$social_2 <- ifelse(all_dists$pairwise_km <2, 1, 0)
  all_dists$social_1 <- ifelse(all_dists$pairwise_km <1, 1, 0)
  all_dists$social_0 <- ifelse(all_dists$pairwise_km <.5, 1, 0)
  all_dists$time <- rownames(all_dists)
  # fill in social matrix
  row_ind <- which(rownames(social_mat)==ves)
  
  social_hrs <- with(subset(all_dists, social_9==1), table(doc.num))
  col_inds <- names(social_hrs)
  social_mat_9[row_ind, col_inds] <- social_mat[row_ind, col_inds] + social_hrs
  
  social_hrs <- with(subset(all_dists, social_8==1), table(doc.num))
  col_inds <- names(social_hrs)
  social_mat_8[row_ind, col_inds] <- social_mat[row_ind, col_inds] + social_hrs
  
  social_hrs <- with(subset(all_dists, social_7==1), table(doc.num))
  col_inds <- names(social_hrs)
  social_mat_7[row_ind, col_inds] <- social_mat[row_ind, col_inds] + social_hrs
  
  social_hrs <- with(subset(all_dists, social_6==1), table(doc.num))
  col_inds <- names(social_hrs)
  social_mat_6[row_ind, col_inds] <- social_mat[row_ind, col_inds] + social_hrs
  
  social_hrs <- with(subset(all_dists, social_5==1), table(doc.num))
  col_inds <- names(social_hrs)
  social_mat_5[row_ind, col_inds] <- social_mat[row_ind, col_inds] + social_hrs
  
  social_hrs <- with(subset(all_dists, social_4==1), table(doc.num))
  col_inds <- names(social_hrs)
  social_mat_4[row_ind, col_inds] <- social_mat[row_ind, col_inds] + social_hrs
  
  social_hrs <- with(subset(all_dists, social_3==1), table(doc.num))
  col_inds <- names(social_hrs)
  social_mat_3[row_ind, col_inds] <- social_mat[row_ind, col_inds] + social_hrs
  
  social_hrs <- with(subset(all_dists, social_2==1), table(doc.num))
  col_inds <- names(social_hrs)
  social_mat_2[row_ind, col_inds] <- social_mat[row_ind, col_inds] + social_hrs
  
  social_hrs <- with(subset(all_dists, social_1==1), table(doc.num))
  col_inds <- names(social_hrs)
  social_mat_1[row_ind, col_inds] <- social_mat[row_ind, col_inds] + social_hrs
  
  social_hrs <- with(subset(all_dists, social_0==1), table(doc.num))
  col_inds <- names(social_hrs)
  social_mat_0[row_ind, col_inds] <- social_mat[row_ind, col_inds] + social_hrs
  
  # fill total social hours for vessel 
  for(j in c(.5,1:9)){
    relevant_dist  <- subset(all_dists, pairwise_km<j)
    idx = which(total_social_hrs$doc.num==ves & total_social_hrs$km_cutoff == j)
    total_social_hrs$social_hrs[idx] <- length(unique(gsub(".[0-9]","",relevant_dist$time)))  
  }
  }


# make proximity network ----
library(igraph)
g <- graph.adjacency(social_mat_0, mode = "lower", weighted = TRUE, diag=FALSE)

plot(g, edge.width = E(g)$weight/10, layout=layout.auto, vertex.size=1, 
     vertex.label = "",vertex.color='black')

# make proximity network based on proportion of time fishing
total_fishing_hrs <- vms %>% filter(predicted_fishing==1) %>% 
  group_by(doc.num) %>% summarize(fishing=sum(predicted_fishing)) %>%
  ungroup() %>% mutate(doc.num = as.character(doc.num))

# reorder to match social_mat
total_fishing_hrs <- total_fishing_hrs[match(rownames(social_mat_0),total_fishing_hrs$doc.num),]

# get lats
ports <- read.csv("processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE) %>%
  rename(pcid=Pcid)
catch <- readRDS("processedData/catch/1_cleaningData/tickets.RDS")
port_lats <- catch %>%
  filter(drvid %in% vs & metier.2010=="TWS_1") %>%
  select(pcid, trip_id,drvid) %>%
  distinct() %>%
  left_join(ports) %>%
  group_by(drvid) %>%
  summarize(mean_lat = mean(lat, na.rm = T)*10^5) 

lat_colors <- colorRampPalette(c("indianred","steelblue"))(diff(range(port_lats$mean_lat)))

# divide each row by total hours (r will do by column if left to own devices)
# http://stackoverflow.com/a/20596490/3137323

prop_social_mat <- t(t(social_mat_9) / total_fishing_hrs$fishing)

g_prop <- graph.adjacency(prop_social_mat, mode = "directed", weighted = TRUE, diag=FALSE)

paint_spectrum <- colorRampPalette(colors = c("grey80","steelblue"))(length(1:max(total_fishing_hrs$fishing)))

V(g_prop)$paint <- paint_spectrum[total_fishing_hrs$fishing]

plot(g_prop, vertex.color = V(g_prop)$paint, edge.width = E(g_prop)$weight*100, 
     layout=layout.auto, vertex.size=3, 
     vertex.label = "",vertex.color='black', 
     vertex.frame.color = V(g_prop)$paint,
     edge.arrow.size=.05, edge.curved=TRUE)

# could also color by mean port latitude for shrimp trips.

port_lats <- port_lats[match(rownames(prop_social_mat), port_lats$drvid),]
V(g_prop)$lat_paint <- lat_colors[(round(port_lats$mean_lat)-4080156)]

plot(g_prop, vertex.color=V(g_prop)$lat_paint,edge.width = E(g_prop)$weight*5, 
     layout=layout.auto, vertex.size=5, 
     vertex.label = "",
     vertex.frame.color = V(g_prop)$lat_paint,
     edge.arrow.size=.05, edge.curved=TRUE)

# look at network proprotional to total social time rather that total fishing hours

prop_social_mat_9 <- t(t(social_mat_9) / subset(total_social_hrs, km_cutoff==9)$social_hrs)

g_prop_9 <- graph.adjacency(prop_social_mat_9, mode = "directed", weighted = TRUE, diag=FALSE)

# color by mean port latitude for shrimp trips.

port_lats <- port_lats[match(rownames(prop_social_mat_9), port_lats$drvid),]
V(g_prop_9)$lat_paint <- lat_colors[(round(port_lats$mean_lat)-4080156)]

plot(g_prop_9, vertex.color=V(g_prop_9)$lat_paint,edge.width = E(g_prop_9)$weight*2, 
     layout=layout.auto, vertex.size=5, 
     vertex.label = "",
     vertex.frame.color = V(g_prop_9)$lat_paint,
     edge.arrow.size=.05, edge.curved=TRUE)

prop_social_mat_0 <- t(t(social_mat_0) / subset(total_social_hrs, km_cutoff==0.5)$social_hrs)

g_prop_0 <- graph.adjacency(prop_social_mat_0, mode = "directed", weighted = TRUE, diag=FALSE)

# color by mean port latitude for shrimp trips.

port_lats <- port_lats[match(rownames(prop_social_mat_0), port_lats$drvid),]
V(g_prop_0)$lat_paint <- lat_colors[(round(port_lats$mean_lat)-4080156)]

plot(g_prop_0, vertex.color=V(g_prop_0)$lat_paint,edge.width = E(g_prop_0)$weight*5, 
     layout=layout.auto, vertex.size=5, 
     vertex.label = "",
     vertex.frame.color = V(g_prop_0)$lat_paint,
     edge.arrow.size=.05, edge.curved=TRUE)

V(g_prop_0)$size <- total_fishing_hrs$fishing

plot(g_prop_0, vertex.color=V(g_prop_0)$lat_paint,edge.width = E(g_prop_0)$weight*2, 
     layout=layout.fruchterman.reingold, vertex.size=V(g_prop_0)$size/500, 
     vertex.label = "",
     vertex.frame.color = V(g_prop_0)$lat_paint,
     edge.arrow.size=.05, edge.curved=TRUE)

# look at how total number of hours changes as distance radius changes ----
total_social_hrs$doc.num <- as.character(total_social_hrs$doc.num)
social_dist_dep <- left_join(total_social_hrs, port_lats, by = c("doc.num"="drvid"))
social_dist_dep$mean_lat <- social_dist_dep$mean_lat/10^5
social_dist_dep <- left_join(social_dist_dep, total_fishing_hrs)

library(tidyr)
ggplot(social_dist_dep, aes(x = km_cutoff, y = social_hrs, group = doc.num, color=mean_lat)) +
  geom_path()+geom_point() + theme_minimal()

# look at it as a function of total hours
ggplot(social_dist_dep, 
       aes(x = km_cutoff, y = social_hrs/fishing, group = doc.num, color=mean_lat)) +
  geom_path()+geom_point() + theme_minimal()

# look at difference as function of total hours fished
delta_social <- social_dist_dep %>%
  group_by(doc.num) %>%
  summarize(delta = max(social_hrs)/min(social_hrs), 
            total_fishing = unique(fishing), mean_lat = unique(mean_lat))

ggplot(delta_social, aes(x = total_fishing, y = delta, color = mean_lat)) + 
  geom_point() + theme_minimal() + ylab("increase in social time")




# two things: 

# 1. home ports, maximum number of boats at a single port that could be theoretically fishing togehter
# also what's the proportion of shrimp trips represented in VMS data?

# 2.  productivity gradient for solitary boats versus social boats. latitude of catch 
# how long are the trips, all the same amount on average? how much of that time is spent fishing?
# how long is the distance covered?

# i think I'm trying to make a model where the response is revenue per unit effort, which is a function
# of both latitude and socialability..
# save some things
saveRDS(social_mat)