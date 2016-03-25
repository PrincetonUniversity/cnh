library(sp)
library(ggplot2)
library(dplyr)
library(tidyr)
# load vms and make time_list ----
  vms <- readRDS("processedData/spatial/vms/intermediate/06_predict_fishing/vms_classified.RDS")
  
  vms$adj_time <- format(round(vms$date.time, units="hours"), 
                         format="%Y-%m-%d %H:%M:%S")
  vms$adj_time <- as.POSIXct(vms$adj_time, 
                             format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-8")
  
  time_list <- split(vms, vms$adj_time)

# look at distribution of pairwise distances when boats are out on the water together ----

# all distances (doesn't distinguish between same boat twice in an hour)
  mult_boats <- time_list[which(sapply(time_list, nrow)>1)]
  get_dists <- function(x){
    coordinates(x) <- ~longitude+latitude
    proj4string(x) <-   CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    pwdist <- spDists(x,longlat=TRUE)
    diag(pwdist) <- NA
    dist_vec <- as.vector(pwdist)[1:nrow(pwdist)] # sym matrix only want half of it
    dist_vec <- dist_vec[-which(is.na(dist_vec))]
    return(dist_vec)
  }
  pairwise_dists <- lapply(mult_boats,get_dists )
  hist(unlist(pairwise_dists),breaks=500)
  qplot(x = unlist(pairwise_dists), geom="histogram") + theme_minimal() + 
    xlab("pairwise distance (km) btwn all vessels 2009-2013")

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
  pairwise_social_dists <- lapply(mult_boats, get_social_dists)
  qplot(x = unlist(pairwise_social_dists), geom="histogram") + theme_minimal() + 
    xlab("pairwise distance (km) btwn all fishing vessels 2009-2013")

# make networks based on amount of time fishing within 0.5 - 9 km (prep data structures) ----
  vs <- unique(vms$doc.num)

  # set up social matrix
  social_mat <- matrix(0, ncol=length(vs), nrow = length(vs))
  colnames(social_mat) <- vs
  rownames(social_mat) <- vs
  
  # make list of social matrices for all vessels
  social_mat_list <- lapply(1:10, function(x) social_mat)
  names(social_mat_list) <- c(0.5,1:9)

  # keep track of total social hours
  total_social_hrs <- data.frame(doc.num = rep(vs,each=10),
                                 km_cutoff = rep(c(.5,1:9), length.out=620),
                                 social_hrs = rep(NA, 620),
                                 stringsAsFactors = FALSE)

  # there are multiple boats in one hour, fyi
  any(sapply(time_list, function(x) any(table(x$doc.num)>1))) 
  # FALSE = no doc.num shows up more than once in an hour
  
# define function to get relevant VMS points
  relevant_vms <- function(vessel_name){
    ves <- vessel_name
    # all times where the vessel is present
    ves_list <- time_list[which(sapply(time_list, function(x) return(any(x$doc.num == ves))))]
    
    # any that have another doc.num that's not vessel of interest
    ves_social <- ves_list[which(sapply(ves_list, function(x) any(x$doc.num!=ves)))];
    rm(ves_list)
    
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
    rm(ves_social)
    
    # for each boat, want to know total hrs fishing next to any vessel, and
    # pairwise total hrs for each partner vessel
    fishing_dists <- function(x){
      #x <- subset(x, predicted_fishing==1) # drop this for the moment
      if(nrow(x)<2) stop("subsetting failed - no data")
      coordinates(x) <- ~longitude+latitude
      proj4string(x) <-   CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      ves_interest <- subset(x, doc.num==ves)[1,] # just take the first entry if 2
      rest_ves <- subset(x, doc.num!=ves)
      if(nrow(rest_ves)==0) stop("subsetting failed - no other vessels")
      pwdist <- spDistsN1(matrix(coordinates(rest_ves),ncol=2),
                          matrix(coordinates(ves_interest),ncol=2),
                          longlat=TRUE)
      doc.num = rest_ves@data$doc.num
      
      return(data.frame(doc.num = doc.num,
                        pairwise_km = pwdist,
                        vessel = rep(ves, length(pwdist)),
                        trip_id = ves_interest$trip_id1,
                        adj_time = ves_interest$adj_time,
                        stringsAsFactors = FALSE))
    }
    ves_fishing_dists <- lapply(ves_social_fishing,
                                fishing_dists)
    # combine to dataframe to sort
    all_dists <- do.call(rbind, ves_fishing_dists)
    all_dists <- as.data.frame(all_dists)
    return(all_dists)
  }

# keep track of all social points and pairwise dists
  social_list <- list()

# flip through vessels to find social VMS ----
for(i in 1:length(vs)){
  # find pairwise distances for vessel
  all_dists <- relevant_vms(vessel_name = vs[i])
  
  # fill in social matrices
  if(nrow(all_dists)==0){
   next
  }else{
    row_ind <- which(rownames(social_mat)==vs[i])
    min_dists <- as.numeric(names(social_mat_list))
    for(j in 1:length(min_dists)){
      social_hrs <- with(subset(all_dists, pairwise_km<min_dists[j]), table(doc.num))
      col_inds <- names(social_hrs)
      social_mat_list[[j]][row_ind, col_inds] <- social_mat_list[[j]][row_ind, col_inds] + social_hrs
    }
  }
    
  # fill total social hours for vessel 
  for(j in c(.5,1:9)){
      relevant_dist  <- subset(all_dists, pairwise_km<j)
      idx = which(total_social_hrs$doc.num==vs[i] & total_social_hrs$km_cutoff == j)
      total_social_hrs$social_hrs[idx] <- length(unique(relevant_dist$adj_time))
    } 
    
  # label VMS points as social or solitary
  # relevant dist is the version w subset < 9km
    social_list[[i]] <- relevant_dist %>% 
      group_by(vessel, trip_id, adj_time, doc.num) %>%
      summarize(pairwise_km = min(pairwise_km)) %>%
    spread(key=doc.num, value=pairwise_km) %>%
      rename(doc.num = vessel)
    
    names(social_list)[i] <- vs[i]
}
  

# label VMS points as social or solitary ----
  # put in minimum distance for each time for each time for each vessel
  sub_social_list <- social_list[-which(unlist(sapply(social_list,is.null))==1)]
  min_list <- vector("list",length=length(sub_social_list))
  for(l in 1:length(sub_social_list)){
    sub_social_list[[l]]$min_distance = as.numeric(
      apply(as.data.frame(sub_social_list[[l]]), 1, 
            function(x) min(x[4:length(x)],na.rm=T)))
    min_list[[l]] <- dplyr::select(sub_social_list[[l]], doc.num, trip_id, adj_time, min_distance)
  }
  
  min_df <- do.call(rbind, min_list) %>% ungroup() %>%
    rename(trip_id1 = trip_id) %>% mutate(trip_id1 = as.character(trip_id1))
  
  vms_ss <- left_join(vms, min_df) %>%
    arrange(doc.num, date.time) %>%
    mutate(type = ifelse(min_distance <= 0.5 & predicted_fishing==1, "social",
                  ifelse(predicted_fishing==1, "solitary", NA)))
  
  # calculate area covered while fishing? what happens when you have two seperate bouts?
  
# look at productivity gradients for social and solitary trips ----
  catch <- readRDS("processedData/catch/1_cleaningData/tickets.RDS")
  ports <- read.csv("processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE) %>%
    rename(pcid=Pcid)
  
  lat_trips <- as.data.frame(vms_ss) %>%
    group_by(doc.num, trip_id1) %>%
    summarize(n_social = length(which(type=="social")),
              n_solitary = length(which(type=="solitary")),
              median_lat = median(latitude[which(predicted_fishing==1)]),
              duration_hrs = as.numeric(duration(interval(min(date.time),max(date.time))))/60/60,
              type=ifelse(n_social==0,"solitary","social")) %>%
    ungroup() %>%
    mutate(doc.num = as.character(doc.num))
  
  trip_stats <- catch %>%
    filter(trip_id %in% unique(vms_ss$trip_id1)) %>%
    group_by(drvid, trip_id) %>%
    summarize(revenue = sum(adj_revenue), lbs = sum(landed_wt),
              pcid = unique(pcid), tdate = unique(tdate), 
              metier = unique(metier.2010), ppp = mean(ppp)) %>%
    left_join(lat_trips, by = c("trip_id"="trip_id1", "drvid"="doc.num")) %>%
    mutate(tdate = as.Date(tdate, format = "%d-%B-%y"),
           doy = as.numeric(format(tdate, "%j")), year = format(tdate, "%Y"),
           bin_lat = cut(median_lat, 40:48), bin_ns = cut(median_lat, c(40,44,48))) %>%
    filter(duration_hrs>6 & !is.na(duration_hrs) & !is.na(median_lat))
  
  ggplot(trip_stats, aes(x = bin_lat, y = lbs)) + 
    geom_boxplot(aes(fill=type)) + facet_grid(year~.) + theme_minimal()
  
  ggplot(trip_stats, aes(x = bin_lat, y = lbs/duration_hrs)) + 
    geom_boxplot(aes(fill=type)) + facet_grid(year~., scales = "free_y") + theme_minimal()
  
  ggplot(trip_stats, aes(x = bin_lat, y = lbs)) + 
    geom_boxplot(aes(fill=type)) + facet_grid(year~.) + theme_minimal()
  
  ggplot(trip_stats, aes(x = bin_lat, y = lbs/duration_hrs)) + 
    geom_boxplot(aes(fill=type)) + theme_minimal() + facet_grid(.~year) + scale_y_log10()
  
  ggplot(trip_stats, aes(x = bin_ns, y = lbs)) + geom_boxplot(aes(fill=type)) + 
    facet_grid(year~.) + theme_minimal()
  
# look at catch over time
  trip_stats %>% filter(type == "solitary", !is.na(median_lat)) %>%
    group_by(year, doy, bin_ns) %>% summarize(lbs = sum(lbs)) %>%
  ggplot(aes(x = doy, y = lbs)) + geom_bar(stat='identity',aes(fill = bin_ns),
                                           position="dodge") + 
    facet_grid(year~.)
  
  # look at lbs/trip versus lbs/hr
  ggplot(trip_stats, aes(x = lbs, y = lbs/duration_hrs)) + geom_point()
  
  # filter for june and look at average lbs/trip in june for each year
  june_lbs <- trip_stats %>% filter(doy>153, doy < 183) %>% group_by(year) %>% 
    summarize(mean_lbs = mean(lbs), sd_lbs = sd(lbs), mean_ppp = mean(ppp)) 
  
    ggplot(june_lbs, aes(x = year, y = mean_lbs)) + geom_point() + 
    geom_errorbar(aes(ymin = mean_lbs-sd_lbs, ymax = mean_lbs + sd_lbs), width = 0) +
    theme_minimal() + geom_hline(yintercept=10000, col='indianred') 
  
  june_cpue <- trip_stats %>% filter(doy>153, doy < 183) %>% 
    group_by(year) %>% 
    summarize(mean_lbs_hr = mean(lbs/as.numeric(duration_hrs)), 
              sd_lbs = sd(lbs/as.numeric(duration_hrs)))
    
    ggplot(june_cpue, aes(x = year, y = mean_lbs_hr)) + geom_point() + 
    geom_errorbar(aes(ymin = mean_lbs_hr-sd_lbs, ymax = mean_lbs_hr + sd_lbs), width = 0) +
    theme_minimal() + geom_hline(yintercept=10000, col='indianred') 
    
    june_together <- left_join(june_lbs, june_cpue, by = "year")
    
    ggplot(june_together, aes(x=mean_lbs, y = mean_lbs_hr)) + geom_point(size=3) +
      theme_minimal()
  
  # maybe could make the case that CPUE is lower for social trips
  ggplot(trip_stats, aes(x = type, y = lbs/as.numeric(duration_hrs))) + 
    geom_boxplot(fill='grey90') + theme_minimal() + ylab("cpue (lbs/hr)") + 
    xlab("type of trip") + scale_y_log10()
  
  library(arm)
  display(with(trip_stats, lm(lbs/as.numeric(duration_hrs)~type)))
  AIC(with(trip_stats, lm(lbs/as.numeric(duration_hrs)~type+doy+median_lat)))
  display(with(trip_stats, lm(lbs/as.numeric(duration_hrs)~doy+median_lat + factor(type))))
  display(with(trip_stats, lm(lbs/as.numeric(duration_hrs)~doy+median_lat + type + factor(drvid))))

  lm1 <- with(trip_stats, lm(lbs~doy+median_lat + drvid + type))
  plot(lm1)

  # some support in model fitting that type of trip adds something to model. still pretty weak
  # (also wonder about adding vessel lenght to standardize for catching power)
  
  # and some correlation between where you fish and how you fish (social tends to be _slightly_ further north)
  ggplot(trip_stats, aes(x = type, y = median_lat)) + geom_boxplot()
  t.test(subset(trip_stats, type=="solitary")$median_lat, subset(trip_stats, type=="social")$median_lat)
  # is gently significant
  
  # what about lbs/hours out as a function of proportion fished socially
  trip_stats %>% group_by(drvid) %>%
    summarize(percent_social = sum(n_social)/(sum(n_social)+sum(n_solitary)),
              median_rev = median(revenue/duration_hrs), 
              sd_rev = sd(revenue/duration_hrs)) %>%
    ggplot(aes(x=percent_social, y = median_rev)) + geom_point() +
    geom_errorbar(aes(ymax = median_rev+sd_rev, ymin = median_rev-sd_rev)) 
    
# make proximity network ----
library(igraph)
g <- graph.adjacency(social_mat_list[[1]], mode = "lower", weighted = TRUE, diag=FALSE)

plot(g, edge.width = E(g)$weight/4, layout=layout.auto, vertex.size=1, 
     vertex.label = "",vertex.color='black')

# make proximity network based on proportion of time fishing
total_fishing_hrs <- vms %>% 
  group_by(doc.num) %>% summarize(fishing=sum(predicted_fishing, na.rm=T)) %>%
  ungroup() %>% mutate(doc.num = as.character(doc.num))

# reorder to match social_mat and assign size based on hours fished in 5yrs
total_fishing_hrs <- total_fishing_hrs[match(rownames(social_mat_list[[1]]),total_fishing_hrs$doc.num),]
V(g)$size <- total_fishing_hrs$fishing

# get lats
  port_lats <- catch %>%
    filter(drvid %in% vs & metier.2010=="TWS_1") %>%
    dplyr::select(pcid, trip_id,drvid) %>%
    distinct() %>%
    left_join(ports) %>%
    group_by(drvid) %>%
    summarize(mean_port_lat = mean(lat, na.rm = T)*10^5) 
  
  median_fish_lat_overall <- vms_ss %>%
    filter(predicted_fishing==1) %>%
    group_by(doc.num) %>%
    summarize(median_lat_vms = median(latitude)) %>%
    mutate(doc.num = as.character(doc.num))

# port lat pretty good proxy for vms
  port_lats <- left_join(port_lats, median_fish_lat_overall,
                         by=c("drvid"="doc.num")) 
  ggplot(port_lats, aes(x = mean_port_lat, y = median_lat_vms)) + geom_point()

# color by mean latitude of port landing
  lat_colors <- colorRampPalette(c("indianred","steelblue"))(diff(range(port_lats$mean_port_lat)))
  port_lats <- port_lats[match(rownames(prop_social_mat), port_lats$drvid),]
  V(g)$lat_paint <- lat_colors[(round(port_lats$mean_port_lat)-4080156)]

# plot
  par(bg='white',mai=rep(0,4))
  plot(g, vertex.color=V(g)$lat_paint, edge.width = E(g)$weight/4, 
       layout=layout.auto, vertex.size=sqrt(V(g)$size)/5, 
       vertex.label = "", edge.color = alpha('grey30',.5),
       vertex.frame.color = V(g)$lat_paint, edge.curved=FALSE)
  
  # add proportions of social hours
  prop_mat <- social_mat_list[[1]]/subset(total_social_hrs, km_cutoff==0.5)$social_hrs
  g_prop <- graph.adjacency(prop_mat, mode ="directed", weighted = TRUE, diag=FALSE)
  V(g_prop)$size <- total_fishing_hrs$fishing
  V(g_prop)$lat_paint <- lat_colors[(round(port_lats$mean_port_lat)-4080156)]
  plot(g_prop, edge.width = E(g_prop)$weight*10, vertex.label="",
       vertex.size = sqrt(V(g_prop)$size)/5, edge.arrow.size=.05, edge.curved=TRUE,
       vertex.color = V(g_prop)$lat_paint, edge.color = alpha('grey',.5))
  
# look at how total number of hours changes as distance radius changes ----
total_social_hrs$doc.num <- as.character(total_social_hrs$doc.num)
social_dist_dep <- left_join(total_social_hrs, port_lats, by = c("doc.num"="drvid"))
social_dist_dep$mean_lat <- social_dist_dep$mean_port_lat/10^5
social_dist_dep <- left_join(social_dist_dep, total_fishing_hrs)

ggplot(social_dist_dep, aes(x = km_cutoff, y = social_hrs, group = doc.num, color=mean_lat)) +
  geom_path()+geom_point() + theme_minimal()

ggplot(social_dist_dep, aes(x = km_cutoff, y = social_hrs, group = doc.num, color=fishing)) +
  geom_path()+geom_point() + theme_minimal()

# suggests correlation betweeen how often you fish and latitude ----
social_dist_dep %>% dplyr::select(doc.num, social_hrs, mean_port_lat) %>% 
  distinct() %>% mutate(lat_bin = cut(mean_port_lat/10^5, 40:48)) %>%
  ggplot(aes(x = lat_bin, y = social_hrs)) + geom_boxplot(fill='grey80') + theme_minimal()

social_dist_dep %>% dplyr::select(doc.num, social_hrs, mean_port_lat,fishing) %>% 
  distinct() %>% mutate(lat_bin = cut(mean_port_lat/10^5, 40:48), bin_ns = cut(mean_port_lat/10^5, c(40,44,48))) %>%
  ggplot(aes(x = lat_bin, y = social_hrs/fishing)) + geom_boxplot(fill='grey80') + theme_minimal()

social_dist_dep %>% dplyr::select(doc.num, social_hrs, mean_port_lat,fishing) %>% 
  distinct() %>% mutate(lat_bin = cut(mean_port_lat/10^5, 40:48), bin_ns = cut(mean_port_lat/10^5, c(40,44,48))) %>%
  ggplot(aes(x = bin_ns, y = social_hrs/fishing)) + geom_boxplot(fill='grey80') + theme_minimal()

# home ports ----
trip_stats %>% group_by(drvid, pcid) %>% 
  summarize(n_trips = length(unique(trip_id))) %>%
  group_by(drvid) %>% 
  mutate(total_trips = sum(n_trips), percent_trips = n_trips/total_trips) %>%
  ungroup() %>%
  mutate(pcid = factor(pcid,levels = c("LWC","WPT","AST","NEW","TLL","COS","BRK","CRS","ERK"), ordered=TRUE)) %>%
  ggplot(aes(x = reorder(drvid, percent_trips, max), y = n_trips, fill=pcid)) +
  geom_bar(stat='identity') + scale_fill_brewer(palette="Spectral") + theme_minimal()

# generalism and sociality ----
# looking at how generalist during shrimp season (April 1-Oct 31)
generalism <- catch %>%
  mutate(tdate = as.Date(tdate, format = "%d-%B-%y"),
         doy = as.numeric(format(tdate, "%j"))) %>%
  filter(doy >= 92, doy < 305) %>%
  group_by(drvid, pcid, trip_id, metier.2010) %>%
  summarize(revenue = sum(adj_revenue)) %>%
  group_by(drvid) %>%
  mutate(shrimp_boat = ifelse(any(metier.2010=="TWS_1"),1,0)) %>%
  filter(shrimp_boat==1) %>%
  group_by(drvid, pcid) %>%
  summarize(total_rev = sum(revenue),
            total_shrimp_rev = sum(revenue[which(metier.2010=="TWS_1")])) %>%
  group_by(drvid) %>%
  mutate(percent_rev_shrimp_overall = sum(total_shrimp_rev)/sum(total_rev),
         percent_rev_shrimp_byport = total_shrimp_rev/total_rev)

majority_ports <- generalism %>%
  group_by(drvid) %>%
  summarize(port_shrimp_rev = total_shrimp_rev/sum(total_shrimp_rev),
         majority_port = ifelse(any(percent_rev_shrimp_byport>.5), pcid[which.max(percent_rev_shrimp_byport)], NA))

sociability <- trip_stats %>%
  ungroup() %>%
  dplyr::select(drvid, trip_id, type, median_lat) %>%
  distinct() %>%
  group_by(drvid) %>%
  summarize(percent_social = length(which(type=="social"))/length(unique(trip_id)),
            median_lat = median(median_lat,na.rm=T)) %>%
  ungroup() %>%
  mutate(drvid = as.integer(drvid)) %>%
  left_join(subset(total_social_hrs, km_cutoff==0.5), by = c("drvid"="doc.num")) %>%
  mutate(drvid = as.character(drvid)) %>%
  left_join(total_fishing_hrs, by = c("drvid"="doc.num")) %>%
  mutate(percent_hrs_social = social_hrs/fishing) %>%
  dplyr::select(-km_cutoff)

both_together <- left_join(generalism, sociability)

# are specialists disproportionally not in VMS data?
ggplot(both_together, aes(x = is.na(percent_social), y = percent_rev_shrimp_overall)) + 
  geom_boxplot(fill='grey90') + theme_minimal() + xlab("Missing from VMS data?")

# for those that are in VMS, is generalism and proportion social positively correlated?
ggplot(both_together, aes(x = percent_rev_shrimp_overall, percent_social)) + 
  geom_point(aes(size = fishing, color=median_lat), alpha=.95) + 
  theme_minimal() + scale_color_continuous(low="indianred",high='steelblue')

ggplot(both_together, aes(y = percent_hrs_social, x = percent_rev_shrimp_overall)) + 
  geom_point(aes(size = fishing, color=median_lat), alpha=.95) + 
  theme_minimal() + scale_color_continuous(low="indianred",high='steelblue')

ggplot(both_together, aes(y = percent_hrs_social, x = fishing)) + 
  geom_point(aes(size = fishing, color=median_lat), alpha=.95) + 
  theme_minimal() + scale_color_continuous(low="indianred",high='steelblue')

ggplot(both_together, aes(y = percent_hrs_social, x = fishing)) + 
  geom_point(aes(size = total_shrimp_rev, color=percent_rev_shrimp_overall), alpha=.95) + 
  theme_minimal() 

ggplot(both_together, aes( x= total_shrimp_rev, y = percent_rev_shrimp_overall)) + geom_point()
# maximum number of boats at a single ?port that could be theoretically fishing togehter
# also what's the proportion of shrimp trips represented in VMS data?

# 2.  productivity gradient for solitary boats versus social boats. latitude of catch 
# how long are the trips, all the same amount on average? how much of that time is spent fishing?
# how long is the distance covered?

# i think I'm trying to make a model where the response is revenue per unit effort, which is a function
# of both latitude and socialability..

# look at CPUE in next week as function of prior week
trip_stats$week <- as.numeric(format(trip_stats$tdate, "%U"))

weekly_aggregates <- trip_stats %>%
  group_by(year, week, drvid) %>%
  summarize(lbs = mean(lbs), n_trips = length(unique(trip_id)), fishing_days = sum(as.numeric(duration_hrs)))

weekly_aggregates$weekly_t1_lbs <- c(weekly_aggregates$lbs[2:nrow(weekly_aggregates)], NA)
weekly_aggregates$t1_fishing_days <- c(weekly_aggregates$fishing_days[2:nrow(weekly_aggregates)], NA)
ggplot(weekly_aggregates, aes(x =lbs , y = t1_fishing_days )) + geom_point()

# inferring gradients of productivity from vms data
prod <- vms %>%
  left_join(dplyr::select(ungroup(trip_stats), trip_id, lbs), by = c("trip_id1"="trip_id")) %>%
  group_by(trip_id1) %>%
  mutate(adj_lbs = lbs/length(which(predicted_fishing==1)), 
         year = as.numeric(format(date.time, "%Y", tx = "Etc/GMT-8")),
         doy = as.numeric(format(date.time, "%j", tz = "Etc/GMT-8")), 
         adj_lbs = ifelse(predicted_fishing==0, 0, adj_lbs), 
         adj_day = as.Date(date.time, tz="Etc/GMT-8")) %>%
  filter(trip_id1 %in% subset(trip_stats, as.numeric(duration_hrs)>50/24)$trip_id)

ggplot(subset(prod, doy == 109 & predicted_fishing==1), aes(x = longitude, y = latitude, color = adj_lbs)) + geom_point(size = .001) + coord_map()

#grid into daily matrices
library(sp);library(raster)
xmn <- min(prod$longitude); xmx = max(prod$longitude)
ymn <- min(prod$latitude); ymx = max(prod$latitude)

m <-  as.data.frame(subset(prod, doy == 112 & predicted_fishing==1))
coordinates(m) <- ~longitude+latitude
r <- raster(nrows = 170, ncols=30, xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx )
ras <- rasterize(m, r, field=m$adj_lbs, fun = sum)
plot(ras)
map('state',add=T)

m1 <- as.data.frame(subset(prod, doy == 113 & predicted_fishing==1))
coordinates(m1) <- ~longitude+latitude
ras1 <- rasterize(m1, r, field=m1$adj_lbs, fun = sum)

m2 <- as.data.frame(subset(prod, doy == 114 & predicted_fishing==1))
coordinates(m2) <- ~longitude+latitude
ras2 <- rasterize(m2, r, field=m2$adj_lbs, fun = sum)

m_stack <- stack(ras, ras1, ras2)
mp <- as(m_stack, 'SpatialGridDataFrame')

spplot(mp)

brks = seq(0,3000, by = 5000)
plot(ras, breaks = brks)
plot(ras)

#make raster time series
xmn <- min(prod$longitude); xmx = max(prod$longitude)
ymn <- min(prod$latitude); ymx = max(prod$latitude)
r <- raster(nrows = 170, ncols=30, xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx )

m <-  as.data.frame(subset(prod, doy == doys[1] & predicted_fishing==1))
coordinates(m) <- ~longitude+latitude
ras <- rasterize(m, r, field=m$adj_lbs, fun = sum)

yr_lists <- list()
yrs <-  2009:2013
for(j in 1:length(yrs)){
  doys <- unique(prod$adj_day[which(prod$year==yrs[j] & prod$predicted_fishing==1)])
  list_rast <- list()
  for(i in 1:length(doys)){
    m <-  as.data.frame(subset(prod, adj_day == doys[i] & year == yrs[j] & predicted_fishing==1))
    if(nrow(m)==0) next
    coordinates(m) <- ~longitude+latitude
    list_rast[[i]] <- rasterize(m, r, field=m$adj_lbs, fun = sum)
  }
  
  rs <- stack(unlist(list_rast))
  
  # get values for each cell over stack
  mat  <- extract(rs, 1:ncell(rs))
  # plot(doys,1:length(doys), type='n',ylim = c(0,70000))
  # for(i in 1:nrow(mat)){
  #   if(all(is.na(mat[i])))next
  #   points(doys,mat[i,],type='l')
  # }
  
  foo <- rasterToPoints( rs )
  foobar <- as.data.frame(foo[,1:2])
  foobar$lbs <- rowSums(foo[,3:ncol(foo)],na.rm=T)
  yr_lists[[j]] <- foobar
  ggplot(foobar, aes(x = x, y = y, color = lbs)) + geom_raster() + coord_map() + theme_dark()
}

for(i in 1:length(yrs)){
  yr_lists[[i]]$year <- yrs[i]
}

yr_all <- as.data.frame(do.call(rbind,yr_lists))
ggplot(yr_all, aes(x=x, y = y, color = lbs)) + geom_point() + facet_grid(year~.)
ggplot(yr_all, aes(x=x, y = y)) + geom_tile(aes(fill=log(lbs))) + facet_grid(.~year) + coord_map()


# going back to cpue for social versus general boats
# looks like solitary fishing is almost always better
trip_stats %>% group_by(drvid) %>% 
  mutate(med_lat = median(median_lat, na.rm = T)) %>%
  ungroup() %>%
  mutate(area_fished = ifelse(med_lat>44, "N","S")) %>%
  filter(!is.na(area_fished)) %>%
  ggplot(aes(x = reorder(drvid, lbs, median), y = lbs/duration_hrs, fill=type)) + 
  geom_boxplot() + facet_wrap(~area_fished, scale='free_x', ncol=1)

# but this is maybe because social trips tend to be longer?
trip_stats  %>% group_by(drvid) %>% 
  mutate(med_lat = median(median_lat, na.rm = T)) %>%
  ungroup() %>%
  mutate(area_fished = ifelse(med_lat>44, "N","S")) %>%
  filter(!is.na(area_fished)) %>%
  ggplot(aes(x = reorder(drvid, lbs, median), y = duration_hrs, fill=type)) + 
  geom_boxplot() + facet_wrap(~area_fished, scale='free_x', ncol=1)

# but they also tend to catch more fish
trip_stats %>% group_by(drvid) %>% 
  mutate(med_lat = median(median_lat, na.rm = T)) %>%
  ungroup() %>%
  mutate(area_fished = ifelse(med_lat>44, "N","S")) %>%
  filter(!is.na(area_fished)) %>%
  ggplot(aes(x = reorder(drvid, lbs, median), y = lbs, fill=type)) + 
  geom_boxplot() + facet_wrap(~area_fished, scale='free_x', ncol=1)

# and often look like they get just as much money
trip_stats %>% group_by(drvid) %>% 
  mutate(med_lat = median(median_lat, na.rm = T)) %>%
  ungroup() %>%
  mutate(area_fished = ifelse(med_lat>44, "N","S")) %>%
  filter(!is.na(area_fished)) %>%
  ggplot(aes(x = reorder(drvid, lbs, median), y = revenue, fill=type)) + 
  geom_boxplot() + facet_wrap(~area_fished, scale='free_x', ncol=1)

trip_stats %>% group_by(drvid) %>% 
  mutate(med_lat = median(median_lat, na.rm = T)) %>%
  ungroup() %>%
  mutate(area_fished = ifelse(med_lat>44, "N","S")) %>%
  filter(!is.na(area_fished)) %>%
  ggplot(aes(x = reorder(drvid, lbs, median), y = revenue/duration_hrs, fill=type)) + 
  geom_boxplot() + facet_wrap(~area_fished, scale='free_x', ncol=1)

# does the difference between average returns predict what proportion of the time they are social?

social_cost <- trip_stats %>% filter(duration_hrs > 6) %>% group_by(drvid) %>%
  mutate(median_lat =median(median_lat, na.rm = T)) %>%
  group_by(drvid,type) %>% 
  summarize(wage = mean(revenue/duration_hrs), median_lat = unique(median_lat)) %>%
  group_by(drvid) %>%
  mutate(n_entries = length(wage)) %>%
  filter(n_entries==2) %>%
  summarize(social_cost = diff(wage), median_lat = unique(median_lat)) %>%
  left_join(distinct(dplyr::select(generalism, drvid, percent_rev_shrimp_overall)))
  
ggplot(social_cost, aes(x = percent_rev_shrimp_overall, y = social_cost)) +
  geom_point(aes(color=median_lat), size = 4) + geom_hline(yintercept = 0) + 
  scale_color_continuous(low="indianred",high="steelblue")

