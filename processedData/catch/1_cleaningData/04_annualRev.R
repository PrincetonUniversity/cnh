annualRev <- function(data){
  # look at relationship between vessel gross adj_revenue and variance (measured as coefficient of variation)
  # load data----
  library(stringr); library(vegan); library(reshape2); library(scales); library(plyr); library(ggplot2)
  
  # calculate gross adj_revenue, coefficient of variation, sd by year
  # added mean latitude. takes mean at year level, and then mean of that across years
  # helper functions
  simp_diversity <- function(x){
    melt_df <- melt(x, id.vars = c("drvid", "metier"), measure.vars = "adj_revenue")
    cast_df <- dcast(melt_df, drvid ~ metier, sum)
    row.names(cast_df) <- cast_df$drvid
    cast_df$drvid <- NULL
    diversity_rev <- diversity(cast_df, index = "simpson")
    return(diversity_rev)
  }
  shannon_diversity <- function(x){
    melt_df <- melt(x, id.vars = c("drvid", "metier"), measure.vars = "adj_revenue")
    cast_df <- dcast(melt_df, drvid ~ metier, sum)
    row.names(cast_df) <- cast_df$drvid
    cast_df$drvid <- NULL
    diversity_rev <- diversity(cast_df, index = "shannon")
    return(diversity_rev)
  }
  adj_revenue_diversity <- function(fishery){
    crab_vessels <- unique(subset(data, metier %in% fishery)$drvid)
    crab_inclusive <- subset(data, drvid %in% crab_vessels)
    
    # remove any duplicates - not sure why they are here to begin with. 
    crab_inclusive <- crab_inclusive[-which(duplicated(crab_inclusive)),]
    
    # get mean latitude - find latitude and mean number of processors for each port 
    all_ports <- read.csv("/Users/efuller/1/CNH/processedData/spatial/ports/all_ports_mpa.csv", 
                          stringsAsFactors=FALSE)
    crab_inclusive <- merge(crab_inclusive, all_ports, by.x="pcid",by.y="Pcid", all.x=TRUE, all.y = FALSE)
    # get yearly adj_revenue for diversity
    yr_mets <- ddply(crab_inclusive, .(drvid, year, metier), summarize, 
                     adj_revenue = sum(adj_revenue))
    # transform into long-format, calculate simpsons diversity per vessel per year
    
    diversity_year <- ddply(yr_mets, .(drvid, year), c(simp_diversity, shannon_diversity))
    colnames(diversity_year) <- c("drvid","year","simpsons","shannons")
    
    # seperate vessels that only do crab, versus those that do other things. 
    num_fish <- ddply(crab_inclusive, .(drvid,year), summarize, 
                      num_fisheries = length(unique(metier)), 
                      lbs = sum(round_wt),
                      latitude = median(lat),
                      # latitude of landings weighted by yearly revenue
                      w.lat = weighted.mean(lat, adj_revenue),
                      # is yearly, so just moves it cluster to yearly resolution
                      cluster = unique(cluster),
                      num_states = length(unique(state)), 
                      single_state = ifelse(num_states == 1, unique(state), "multi"),
                      num_ports  = length(unique(pcid)), 
                      name_single_port = ifelse(num_ports==1, unique(pcid), "multi"),
                      w.num_procs = weighted.mean(num_procs, adj_revenue),
                      w.dist_city = weighted.mean(km.big.city, adj_revenue),
                      w.hab1 = weighted.mean(h1, adj_revenue),
                      w.hab2 = weighted.mean(h2, adj_revenue),
                      w.hab3 = weighted.mean(h3, adj_revenue),
                      w.hab4 = weighted.mean(h4, adj_revenue),
                      w.per.hab1 = weighted.mean(per.h1, adj_revenue),
                      w.per.hab2 = weighted.mean(per.h2, adj_revenue),
                      w.per.hab3 = weighted.mean(per.h3, adj_revenue),
                      w.per.hab4 = weighted.mean(per.h4, adj_revenue),
                      w.hab_simp_div = weighted.mean(habitat_simp, adj_revenue),
                      w.dist_upper_slope = weighted.mean(dist_upper_slope, adj_revenue),
                      w.dist_lower_slope = weighted.mean(dist_lower_slope, adj_revenue),
                      w.mpa_cover = weighted.mean(mpa_cover, adj_revenue),
                      adj_revenue = sum(adj_revenue),
                      num_trips = length(unique(trip_id)))
    
    num_fish <- merge(num_fish, diversity_year, by=c("drvid","year"))
    
    # average number of vessels, CV of adj_revenue, average adj_revenue, average diversity
    yr_stats <- ddply(num_fish, .(drvid), summarize, 
                      median_num_fisheries = median(num_fisheries), 
                      cv_adj_revenue = sd(adj_revenue)/mean(adj_revenue), 
                      sd_adj_revenue = sd(adj_revenue),
                      mean_adj_revenue = mean(adj_revenue),
                      mean_simpson = mean(simpsons),
                      sd_simpson = sd(simpsons),
                      mean_shannon = mean(shannons),
                      med_latitude = median(latitude),
                      mean_w.lat = weighted.mean(w.lat, adj_revenue),
                      single_cluster = ifelse(length(unique(cluster))==1, unique(cluster),"multi"), 
                      single_landing_state = ifelse(length(unique(single_state))==1, 
                                                    single_state, "multi"),
                      single_port = ifelse(length(unique(name_single_port))==1, 
                                           name_single_port, "multi"),
                      w.num_procs = weighted.mean(w.num_procs, adj_revenue),
                      w.dist_city = weighted.mean(w.dist_city, adj_revenue), 
                      w.hab1 = weighted.mean(w.hab1, adj_revenue), 
                      w.hab2 = weighted.mean(w.hab2, adj_revenue), 
                      w.hab3 = weighted.mean(w.hab3, adj_revenue),
                      w.hab4 = weighted.mean(w.hab4, adj_revenue),
                      w.per.hab1 = weighted.mean(w.per.hab1, adj_revenue), 
                      w.per.hab2 = weighted.mean(w.per.hab2, adj_revenue),
                      w.per.hab3 = weighted.mean(w.per.hab3, adj_revenue),
                      w.per.hab4 = weighted.mean(w.per.hab4, adj_revenue),
                      w.habitat.simp.div = weighted.mean(w.hab_simp_div, adj_revenue),
                      w.dist_upper_slope = weighted.mean(w.dist_upper_slope, adj_revenue),
                      w.dist_lower_slope = weighted.mean(w.dist_lower_slope, adj_revenue),
                      w.mpa_cover = weighted.mean(w.mpa_cover, adj_revenue),
                      median_trips = median(num_trips),
                      sd_num_trips = sd(num_trips))
    
    return(list(num_fish=num_fish, yr_stats=yr_stats))
  }
  
  # look for all metiers - calculate adj_revenue diversity
  all_metiers <- unique(data$metier)
  yrdf <- adj_revenue_diversity(all_metiers)
  
  saveRDS(yrdf, "/Users/efuller/1/CNH/processedData/catch/1_cleaningData/yrdf.RDS")
  return(yrdf)
}