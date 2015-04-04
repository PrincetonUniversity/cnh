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
    all_ports <- read.csv("/Users/efuller/1/CNH/processedData/spatial/ports/all_ports.csv", 
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
                      weighted_lat = weighted.mean(lat, adj_revenue),
                      # is yearly, so just moves it cluster to yearly resolution
                      cluster = unique(cluster),
                      num_states = length(unique(state)), 
                      single_state = ifelse(num_states == 1, unique(state), "multi"),
                      num_ports  = length(unique(pcid)), 
                      name_single_port = ifelse(num_ports==1, unique(pcid), "multi"),
                      w.num_procs = weighted.mean(num_procs, adj_revenue),
                      adj_revenue = sum(adj_revenue))
    
    num_fish <- merge(num_fish, diversity_year, by=c("drvid","year"))
    
    # average number of vessels, CV of adj_revenue, average adj_revenue, average diversity
    yr_stats <- ddply(num_fish, .(drvid), summarize, 
                      median_num_fisheries = median(num_fisheries), 
                      cv_adj_revenue = sd(adj_revenue)/mean(adj_revenue), 
                      sd_adj_revenue = sd(adj_revenue),
                      mean_adj_revenue = mean(adj_revenue),
                      mean_simpson = mean(simpsons),
                      mean_shannon = mean(shannons),
                      med_latitude = median(latitude),
                      mean_weight_lat = weighted.mean(weighted_lat, adj_revenue),
                      single_cluster = ifelse(length(unique(cluster))==1, unique(cluster),"multi"), 
                      single_landing_state = ifelse(length(unique(single_state))==1, single_state, "multi"),
                      single_port = ifelse(length(unique(name_single_port))==1, name_single_port, "multi"))
    
    return(list(num_fish=num_fish, yr_stats=yr_stats))
  }
  
  # look for all metiers - calculate adj_revenue diversity
  all_metiers <- unique(data$metier)
  yrdf <- adj_revenue_diversity(all_metiers)
  
  saveRDS(yrdf, "/Users/efuller/1/CNH/processedData/catch/1_cleaningData/yrdf.RDS")
  return(yrdf)
}