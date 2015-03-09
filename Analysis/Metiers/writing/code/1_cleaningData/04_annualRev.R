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
    
    # get mean latitude - find latitude for each port. 
    pcid <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/data/pcid.csv", stringsAsFactors = F)
    pcid <- subset(pcid, Pcid %in% unique(data$pcid))
    # painful, doing it for now
    
    pcid$latitude <- c(39.224893,37.751957, 48.500226, 38.911100, 46.184623,35.175198, 38.313621,43.119615,37.859178, 48.689689,48.990874,37.902163,39.441044,42.050410, 45.884918, 43.349049, 41.749900,36.947737, 33.454366,44.810097, 40.805712, 47.998856,40.726245, 43.987470, 48.538127,42.405515,46.900394,46.012960, 34.141570, 48.369945,47.902987, 33.706400,46.299853,36.618916,36.806517,35.366324,48.370974, 44.625197, 45.701929,45.404521, 33.605090, 37.831454, 34.354413,NA,36.778984,33.178001,46.247297,41.834321,40.933045,33.646691, 47.063427,39.314516, 47.854287,42.739815,32.684806,37.597894,35.223022, 38.087555,NA,NA,34.147309,48.123373,45.206981,37.466655,37.923771,38.095665,34.387405,32.670935,47.601886, 48.073745,37.748742,47.216280, 37.856995,44.910513,33.701236,47.281443,45.485446,38.177928,48.112239,33.712004,41.054473,34.248640,43.666526,46.539371,44.422257,33.714138,46.888297)
    crab_inclusive <- merge(crab_inclusive, pcid, by.x="pcid",by.y="Pcid", all.x=TRUE)
    # get yearly adj_revenue
    yr_mets <- ddply(crab_inclusive, .(drvid, year, metier), summarize, 
                     adj_revenue = sum(adj_revenue),latitude = mean(latitude), cluster = unique(cluster))
    # transform into long-format, calculate simpsons diversity per vessel per year
    
    diversity_year <- ddply(yr_mets, .(drvid, year), c(simp_diversity, shannon_diversity))
    colnames(diversity_year) <- c("drvid","year","simpsons","shannons")
    
    # seperate vessels that only do crab, versus those that do other things. 
    num_fish <- ddply(crab_inclusive, .(drvid,year), summarize, 
                      num_fisheries = length(unique(metier)), 
                      adj_revenue = sum(adj_revenue),
                      latitude = mean(latitude),
                      cluster = unique(cluster))
    
    num_fish <- merge(num_fish, diversity_year, by=c("drvid","year"))
    
    # average number of vessels, CV of adj_revenue, average adj_revenue, average diversity
    yr_stats <- ddply(num_fish, .(drvid), summarize, 
                      median_num_fisheries = median(num_fisheries), 
                      cv_adj_revenue = sd(adj_revenue)/mean(adj_revenue), 
                      sd_adj_revenue = sd(adj_revenue),
                      mean_adj_revenue = mean(adj_revenue),
                      mean_simpson = mean(simpsons),
                      mean_shannon = mean(shannons),
                      mean_latitude = mean(latitude),
                      single_cluster = ifelse(length(unique(cluster))==1, unique(cluster),"multi"))
    
    return(list(num_fish=num_fish, yr_stats=yr_stats))
  }
  
  # look for all metiers - calculate adj_revenue diversity
  all_metiers <- unique(data$metier)
  yrdf <- adj_revenue_diversity(all_metiers)
  
  saveRDS(yrdf, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/yrdf.RDS")
  return(yrdf)
}