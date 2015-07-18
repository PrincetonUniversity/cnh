# look at relationship between vessel gross revenue and variance (measured as coefficient of variation)
# load data
#----
library(stringr); library(vegan); library(reshape2); library(scales); library(plyr); library(ggplot2)

tickets <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/3_analyzeMetiers/tickets.RDS")
#----
# calculate gross revenue, coefficient of variation, sd by year
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
revenue_diversity <- function(fishery){
  crab_vessels <- unique(subset(tickets, metier %in% fishery)$drvid)
  crab_inclusive <- subset(tickets, drvid %in% crab_vessels)
  
  # get yearly revenue
  yr_mets <- ddply(crab_inclusive, .(drvid, year, metier), summarize, 
                   adj_revenue = sum(adj_revenue))
  # transform into long-format, calculate simpsons diversity per vessel per year
  
  diversity_year <- ddply(yr_mets, .(drvid, year), c(simp_diversity, shannon_diversity))
  colnames(diversity_year) <- c("drvid","year","simpsons","shannons")
  
  # seperate vessels that only do crab, versus those that do other things. 
  # calculate yearly revenue
  num_fish <- ddply(crab_inclusive, .(drvid,year), summarize, 
                    num_fisheries = length(unique(metier)), 
                    yr_revenue = sum(adj_revenue))
  
  num_fish <- merge(num_fish, diversity_year, by=c("drvid","year"))
  
  # average number of vessels, CV of revenue, average revenue, average diversity
  yr_stats <- ddply(num_fish, .(drvid), summarize, 
                    mean_num_fisheries = mean(num_fisheries), 
                    cv_revenue = sd(yr_revenue)/mean(yr_revenue), 
                    sd_revenue = sd(yr_revenue),
                    mean_revenue = mean(yr_revenue),
                    mean_simpson = mean(simpsons),
                    mean_shannon = mean(shannons)
                    )
  return(list(num_fish=num_fish, yr_stats=yr_stats))
}

# look for all metiers - calculate revenue diversity
all_metiers <- unique(tickets$metier)
yrdf <- revenue_diversity(all_metiers)

saveRDS(yrdf, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/3_analyzeMetiers/yrdf.RDS")

