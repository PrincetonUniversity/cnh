# look at relationship between vessel gross revenue and variance (measured as coefficient of variation)
# load data
#----
library(stringr); library(vegan); library(reshape2); library(scales); library(plyr); library(ggplot2)

tickets <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/3_analyzeMetiers/tickets.RDS")
#----
# calculate gross revenue, coefficient of variation, sd by year
# helper functions
simp_diversity <- function(x){
  melt_df <- melt(x, id.vars = c("veid", "metier"), measure.vars = "revenue")
  cast_df <- dcast(melt_df, veid ~ metier, sum)
  row.names(cast_df) <- cast_df$veid
  cast_df$veid <- NULL
  diversity_rev <- diversity(cast_df, index = "simpson")
  return(diversity_rev)
}
shannon_diversity <- function(x){
  melt_df <- melt(x, id.vars = c("veid", "metier"), measure.vars = "revenue")
  cast_df <- dcast(melt_df, veid ~ metier, sum)
  row.names(cast_df) <- cast_df$veid
  cast_df$veid <- NULL
  diversity_rev <- diversity(cast_df, index = "shannon")
  return(diversity_rev)
}
revenue_diversity <- function(fishery){
  crab_vessels <- unique(subset(tickets, metier %in% fishery)$veid)
  crab_inclusive <- subset(tickets, veid %in% crab_vessels)
  
  # add revenue
  crab_inclusive$revenue <- crab_inclusive$ppp * crab_inclusive$landed_wt
  # get yearly revenue
  yr_mets <- ddply(crab_inclusive, .(veid, year, metier), summarize, 
                   revenue = sum(revenue))
  # transform into long-format, calculate simpsons diversity per vessel per year
  
  diversity_year <- ddply(yr_mets, .(veid, year), c(simp_diversity, shannon_diversity))
  colnames(diversity_year) <- c("veid","year","simpsons","shannons")
  
  # seperate vessels that only do crab, versus those that do other things. 
  num_fish <- ddply(crab_inclusive, .(veid,year), summarize, 
                    num_fisheries = length(unique(metier)), 
                    revenue = sum(ppp*landed_wt))
  
  num_fish <- merge(num_fish, diversity_year, by=c("veid","year"))
  
  # average number of vessels, CV of revenue, average revenue, average diversity
  yr_stats <- ddply(num_fish, .(veid), summarize, 
                    mean_num_fisheries = mean(num_fisheries), 
                    cv_revenue = sd(revenue)/mean(revenue), 
                    sd_revenue = sd(revenue),
                    mean_revenue = mean(revenue),
                    mean_simpson = mean(simpsons),
                    mean_shannon = mean(shannons))
  return(list(num_fish=num_fish, yr_stats=yr_stats))
}

# look for all metiers - calculate revenue diversity
all_metiers <- unique(tickets$metier)
yrdf <- revenue_diversity(all_metiers)

saveRDS(yrdf, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/3_analyzeMetiers/yrdf.RDS")

