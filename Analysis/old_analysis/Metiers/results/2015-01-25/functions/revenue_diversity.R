# requires simp_diversity() and shannon_diversity() to be run
# takes a given fishery and calculates
#   revenue by year
#   simpons diversity by year
#   shannons diversity by year
#   number of fisheries participated in by year
#   means of all these things over 5 years plus variances
#   returns a list of two data frames. one by year, one averaged

revenue_diversity <- function(fishery){
  crab_vessels <- unique(subset(tickets, metier %in% fishery)$drvid)
  crab_inclusive <- subset(tickets, drvid %in% crab_vessels)
  
  # get yearly revenue
  yr_mets <- ddply(crab_inclusive, .(drvid, year, metier), summarize, 
                   adj_revenue = sum(adj_revenue))
  # transform into long-format, calculate simpsons diversity per vessel per year
  
  diversity_year <- ddply(yr_mets, .(drvid, year), 
                          c(simp_diversity, shannon_diversity))
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
