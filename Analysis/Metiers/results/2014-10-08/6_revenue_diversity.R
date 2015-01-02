# look at relationship between vessel gross revenue and variance (measured as coefficient of variation)
# load data
#----
library(stringr); library(vegan); library(reshape2); library(scales); library(plyr); library(ggplot2)

path1 <- "/Users/efuller/1/CNH/Analysis/Metiers/results/2014-10-01/"
path2 <- "/Users/efuller/1/CNH/Analysis/Metiers/results/2014-10-08/predicted_metiers/2010/"
path3 <- "/Users/efuller/1/CNH/Analysis/Metiers/results/2014-10-08/metier_lists/"
# load data
filtered_ftl <- readRDS(paste0(path1,"filtered_ftl.RDS"))

# load metiers
files <- list.files(path=path2)
predicteds <- do.call(rbind, lapply(paste0(path2,files), readRDS))
other_files <- list.files(path=path3)
other_files <- other_files[grep(2010, other_files)] # just want 2010 classified metiers

predicteds$predicted_metier <- paste0(predicteds$predicted_metier,str_sub(predicteds$trip_id, start = -4))

classifieds <- do.call(rbind, lapply(paste0(path3, other_files), read.csv))
classifieds$X <- NULL
classifieds$metier <- paste0(classifieds$metier, 2010)
classifieds$ftid <- paste0(classifieds$ftid, 2010)
colnames(classifieds) <- c("trip_id", "metier")
colnames(predicteds) <- c("trip_id", "metier")

metiers <- rbind(classifieds, predicteds)

# merge metiers
tickets <- merge(filtered_ftl, metiers, by = "trip_id")
length(unique(tickets$trip_id)) == length(unique(metiers$trip_id))
# should be TRUE

# remove the last four digits from metiers, all from the same year now, directly comparable
tickets$metier <- str_sub(tickets$metier, end=-4 )

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
                    mean_revenue = mean(revenue),
                    mean_simpson = mean(simpsons),
                    mean_shannon = mean(shannons))
  return(list(num_fish=num_fish, yr_stats=yr_stats))
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# look for all metiers
all_metiers <- unique(tickets$metier)
yrdf <- revenue_diversity(all_metiers)
saveRDS(yrdf, "/Users/efuller/1/CNH/Analysis/Metiers/results/2014-10-08/yrdf.RDS")
#----
# plot diversity and variance
#----
p1 <- ggplot(subset(yrdf[["yr_stats"]], !is.na(cv_revenue)), aes(x = mean_simpson, y = cv_revenue)) + 
  geom_point(alpha=.75) +
  geom_smooth(method = "lm", colour="steelblue", size=2) +
  theme_minimal() + 
  ylab("CV(annual revenue)") + 
  xlab("mean Simpson's index")

p2 <- ggplot(subset(yrdf[["yr_stats"]], !is.na(cv_revenue)), aes(x = mean_shannon, y = cv_revenue)) + 
  geom_point(alpha=.75) + 
  geom_smooth(method="lm", colour="steelblue", size = 2) + 
  theme_minimal() + 
  ylab("CV(annual revenue)") + 
  xlab("mean Shannon-Wiener Diversity index")

multiplot(p1, p2, cols=2)

# calculate SW index for each vessel for each year
tickets$revenue <- with(tickets, ppp*landed_wt)
melt_df <- melt(tickets, id.vars = c("veid_year", "metier"), measure.vars = "revenue")
cast_df <- dcast(melt_df, veid_year ~ metier, sum)
row.names(cast_df) <- cast_df$veid_year
cast_df$veid_year <- NULL
diversity_rev <- diversity(cast_df)

diversity_rev <- as.data.frame(diversity_rev)
diversity_rev$veid_year <- row.names(diversity_rev)
row.names(diversity_rev) <- NULL

revenues_diversity <- merge(revenues, diversity_rev, by = "veid_year")
# get rid of weird veids
revenues_diversity <- revenues_diversity[-c(1,2,3,4,5),]

# plot gross revenue on y, coefficient of variation, also with sd. sethi and holland papers do it with coefficient of variation, james wanted sd
ggplot(revenues_diversity, aes(x = log(sd_rev), y = log(gross_revenue))) + geom_point(aes(colour = log(diversity_rev)), alpha = 1, size = 1.15) + theme_minimal()

ggplot(revenues_diversity, aes(x = sqrt(sd_rev), y = sqrt(gross_revenue))) + geom_point(aes(colour = log(diversity_rev)), alpha = 1) + theme_minimal()

ggplot(revenues_diversity, aes(x = sd_rev, y = gross_revenue)) + geom_point(aes(colour = log(diversity_rev))) + theme_minimal()

ggplot(revenues_diversity, aes(x = log(covar_rev), y = log(gross_revenue))) + geom_point(alpha=.45) + theme_minimal()

ggplot(revenues_diversity, aes(x = log(covar_rev), y = log(gross_revenue))) + geom_point(aes(colour = log(diversity_rev))) + theme_minimal()


ggplot(revenues_diversity, aes(x = log(diversity_rev), y = log(gross_revenue))) + geom_point() + theme_minimal()

ggplot(revenues_diversity, aes(x = log(diversity_rev), y = log(gross_revenue))) + geom_point(alpha=.25) + theme_minimal()

# variability versus diversity
ggplot(revenues_diversity, aes(x = diversity_rev, y = covar_rev)) + geom_point(alpha = .5) + theme_minimal()

ggplot(revenues_diversity, aes(x = log(diversity_rev), y = log(covar_rev))) + geom_point(alpha = .5) + theme_minimal()

ggplot(revenues_diversity, aes(x = diversity_rev, y = sd_rev)) + geom_point(alpha = .5) + theme_minimal()

ggplot(revenues_diversity, aes(x = log(diversity_rev), y = log(sd_rev))) + geom_point(alpha = .5) + theme_minimal()

