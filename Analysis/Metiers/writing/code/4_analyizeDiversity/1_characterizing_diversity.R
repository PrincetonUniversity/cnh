rm(list=ls())
yrdf <- readRDS("code/3_analyzeMetiers/yrdf.RDS")
tickets <- readRDS("code/3_analyzeMetiers/tickets.RDS")

library(igraph); library(vegan)

# color code vessels by strategy
#----
# first clustery by strategy
  # make trip table: rows are ves, cols are number of trips across all years. 
  # IMPORTANT: i am not splitting up by years and using one year to classify. 
  # figure that the plots of income diversity integrate across all five years, 
  # so strategies should too
    # make table of strategy partcipation by vessel
    strat_table <- with(tickets, table(veid, metier))
    
    # calculate bray curtis
    bc <- as.matrix(vegdist(strat_table))

    # make similarity matrix
    sim_mat <- 1 - bc

    # build undirected, weighted network
    strat_net <- graph.adjacency(sim_mat, weighted = TRUE, mode = "undirected")

    # use infoMap to find communities
    strats <- infomap.community(strat_net, e.weights = E(strat_net)$weight)

    # add communities to yrdf$yr_stats
      yrdf$yr_stats$strategies <- strats$membership

      # 37 strategies 
      max(yrdf$yr_stats$strategies)
      table(yrdf$yr_stats$strategies)
    
    # make colorscheme
    paint <- colorRampPalette(brewer.pal(8,"Spectral"))(max(yrdf$yr_stats$strategies))
    yrdf$yr_stats$color <- paint[yrdf$yr_stats$strategies]

# try to plot with color

with(yrdf$yr_stats, plot(mean_simpson, cv_revenue, pch = 19, cex=.75, col = color, bty = "n"))
quad <- lm(cv_revenue ~ mean_simpson + I(mean_simpson^2), yrdf$yr_stats)
fitline <- predict(quad, data.frame(mean_simpson=seq(0,1,.01)))
lines(seq(0,1,.01), fitline, lwd = 5, col = "white")

# looking at residuals
plot(quad)
# suggests heteroscedascity


# more plots
## mean revenue versus simpsons

## sd revenue versus simpsons

## residuals of CV(x) versus S


# old stuff, for referenc
#----
#----
# plot diversity and variance
#----

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
tickets$veid_year <- paste0(tickets$veid, tickets$year)
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