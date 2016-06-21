tickets <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-10-08/tickets.RDS")

#revenue by vessel, by fishery, by year
yrlyrev <- ddply(tickets, .(veid, year, metier), summarize, revenue = sum(ppp*landed_wt))

# calculate simpons, find proportion of each metier in each year and square
simp_yr <- ddply(yrlyrev, .(veid, year), summarize, si = sum(revenue/sum(revenue)^2))
mean_simp <- ddply(simp_yr, .(veid), summarize, mean_simp = mean(si))

annual_rev <- ddply(yrlyrev, .(veid, year), summarize, rev = sum(revenue))
# remove those that make < 5000 a year
annual_rev <- annual_rev[-which(annual_rev$rev < 5000),]
cov_rev <- ddply(annual_rev, .(veid), summarize, covar = sd(rev)/mean(rev))

all <- merge(mean_simp, cov_rev, by = "veid")
all <- all[-which(is.nan(all$mean_simp)),]
all <- all[-which(is.na(all$covar)),]
ggplot(all, aes(x = mean_simp, y = covar)) + geom_point(alpha=.5) + theme_minimal()

# look at how mean yearly revenue compares to mean variance within that year...?
mean_rev <- ddply(annual_rev, .(veid) , summarize, mean(rev), sd)

