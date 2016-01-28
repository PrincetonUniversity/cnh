# look at crab specialists versus generalists
revenue_diversity <- function(fishery){
  crab_vessels <- unique(subset(tickets, metier %in% fishery)$veid)
  crab_inclusive <- subset(tickets, veid %in% crab_vessels)

  # seperate vessels that only do crab, versus those that do other things. 
  num_fish <- ddply(crab_inclusive, .(veid,year), summarize, 
                    num_fisheries = length(unique(metier)), 
                    revenue = sum(ppp*landed_wt))
  
  # average number of vessels, CV of revenue, average revenue
  yr_stats <- ddply(num_fish, .(veid), summarize, 
                    mean_num_fisheries = mean(num_fisheries), 
                    cv_revenue = sd(revenue)/mean(revenue), 
                    mean_revenue = mean(revenue))
  return(list(num_fish=num_fish, yr_stats=yr_stats))
}

crab <- revenue_diversity("POT_12")

with(crab[["yr_stats"]], plot(mean_num_fisheries, cv_revenue, pch = 19, cex=.5))
slope <- lm(cv_revenue ~ mean_num_fisheries, crab[["yr_stats"]])
abline(slope, lwd=4, col="indianred")

with(subset(crab[["yr_stats"]], mean_num_fisheries < 10 & mean_revenue >5000 & mean_revenue < 4e06), plot(mean_num_fisheries, mean_revenue, pch=19, cex=.5))
slope <- lm(mean_revenue ~ mean_num_fisheries, subset(crab[["yr_stats"]], mean_num_fisheries < 10 & mean_revenue >5000 & mean_revenue < 4e06))
abline(slope, lwd=4, col="indianred")

# look at dover specialists versus generalists
dover <- revenue_diversity("TWL_12")

with(subset(dover[["yr_stats"]], mean_revenue>5000), plot(mean_num_fisheries, cv_revenue, pch = 19, cex=.5))
slope <- lm(cv_revenue ~ mean_num_fisheries, subset(dover[["yr_stats"]], mean_revenue>5000))
abline(slope, lwd=4, col="indianred")
with(subset(dover[["yr_stats"]], mean_revenue>5000), plot(mean_num_fisheries, mean_revenue,pch=19, cex=.5))
slope <- lm(mean_revenue ~ mean_num_fisheries, subset(dover[["yr_stats"]], mean_revenue>5000))
abline(slope, lwd=4, col="indianred")

# look at tuna
albacore <- revenue_diversity("TLS_22")
with(subset(albacore[["yr_stats"]], mean_revenue>5000), plot(mean_num_fisheries, cv_revenue, pch = 19, cex=.5))
slope <- lm(cv_revenue ~ mean_num_fisheries, subset(albacore[["yr_stats"]], mean_revenue>5000))
abline(slope, lwd=4, col="indianred")
with(subset(albacore[["yr_stats"]], mean_num_fisheries<15), plot(mean_num_fisheries, mean_revenue,pch=19, cex=.5))
slope <- lm(mean_revenue ~ mean_num_fisheries, albacore[["yr_stats"]])
abline(slope, lwd=4, col="indianred")

# look at salmon 
salmon <- revenue_diversity("TLS_12")
with(subset(salmon[["yr_stats"]],mean_revenue>5000), plot(mean_num_fisheries, cv_revenue, pch = 19, cex=.5))
slope <- lm(cv_revenue ~ mean_num_fisheries,subset(salmon[["yr_stats"]],mean_revenue>5000))
abline(slope, lwd=4, col="indianred")
with(subset(salmon[["yr_stats"]], mean_revenue>5000 & mean_revenue<8e06), plot(mean_num_fisheries, mean_revenue,cex=.5, pch=19))
slope <- lm(mean_revenue ~ mean_num_fisheries, subset(salmon[["yr_stats"]], mean_revenue>5000 & mean_revenue<8e06))
abline(slope, lwd=4, col="indianred")
abline(h=5000, col="steelblue",lwd=5)

# look at shrimp
shrimp <- revenue_diversity("TWS_12")
with(subset(shrimp[["yr_stats"]],mean_revenue>5000), plot(mean_num_fisheries, cv_revenue, pch = 19, cex=.5))
with(subset(shrimp[["yr_stats"]], mean_revenue>5000 & mean_revenue <1e07), plot(mean_num_fisheries, mean_revenue, pch=19, cex=.5))

# sablefish longline 
sbl_lgl <- revenue_diversity("HKL_12")
with(sbl_lgl[["yr_stats"]], plot(mean_num_fisheries, cv_revenue, pch = 19, cex=.5))
with(subset(sbl_lgl[["yr_stats"]], mean_revenue<8e06), plot(mean_num_fisheries, mean_revenue))

# sablefish pots 
sbl_pot <- revenue_diversity("POT_42")
with(sbl_pot[["yr_stats"]], plot(mean_num_fisheries, cv_revenue, pch = 19, cex=.5))
with(subset(sbl_pot[["yr_stats"]], mean_revenue<8e06), plot(mean_num_fisheries, mean_revenue))

# whiting 
whiting <- revenue_diversity("TWL_32")
with(whiting[["yr_stats"]], plot(mean_num_fisheries, cv_revenue, pch = 19, cex=.5))
with(subset(whiting[["yr_stats"]], mean_revenue<8e06), plot(mean_num_fisheries, mean_revenue))

# salmon/dogfish midwater trawl
mid_salmon <- revenue_diversity("TWL_52")
with(mid_salmon[["yr_stats"]], plot(mean_num_fisheries, cv_revenue, pch = 19, cex=.5))
with(subset(mid_salmon[["yr_stats"]], mean_revenue<8e06), plot(mean_num_fisheries, mean_revenue))

# pole albacore
pol_albc <- revenue_diversity("HKL_92")
with(pol_albc[["yr_stats"]], plot(mean_num_fisheries, cv_revenue, pch = 19, cex=.5))
with(subset(pol_albc[["yr_stats"]], mean_revenue<8e06), plot(mean_num_fisheries, mean_revenue))

slope <- lm(mean_revenue ~ mean_num_fisheries, pol_albc[["yr_stats"]])
abline(foo,col="red")


# specialists
specialists <- unique(subset(crab[["yr_stats"]], mean_num_fisheries <=1)$veid)

svg <- subset(tickets, metier=="POT_12")
svg$type <- "g"
svg$type[which(svg$veid %in% specialists)] = "s"

by_day <- ddply(svg, .(tdate,type), summarize, num_type = length(unique(veid)) )
by_day$tdate <- as.POSIXlt(by_day$tdate, format = "%d-%b-%y")
by_day$percent <- NA
by_day$percent[by_day$type=="s"] <- by_day$num_type[by_day$type=="s"]/length(specialists)
by_day$percent[by_day$type=="g"] <- by_day$num_type[by_day$type=="g"]/(length(unique(svg$veid))-length(specialists))


ggplot(by_day, aes(x = tdate, y = num_type, group = type,colour=type)) + geom_line() + theme_minimal()

ggplot(by_day, aes(x = tdate, y = percent, group = type,colour=type)) + geom_line() + theme_minimal()

