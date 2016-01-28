# using gear as proxy for fishery

# plot shannon weiner diversity index for vessel by coefficient of variation
# subset to main fisheries (MDT, RLT, FTS, DST, FPT, GFT, TRL, CPT, LGL)

gear_trips <- subset(ftl, grid %in% c("MDT", "RLT","FTS","DST","FPT","GFT","TRL","CPT","LGL"))

gear_trips <- subset(gear_trips, veid != "0")
gear_trips <- subset(gear_trips, veid != "UNKNOWN")


gear_trips$revenue <- gear_trips$landed_wt * gear_trips$ppp
gear_trips <- melt(gear_trips, variables = c("spid","ftid","grid"), measure.vars = c("revenue"))

cast_trips <- dcast(gear_trips, veid ~ grid, fun.aggregate = length)
rownames(cast_trips) <- cast_trips$veid
cast_trips$veid <- NULL
shannon <- diversity(cast_trips)
div_df <- data.frame(veid = names(shannon), diversity = shannon, stringsAsFactors = F)
rownames(div_df) <- NULL

# coefficient of variation by vessel. 
co.var <- function(x) ( 100*sd(x)/mean(x) )

library(plyr)


profits <- ddply(gear_trips, .(veid, ftid), summarize, trip_profit = sum(value), .progress = "text")
ves_profits <- ddply(profits, .(veid), summarize, covar=co.var(trip_profit), total= sum(trip_profit))

div_profits <- merge(div_df, ves_profits, by = "veid")

library(scales)
plot(log(div_profits$diversity), log(div_profits$covar), pch = 19, cex = .5)
plot(div_profits$diversity, div_profits$covar, pch = 19, cex=.5, col=alpha("black",.25))

lm.1 <- lm(covar ~ diversity, subset(div_profits, diversity > 0))
abline(lm.1)
# lots have zero diversity
hist(subset(div_profits, diversity==0)$covar, breaks = 30)

# what about crab vessels, bimodal distribution?

sv <- read.csv("/Users/efuller/1/CNH/Data/Catch/sv_2009-2013_2014-03-21.csv",stringsAsFactors=F)
cg <- read.csv("/Users/efuller/1/CNH/Data/Catch/cg_2009-2013_2014-03-21.csv", stringsAsFactors = F)

crab_trips <- subset(ftl, pcid%in% c("AST", "NEW") & grid =="CPT",select=c("spid", "ftid","landed_wt", "grid", "veid", "tdate","ppp"))

sv_len <- subset(sv, SVID %in% unique(crab_trips$veid), select = c("SVID","LEN"))
sv_len <- sv_len[!duplicated(sv_len$SVID),]
crab_trips_len <- merge(crab_trips, sv_len, by.x = "veid", by.y = "SVID", all.x = T, all.y= FALSE)

cg_len <- subset(cg, VID %in% unique(crab_trips$veid), select = c("VID", "LEN"))
cg_len <- cg_len[!duplicated(cg_len$VID),]

crab_trips_all_len <- merge(crab_trips_len, cg_len, by.x = "veid", by.y = "VID", all.x=TRUE, all.y=FALSE)

hist(sv_len$LEN,breaks=50)
hist(cg_len$LEN,breaks=50,add=T)
plot(density(sv_len$LEN))

# put them in 5 bins: < 20, 20-40, 40-60, 60-80, 80+
crab_trips_len$bin <- ifelse(crab_trips_len$LEN < 20, 1, ifelse(crab_trips_len$LEN >= 20 & crab_trips_len$LEN < 40, 2, ifelse(crab_trips_len$LEN >= 40 & crab_trips_len$LEN < 60, 3, ifelse(crab_trips_len$LEN >=60 & crab_trips_len$LEN < 80, 4, 5))))

length_day <- ddply(crab_trips_len, .(tdate, bin), summarize,num_trips=length(unique(ftid)), .progress="text")

length_day$tdate <- as.POSIXlt(length_day$tdate, format = "%d-%b-%y")
length_day$bin <- factor(length_day$bin)
library(ggplot2)
ggplot(length_day, aes(x = tdate, y = num_trips, colour = bin)) + geom_line(size=2)  

# price over time

crab_price <- ddply(crab_trips_len, .(tdate), summarize, mean_price = mean(ppp, na.rm=T), sd_price = sd(ppp, na.rm =T))

crab_price$tdate <- as.POSIXlt(crab_price$tdate, format = "%d-%b-%y")
crab_price <- crab_price[order(crab_price$tdate),]
plot(crab_price$tdate, crab_price$mean_price,"l")
lines(crab_price$tdate, crab_price$mean_price+crab_price$sd_price, "l", col="grey")
lines(crab_price$tdate, crab_price$mean_price-crab_price$sd_price, "l", col="grey")

plot(crab_price$tdate, crab_price$mean_price+crab_price$sd_price, "h", col="grey")
lines(crab_price$tdate, crab_price$mean_price, "l")
lines(crab_price$tdate, crab_price$mean_price-crab_price$sd_price, "h", col="white")
