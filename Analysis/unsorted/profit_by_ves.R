# look at profit variability by vessel. 

# load data
ftl <- read.csv("/Users/efuller/1/CNH/Data/Catch/FTL_2009-2013_w-o-c_samhouri.csv", stringsAsFactors=FALSE)

library(plyr)

pv <- ddply(ftl, .(veid, ftid), summarize, profit =  sum(landed_wt*ppp), gear = unique(grid)[1], .progress = "text")

pv_av <- ddply(pv, .(veid), summarize, av_profit = mean(profit), sd_profit = sd(profit), gear = unique(gear)[1], .progress = "text")

# remove unknown vessels, vessels that didn't more than 1 trip
pv_av <- subset(pv_av, veid != "UNKNOWN")
pv_av <- pv_av[-grep("[*]", pv_av$veid),]
pv_av <- pv_av[-1, ]
pv_av <- pv_av[-which(is.na(pv_av$sd_profit)),]

library(scales)
library(RColorBrewer)
paint <- colorRampPalette(brewer.pal(11, "Spectral"))(length(unique(pv_av$gear)))
pv_av$gear <- factor(pv_av$gear)
plot(pv_av$av_profit, pch=19, cex=.4, col=alpha(paint[pv_av$gear],.45), bty="n")
arrows(1:nrow(pv_av), pv_av$av_profit+pv_av$sd_profit, 1:nrow(pv_av), pv_av$av_profit-pv_av$sd_profit, length = 0, lwd = .3, col = paint[pv_av$gear])
legend("topright", legend=levels(pv_av$gear), col=paint, pch=19, ncol=8, bty = "n", cex=.7)

# trawls have highest average profit looks like. 

# would like to color by gear groups. so mobile trawls one set of colors (blues). fixed gear is another set, shrimp is another. etc. 

# also coefficient of variation
