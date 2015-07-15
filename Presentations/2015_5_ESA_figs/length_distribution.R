rm(list=ls())
library(dplyr); library(scales)
ftl_len <- readRDS("/Users/efuller/1/CNH/processedData/catch/5_add_length/ftl_len.RDS")

met_len <- ftl_len %>%
  group_by(trip_id) %>%
  summarize(metier = unique(metier), cluster = unique(cluster), dahl_sector = unique(dahl_sector), len = unique(len), pcid = unique(pcid), year = unique(year), agid = unique(agid), lbs = sum(landed_wt))

pdf(file = "/Users/efuller/1/CNH/Presentations/2015_5_ESA_figs/length_distribution.pdf", width = 8, height = 6)
par(mai=c(.75,.75,0,0), cex.lab=1.1, cex.axis=.8)
with(subset(met_len, metier == "TWL_1"), hist(len, col = alpha("orange",.75),bor="white", xlim=c(0,130),bty = "n", xlab = "", main="", ylab="",breaks=30 ,freq =F,ylim=c(0,.10)) )
with(subset(met_len, metier == "TWL_3"), hist(len, col = alpha("dodgerblue",.5),add=T,bor="white",freq=F))
with(subset(met_len, metier == "POT_1"), hist(len, col = alpha("red",.5),add=T,breaks=50,bor="white",freq=F))
#with(subset(met_len, metier == "TLS_1"), hist(len, col = alpha("pink",.5),add=T, bor="white", freq=F))
with(subset(met_len, metier == "HKL_2"), hist(len, col = alpha("slategrey",.5),add=T, bor="white", freq=F))
#with(subset(met_len, metier == "TWS_1"), hist(len, col = alpha("magenta",.5),add=T, bor="white",freq=F))
#with(subset(met_len, metier == "HKL_1"), lines(density(len,na.rm=T), col = "slateblue", lwd = 3))
#with(subset(met_len, metier == "POT_4"), lines(density(len,na.rm=T), col = "slateblue", lwd = 3))
mtext("Density",side = 2, line = 2)
mtext("Vessel length (feet)",side = 1, line = 2)
legend("topright",fill = c(alpha("dodgerblue",.5),alpha("orange",.5),alpha("red",.5),alpha("slategrey",.5)), legend = c("whiting midwater", "DTS trawl","Dungeness crab pot","black rockfish hook/line"), bty = 'n', cex = 1,border = "white")

dev.off()