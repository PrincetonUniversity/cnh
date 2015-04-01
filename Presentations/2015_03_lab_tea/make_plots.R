# VMS trips 
vms <- readRDS("/Users/efuller/1/CNH/processedData/spatial/vms/intermediate/04_with_trip/VMS_catch.RDS")

# git rid of random outliers
vms <- subset(vms, longitude < -110 &  longitude > -130 & latitude >30 & latitude < 58 )

load(file = "/Users/efuller/1/CNH/processedData/spatial/2_coastline.Rdata")

# plot groundfish trawl ----
library(scales)

twl_1 <- subset(vms, metier1 == "TWL_1" & avg.speed > 0 & avg.speed < 7)
north_twl <- subset(twl_1, latitude > 40 & latitude < 50)
coordinates(north_twl) <- ~longitude + latitude

png("/Users/efuller/1/CNH/Presentations/2015_03_lab_tea/twl_1.png", res = 300, height = 8, width = 4, units = "in", bg = "transparent")
par(mai=rep(0,4))
plot(north_twl, cex = .1, col = alpha("indianred",.05), pch = 3)
plot(WC, add = T, col = "grey",bor=FALSE)
dev.off()

# plot tuna ----
tls_2 <- subset(vms, metier1 == "TLS_2" & avg.speed > 0 & avg.speed < 7)
north_tls <- subset(tls_2, latitude > 40 & latitude < 50)
coordinates(north_tls) <- ~longitude + latitude

png("/Users/efuller/1/CNH/Presentations/2015_03_lab_tea/tls_2.png", res = 300, height = 8, width = 6, units = "in", bg='transparent')
par(mai=rep(0,4))
plot(north_tls, cex = 0.1, col = alpha("dodgerblue",.1))
plot(WC, add = T, col = "grey", bor = FALSE)
dev.off()

# plot sablefish longline ----
hkl_1 <- subset(vms, metier1 == "HKL_1" & avg.speed > 0 & avg.speed < 7)
nrth_hkl_1 <- subset(hkl_1, latitude > 40 & latitude < 50)
coordinates(nrth_hkl_1) <- ~longitude + latitude

png("/Users/efuller/1/CNH/Presentations/2015_03_lab_tea/hkl_1.png", res = 300, height = 8, width = 6, units = "in",bg="transparent")
par(mai=rep(0,4))
plot(nrth_hkl_1, cex = .1, col = alpha('darkorange', .1))
plot(WC, add = T, col = "grey", bor = FALSE)
dev.off()

# plot time series of these fisheries -----
library(plyr)
tickets <- readRDS("/Users/efuller/1/CNH/processedData/catch/fisheries_participation_profiles/tickets_plus.RDS")
landings <- subset(tickets, metier %in% c("TWL_1","TLS_2","HKL_1"))

poundage <- ddply(landings, .(metier,tdate), summarize, lbs = sum(round_wt), dollars = sum(round_wt*ppp))
poundage$tdate <- as.Date(poundage$tdate, format = "%d-%b-%y")
poundage <- poundage[order(poundage$tdate),]


png("/Users/efuller/1/CNH/Presentations/2015_03_lab_tea/catch.png", res = 300, height = 8, width = 8, units = "in",bg = "transparent")
par(mfrow=c(3,1), mai = rep(0,4), cex = .8, oma = rep(0,4))
with(subset(poundage, metier == "TWL_1"), plot(tdate, lbs,type='h', axes = FALSE, col = "indianred"))
with(subset(poundage, metier == "TLS_2"), plot(tdate, lbs,type='h', axes = FALSE, col = "dodgerblue"))
with(subset(poundage, metier == "HKL_1"), plot(tdate, lbs,type='h', axes = FALSE, col = "darkorange"))
dev.off()
