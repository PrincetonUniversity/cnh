# load VMS
vms <- readRDS("/Users/efuller/1/CNH/processedData/spatial/vms/intermediate/04_with_trip/VMS_catch.RDS")

# load west coast
load("/Users/efuller/1/CNH/processedData/spatial/2_coastline.Rdata")

library(scales)
par(bg="transparent")
png("/Users/efuller/Desktop/tls_2.png", width = 12, height = 12,res=300, bg = "transparent", units = "in")
with(subset(vms, metier1 %in% "TLS_2" & longitude < -100 & longitude > -130 & latitude >31 & latitude < 49), plot(longitude, latitude, asp = 1, pch = 19, cex = .001, col= 'dodger blue',bty='n', type='p', axes = FALSE, xlab = "", ylab=""))
plot(WC, add = T, col = "grey80", border = "grey80")
dev.off()

png("/Users/efuller/Desktop/pot_1.png", width = 12, height = 12,res=300, bg = "transparent", units = "in")
with(subset(vms, metier1 %in% "POT_1" & longitude < -100 & longitude > -130 & latitude >31 & latitude < 49), plot(longitude, latitude, asp = 1, pch = 19, cex = .001, col= 'indian red',bty='n', type='p', axes = FALSE, xlab = "", ylab=""))
plot(WC, add = T, col = "grey80", border = "grey80")
dev.off()

png("/Users/efuller/Desktop/twl_1.png", width = 12, height = 12,res=300, bg = "transparent", units = "in")
with(subset(vms, metier1 %in% "TWL_1" & longitude < -100 & longitude > -130 & latitude >31 & latitude < 49), plot(longitude, latitude, asp = 1, pch = 19, cex = .001, col= 'orange',bty='n', type='p', axes = FALSE, xlab = "", ylab=""))
plot(WC, add = T, col = "grey80", border = "grey80")
dev.off()


png("/Users/efuller/Desktop/tws_1.png", width = 12, height = 12,res=300, bg = "transparent", units = "in")
with(subset(vms, metier1 %in% "TWS_1" & longitude < -100 & longitude > -130 & latitude >31 & latitude < 49), plot(longitude, latitude, asp = 1, pch = 19, cex = .001, col= 'magenta',bty='n', type='p', axes = FALSE, xlab = "", ylab=""))
plot(WC, add = T, col = "grey80", border = "grey80")
dev.off()

# all together
png("/Users/efuller/Desktop/together.png", width = 12, height = 12,res=300, bg = "transparent", units = "in")
with(subset(vms, longitude < -100 & longitude > -130 & latitude >31 & latitude < 49), plot(longitude, latitude, asp = 1, pch = 19, cex = .001, col= 'black',bty='n', type='p', axes = FALSE, xlab = "", ylab=""))
plot(WC, add = T, col = "grey80", border = "grey80")
dev.off()

# how many boats of each fishery
num_boats <- subset(vms, metier1 %in% c("TWS_1","TWL_1","TLS_2","POT_1")) %>%
                      group_by(metier1) %>%
                      summarize(n.boats = length(unique(doc.num)))
