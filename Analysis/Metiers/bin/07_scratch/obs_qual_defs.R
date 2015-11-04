# looking at lengths of vessels, species composition, range compared to NWFSC observer sectors
library(dplyr)
# load data: tickets, length data, ports, species ids for management groups

# ports
ports <- read.csv("/Users/efuller/Desktop/CNH/processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE)
ports <- rename(ports, pcid = Pcid)

# length data
# federal registration
cg1 <- read.csv("/Users/efuller/Desktop/CNH/rawData/Catch/vessel_registration/CG_2009-2012_woc_141210_three.csv",
                stringsAsFactors = FALSE, skip = 2)
cg2 <- read.csv("/Users/efuller/Desktop/CNH/rawData/Catch/vessel_registration/CG_2013_woc_141210_three.csv",
                stringsAsFactors = FALSE, skip = 2)
cg <- rbind(cg1, cg2); rm(cg1, cg2) # lots of blank rows in there
cg <- cg[-which(cg$pubyr < 2009 | cg$pubyr > 2013),]
cg_vessels <- unique(cg[,c("hull_number","vessel_id","vessel_name","pubyr",
                           "length","horsepower","hp_main_astern","breadth",
                           "depth")])
# state registration
sv <- read.csv("/Users/efuller/Desktop/CNH/rawData/Catch/vessel_registration/SV_2009-2013_woc_141210_two.csv", 
               stringsAsFactors = FALSE, skip = 2)
colnames(sv)[1] <- "year"
sv$year <- as.integer(sv$year) # NAs are real, remove
sv <- sv[-which(is.na(sv$year)),]
sv_vessels <- unique(sv[,c("year","svid","plate","name","length",
                           "weight","horsepower","charterboat")])
# combining vessel length data
sv_cg <- merge(sv_vessels, cg_vessels, by.x = c("svid","year"), 
               by.y = c("vessel_id", "pubyr"), all.x = TRUE, all.y = TRUE)
sv_cg <- rename(sv_cg, drvid = svid)

length_ref <- sv_cg %>%
  group_by(drvid) %>%
  summarize(len = mean(length.x, length.y, na.rm = TRUE, trim = 0),
            hp = mean(horsepower.x, horsepower.y, hp_main_astern,trim = 0, na.rm = TRUE),
            weight = mean(weight, na.rm = T, trim = 0),
            breadth = mean(breadth, na.rm = T, trim = 0),
            depth = mean(depth, na.rm = T, trim = 0))

# tickets 
tickets <- readRDS("/Users/efuller/Desktop/CNH/processedData/catch/1_cleaningData/tickets.RDS")
tickets <- tickets %>%
  left_join(ports) %>%
  left_join(length_ref)

# look at limited entry groundfish length ----
twl_1 <- tickets %>%
  filter(metier.2010 == "TWL_1") %>%
  select(drvid, trip_id, pcid, metier.2010, len, lat, Name) %>%
  distinct()

null <- tickets %>%
  filter(metier.2010 != "TWL_1") %>%
  select(drvid, trip_id, pcid, metier.2010, len, lat, Name) %>%
  distinct()

plot(density(null$len, na.rm = T, adjust = 2),main="", xlab = "length of vessel (ft)",bty="n",lwd=2,ylim = c(0,.04))
lines(density(twl_1$len, na.rm = T, adjust = 2),col='red',lwd=2)
abline(v= 35, lty=2,col='grey20')
abline(v= 95, lty=2, col='grey20')
legend("topright", c("twl_1 vessels","rest of boats"), col = c('red','black'),lwd=2,bty='n')

# look at limited entry groundfish latitudinal distribution
plot(density(null$lat, na.rm = T, adjust = 1),main="", xlab = "latitude of landing port",bty="n",lwd=2)
lines(density(twl_1$lat, na.rm = T, adjust = 1),col='red',lwd=2)
abline(v= 35.37299, lty=2,col='grey20')
abline(v= 49, lty=2, col='grey20')
legend("topright", c("twl_1 vessels","rest of boats"), col = c('red','black'),lwd=2,bty='n')

length(which(twl_1$lat>35.37299 & twl_1$lat<49))/nrow(twl_1)

# look at limited entry species richness
twl_1sp <- tickets %>%
  filter(metier.2010 == "TWL_1") %>%
  group_by(trip_id) %>%
  summarize(n.sp = length(unique(spid))) %>%
  left_join(unique(tickets[,c("trip_id","drvid","pcid","metier.2010","len","lat","Name")]))

nullsp <- tickets %>%
  filter(metier.2010 != "TWL_1") %>%
  group_by(trip_id) %>%
  summarize(n.sp = length(unique(spid))) %>%
  left_join(unique(tickets[,c("trip_id","drvid","pcid","metier.2010","len","lat","Name")]))

plot(density(nullsp$n.sp, na.rm = T, adjust = 5),main="", xlab = "species richness of landing",bty="n",lwd=2)
lines(density(twl_1sp$n.sp, na.rm = T, adjust = 1),col='red',lwd=2)
abline(v= 15, lty=2,col='grey20')
abline(v= 20, lty=2, col='grey20')
legend("topright", c("twl_1 vessels","rest of boats"), col = c('red','black'),lwd=2,bty='n')

length(which(twl_1sp$n.sp>15 & twl_1sp$n.sp<20))/nrow(twl_1sp)

# look at pink shrimp
