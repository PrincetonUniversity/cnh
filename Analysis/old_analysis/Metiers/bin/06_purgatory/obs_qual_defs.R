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

length(which(twl_1$len>=34 & twl_1$len<=96))/nrow(twl_1[-which(is.na(twl_1$len)),])


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

length(which(twl_1sp$n.sp>1))/nrow(twl_1sp)

# look at pink shrimp ----
tws_1 <- tickets %>%
  filter(metier.2010 == "TWS_1") %>%
  select(drvid, trip_id, pcid, metier.2010, len, lat, Name,tdate) %>%
  distinct()

null <- tickets %>%
  filter(metier.2010 != "TWS_1") %>%
  select(drvid, trip_id, pcid, metier.2010, len, lat, Name,tdate) %>%
  distinct()

plot(density(null$len, na.rm = T, adjust = 2),main="", xlab = "length of vessel (ft)",bty="n",lwd=2,ylim = c(0,.05))
lines(density(tws_1$len, na.rm = T, adjust = 2),col='red',lwd=2)
abline(v= 38, lty=2,col='grey20')
abline(v= 105, lty=2, col='grey20')
abline(v=65, lty=3, col='grey50',lwd=2)
legend("topright", c("tws_1 vessels","rest of boats"), col = c('red','black'),lwd=2,bty='n')

length(which(tws_1$len >=37 & tws_1$len <=106))/nrow(tws_1)

# look at limited entry groundfish latitudinal distribution
plot(density(null$lat, na.rm = T, adjust = 1),main="", xlab = "latitude of landing port",bty="n",lwd=2,ylim=c(0,.5))
lines(density(tws_1$lat, na.rm = T, adjust = 1),col='red',lwd=2)
abline(v= 35.37299, lty=2,col='grey20')
abline(v= 49, lty=2, col='grey20')
legend("topright", c("twl_1 vessels","rest of boats"), col = c('red','black'),lwd=2,bty='n')

length(which(twl_1$lat>35.8 & twl_1$lat<49))/nrow(twl_1)

# look at seasonality
# find day of year for head landing
tws_1$doy <- as.Date(format(as.Date(tws_1$tdate, format="%d-%b-%y"),"%m-%d"), format="%m-%d")

length(which(tws_1$doy >=as.Date("04-01-2015",format="%m-%d-%Y") & tws_1$doy <=as.Date("10-31-2015","%m-%d-%Y")))/nrow(tws_1) # problem because of leap years (i.e. 2012)

# look at california halibut trawl
twl_2 <- tickets %>%
  filter(metier.2010 == "TWL_2") %>%
  select(drvid, trip_id, pcid, metier.2010, len, lat, Name,tdate) %>%
  distinct()

length(which(twl_2$len>=28 & twl_2$len<=72))/nrow(twl_2[-which(is.na(twl_2$len))])
twl_2[which(twl_2$len<29 | twl_2$len>71),] # but it's because one boat is 28.4 feet

length(which(twl_2$lat <= 37.7833 & twl_2$lat >= 34.0500))/nrow(twl_2)

# find top 10 fisheries by revenue
rev <- tickets %>%
  group_by(metier.2010) %>%
  summarize(revenue = sum(adj_revenue, na.rm = T)) %>%
  arrange(-revenue) %>%
  mutate(per.rev = cumsum(revenue)/sum(revenue))

# summarize these distributions
pot_1 <- tickets %>%
  filter(metier.2010 == "POT_1") %>%
  select(drvid, trip_id, pcid, metier.2010, len, lat, Name,tdate) %>%
  distinct()

hist(pot_1$len)
range(pot_1$len,na.rm=T)
summary(pot_1$len)
?quantile
quantile(pot_1$len, probs=c(0.025,0.975),na.rm=T)
quantile(pot_1$lat, probs=c(0.025,0.975),na.rm=T)

pot_1sp <- tickets %>%
  filter(metier.2010=="POT_1") %>%
  group_by(trip_id) %>%
  summarize(nsp = length(unique(modified)))
quantile(pot_1sp$nsp, probs = c(0.025, 0.975))

pot_1$doy <- as.numeric(format(as.Date(pot_1$tdate, "%d-%b-%y"),"%j"))
pot_1$doy_adj <- pot_1$doy - 300
hist(abs(pot_1$doy_adj))
quantile(abs(pot_1$doy),probs = c(.025, .975))
length(which(pot_1$doy < 231 | pot_1$doy > 330))/nrow(pot_1) # roughly november 26 - august 19 (not leap years)

net_1 <- tickets %>%
  filter(metier.2010=="NET_1") %>%
  select(drvid, trip_id, pcid, metier.2010, len, lat, Name,tdate) %>%
  distinct()
  
quantile(net_1$len, probs = c(0.025, 0.975),na.rm=T)
quantile(net_1$lat, probs = c(0.025, 0.975),na.rm=T)

net_1sp <- tickets %>%
  filter(metier.2010=="NET_1") %>%
  group_by(trip_id) %>%
  summarize(nsp = length(unique(modified)))

head(net_1sp)
length(which(net_1sp$nsp>1))/nrow(net_1sp)

net_1$doy <- as.numeric(format(as.Date(net_1$tdate, "%d-%b-%y"),"%j"))
quantile(net_1$doy, probs=c(0.025, 0.975))
length(which(net_1$doy < 56 | net_1$doy > 144))/nrow(net_1) # roughly november 26 - august 19 (not leap years)

tls_1 <- tickets %>%
  filter(metier.2010=="TLS_1") %>%
  select(drvid, trip_id, pcid, metier.2010, len, lat, Name,tdate) %>%
  distinct() %>%
  mutate(doy = as.numeric(format(as.Date(tdate, "%d-%b-%y"), "%j")))

quantile(tls_1$len, na.rm = T, probs = c(.025, .975))
round(quantile(tls_1$lat, na.rm = T, probs = c(.025, .975)),1)

tls_1sp <- tickets %>%
  filter(metier.2010=="TLS_1") %>%
  group_by(trip_id) %>%
  summarize(nsp = length(unique(modified)))

length(which(tls_1sp$nsp>1))/nrow(tls_1sp)
hist(tls_1$doy)
quantile(tls_1$doy, probs = c(0.025, .975))

fish_stats <- function(met){
  tls_1 <- tickets %>%
    filter(metier.2010==met) %>%
    select(drvid, trip_id, pcid, metier.2010, len, lat, Name,tdate) %>%
    distinct() %>%
    mutate(doy = as.numeric(format(as.Date(tdate, "%d-%b-%y"), "%j")))
  
  tls_1sp <- tickets %>%
    filter(metier.2010==met) %>%
    group_by(trip_id) %>%
    summarize(nsp = length(unique(modified)))
  
  len_dist = quantile(tls_1$len, na.rm = T, probs = c(.025, .975))
  lat_dist = round(quantile(tls_1$lat, na.rm = T, probs = c(.025, .975)),1)
  per.multi = length(which(tls_1sp$nsp>1))/nrow(tls_1sp)
  hist(tls_1$doy)
  doy_dist = quantile(tls_1$doy, probs = c(0.025, .975))
  return(list(len_dist, lat_dist, per.multi, doy_dist))
}

tls_1 <- fish_stats("TLS_1")
hkl_1 <- fish_stats("HKL_1")
twl_4 <- fish_stats("TWL_4")
tls_2 <- fish_stats("TLS_2")
net_2 <- fish_stats("NET_2")
pot_2 <- fish_stats("POT_2")
