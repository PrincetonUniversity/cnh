# shrimp data 
library(maps); library(scales)

VMS_groundtruthed <- read.csv("/Users/efuller/1/CNH/Analysis/VMS/2014-08-19/groudtruthed_ref.csv", stringsAsFactors = F)

fishing <- subset(VMS_groundtruthed, fishing == 1)

plot(fishing$Longitude, fishing$Latitude, asp = 1, pch = 19, cex = .5, col = alpha("black", .15))
points(fishing$Longitude, fishing$Latitude, cex = .5, col = alpha("black",.15))
map('state', add = T, col = "grey", bor="white", fill=TRUE)

# can cluster by distance
lat_lons <- fishing[,c("Latitude","Longitude")]

cd <- hclust(dist(lat_lons))
plot(cd)

# can see the four big breaks
Y <- cutree(cd, h = 2)
head(Y)
length(Y)
lat_lons$Y <- Y
plot(lat_lons$Longitude, lat_lons$Latitude, col = alpha(lat_lons$Y,.15), asp = 1, pch = 19, cex = .5)
map('state', add = T, fill = TRUE, col = "grey", bor="white")

library(lubridate)



head(VMS_groundtruthed)
time_date <- with(VMS_groundtruthed, paste0(year,"-",month,"-",day," ",time))
time_date <- as.POSIXct(time_date, format = "%Y-%m-%d %H:%M", tz = "US/Pacific")
time_date <- round(time_date, "hours")

VMS_groundtruthed$time_date <- time_date

trip_times <- seq(from=min(time_date), to = max(time_date), by = "hours")


for(i in 29848:30000){
  
  if (i < 10) {name = paste('000',i,'plot.png',sep='')}
  if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
  if (i >= 100) {name = paste('0', i,'plot.png', sep='')}
  
  png(paste0("/Users/efuller/1/CNH/Analysis/VMS/2014-10-03/", name))
  
  time_sub <- subset(VMS_groundtruthed,time_date == trip_times[i])
  with(time_sub, plot(Longitude, Latitude, pch=as.numeric(as.factor(time_sub$Ship_Number)), 
                      xlim = range(VMS_groundtruthed$Longitude), 
                      ylim = range(VMS_groundtruthed$Latitude)),
       col = time_sub$fishing+1, asp = 1, cex = .5, main = trip_times[i])
  
  if(i > 3){
    time_minus1 <- subset(VMS_groundtruthed, time_date == trip_times[i-1])
    with(time_minus1, points(Longitude, Latitude, pch=as.numeric(as.factor(time_sub$Ship_Number)), 
                        xlim = range(VMS_groundtruthed$Longitude), 
                        ylim = range(VMS_groundtruthed$Latitude)),
         col = alpha(time_minus1$fishing+1, .5), cex = .25)
    time_minus2 <- subset(VMS_groundtruthed, time_date == trip_times[i-1])
    with(time_minus2, points(Longitude, Latitude, pch=as.numeric(as.factor(time_sub$Ship_Number)), 
                           xlim = range(VMS_groundtruthed$Longitude), 
                           ylim = range(VMS_groundtruthed$Latitude)),
         col = alpha(time_minus2$fishing+1, .25), cex =  .15)
  }
  map('state',add=T, col="grey",bor="white", fill=TRUE)
  cat(i/100, " ")
  
  dev.off()
}


