# Develop pipe-line to link metiers to VMS points. 

library(maps)
library(mapdata)
# load data ----
# load tickets. Need date and metier landed. 
tickets <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/fisheries_participation_profiles/tickets_plus.RDS")

landings <- unique(tickets[,c("trip_id","drvid","tdate","metier")])
# change tdate to date format

VMS <- readRDS("/Users/efuller/1/CNH/VMS_cleaning/src/VMS.RDS")

# checking things ----
ship_number <- unique(VMS[,c("ship.number","doc.num")])

# think it's doc number and drvid that are matching
length(which(ship_number$ship.number %in% landings$drvid))

length(which(ship_number$doc.num %in% landings$drvid))

# which are the ones not in?

missing<-ship_number[which(!(ship_number$doc.num %in% landings$drvid)),]

# where are they?
missing_path = subset(VMS, doc.num %in% missing$doc.num)
with(missing_path[1:500000,], plot(longitude, latitude, type="p",asp=1,cex=.25))
map('state',add=T)
# definitely some moveing around. hm, let those go
# (a lot of those vessels, like half, don't have doc numbers)
length(which(is.na(missing$doc.num)))

# subset to VMS for which I have data
rm(missing_path)

VMS_land <- subset(VMS, doc.num %in% unique(landings$drvid))
# keeps about 80% of datapoints
nrow(VMS_land)/nrow(VMS)
# clean up
rm(VMS, missing, ship_number,tickets) 

# settle on data for which I have both landings and VMS ----

# for each vessel in VMS dataset, take last landing, assign all points prior
# that metier. Then work back to the next data. Repeat until no more landings. 

# get all vessels
  vms_vessels <- unique(VMS_land$doc.num)
# get all landings
  vms_landings <- subset(landings, drvid %in% vms_vessels)
# clean up
  rm(landings)

# go through each vessel, save new vessl track with trip_id to a list
vessel_list <- vector("list", length = length(vms_vessels))

i = 315 # is an observed vessel, morning light

for(i in 1:length(vms_vessels)){
  
  # subset to just one track
    ves = subset(VMS_land, doc.num == vms_vessels[i])
  # order by date and time
    ves <- ves[order(ves$date.time),]
  
  # find landings
    ves_landings <- subset(vms_landings, drvid == vms_vessels[i])
  
  # need to adjust landings date to be in comparable format
    ves_landings$tdate <- as.Date(ves_landings$tdate, format = "%d-%b-%y",
                                tz = "Etc/GMT-8")
  
  # landing days - may have more than on trip per day so use trip_id
    landing_days <- unique(ves_landings[,c("tdate","trip_id","metier")])
  # order from last to first
    landing_days <- landing_days[order(landing_days$tdate, decreasing = T),]
  # find unique days to loop through
    days <- sort(unique(landing_days$tdate),decreasing = T)
    
  # set a column for landing ticket id(s)
  # need to if they land two different metiers in same day
    ves$trip_id1 <- NA
    ves$trip_id2 <- NA
  
  # go through landings one by one, find VMS points that associated
  for(j in 1:length(days)){
    
    # find landing date
    land_date <- days[j]
    possible_ids <- landing_days$trip_id[which(landing_days$tdate==days[j])]
    trip_id1 <- possible_ids[1]
    if(length(possible_ids)>1){
      trip_id2 <- possible_ids[2]
    } else {
      trip_id2 <- NA
    }
    
    # find last VMS point for this day
    if(as.Date(ves$date.time[1], tz = "Etc/GMT-8") > land_date){
      next
    }else{
      at_dock <- tail(which(as.Date(ves$date.time, tz = "Etc/GMT-8")<land_date),1)
      if(at_dock < ves$date.time[1])
        ves$trip_id1[1:at_dock] <- trip_id1
      ves$trip_id2[1:at_dock] <- trip_id2
    }
    
  }
  # merge to metier
  # get trip_id1 metiers
    ves_complete <- merge(ves, ves_landings[,c("trip_id","metier")], 
                          by.x = "trip_id1", by.y = "trip_id")
    ves_complete$metier1 <- ves_complete$metier
    ves_complete$metier <- NULL
    
  # get trip_id2 metiers
  if(!any(is.na(unique(ves_complete$trip_id2))) & length(unique(ves_complete$trip_id1==1))){
        ves_complete<- merge(ves_complete, ves_landings[,c("trip_id","metier")], 
                             by.x = "trip_id2", by.y = "trip_id")
        ves_complete$metier2 <- ves_complete$metier
        ves_complete$metier <- NULL 
      }else{
        ves_complete$metier2 <- NA
      } 
  
vessel_list[[i]] <- ves_complete
if(i %% 10 == 0){
  cat(i," finished\n")
}
}

vms_catch <- do.call(rbind, vessel_list[1:6])

# plot by fishery
paint = colorRampPalette(brewer.pal(8, "Accent"))(length(unique(vms_catch$metier1)))

# dover sole
with(subset(vms_catch, metier1=="TWL_1"), plot(Longitude,Latitude,asp=1, pch = 3, cex = .5, col = paint[5]))

# salmon
with(subset(vms_catch, metier1=="TLS_1"), plot(longitude,latitude,asp=1, pch = 3, cex = .5, col = paint[1]))

# tuna
with(subset(vms_catch, metier1=="TLS_2"), points(longitude,latitude,asp=1, pch = 3, cex = .5, col = paint[2]))

# sablefish pot
with(subset(vms_catch, metier1=="POT_4"), points(longitude,latitude,asp=1, pch = 3, cex = .5, col = paint[3]))

# sablefish longline
with(subset(vms_catch, metier1=="HKL_1"), points(longitude,latitude,asp=1, pch = 3, cex = .5, col = paint[4]))

# crab pot
with(subset(vms_catch, metier1=="POT_1"), points(longitude,latitude,asp=1, pch = 3, cex = .5, col = paint[5]))

map('state',add=T, col = "grey40",border="black",fill=TRUE)

# plot it all - colors aren't great. would be nice to color by similar species/types i.e. gear-type?
plot(vms_catch$longitude, vms_catch$latitude, asp = 1, col = paint[as.numeric(as.factor(vms_catch$metier1))], pch = 3, cex = .5)

map('state',add=T, col = "grey40",border="black",fill=TRUE)

legend("bottomleft", legend = tolower(unique(vms_catch$metier1)), col = paint, pch = 3, lwd=5,ncol= 2, box.col = "grey70", bg = "grey90", cex = .8)

# let's plot a few!


paint = brewer.pal(7,"Accent")
par(mai=rep(0,4))
par(bg="steelblue")
with(ves_complete, plot(Longitude, Latitude, asp = 1, pch = 3, cex = .5,col = paint[as.numeric(as.factor(metier1))]))
map('state',add=T, col = "grey40",bor="black",fill=T)
legend("bottomright", col=paint, legend=unique(ves_complete$metier1), pch = 3, lwd = 5, bg = "grey90",box.col="grey90")

# vesesl 1  is crazy nearshore and diverse. 
# Out of SB. mostly C. Halibut and cukes. 



