# Develop pipe-line to link metiers to VMS points. 

# load tickets. Need date and metier landed. 
tickets <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/results/2015-01-09/code/3_exploreBuildwebs/tickets.RDS")

landings <- unique(tickets[,c("trip_id","drvid","tdate","metier")])
# change tdate to date format

load("/Users/efuller/1/CNH/Analysis/VMS/results/2014-10-29/3_VMSdf.Rdata")

ship_number <- unique(VMSdf[,c("Ship_Number","Doc_Number")])

# think it's doc number and drvid that are matching
length(which(ship_number$Ship_Number %in% landings$drvid))

length(which(ship_number$Doc_Number %in% landings$drvid))

# which are the ones not in?

missing<-ship_number[which(!(ship_number$Doc_Number %in% landings$drvid)),]

# where are they?
missing_path = subset(VMSdf, Ship_Number %in% missing$Ship_Number)
with(missing_path[1:500000,], plot(Longitude, Latitude, type="p",asp=1,cex=.25))
map('state',add=T)
# definitely some moveing around. hm, let those go

# subset to VMS for which I have data
rm(missing_path)

VMS_land <- subset(VMSdf, Doc_Number %in% unique(landings$drvid))
# keeps about 95% of datapoints
nrow(VMS_land)/nrow(VMSdf)
# clean up
rm(VMSdf, missing, ship_number,tickets) 

# for each vessel in VMS dataset, find all landings. Then for each date go backwards until on land. 

vms_vessels <- unique(VMS_land$Doc_Number)
vms_landings <- subset(landings, drvid %in% vms_vessels)

rm(landings)

for(i in 1:length(vms_vessels)){
  # subset to just one track
  ves = subset(VMS_land, Doc_Number == vms_vessels[i])
  with(ves, plot(Longitude, Latitude,asp=1,cex=.5,type='b'))
  map('state',add=T,col="darkseagreen4",fill=TRUE,bor="white")
  
  # find landings
  ves_landings <- subset(vms_landings, drvid == vms_vessels[i])
  
  # need to adjust landings date and VMS data/time to be in comparable format
  ves$Date_Time <- as.POSIXct(ves$Date_Time, format = "%Y-%m-%d %H:%M", tz = "Etc/GMT-8")
  ves_landings$tdate <- as.Date(ves_landings$tdate, format = "%d-%b-%y")
  
  # landing days - may have more than on trip per day
  days <- unique(ves_landings$tdate)
  
  # go through landings one by one, find VMS points that associated
  for(j in 1:nrow(days)){
    
    # find trips for that day
    lands <- subset(ves_landings, tdate == days[j])
        
    # find VMS data
    trips <- ves[which(as.Date(ves$Date_Time) %in% days[j]),]
    with(trips[1:k,] ,plot(Longitude, Latitude,asp=1,type="o",xlim =c(-119.9,-119.5)))
    map('state',add=T)
  }
}

# two problems: one, landings may both be for same trip. but for completeness, should list both trip-ids and metiers. may be the same. but also I think I removed any "onland" data. so i don't know when they get back to shore. maybe could go back to figure out lat/lon of ports and add that in as a column. and use that to match with pcid of port. need to figure out what radius that should be though (for when I start labeling with ports). so need to go back to VMS data to figure out how much I did with the masking of land. 

# while I'm thinking through this. Interested in knowing how many trips of each metier each vessel did

trips = table(vms_landings$drvid, vms_landings$metier)
names = c("dungenss\ncrab pot","long-line\nsablefish","salmon troll","DTS trawl","red urchin\ndiving","pink shrimp\ntrawl","black rockfish\nnearshore","albacore troll","rock crab\npot","sablefish\npot","ca. halibut\nmidwater trawl","ca spiny\nlobster pot","whiting trawl","hagfish pot","ridgeback\nprawn trawl","sea cucumber\ntrawl","ca halibut\nnet","white seabass\nnet",'spotted prawn\npot',"nearshore\nmult. rockfish")
pdf("/Users/efuller/1/CNH/Analysis/VMS/results/2015-01-30/VMS_fisheries.pdf")
barplot(trips[,names(sort(colSums(trips),decreasing=T))][,1:20],cex.names=.75,beside=F,las=2,names.arg=names,main="number of trips for vessels that have landings and VMS data")
dev.off()

# what proportion of all trips do we have?
vms_trips <- as.data.frame(colSums(trips))
colnames(vms_trips) <- "num_vms"
vms_trips$metier <- rownames(vms_trips)
rownames(vms_trips) <- NULL
all_trips <- table(landings$metier)

percent_trips <- data.frame(all_trips = all_trips)
colnames(percent_trips) <- c("metier", "num_all")

percent_trips <- merge(percent_trips, vms_trips, all.x=TRUE)
percent_trips$num_vms[which(is.na(percent_trips$num_vms))] = 0
percent_trips$percent_covered = percent_trips$num_vms/percent_trips$num_all

percent_trips <- percent_trips[order(percent_trips$percent_covered,decreasing=T),]

# major fisheries plus others i'm interested in. 
maj_fish = c("TWL_1","POT_1","MSC_1","TLS_1","TLS_2","POT_2","HKL_1","NET_1","HKL_2","HKL_3","POT_2","NET_2","TWS_1","POT_4","HKL_5","TWL_3","HKL_8","NET_5")

maj_trips <- subset(percent_trips, metier %in% maj_fish)
maj_trips$common.name = c("pink shrimp trawl","DTS trawl","whiting trawl","sablefish pot","sablefish longline","vermillion nearshore","dungeness crab pot","Albacore troll","black rockfish nearshore","salmon troll","lingcod nearshore","swordfish gillnet","ca spiny lobster","multisp rockfish nearshore","red urchin diving","squid purse seine","sardine seine")

write.csv(maj_trips,"/Users/efuller/1/CNH/Analysis/VMS/results/2015-01-30/vms_fisheries.csv",row.names=FALSE)
