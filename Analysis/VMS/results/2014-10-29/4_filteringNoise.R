# Load data
library(rgeos); library(geosphere); library(data.table)
setwd("/Users/efuller/1/CNH/Analysis/VMS/results/2014-10-29/")
load("3_VMSdf.Rdata")
load("2_coastline.Rdata")

# for now will do a single vessel -- shrimp vessel I know, Ms. Law
v1 <- subset(VMSdf, Vessel_Name=="Ms. Law")
rm(VMSdf)

# look at instantaneous records
# going to remove everything above Avg_Speed > 30
v1 <- subset(v1, Avg_Speed <=30)

#----
# now what about inferred speed
#----

# calculate delta_time between points
# recheck for any erroneous speeds. 

# to avoid getting NAs from DST, just use US Pacific Standard Time.
v1$Date_Time=as.POSIXct(v1$Date_Time, format="%Y-%m-%d %H:%M",tz="Etc/GMT+8")
v1 <- v1[order(v1$Date_Time),]


# calculate delta_distance between points
v1$dist <- c(NA, sapply(2:nrow(v1),function(i){
      distm(v1[,c("Longitude","Latitude"),with=FALSE][i-1,],
            v1[,c("Longitude","Latitude"),with=FALSE][i,])
    }))

# for multiple vessels #
      # # takes forever, need to find a better way to speed up. 
      # 
      # list_dist <-  lapply(split(VMSdf, VMSdf$Ship_Number), function(x) {
      #   x$dist_foo <- c(NA, sapply(2:nrow(x),function(i){
      #     distm(x[,c("Longitude","Latitude"),with=FALSE][i-1,],
      #           x[,c("Longitude","Latitude"),with=FALSE][i,])
      #   }))
      # })

      # VMSdf$dist <- unlist(list_dist) 

# make distances kilometers
v1$dist <- v1$dist/1000

# taking out outliers (probably moving from one port to another), nice bimodal distribution
with(subset(v1, dist <30 & dist !=0), hist(dist,breaks=100,col="grey",bor=F,freq=F))
with(subset(v1, dist <30 & dist !=0), lines(density(dist),lwd=3,col="steelblue"))

# look at that outlier -- not port.. hm extremely long time between trips. Should disappear in speed. But Also should slice trips that have an at-sea break in them. 
with(v1[20223:20226], plot(Longitude, Latitude,asp=1,type="b",ylim=c(40,45))); plot(WC,add=T,col="grey")

# find differences in time
v1$time <- c(NA,diff(v1$Date_Time))
# make time hours
v1$time <- v1$time/60
with(subset(v1, time < 1.75),hist(time,breaks=50,bor=F,col="grey")) # most at 60 minutes

# but a bunch sub 1, and think a lot of these are fliers. esp looking at their distances
with(subset(v1, time < 1),hist(time,breaks=50))

subset(v1, time < 1)

# but should come out with finding ridiculous new speeds

# now can find speed
v1$speed <- with(v1, dist/time) # will be in km per hour

with(subset(v1, speed>0),hist(speed))
max(subset(v1, speed>0)$speed) # maximum is 27 kmph, is fine

hist(v1$speed) # don't see any really high speeds

with(subset(v1, dist>0),hist(Avg_Speed,breaks=100)) # Avg_Speed looks pretty good. 

with(v1, plot(speed, Avg_Speed))
abline(0,1,col="red")
# definitely proportional, but lower. which is what nautical miles would be.
# convert speed to nautical miles by multipling by 0.539956803 (according to google)
with(v1, plot(speed*0.539956803, Avg_Speed))
abline(0,1,col="red") # nice definitely nautical miles. 
# also most points are lower in new speed than in old, which is what i'd predict if I remove fliers. 

# make speed nautical miles 
v1$speed <- v1$speed * 0.539956803

# would like to remove all points that boat is sitting in dock, so find segments where speed is non zero for more than 2/3/5 points? 

moving <- ifelse(v1$speed>0,1,0)
head(which(moving==1),100)

pdf(file="trip_1.pdf")
for(i in 44:80){
  with(v1[43:i],plot(Longitude,Latitude,asp=1,ylim=c(44.2,44.63),type="o",cex=.5,pch=19));plot(WC,add=T)}
dev.off()

trips <- rle(v1$speed>0 & v1$onland==FALSE) # find segments in which the speed is greater than zero and it's not onland. 

v1$run <- rep(trips$lengths, trips$lengths) # length of each segment 
v1$cond <- rep(trips$values, trips$lengths) # whether or not it matches the conditions: speed > 0 and not onland
total_ids <- length(trips$lengths) # total number of segments
v1$seg_id <- rep(1:total_ids,trips$lengths)  #give each segment own id

# now each point has how long the segment is, and whether or not it matches conditions, so now I can subset for segments that are both true and have segment length greater to or equal to 3. 
v1_trips <- subset(v1, run >= 3 & cond)
length(unique(v1_trips$seg_id)) # 220 trips over 5 years
nrow(v1_trips)/nrow(v1) # drops about 2/3 of the data. 

hist(table(v1_trips$seg_id),breaks=15) # can look at how long most trips are

# next step is to link up with observer data, when is known fishing (1), known not fishing (0) and unknown (NA)

obs <- fread("../../../../Data/Observers/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv")

obsv1 <- subset(obs, CG_NUM == unique(v1$Doc_Number),select=c("TRIPID","CATCH_ID","LOGBOOK_NUMBER","D_PORT","R_PORT","SET_MONTH","SET_DAY","SET_YEAR","SET_TIME","SET_LONG","SET_LAT","SET_DEPTH","UP_MONTH","UP_DAY","UP_YEAR","UP_TIME","UP_LAT","UP_LONG","HAUL_DURATION","DMONTH","DDAY","DYEAR","RMONTH","RDAY","RYEAR","sector"))

# have to add leading zeros to dates -- prob should make this a function

fixDate <- function(vec){
  vec <- ifelse(nchar(vec)==1, paste0(0,vec), vec)
}

obsv1$DMONTH <- fixDate(obsv1$DMONTH)
obsv1$DDAY <- fixDate(obsv1$DDAY)
obsv1$RMONTH <- fixDate(obsv1$RMONTH)
obsv1$RDAY <- fixDate(obsv1$RDAY)
obsv1$UP_MONTH <- fixDate(obsv1$UP_MONTH)
obsv1$UP_DAY <- fixDate(obsv1$UP_DAY)
obsv1$SET_MONTH <- fixDate(obsv1$SET_MONTH)
obsv1$SET_DAY <- fixDate(obsv1$SET_DAY)

# changing times to minutes, seconds
fixTime <- function(vec){
  hours <- floor(vec)
  minutes <- (vec - hours)*60
  minutes <- round(minutes, 2)
  minutes <- ifelse(nchar(minutes)==1,paste0(0,minutes),minutes)
  time <- paste(hours,minutes,sep=":")
}

obsv1$UP_TIME <- fixTime(obsv1$UP_TIME)
obsv1$SET_TIME <- fixTime(obsv1$SET_TIME)

# make new date_time
obsv1$D_DATE <- as.POSIXct(
  with(obsv1, paste(DMONTH, DDAY, DYEAR, sep = "-")), 
  format = "%m-%d-%Y", tz = "Etc/GMT+8")

obsv1$R_DATE <- as.POSIXct(
  with(obsv1, paste(RMONTH, RDAY, RYEAR, sep = "-")), 
  format = "%m-%d-%Y", tz = "Etc/GMT+8")

# make new up and set dates and times

obsv1$up_dateTime <- as.POSIXct(
  with(obsv1, paste0(UP_MONTH,"-",UP_DAY,"-",UP_YEAR," ",UP_TIME)), 
  format = "%m-%d-%Y %H:%M", tz = "Etc/GMT+8")

obsv1$set_dateTime <- as.POSIXct(
  with(obsv1, paste0(SET_MONTH,"-",SET_DAY,"-",SET_YEAR," ",SET_TIME)), 
  format = "%m-%d-%Y %H:%M", tz = "Etc/GMT+8")

# find VMS which was observed
# for each observed trip, mark in VMS data
library(lubridate)
Odepart <- unique(obsv1$D_DATE)
Oreturns <- unique(obsv1$R_DATE)
Oreturns <- paste(Oreturns,"23:59") # otherwise defauls to just when the day turns into a new one. 
Oreturns <- as.POSIXct(Oreturns, tz = "Etc/GMT+8")
Ointervals <- as.interval(Odepart, Oreturns)
Fstart <- unique(obsv1$set_dateTime)
Fend <- unique(obsv1$up_dateTime)
Fintervals <- as.interval(Fstart, Fend)

v1_trips$obs <- NA
v1_trips$fishing <- NA
for(i in 1:length(Ointervals)){
  # check for observations
  v1_trips$obs <- ifelse(v1_trips$Date_Time %within% Ointervals[i],
                         1,v1_trips$obs) 
  # if it's already been marked as observed, leave it or if it's in this new observed period, mark as 1
}

for(j in 1:length(Fintervals)){
  v1_trips$fishing <- ifelse(v1_trips$Date_Time %within% Fintervals[j],
                             1, v1_trips$fishing)
}

# for times when not fishing, but observed, put a zero
v1_trips$fishing <- ifelse(is.na(v1_trips$fishing) & v1_trips$obs==1, 0, v1_trips$fishing)

with(v1_trips[253:330],plot(Longitude,Latitude,asp=1, type="o",pch=19, cex=.5,col=fishing+1))
with(subset(v1_trips[253:330],fishing==1), points(Longitude, Latitude, col="red",cex=.5))
plot(WC,add=T,col="grey",bor=F)

# now have observed vessels, next is to find just the shrimp trips -- requires fish ticket data
# so for now i'll only look at observed trips. which I'm assuming are all shrimp trips but should check
unique(obsv1$sector) # we're good

# next step is building an ROC analysis to classify fishing points.
library(pROC)

with(subset(v1_trips,obs==1), roc(v1_trips$fishing, v1_trips$speed,plot=TRUE))

# choose some speed to filter on -- sensitivity = true positives, false positives = 1- specificity
# using specificity = .7, 70% 
abline(v=.7,col="indianred",lwd=2)

outcome <- roc(v1_trips$fishing, v1_trips$speed)

outcome <- data.frame(sens = outcome$sensitivities, specif = outcome$specificities, thresh = outcome$thresholds)
which(outcome$specif>.69 & outcome$specif<.71)

# by visual inspection it's when thresh = 1.385652, but that's the average. really we need a class that the speed falls within. 

plot(density(v1_trips$speed))
abline(v=1.385652) # very encouraging. but that means anything less than 3 nautical miles per hour is probably fishing..
abline(v=3,lty=3)

# will use that for the moment
v1_trips$guess_fishing <- NA
v1_trips$guess_fishing <- ifelse(v1_trips$speed < 3, 1, 0)

# for first trip, plot fishing points
all_trips <- unique(v1_trips$seg_id)

with(subset(v1_trips, seg_id==all_trips[1]), plot(Longitude, Latitude, asp=1, pch=19, cex=.5, type="o",col=ifelse(guess_fishing==1,"tomato","steelblue"))); plot(WC,add=T)

# or could look at all fishing points
library(scales)
with(v1_trips, plot(Longitude,Latitude, asp=1, pch=19, cex=.15, col=alpha("black",.01)))
with(subset(v1_trips, guess_fishing==1), points(Longitude,Latitude, asp=1, pch=19, cex=.15, col=alpha("tomato",.05)))
plot(WC,add=TRUE, col="grey90",bor=F)

# lets zoom
with(v1_trips, plot(Longitude,Latitude, asp=1, pch=19, cex=.5, col=alpha("black",.1),ylim=c(43.5,44),type="o"))
with(subset(v1_trips, guess_fishing==1), points(Longitude,Latitude, asp=1, pch=19, cex=.25, col=alpha("tomato",.5)))
plot(WC,add=TRUE, col="grey90",bor=F)
# does a pretty good job looking at this.

# next is to find the fishing grounds # using Branch et al. 2005 as a starting place
fishings <- subset(v1_trips, guess_fishing==1, select=c("Longitude","Latitude"))

distance <- dist(fishings,method="euclidean")
cluster.tree <-hclust(distance,method="average")

# to choose cutpoint, branch et al. recommend taking sqrt(2d) where d is the average length of a trawl. could get that from observer data..? can't think of a fast way to do that. for now will use .15

patches <- cutree(cluster.tree,h= .15)
v1_trips$ground <-NA
v1_trips$ground <- ifelse(v1_trips$guess_fishing==1, patches, NA)

library(RColorBrewer)
paint <- colorRampPalette(brewer.pal(8,"Dark2"))(length(unique(v1_trips$ground)))

with(v1_trips, plot(Longitude, Latitude, asp=1, col=paint[ground],cex=1,pch=19,ylim=c(45.5,46)));plot(WC,add=T,col="grey85",bor=F) # colors not great here

# try shapes
with(v1_trips, plot(Longitude, Latitude, asp=1, col=alpha("black", .5),cex=1,pch=ground,ylim=c(45.5,46)));plot(WC,add=T,col="grey85",bor=F)

# think this is too many patches. Try with higher value
plot(cluster.tree,labels=FALSE)
abline(h=.41,col="red")

new_grounds <- cutree(cluster.tree,h=.41)
v1_trips$new_ground <- ifelse(v1_trips$guess_fishing==1, new_grounds, NA)

paint <- brewer.pal(9,"Spectral")
with(subset(v1_trips,guess_fishing==1), plot(Longitude, Latitude, asp=1, col=paint[new_ground],cex=.5,pch=19));plot(WC,add=T,col="grey85",bor=F)

# maybe should just cluster latitude
fishings <- subset(v1_trips, guess_fishing==1, select=c("Latitude"))

distance <- dist(fishings,method="euclidean")
cluster.tree <-hclust(distance,method="average")
plot(cluster.tree, labels=FALSE)

# will try 3
abline(h=.3)
newest_ground <- cutree(cluster.tree, h=.3

v1_trips$newest_ground <- ifelse(v1_trips$guess_fishing==1, newest_ground, NA)

length(unique(newest_ground))
paint <- colorRampPalette(brewer.pal(8,"Spectral"))(11)

with(subset(v1_trips,guess_fishing==1), plot(Longitude, Latitude, asp=1, col=paint[newest_ground],cex=.5,pch=19));plot(WC,add=T,col="grey85",bor=F)

# still not good - just look at patches one by one
with(subset(v1_trips, newest_ground==4), plot(Longitude, Latitude, asp=1)
# yeah, all spread out down the coast.. why is that?

# all fishing is mostly 
plot(density(fishings$Latitude))

# could do kernal density to see if there looks like discrete patches
library(KernSmooth)
longlats <- subset(v1_trips, guess_fishing==1,select=c("Longitude","Latitude"))

est <- bkde2D(longlats, bandwidth=c(.03,.02))
par(bg="black",mai=rep(0,4))
with(longlats, plot(Longitude, Latitude,cex=.1,asp=1,col="goldenrod"))
contour(est$x1, est$x2, est$fhat,labels=NULL,add=T,col="white",lwd=3)
plot(WC,add=T,col="grey55",bor=F)
points(-124.045934,44.605168,pch=3, lwd=3, col="sienna1") # newport, according to wikipedia
arrows(x0 = -123.0,y0 = 44.605168, x1 = -123.9, y1=44.605168, col="white",length = .1,lwd=2 )
text(-122.5,44.605168,"Newport",col="white")
par(bg="white")
persp(est$fhat)
persp(est$fhat,theta=0,phi=60) # cool

# well fair enough, when you zoom out, hard to find clusters...
# so still need to work on this clustering. 

# just looking at it, see 7 really
with(subset(v1_trips, guess_fishing==1),plot(Longitude,Latitude, asp=1,cex=.5));plot(WC,add=T,col="grey90",bor=F)

# ah but did euclidean distance, so try distm
longlats <- subset(v1_trips, guess_fishing==1,select=c("Longitude","Latitude"))

distmat <- distm(longlats)
distances <- as.dist(distmat)

cluster.tree <- hclust(distances, method = "average")
plot(cluster.tree,labels=FALSE )
abline(h = 70000)
longlats <- subset(v1_trips, guess_fishing==1,select=c("Longitude","Latitude"))
again_grounds <- cutree(cluster.tree, h=70000)
v1_trips$yetagain <- ifelse(v1_trips$guess_fishing==1, again_grounds, NA) # this could be messing it up, actually..

with(subset(v1_trips, yetagain==4),plot(Longitude, Latitude, asp=1,cex=.5))

# try kmeans
k_patches <- kmeans(distances, 7)
v1_trips$k_patches <- ifelse(v1_trips$guess_fishing==1, k_patches$cluster, NA)
with(subset(v1_trips, guess_fishing==1),plot(Longitude, Latitude, asp=1,cex=.5,col=k_patches))

with(subset(v1_trips, k_patches==2),plot(Longitude, Latitude, asp=1,cex=.5,col=k_patches))
# meh, still not working. 

# could do any clustering though, since it's 2D

# then link with metier analysis, to find each trip end date. go back to last 0 movement time from that to call a trip? slice out in port (0 movement) data. save each trip seperately...?


# any common pattern to visitation? which ones are most visited, least? how long is spent on each one? does he go to more than one? look at each pattern of visiting order, connect to fish ticket and look at average/sd catch

# repeat for second vessel out of same port. then see if they visit the patches at the same time. round times by hour, and then search for times when that patch and same date/time exist for both vessels. See `Vessel_I_know.Rmd` for finding another shrimp vessel. 

# then need to think about what questions will answer with this, flesh out other things. maybe sunday, so can run through it with james on monday?


# goal for stockholm, demonstrate speed filter and fishing grounds analysis, maybe network of who fishes together? maybe a plot of average latitude of landings versus proportion of times fishing with another vessel on same grounds? or show vessels that spend time at the same grounds versus those that don't?
  