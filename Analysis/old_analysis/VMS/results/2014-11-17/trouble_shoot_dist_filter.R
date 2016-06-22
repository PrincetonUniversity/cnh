# need to reconsider vessels 10, 27, 31, 38, 42, 49, 61, 62, 66, 71, 72, 73 -- filtering is weird.   
# assumes most of 1_load_process_data.R is run, and that vms_list is generated
load("/Users/efuller/1/CNH/Analysis/VMS/results/2014-10-29/2_coastline.Rdata")

# try vessel 10
library(geosphere)
v10 <- subset(vms_obs, Doc_Number==ves_names[10])
v10$time <- NULL
with(v10, plot(Longitude, Latitude, asp=1,cex=.5))
with(subset(v10, Avg_Speed > 0), hist(Avg_Speed,breaks=50)) # looks decent

# calculate distances
longlat <- as.data.frame(v10[,c("Longitude","Latitude"),with=FALSE])
v10_dist <- cbind(longlat, rbind(tail(longlat,-1),c(NA,NA))) # tail drops the first element

distances <- apply(v10_dist, 1,function(i) distm(i[1:2], i[3:4]))/1000
time <- diff(v10$Date_Time)/60
speed <- head(distances,-1)/as.numeric(time) # drops the last element
range(speed) # maximum at 586.. wrong. where is it?

which.max(speed)
with(v10[5175:5180], plot(Longitude, Latitude, pch=19,type="o")); plot(WC,add=T)
with(v10[5177], points(Longitude, Latitude, pch=19, col="red")) # this is the problem
# also just 2 minutes after original.. 
# so remove

with(subset(v10[5175:5180], Avg_Speed!=0) , plot(Longitude, Latitude, pch=19,type="o")); plot(WC,add=T) # this looks better. let's remove. 

v10 <- cbind(v10, c(NA, distances=head(distances,-1)), time=c(NA, time), speed=c(NA, speed))
v10 <- subset(v10, speed < 500)
range(v10$speed) # still have a high value at 115. Where's that. 

which.max(v10$speed)

v10[5188:5199,] # this is probably what caused the constant refiltering. looking at this, 5189 looks like the flier, but the high speed is associated with point after. So that gets removed. Also there are a bunch more fliers after the one at 5189. And this is pretty far north, so this might also be why the points are problematic. Wonder if all problematic vessels are up in Washington.  

with(v10[5188:5190,], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)

# looking at an actual trip, does look there is a possibly legitimate point east along the coastline. It's possible that's a processor he's landing at, and then docking further west? 
with(v10[10185:10284,], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)

# but if drop this point, what does it look like. 
with(subset(v10[5188:5192,],speed<100), plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)

# so this is a mess, but ultimately doesn't affect any trips. what about other points
which(v10$speed>30)

with(v10[9440:9449], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)
# yep another messed up set in the same place. Again, luckily doesn't affect a trip

with(v10[5175:5178], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)
# this one does work properly. Should reduce some of it. But still bouncing between points along the coast. 

with(v10[4800:4805], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)
# another bouncing set. but again not affecting a trip

with(v10[4570:4575], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)
with(subset(v10[4570:4575],speed < 30), plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)
# this works, looks fine. 

with(v10[4265:4271], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)
with(subset(v10[4265:4271], speed<30), plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)
# this works. 

with(v10[4180:4188], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)
with(subset(v10[4180:4188],speed<30), plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T) # works ok, still one weird point

with(v10[4100:4113], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)
with(subset(v10[4100:4113], speed < 30), plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)
# this works too... i hope. it might just make the rest of the trip disappear.. let's check

try_remove <-subset(v10[4100:4113], speed < 30)
tryll <- as.data.frame(try_remove[,c("Longitude","Latitude"),with=FALSE])
try_dist <- cbind(tryll, rbind(tail(tryll,-1),c(NA,NA))) # tail drops the first element

try_distances <- apply(try_dist, 1,function(i) distm(i[1:2], i[3:4]))/1000
time <- diff(try_remove$Date_Time)/60
speed <- head(try_distances,-1)/as.numeric(time) # drops the last element
speed
# nope we're good

with(v10[4024:4030], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T) # doesn't look like a big deal

with(subset(v10[4024:4030],speed < 30), plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T) 
# might change the turning angle here.. because it pulled the wrong one. 

# which ones are right around 30?
which(v10$speed>25 & v10$speed < 30)

with(v10[2999:3004], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T) 
# above 20 bad too. 
with(subset(v10[2998:3004], speed < 20), plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T) 

with(v10[3550:3555], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T) 
# try subsetting
with(subset(v10[3549:3554],speed < 25), plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T) 
# still a flier with lower value (14). but this will get dropped by filtering for trips that are at least 3 points. should consider maybe setting speed threshold lower?

with(v10[4013:4019], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T) 
with(subset(v10[4013:4019],speed<25), plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T) 
# yep, no difference. but removes that flier. Again would affect turning angles

with(v10[4132:4138], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)
# another shoreline one, somewhat problematic because lots of moving but not a trip. but many of these are onland. so this one will be gone. 

with(v10[9642:9653], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o',ylim=c(46,46.5)));plot(WC,add=T)
# this has got to be him commuting back and forth. This is astoria, these are the piers. bet he is.. 

# let's try 20-25
which(v10$speed>20 & v10$speed < 25)

with(v10[4298:4305], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)
# another one, shouldn't be a problem.
with(v10[4795:4800], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)

with(v10[5020:5023], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)

with(subset(v10[5020:5023],speed < 20), plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)
# should be fine

with(v10[5029:5033], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)

# more weird fliers... but will drop with trip filter. 
with(v10[13622:13628], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)

with(subset(v10[13622:13628],speed<20), plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)
# looks fine. 

with(v10[30117:30119], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)
# hm, this looks possibly right

with(v10[31362:31364], plot(Longitude, Latitude, asp=1, cex=.5,pch=19, type='o'));plot(WC,add=T)
# again looks possibly right. so maybe sub 25 is fine. 

# feel pretty good about v10. look at 27

v27 <- subset(vms_obs, Doc_Number==ves_names[27])
v27$time <- NULL
with(v27, plot(Longitude, Latitude, asp=1,cex=.5, type="o")); plot(WC,add=T)
# definitely some fliers here. 

# calculate distances
longlat <- as.data.frame(v27[,c("Longitude","Latitude"),with=FALSE])
v27_dist <- cbind(longlat, rbind(tail(longlat,-1),c(NA,NA))) # tail drops the first element

distances <- apply(v27_dist, 1,function(i) distm(i[1:2], i[3:4]))/1000
time <- diff(v27$Date_Time)/60
speed <- head(distances,-1)/as.numeric(time) # drops the last element
range(speed) # maximum at 101.. wrong. where is it?

v27 <- cbind(v27, c(NA, distances=head(distances,-1)), time=c(NA, time), speed=c(NA, speed))

which.max(v27$speed)
with(v27[667:669], plot(Longitude, Latitude, asp=1,type='o')); plot(WC, add=T)
with(subset(v27[667:671],speed<27), plot(Longitude, Latitude, asp=1,type='o')); plot(WC, add=T)
# subsetting to 30 only gets half of the flier. but doesn't matter because on land. 

which(v27$speed>25)
with(v27[7395:7399], plot(Longitude, Latitude, asp=1,type='o')); plot(WC, add=T)
with(subset(v27[7350:7451],speed<25), plot(Longitude, Latitude, asp=1,type='o')); plot(WC, add=T)
# this is a problem, keeps subsetting the point at port away.. 