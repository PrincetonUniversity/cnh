# Look for duplicated points
rm(list=ls())
library(data.table)
# setwd to top folder
setwd("/Users/efuller/1/CNH/Analysis/VMS/results/2014-10-29/")

library(dplyr); library(scales); library(maps); library(sp)
if(!file.exists("VMSdf")){
  VMS <- readRDS("/Users/efuller/1/CNH/Analysis/VMS/writing/code/2_VMS_masked.RDS")
  VMSdf <- data.table(VMS@data)
  VMSdf <- VMSdf[,c("Longitude","Latitude") := list(coordinates(VMS)[,1], coordinates(VMS)[,2]), with=FALSE]
  write.table(VMSdf, "VMSdf.csv",sep=",", row.names=FALSE, quote=FALSE)
  rm(VMS)
}

#load("/Users/efuller/1/CNH/Analysis/VMS/writing/code/2_coastline.Rdata")
#----
# characterizing them
#----
# remove any absolute duplicates (all entries are the same)

# have to set all columns to be key
setkey(VMSdf,NULL)
# remove duplicates
VMSfiltered <- unique(VMSdf)

# drops by >13000 points
nrow(VMSfiltered)-nrow(VMSdf)
rm(VMSdf)

# set keys
setkey(VMSfiltered)
ves_times <- group_by(VMSfiltered, Ship_Number)
dupTimes <- summarise(ves_times, 
                      num_dups = length(which(duplicated(Date_Time))))
length(which(dupTimes$num_dups > 0))/nrow(dupTimes) # almost 60% of vessels have duplicated points

# median is 15
median(dupTimes$num_dups[which(dupTimes$num_dups>0)])

# max is > 3000
dupTimes[which.max(dupTimes$num_dups),]

# find max
dupTimes_filtered[which.max(dupTimes_filtered$num_dups),]
# max vessel only has 3027 duplicate points now.
#----
# look at max vessel
#----
maxdups <- subset(VMSfiltered, Ship_Number==dupTimes$Ship_Number[which.max(dupTimes$num_dups)])

# plot to look at dups
forward_dups <- duplicated(maxdups$Date_Time)
forward_paint <- ifelse(forward_dups==TRUE, "red","black")
backward_dups <- duplicated(maxdups$Date_Time, fromLast = TRUE)
backward_paint <- ifelse(forward_dups==TRUE, "blue","black")

with(maxdups, plot(x, y, col=forward_paint, cex=.25, asp=1))
# no obvious pattern. Points seem along trajectories. 

# look at a subset of trips. Find when the first duplicates are and look at a few of those trips
alldups <- duplicated(maxdups$Date_Time) | duplicated(maxdups$Date_Time, fromLast=TRUE)

head(which(alldups==TRUE),20)

# first one is at port. 
with(maxdups[6660:7000,], plot(x, y, col=forward_paint, cex=.25, asp=1))
plot(WC, add=T)

# second one is at port
with(maxdups[7050:7070,], plot(x, y, col=forward_paint, cex=.25, asp=1))
plot(WC, add=T)

# also onland
with(maxdups[12673:12676,], plot(x, y, col=forward_paint[12673:12676], cex=1, asp=1,pch=19,xlim=c(-124.3,-124)))
plot(WC, add=T)

# find duplicates that are not onland
maxdups$dup = alldups
head(with(maxdups, which(dup==TRUE & onland==FALSE)),20)

paint = ifelse(alldups==TRUE,"indianred","slategray")
ind = 12715:12718
with(maxdups[ind,], plot(x, y, col=paint[ind], cex=1, asp=1,pch=19))
plot(WC, add=T)
# these are essentially on top of each other. The only difference is that the average speed and average direction are 0 for the first tuplicate. 
maxdups[ind,]

# look at the next at sea one
ind = 12719:12723
with(maxdups[ind,], plot(x, y, col=paint[ind], cex=1, asp=1,pch=19))
plot(WC, add=T)
maxdups[ind,]
# same. 

# look at the next at sea one
ind = 12730:12733
with(maxdups[ind,], plot(x, y, col=paint[ind], cex=1, asp=1,pch=19))
plot(WC, add=T)
maxdups[ind,]

# and next one
plot_dups <- function () {
  ind = 12855:12859
  with(maxdups[ind,], plot(x, y, col=paint[ind], cex=1, asp=1,pch=19))
  plot(WC, add=T)
  maxdups[ind,]
}
#----
# let's try another vessel
#----
dupTimes <- dupTimes[order(dupTimes$num_dups, decreasing=T),]
head(dupTimes)

v2 <- subset(VMSfiltered, Ship_Number==dupTimes$Ship_Number[2])
plot(v2$x, v2$y,asp=1, cex=.5)
plot(WC,add=T)
v2$dup <- duplicated(v2$Date_Time) | duplicated(v2$Date_Time,fromLast=TRUE)
paint <- ifelse(v2$dup=="TRUE","indianred","slategray")

# find at sea points
head(with(v2, which(dup==TRUE & onland==FALSE)),20)

ind <- 6733:6739
  with(v2[ind,], plot(x, y, col=paint[ind], cex=1, asp=1,pch=19))
  plot(WC, add=T)
  v2[ind,]
  # erg. now the first one seems to be the weird one. previous ones it was the second. and in this case it seems like the zero is the weird one. but here it'd make more sense for 0 to be the true

ind <-6750:6753
with(v2[ind,], plot(x, y, col=paint[ind], cex=1, asp=1,pch=19))
plot(WC, add=T)
v2[ind,]
# also this one, 180 is the weird thing

# next point
ind <-6755:6760
with(v2[ind,], plot(x, y, col=paint[ind], cex=1, asp=1,pch=19))
plot(WC, add=T)
v2[ind,]
# yep, same pattern.

# next point
ind <-6892:6895
with(v2[ind,], plot(x, y, col=paint[ind], cex=1, asp=1,pch=19))
plot(WC, add=T)
v2[ind,]
# and now we're back to 0s being the weird ones. 

ind <-6908:6913
with(v2[ind,], plot(x, y, col=paint[ind], cex=1, asp=1,pch=19))
plot(WC, add=T)
v2[ind,]
# and now we're back to 0s being the weird ones. 
#----

# seems like one thing I could do is to search for duplicates in which one entry has non-zero entries for both average speed and direction and the other has zero. and remove the zero ones. 

# but i guess more broadly i'm interested in what the differences are between these duplicates. and so far all of these duplicates have the same lat/lon positions, but the instanteneous speed and direction are what's differing. 

# so want to see the differences between average speed, direction, lat/lon for all duplicated time points
#----
# so subset to duplicate instances
all_boats_dups <- duplicated(VMSfiltered[,c("Ship_Number","Date_Time")]) | duplicated(VMSfiltered[,c("Ship_Number","Date_Time")], fromLast=TRUE)

duplicates <- VMSfiltered[which(all_boats_dups==TRUE),]
dup_ves <- group_by(duplicates, Ship_Number, Date_Time)

diff_ves <- summarise(dup_ves, dlat = mean(diff(y)), dlon = mean(diff(x)), 
                      dspeed = mean(diff(Avg_Speed)), 
                      ddirection = mean(diff(Avg_Direction)))

diff_ves <- as.data.frame(diff_ves)
# should expect that there should be at least one non-zero value among all the calculated values. 
any(rowSums(diff_ves[,3:6])==0)
# but is true.. which?
diff_ves[which(rowSums(diff_ves[,3:6])==0),]
# those aren't really. just happen to totally cancel each other out. 
# which is weird, really. 

# look at frequency of occurance for each combination of non-zero entries
dif_df = data.frame(dlat = diff_ves$dlat!=0, dlon = diff_ves$dlon!=0, dspeed = diff_ves$dspeed!=0, ddirection = diff_ves$ddirection!=0)

barplot(table(rowSums(dif_df)))
# most have two that are different, then just have one. Much smaller number have 3 that are different, adn even fewer have all 4. 
#----
# of those that just have 1, which one is most common?
#----
just_one <- dif_df[which(rowSums(dif_df)==1),]

barplot(colSums(just_one))
# mostly direction

# just looking at those that have on difference, and it's direction, subset those
direction_diff <- subset(diff_ves, ddirection !=0 & dspeed==0 & dlon==0 & dlat==0)
plot(density(direction_diff$ddirection),lwd=2)
abline(v=-180)
abline(v=-270)
abline(v=270)
abline(v=90)
abline(v=180)
abline(v=-90)
hist(direction_diff$ddirection,breaks=500, freq=F, col="slategray",bor=F)
lines(density(direction_diff$ddirection),lwd=3, col='indianred')
# some definite regularities going on here. 
abline(v=-180)
abline(v=-270)
abline(v=-90)
abline(v=90)
abline(v=180)
abline(v=270)
#----
# one idea is to do a second difference for duplicated points. Is it way more different to a point two steps away? If it is, that suggests a bad duplicate. 

# taking v2 for example
#----
ind = 7995:8010
v2[ind,]
with(v2[ind,], diff(Avg_Direction))
with(v2[ind,], diff(Avg_Direction,2))

ind = 8175:8190
v2[ind,]
with(v2[ind,], diff(Avg_Direction))
with(v2[ind,], diff(Avg_Direction,2))
# average difference of 0 indicates the correct one. so it's nonzero to start, then goes to zero...?

# try v1
ind = 12710:12724
maxdups[ind,]
with(maxdups[ind,], diff(Avg_Direction))
with(maxdups[ind,], diff(Avg_Direction,2))

# shoot not obvious yet that there's a clear signal.. I think that it has to be a duplicate and which has the max difference from surrounding points?, or highest mean difference?

praxis =maxdups[ind,c("dup", "Avg_Direction")]
praxis$d1 = c(NA,diff(praxis$Avg_Direction))
praxis$d2 = c(NA,NA, diff(praxis$Avg_Direction,2))
praxis$d3 = c(NA,NA,NA, diff(praxis$Avg_Direction,3))
praxis$sum = NA
praxis$sum[which(praxis$dup==TRUE)] <- apply(abs(praxis[which(praxis$dup==TRUE),c("d1","d2","d3")]), 1, sum)
# here the maxes will pull the correct ones. let's try v2 again

ind = 8175:8190
v2[ind,]

p2 <- v2[ind, c("dup","Avg_Direction")]
p2$d1 <- c(NA, diff(p2$Avg_Direction))
p2$d2 <- c(NA,NA, diff(p2$Avg_Direction,2))
p2$d3 <- c(NA,NA, NA,diff(p2$Avg_Direction,3))
p2$sum <- NA
p2$sum[which(p2$dup==TRUE)] <- apply(abs(p2[which(p2$dup==TRUE),c("d1","d2","d3")]), 1, sum)
# this also pulls it out. 
#----
# I think the "right" way to do this is to make a correct dataset and try this method on it. I'm not totally sure how to make a "correct" dataset... It seems time consuming.

#----
# of those that have two different, which are those? 3?
#----
just_two <- dif_df[which(rowSums(dif_df)==2),]

barplot(colSums(just_two)) # direction and speed, what I guessed. 

just_three <- dif_df[which(rowSums(dif_df)==3),]

barplot(colSums(just_three)) # some combinations of all three

just_four <- dif_df[which(rowSums(dif_df)==4),]

barplot(colSums(just_four)) # some combinations of all three
# ah, right. all would have to be different. 
#----
# one shortcut would be to take out any duplicate that has 0 for both avg_speed and avg_ direction. When on the water, I but I'd be correct. And if it's the wrong one at port, it doesn't matter because I'm not going to be using the at_port vessels anyway.. 

# and I wonder how many of them it would remove. 
#----
VMSfiltered$dups <- all_boats_dups
length(which(VMSfiltered$dups==TRUE & VMSfiltered$Avg_Speed==0 & VMSfiltered$Avg_Direction==0))*2/length(which(all_boats_dups==TRUE))
# that knocks out about 80% of them. a lot.  should know what the other ones are though.

#----
# looking at the speed ones
#----
with(dif_df, which(dspeed==TRUE & ddirection==FALSE & dlon==FALSE & dlat==FALSE))
# only 421 of them

# look at first one
ruby <- VMSfiltered[which(VMSfiltered$Ship_Number=="000074"),]
which(ruby$Date_Time=="2009-08-17 10:01")
with(ruby[4690:4705,],plot(x,y,asp=1,xlim=c(-124.5,-124),type='o'))
plot(WC,add=T)
# not totally clear, but also probably doesn't matter that much.. 

# look at next one
diff_ves[254,]
dawn <- VMSfiltered[which(VMSfiltered$Ship_Number=="X00222"),]
which(dawn$Date_Time=="2011-06-04 20:08")
with(dawn[2225:2239,], plot(x,y,asp=1,type='o',pch=19, col=alpha("black",.25)))
plot(WC,add=T)
# three different ones. all have a double zero partner

# look at the next one
diff_ves[716,]
nicole <- VMSfiltered[which(VMSfiltered$Ship_Number=="X00378"),]
which(nicole$Date_Time=="2009-09-21 21:29")
with(nicole[6248:6260,],plot(x,y,asp=1,xlim=c(-125,-124)))
plot(WC,add=T)
#again with the double zeros

# look at the next one -- still the nicole
diff_ves[722,]
which(nicole$Date_Time=="2009-10-04 19:07")
with(nicole[6665:6678,],plot(x,y,asp=1,xlim=c(-125,-124)))
plot(WC,add=T)
# this one has a ton. Like every other. And a bunch of double zeros. 
# pretty convinced that I should take out the double zeros and try again. 

#----
# taking out double zeros
#----
duples <- VMSfiltered[-which(VMSfiltered$dups==TRUE & VMSfiltered$Avg_Speed==0 & VMSfiltered$Avg_Direction==0),]
# update the dups to be the new ones
duples$dups <- duplicated(duples[,c("Ship_Number","Date_Time")]) | duplicated(duples[,c("Ship_Number","Date_Time")],fromLast = TRUE)

duplicates <- duples[which(duples$dups==TRUE),]
dup_ves <- group_by(duplicates, Ship_Number, Date_Time)

diff_ves <- summarise(dup_ves, dlat = mean(diff(y)), dlon = mean(diff(x)), 
                      dspeed = mean(diff(Avg_Speed)), 
                      ddirection = mean(diff(Avg_Direction)))

diff_ves <- as.data.frame(diff_ves)

dif_df = data.frame(dlat = diff_ves$dlat!=0, dlon = diff_ves$dlon!=0, dspeed = diff_ves$dspeed!=0, ddirection = diff_ves$ddirection!=0)

just_one <- dif_df[which(rowSums(dif_df)==1),]
barplot(colSums(just_one))
# still direction by a long shot

just_two <- dif_df[which(rowSums(dif_df)==2),]
barplot(colSums(just_two))
# still speed and direction

just_three <- dif_df[which(rowSums(dif_df)==3),]
barplot(colSums(just_three))

# let's look at vessels with direction differences
head(with(diff_ves, which(ddirection!=0 & dlat==0 & dlon==0 & dspeed==0)))
diff_ves[5,]
sunset <- duples[which(duples$Ship_Number=="55"),]
which(sunset$Date_Time=="2009-09-05 02:38")
sunset[4875:4884,]
# very small difference

# another one
diff_ves[9,]
hunter <- duples[which(duples$Ship_Number=="6F0136"),]
which(hunter$Date_Time=="2010-09-05 11:24")
hunter[2700:2710,]
# onland, and not obvious which is correct. 
diff_ves[12,]
which(hunter$Date_Time=="2010-10-04 17:28")
hunter[2940:2948,]
with(hunter[2940:2948,], plot(x,y,asp=1,xlim=c(-124,-123)))
plot(WC,add=T)
# also these past three are duplicated at sub one hour intervals. If i dropped the duplicates altogether, it would still be an hour between them. 

# also not clear which one is correct

# let's try another
diff_ves[26,]
spray <- duples[which(duples$Ship_Number=="X00025"),]
which(spray$Date_Time=="2009-09-01 05:52")
spray[325:335,]
# hard to know which one is right. but again at port. 

diff_ves[35,]
west <- duples[which(duples$Ship_Number=="X00222"),]
which(west$Date_Time=="2012-10-22 17:24")
west[2763:2768,]
# onland once again, and at sub 1 hour intervals. 
#----
# looking at the lat ones
#----
head(with(diff_ves, which(ddirection==0 & dlat!=0 & dlon==0 & dspeed==0)))

diff_ves[1,]
lawrence <- duples[which(duples$Ship_Number=="50"),]
which(lawrence$Date_Time=="2009-02-25 19:03")
lawrence[870:875,]
with(lawrence[870:875,], plot(x,y,asp=1,type='o', col = dups+1))
plot(WC,add=T)
# this makes it seem like both are wrong. try another

diff_ves[2,]
which(lawrence$Date_Time=="2009-04-21 03:11")
lawrence[2580:2590,]
with(lawrence[2580:2590,],plot(x,y,asp=1,type='o', col = dups+1) )
plot(WC,add=T)
# yep all look wrong

diff_ves[59,]
cecelia <- duples[which(duples$Ship_Number=="X00352"),]
which(cecelia$Date_Time=="2009-02-19 15:10")
with(cecelia[855:870,],plot(x,y,asp=1,type='o', col = dups+1,xlim=c(-125,-124)))
plot(WC,add=T)
#----
# looking at the lon ones
#----
head(with(diff_ves, which(ddirection==0 & dlat==0 & dlon!=0 & dspeed==0)))

diff_ves[55,]
which(cecelia$Date_Time=="2009-01-14 14:54")
cecelia[280:290,]
# this looks like a run of them. and not all of them are duplicates
par(mfrow=c(1,2))
with(cecelia[280:295,], plot(x,y,asp=1,type='o', col=dups+1))
with(cecelia[c(280:282,289:295),],plot(x,y,asp=1,type='o', col=dups+1))
plot(WC,add=T)

# another one
diff_ves[130,]
nicole <- duples[which(duples$Ship_Number=="X00378"),]
which(nicole$Date_Time=="2009-06-23 15:37")
nicole[4080:4095,]
# also sub one hour, and weird after it to with instantaneous speed of 32.7
with(nicole[4080:4095,], plot(x,y,asp=1, col=dups+1,type='o'))
plot(WC,add=T)

#----
# feeling pretty good about this. Will delete all other dups
#----

VMS_nd <- duples[-which(duples$dups==TRUE),]
any(duplicated(VMS_nd[,c("Ship_Number","Date_Time")]))
#----
saveRDS(VMS_nd,file="/Users/efuller/1/CNH/Analysis/VMS/results/2014-10-29/3_nodupsVMS.RDS")
