# date: 2014-04-09
# trying a dynamic brownian bridge on sub_VMS (one vessel with known fishing locations)
# most of these objects come from 01_Obs-VMS.R
# this is a draft, has too much flipping from one POSIX object to another. See dyBBvar_2ndvessel.R for more updated versions

load("data/VMS_move.Rdata")
VMS1 <- VMS_ordered[which(VMS_ordered$Doc_Number==511697),]

VMS1 <- subset(VMS1,Avg_Speed < 30)

VMS1_move <- move(
  x = VMS1$Longitude, 
  y = VMS1$Latitude, 
  time=as.POSIXct(VMS1$Date_Time, "%Y-%m-%d %H:%M",tz="US/Pacific"), 
  data=VMS1, 
  proj=CRS("+proj=longlat"),
  sensor="GPS",
  animal=VMS1$Doc_Number)

# finding a substantial amount of inferred speeds which mean that vessels are traveling faster than 44mph. Also the distances are often an order of magnitude greater around these points. Should remove them. 

# find ridiculously high speeds: faster than 33 mph
which(speed(VMS1_move)>13)

# some examples from these values
  distance(VMS1_move[6290:6300,])/1000
  distance(VMS1_move[4950:4962,])/1000

# remove points where speed > 20 meters/sec, or 29 mph

VMS_filter <- VMS1_move[-which(speed(VMS1_move)>13),]
nrow(VMS1_move)-nrow(VMS_filter) # lost 10 points

data <- spTransform(VMS_filter,center=T)

dBBMM<- brownian.motion.variance.dyn(data, location.error=rep(100,n.locs(data)),margin=11,window.size=31)
# window.size, margin in units of steps/points. the smaller you are the more likely you are to pick up faint (but possibly spurious) changes
  
  plot(dBBMM@means,type='l')

  abline(v=head(rev(order(speed(VMS_filter))),2),col=alpha("red",0.25),lwd=2)
  plot(data$)

#


outliers <- sort(head(order(-dBBMM@means),5))
plot(VMS1_move[outliers],pch=19,xlim=range(VMS1_move$Longitude[outliers]))
distance(VMS1_move[outliers])


  plot(dBBMM@means,type='l',xlim=c(2200,2500),ylim=c(0,500000))
  plot(dBBMM@means,type="l")
  lines(sub_move$fishing*max(dBBMM@means,na.rm=T),col=alpha("red",0.5))
  
  
require(classInt)
my.class <- classIntervals(dBBMM@means, n=5, style="quantile")
my.class <- classIntervals(dBBMM@means, n=5, style="kmeans")
my.class <- classIntervals((dBBMM@means)^(1/2), n=5, style="pretty")

my.pal <- findColours(my.class, c("red", "blue"))

par(bg="black")
lines(VMS1_move, type="l",col=alpha("white",0.25),cex=.25)
lines(sub_move,type="l",col=alpha("yellow",0.25),cex=0.25)
points(sub_move,col=alpha(my.pal,0.25),pch=19,cex=.25)
points(VMS1_move, col=alpha(my.pal,0.25),pch=19,cex=0.25)
plot(WC,add=T,,col=alpha("darkgrey",0.25),bor=FALSE)

# find those high peaks
points(sub_move[head(order(-dBBMM@means),5),],pch=4,cex=1.25,col="red",lwd=3)

# find in time series of sigma
plot(log(dBBMM@means[19950:20150]),type="l")

plot(sub_move[19950:20025,],type="l")
points(sub_move[head(order(-dBBMM@means),5),],pch=4,cex=1.25,col="red",lwd=3)


pdf("outliers.pdf")
for(i in 1:nrow(sub_move[19950:20025,])){
  if(row.names(sub_move[19950:20025,][i]) %in% sort(outliers)){
    plot(sub_move[19950:20025]$Longitude[i],sub_move[19950:20025]$Latitude[i],type="p",pch=19,xlim=range(sub_move[19950:20025]$Longitude),ylim=range(sub_move[19950:20025]$Latitude),col="red")
  }else{
  plot(sub_move[19950:20025]$Longitude[i],sub_move[19950:20025]$Latitude[i],type="p",pch=19,xlim=range(sub_move[19950:20025]$Longitude),ylim=range(sub_move[19950:20025]$Latitude))
}
lines(sub_move[19950:20025]$Longitude[1:i],sub_move[19950:20025]$Latitude[1:i],cex=0.15,col=rgb(0.15,0.15,0.15,0.15))
plot(WC,col=rgb(0.15,0.15,0.15,0.15),bor=FALSE,add=T)
print(i)
}
notify("finished!")

plot(log(dBBMM@means),type="o",pch=19,cex=0.5)


behavior <- kmeans(dBBMM@means[!is.na(dBBMM@means)], 3)

nclusters <- 10
wsum <- vector()
clusts <- list()
for(i in 1:nclusters){
  clusts[[i]] <- kmeans(dBBMM@means[!is.na(dBBMM@means)],i)
  wsum[i] <- clusts[[i]]$tot.withinss
}

plot(wsum,type="o")
plot(diff(wsum),type="o")
# looks like 3 is probably a good choice

behavior_NA <- c(rep("NA",10),behavior$cluster, rep("NA",11))
plot(sub_move,col=behavior_NA,type="o",pch=19,cex=0.75)
plot(WC,add=T)

# plotting to see
pdf("dBBMM_mov.pdf")
for(i in 1:500){
  plot(sub_move[i],col=behavior_NA[i],pch=19,ylim=range(sub_move$Latitude),xlim=range(sub_move$Longitude))
  plot(WC,add=T,col=rgb(0.15,0.15,0.15,0.15))
  print(i)
}
dev.off()

# looks not obvious, what about bursting because i'll loose the movement into and out of port, but that's not the behavior I'm interested in looking for orginally. So will filter out port data, then cluster on sigmas

sub_move$dBvar <- dBBMM@means
sub_move$dBvar_noport <- vector("numeric",length=nrow(sub_move))
sub_move$dBvar_noport[sub_move$behav==FALSE] <- sub_move$dBvar[sub_move$behav==FALSE]

sub_move$dBvar_noport[which(sub_move$dBvar_noport==0)]<-NA

# cluster only dBvar when behav==FALSE, i.e. dB


nclusters <- 10
wsum <- vector()
clusts <- list()
for(i in 1:nclusters){
  clusts[[i]] <- kmeans(sub_move$dBvar_noport[!is.na(sub_move$dBvar_noport)],i)
  wsum[i] <- clusts[[i]]$tot.withinss
  print(i)
}

plot(wsum,type="o")

plot(sub_move[!is.na(sub_move$dBvar_noport),],col=clusts[[3]]$cluster,type="o",pch=19,cex=0.75)
plot(WC,add=T)

plot(sub_move$dBvar_noport[!is.na(sub_move$dBvar_noport)],col=clusts[[3]]$cluster,type="o",pch=19,cex=0.75,xlim=c(0,300),ylim=c(0,max(sub_move$dBvar_noport,na.rm=T)))

lines(sub_move$fishing[!is.na(sub_move$dBvar_noport)]*max(sub_move$dBvar_noport,na.rm=T),col="grey",lwd=2)

pdf("dBBMM_mov.pdf")
for(i in 1:500){
  plot(sub_move[!is.na(sub_move$dBvar_noport),][i],col=clusts[[3]]$cluster[i],pch=19,ylim=range(sub_move$Latitude),xlim=range(sub_move$Longitude))
  plot(WC,add=T,col=rgb(0.15,0.15,0.15,0.15))
  print(i)
}
dev.off()

# plot distribution of sigmas associated with known fishing. But need to make sure that absence of fishing is evidence of absence (some of these trips could just not be observed). 
# but to figure this out, need to assume that the "wrong" ids would be things identified by fishing that were not also marked as fishing by an observer. And then could project into 

plot(density(sub_move$dBvar_noport[which(sub_move$fishing==1)],na.rm=T),ylim=c(0,.000025),col="dodgerblue",lwd=2)
lines(density(sub_move$dBvar_noport[which(sub_move$fishing==0)],na.rm=T),lwd=2,col="darkgrey")
legend("topright",legend=c(paste(expression("sigma")," for known\nfishing\n", sep=" "), paste(expression("sigma"), " for not\n known fishing", sep = " ")), lwd=2, col=c("dodgerblue","darkgrey"),bty="n")

# look at sigmas associated with each cluster (3)
sigma_cluster <- data.frame(sigmas = sub_move$dBvar_noport[!is.na(sub_move$dBvar_noport)],cluster=clusts[[3]]$cluster)

plot(density(sigma_cluster$sigmas[which(sigma_cluster$cluster==1)]))
plot(density(sigma_cluster$sigmas[which(sigma_cluster$cluster==2)]),col="dodgerblue")
plot(density(sigma_cluster$sigmas[which(sigma_cluster$cluster==3)]),col="red")

hist(sigma_cluster$sigmas[which(sigma_cluster$cluster==1)],xlim=c(9000,360000),col="dodgerblue",bor="blue",freq=F, main="distribution of sigmas",xlab=expression(sigma))
hist(sigma_cluster$sigmas[which(sigma_cluster$cluster==2)],add=T,col="grey",bor="darkgrey",freq=F)
hist(sigma_cluster$sigmas[which(sigma_cluster$cluster==3)],add=T,col="indianred",bor="darkred",freq=F)
legend("topright",legend=c("1","2","3"), title="cluster:",col=c("dodgerblue","grey","indianred"),lwd=5,bty="n")

# number of points classified as fishing that were known to be fishing


# hard to find evidence of absence: did an observer just miss a haul, or actually there was no haul then? one thing to do is to look at daily pattern of supposed fishing activities, are they always at night? until then, just look at the proportion of known fishing that falls in each cluster

# plot number of pings for each cluster that are also labelled as fishing
# would be the number of fishing points that are also dBvar_noport is not NA
one_fish <- length(which(clusts[[3]]$cluster==1 & sub_move$fishing[which(!is.na(sub_move$dBvar_noport)==1)])==TRUE)
two_fish <- length(which(clusts[[3]]$cluster==2 & sub_move$fishing[which(!is.na(sub_move$dBvar_noport)==1)])==TRUE)
three_fish <- length(which(clusts[[3]]$cluster==3 & sub_move$fishing[which(!is.na(sub_move$dBvar_noport)==1)])==TRUE)

fishing_capt <- c(one_fish,two_fish, three_fish)
barplot(fishing_capt,col=c("dodgerblue","indianred","grey"),xlab="cluster",ylab="number of fishing points captured")
legend("topright",legend=c("1","2","3"),title="cluster:",lwd=5,col=c("dodgerblue","indianred","grey"),bty="n")

# what about looking at how sigma correlates to turning angle? Hard to do speed.. I think. 

# but even for random forest, need to find types of movement that I know are not fishing. And it needs to represent the variation in the movement data. Need to ask Kami about this.

# other thing to do is try bivariate brownian bridge and see those. still problem of confirming false positives. 


try <- markovchainFit(clusts[[3]]$cluster)
foo <- markovchainSequence(n=20,try[[1]])