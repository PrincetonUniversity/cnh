# date: 2014-04-11
# purpose: loading sets of saved .Rdata, plots of sigma across time and space. 

load("data/all_vessels.Rdata")



# then run for loop for calculating brownian bridges for each

# should make each individual be a bursted move object, calculate a brownian bridge for each burst (within individuals), then add up all layers for individuals, normalize, and then add up all brownian bridges across all vessels. 

path= "data/"

vars <- rep("vessel",4)

for(i in 1:4){
	var_names[i] <- paste(vars[i],i,"move",sep='_')
}
# load vessel trajectories
for(i in 1:4){
	 file_load <- grep(paste(all_vessels[i],"_move_",Sys.Date(),sep=""),list.files("data/"))
	 load(paste(path,list.files("data/")[file_load],sep=""))
	 assign(var_names[i],VMS1_move)

}

for(i in 1:4){
	var_names[i] <- paste(vars[i],i,"sigma",sep='_')
}

for(i in 1:4){
  file_load <- grep(paste(all_vessels[i],"_dBBMMvar_",Sys.Date(),".Rdata",sep=""),list.files("data/"))
  load(paste(path,list.files("data/")[file_load],sep=""))
 assign(var_names[i],dBBMM)
 #vessel_trajs[[i]] <- brownian.bridge.dyn(dBBMM, location.error=rep(100,n.locs(dBBMM)),margin=11,window.size=31,ext=.5)
}
save(vessel_trajs,file=paste("data/vessel_trajs",Sys.Date(),".Rdata",sep=""))

## for two vessels i have, do a joint brownian bridge to make sure I know how

# burst by port for vessel 1
vessel1_b <-burst(vessel_1,head(vessel_1$behavior,-1))
vessel1_sp <- spTransform(vessel1_b,CRSobj="+proj=aeqd",center=TRUE)
vessel2_b <- burst(vessel_2,head(vessel_2$behavior,-1))

bridge1 <- brownian.bridge.dyn(vessel1_sp[vessel1_b@burstId=="port"], location.error=100,margin=11,window.size=31,ext=0.3)

vessel_list <- list(vessel1_b,vessel1_b)

#### takes a long time to run: setting this aside for the moment. 

# looking at differences in sigma


plot(vessel_2_sigma$Date_Time[vessel_2_sigma$behavior=="water"],vessel_2_sigma@means[vessel_2_sigma$behavior=="water"],type='h',cex=.5,ylim=range(vessel_4_sigma@means[vessel_4_sigma$behavior=="water"],na.rm=T))

points(vessel_1_sigma$Date_Time[vessel_1_sigma$behavior=="water"],vessel_1_sigma@means[vessel_1_sigma$behavior=="water"],type='h',cex=.5,col="red")

points(vessel_3_sigma$Date_Time[vessel_3_sigma$behavior=="water"],vessel_3_sigma@means[vessel_3_sigma$behavior=="water"],type='h',cex=.5,col="orange")

points(vessel_4_sigma$Date_Time[vessel_4_sigma$behavior=="water"],vessel_4_sigma@means[vessel_4_sigma$behavior=="water"],type='h',cex=.5,col="forestgreen")


### just doing vessel 4

plot(vessel_4_sigma$Date_Time[vessel_4_sigma$behavior=="water"],vessel_4_sigma@means[vessel_4_sigma$behavior=="water"],type='h',cex=.5,col="gold")

pdf("ppt/vessel_4_sigma.pdf")
par(bg="black")
plot(vessel_4_sigma$Date_Time[vessel_4_sigma$behavior=="water"],vessel_4_sigma@means[vessel_4_sigma$behavior=="water"],type='h',cex=.5,col="gold",col.lab="white",col.xaxis="white",axes=FALSE,xlab="",ylab="")
axis(2,col="white")
mtext(expression(sigma), side=2, line=1, cex.lab=1, col="white")
axis(1,col="white")
mtext("Time",side=1,line=1,col="white")
dev.off()

require(classInt)

my.class <- classIntervals(vessel_1_sigma@means[vessel_1_sigma$behavior=="water"], n=2, style="quantile")
my.class <- classIntervals(dBBMM@means, n=5, style="kmeans")
my.class <- classIntervals((dBBMM@means)^(1/2), n=5, style="pretty")

my.pal <- findColours(my.class, c("gold", "white")) # high values of sigma = white, low = gold

par(bg="black")
plot(vessel_1_sigma[vessel_1_sigma$behavior=="water"], type="l",col=alpha("grey",0.1),cex=.25,asp=1)
points(vessel_1_sigma[vessel_1_sigma$behavior=="water"],col=alpha(my.pal,0.15),pch=19,cex=0.15,type='p')

lines(sub_move,type="l",col=alpha("yellow",0.25),cex=0.25)
points(sub_move,col=alpha(my.pal,0.25),pch=19,cex=.25)
points(VMS1_move, col=alpha(my.pal,0.25),pch=19,cex=0.25)
plot(WC,add=T,,col=alpha("darkgrey",0.25),bor=FALSE)

# cluster values of sigma

nclusters <- 7
wsum <- vector()
clusts <- list()
for(i in 1:nclusters){
  clusts[[i]] <- kmeans(vessel_4_sigma@means[vessel_4_sigma$behavior=="water" & !is.na(vessel_4_sigma@means)],i)
  wsum[i] <- clusts[[i]]$tot.withinss
  print(i)
}
pdf(file="ppt/cluster_ves4.pdf")
par(bg="black")
plot(wsum,type='o',col="gold",pch=19,yaxt="n",xaxt="n",bty="n")
axis(2,col="white")
mtext("sum of squares", side=2, line=1, cex.lab=1, col="white")
axis(1,col="white")
mtext("number of clusters", side=1, line=1, cex.lab=1, col="white")
dev.off()

axis(2,col="white",lab="number of clusters")

# color vecotr
col_vec <- clusts[[3]]$cluster # 1 = highest, 2 = lowest, 3 = intermediate
col_vec[col_vec==1] <- "white"
col_vec[col_vec==2] <- "gold"
col_vec[col_vec==3] <- "cyan3"


# plot sigma with color = cluster
pdf("ppt/vessel_4_sigma_clust.pdf")
par(bg="black",mar=c(2,2,0,0))
plot(vessel_4_sigma$Date_Time[vessel_4_sigma$behavior=="water" & !is.na(vessel_4_sigma@means)], vessel_4_sigma@means[vessel_4_sigma$behavior=="water" & !is.na(vessel_4_sigma@means)],type='h',ylim=range(vessel_4_sigma@means[vessel_4_sigma$behavior=="water"& !is.na(vessel_4_sigma@means)],na.rm=T),col=col_vec)
axis(2,col="white")
mtext(expression(sigma), side=2, line=1, cex.lab=1, col="white",cex=1.5)
axis(1,col="white")
mtext("Time",side=1,line=1,col="white",cex=1.5)
dev.off()

# plot sigma with color = cluster, plus known fishing
pdf("ppt/vessel_4_sigma_clust_known_fish.pdf")
par(bg="black",mar=c(2,2,0,0))
plot(vessel_4_sigma$Date_Time[vessel_4_sigma$behavior=="water" & !is.na(vessel_4_sigma@means)], vessel_4_sigma@means[vessel_4_sigma$behavior=="water" & !is.na(vessel_4_sigma@means)],type='h',ylim=range(vessel_4_sigma@means[vessel_4_sigma$behavior=="water"& !is.na(vessel_4_sigma@means)],na.rm=T),col=col_vec)
axis(2,col="white")
mtext(expression(sigma), side=2, line=1, cex.lab=1, col="white",cex=1.5)
axis(1,col="white")
mtext("Time",side=1,line=1,col="white",cex=1.5)

behaviour_vec <- vessel_4_move$known_fish[vessel_4_sigma$behavior=="water" & !is.na(vessel_4_sigma@means)]
behaviour_vec[is.na(behaviour_vec)]<-0

lines(vessel_4_sigma$Date_Time[vessel_4_sigma$behavior=="water" & !is.na(vessel_4_sigma@means)], behaviour_vec*.5e05,lwd=2,col=alpha("red",1))

dev.off()

# reduce xlim
pdf(file="ppt/zoom_vessel4.pdf")
par(bg="black",mar=c(0,0,2,0))
plot(vessel_4_sigma$Date_Time[vessel_4_sigma$behavior=="water" & !is.na(vessel_4_sigma@means)], 
	vessel_4_sigma@means[vessel_4_sigma$behavior=="water" & !is.na(vessel_4_sigma@means)],
	type='h',ylim=c(0,300000)
	,col=col_vec,
	xlim=c(as.POSIXct('2012-05-15'),as.POSIXct('2012-07-01')),lwd=2)

behaviour_vec <- vessel_4_move$known_fish[vessel_4_sigma$behavior=="water" & !is.na(vessel_4_sigma@means)]
behaviour_vec[is.na(behaviour_vec)]<-0

lines(vessel_4_sigma$Date_Time[vessel_4_sigma$behavior=="water" & !is.na(vessel_4_sigma@means)], behaviour_vec*.5e05,lwd=2,col=alpha("red",1),type="h")

mtext("June 15- July 7, 2012",side=3,col="white",cex=2)
dev.off()

pdf("ppt/cluster_vessel4_traj.pdf")
par(bg="black")
plot(vessel_4_sigma$x[vessel_4_sigma$behavior=="water"& !is.na(vessel_4_sigma@means)],vessel_4_sigma$y[vessel_4_sigma$behavior=="water"& !is.na(vessel_4_sigma@means)], type="l",col=alpha("grey",0.1),cex=.15,asp=1,xlim=c(-128,-122),ylim=range(vessel_4_sigma$y))
points(vessel_4_sigma$x[vessel_4_sigma$behavior=="water" & !is.na(vessel_4_sigma@means)],vessel_4_sigma$y[vessel_4_sigma$behavior=="water" & !is.na(vessel_4_sigma@means)],type='p',cex=.15,col=alpha(col_vec,0.15),pch=19)
plot(WC,add=T,,col=alpha("darkgrey",0.25),bor=FALSE)

dev.off()

# compare to just plotting sigma
require(classInt)

my.class <- classIntervals(vessel_4_sigma@means[vessel_4_sigma$behavior=="water"& !is.na(vessel_4_sigma@means)], n=2, style="quantile")
my.class <- classIntervals(dBBMM@means, n=5, style="kmeans")
my.class <- classIntervals((dBBMM@means)^(1/2), n=5, style="pretty")

my.pal <- findColours(my.class, c("gold", "white")) # high values of sigma = white, low = gold

par(bg="black")
plot(vessel_4_sigma[vessel_4_sigma$behavior=="water"& !is.na(vessel_4_sigma@means)], type="l",col=alpha("grey",0.1),cex=.25,asp=1)
points(vessel_4_sigma[vessel_4_sigma$behavior=="water"& !is.na(vessel_4_sigma@means)],col=alpha(my.pal,0.15),pch=19,cex=0.15,type='p')
