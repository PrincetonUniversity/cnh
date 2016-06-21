# quantifying species richness of fisheries: i.e. how many fisheries do vessels participate in each year?
rm(list=ls())
require(vegan)

# load tickets
tickets <- readRDS("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/tickets.Rda")

# find out the lbs per year per mgmt group. (this would be affected by the mgmt regulations right?)
lbs_f <- ddply(tickets, .(year, mgmt_grp), summarize, total_caught=sum(landed_wt))

by_mgmt <- dcast(lbs_f, year~mgmt_grp)
by_mgmt <- as.matrix(by_mgmt)
row.names(by_mgmt) <- by_mgmt[,1]
by_mgmt <- by_mgmt[,-1]
f <- diversity(by_mgmt)
plot(f, type='o')

par(mfrow=c(1,1))
barplot(by_mgmt, beside=T,bor=T)

# find dollars per year per mgmt grp
tickets$rev <- tickets$ppp*tickets$landed_wt
dol_f <- ddply(tickets, .(year, mgmt_grp), summarize, total_caught=sum(rev))

dol_mgmt <- dcast(dol_f, year~mgmt_grp)
dol_mgmt <- as.matrix(dol_mgmt)
row.names(dol_mgmt) <- dol_mgmt[,1]
dol_mgmt <- dol_mgmt[,-1]

f_dol <- diversity(dol_mgmt)
plot(f_dol, type='o')

par(mfrow=c(1,1))
barplot(dol_mgmt[,2:ncol(dol_mgmt)], beside=T,bor=T)

# tourmiso's definition of diversity
qD <- rowSums((prop.table(by_mgmt,margin=1))^2)^(-1)

# function form
qD <- function(df){
  return(rowSums((prop.table(df,margin=1))^2)^(-1))
}

# compare shannon diversity 
plot(f_dol,ylim=c(1.3,1.9),type="o")
lines(f,type="o")
# compare qD

plot(qD(dol_mgmt),type="o",ylim=c(2.5,6))
lines(qD(by_mgmt),type="o")

# let's look at port level diversity

ports <- ddply(tickets, .(year, pcid, mgmt_grp),summarize, total_caught=sum(landed_wt))

by_port <- split(ports, ports$pcid)
new_ports <- vector("list",length=length(by_port))
for(i in 1:length(by_port)){
  new_ports[[i]] <- dcast(by_port[[i]],year~mgmt_grp)
  new_ports[[i]][is.na(new_ports[[i]])] <- 0
  row.names(new_ports[[i]]) <- new_ports[[i]][,1]
  new_ports[[i]][,1] <- NULL
  new_ports[[i]] <- as.matrix(new_ports[[i]])
}

names(new_ports) <- names(by_port)
div_port <- do.call(qD,new_ports)

qD_list <- vector("list",length(new_ports))

for(i in 1:length(new_ports)){
  qD_list[[i]] <- qD(as.matrix(new_ports[[i]]))
}

names(qD_list) <- names(new_ports)

plot(as.numeric(names(qD_list[[1]])),qD_list[[1]],type="o",ylim=c(.5,4.5),pch=19, col=alpha("black",.5),lwd=2,cex=1.5)
pdf(file="/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-06-30/by_port.pdf",height=7,width=10)
for(i in 1:length(qD_list)){
plot(as.numeric(names(qD_list[[i]])),qD_list[[i]],type="o",pch=19,col=alpha("black",.55),lwd=2,cex=1.5,xlab="year",ylab="qD",ylim=c(.5,4.5),xlim=c(2009,2013),main=names(qD_list[i]))  
}
dev.off()


pdf(file="/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-06-30/by_port.pdf",height=7,width=10)
for(j in 1:length(qD_list)){
plot(as.numeric(names(qD_list[[1]])),qD_list[[1]],type="o",ylim=c(.5,4.5),pch=19, col=alpha("black",0),lwd=2,cex=1.5,xlab="year",ylab="qD")
for(i in 1:length(qD_list)){
  lines(as.numeric(names(qD_list[[i]])),qD_list[[i]],type="o",pch=19,col=alpha("black",.25),lwd=2,cex=1.5,xlab="year",ylab="qD",ylim=c(.5,4.5),xlim=c(2009,2013))  
}

lines(as.numeric(names(qD_list[[j]])),qD_list[[j]],type="o",pch=19,col=alpha("tomato1",1),lwd=5,cex=1.5,xlab="year",ylab="qD",ylim=c(.5,4.5),xlim=c(2009,2013),main=names(qD_list[j]))
text(2010,4,names(qD_list[j]))
}
dev.off()

# what about vessel level diversity?

# find out the lbs per year per mgmt group. (this would be affected by the mgmt regulations right?)
by_ves <- ddply(tickets, .(year, veid, mgmt_grp), summarize, total_caught=sum(landed_wt))

# remove the two weird IDs, "unknown" and "***"
by_ves <- subset(by_ves, veid!="UNKNOWN")
by_ves <- by_ves[-grep("[*]",by_ves$veid),]

# want df where each column is vessel/year combo and columns are what proportion of catch was made up of each mgmt_grp

foo <- ddply(by_ves, .(year,veid),summarize,all=sum(total_caught))
by_ves <- merge(by_ves, foo, by=c("year", "veid"))
by_ves$prop <- by_ves$total_caught/by_ves$all

qD_ves <- ddply(by_ves, .(year, veid),function(df)sum((df$prop)^2)^(-1))

ggplot(qD_ves, aes(x=year, y=V1, group=factor(veid))) + geom_line(alpha=.05)

# would be nice to know now how to break these vessels down. Which are the ITQ trawlers for example? Or primarily gf fishermen?

# in this case it would be good to try to cluster on just the total number of lbs each fishermen brings in each year. Which I've done with hclust. Then could break down vessels into "primarily groundfish", "primarily shrimp", etc. Or split among them.. Would this loose people that do 50-50 things? I suppose there would be some reflection in the number of clusters and the silhouette width. 

# have the proportion of lbs in each mgmt_grp. Will cluster on unique vessel_year pairs. Or just do 2009, and assume all strategies are well represented? No, because I think there might be new strategies emerging, or at least that possibility. Let's do all years. 

by_ves$yvID <- paste(by_ves$year,by_ves$veid,sep="_")

st_ves <- dcast(by_ves, yvID~mgmt_grp,value.var="prop")
st_ves[is.na(st_ves)] <- 0

pc_ves <- prcomp(st_ves[2:ncol(st_ves)])

# retain pcs that give me at least 80% of variation

pc_dat <- pc_ves$x[,1:4]

# clustering
require(cluster)
samples = 100
max.clusters=30

clust_sol <- vector("list",length=max.clusters)
for(i in 1:max.clusters){
  clust_sol[[i]] <- clara(pc_dat, i, samples = samples)
  cat(i,"...")
}

objectives <- vector("numeric",length=max.clusters)
for(i in 1:max.clusters){
  objectives[i] <- clust_sol[[i]]$objective
}

asw <- vector("numeric",length=max.clusters)
for(i in 2:max.clusters){
  asw[i] <- clust_sol[[i]]$silinfo$avg.width
}