#  show spatial variability in participation network complexity

# load data
#----
tickets <- readRDS("tickets.RDS") 
ports <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/data/wc_fishing_communities.csv",col.names=c("port","lon","lat"))
pcid <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/data/pcid.csv")
#----
# format ports - get lat/lons
#----
# reformat names
  pcid$Name <- tolower(pcid$Name)
  ports$port <- tolower(ports$port)
# find only ports that are in ticket data
  represented_ports <- unique(tickets$pcid)
  all_ports <- subset(pcid,Pcid %in% represented_ports ) # find ports only in tickets
# seaside-gearhart is weird, deal with whitespace
  all_ports$Name[grep("gearhart",all_ports$Name)] <- "seaside-gearhart" # reverse. gah
  all_ports <- merge(all_ports, ports, by.x="Name",by.y="port",all.x=TRUE) # add latitudes
# some pcids are "/", find those and take mean between latitudes
  slashed <- grep("/",all_ports$Name)
  
  for(i in slashed){
    # unlist the two names
      two_names <- unlist(strsplit(all_ports$Name[i],"/"))
    # gets rid of leading/trailing whitespace
      two_names <- gsub("^\\s+|\\s+$", "", two_names) 
    # find the lats listed in original fishing communities
      lat1 <- ports$lat[grep(two_names[1],ports$port)]
      lat2 <- ports$lat[grep(two_names[2],ports$port)]
      lon1 <- ports$lon[grep(two_names[1],ports$port)]
      lon2 <- ports$lon[grep(two_names[1],ports$port)]
    # take means of those
      lat <- mean(c(lat1,lat2))
      lon <- mean(c(lon1,lon2))
    # assign to proper port
      all_ports$lat[i] <- lat
      all_ports$lon[i] <- lon
  }

# missing latitudes for following ports
  misses <- all_ports[which(is.na(all_ports$lat)),]
# those that don't have "other" in title should be included
  to_find <- misses[-grep("other",misses$Name),]
# search google maps with these names
  library(ggmap); library(maps)
# add states using agency so google maps doesn't get confused
  add_agency <- function(adgency){
    state = ifelse(adgency=="CDFG","CA",ifelse(adgency=="ODFW", "OR",ifelse(adgency=="WDFW","WA","NA")))
    to_find$Name[which(to_find$Agency==adgency)] <- paste0(to_find$Name[which(to_find$Agency==adgency)],", ",state)
    return(to_find)
  }
  to_find <- add_agency("CDFG")
  to_find <- add_agency("ODFW")
  to_find <- add_agency("WDFW")
# use google maps API to find lat/lons
  to_find[,c("lon","lat")] <- geocode(to_find$Name)
# check
  with(to_find, plot(lon,lat,asp=1))
  map('state',add=T)
# looks good!

# assign to dataframe
  put_in <- function(c.name){
  all_ports[,c.name][ind] <- to_find[,c.name][i]
  return(all_ports)
}

for(i in 1:nrow(to_find)){
  ind <- which(all_ports$Pcid==to_find$Pcid[i])
  all_ports <- put_in("lon")
  all_ports <- put_in("lat")
}
#----
# calculate participation networks across all ports
#----
library(igraph); library(reshape2); library(vegan); library(RColorBrewer)

# different measures: total is what I'm using now. Could be bray-curtis dissimilarity to look at vessel composition across fisheries or hellinger. A fishery is very similar if the same vessels participate in the same way. Using drvid as vessel ID. measure = "total" is just the total number of vessels that participate in both. 

define_participationPlot <- function(year_choose=2009:2013, port=NA, restrict=TRUE, measure="total",graph=FALSE){
  if(is.na(port)){
    yr_tickets <- tickets[which(tickets$year %in% year_choose),]}
  if(!is.na(port)){
    yr_tickets <- tickets[which(tickets$year %in% year_choose & tickets$pcid==port),]
  }
  m_by_v <- melt(yr_tickets, id.vars = c("metier","drvid"), measure.vars = "trip_id")
  m_by_v <- unique(m_by_v)
  if(length(unique(m_by_v$drvid))==1){
    return(warning("only one vessel at this port"))
  }
  cast_mv <- dcast(m_by_v, metier~drvid, length)
  rownames(cast_mv) <- cast_mv$metier
  cast_mv <- cast_mv[,-1]

  # remove any metiers that aren't used at all
  
  # restrict to metiers responsible for at least 95% of trips 
  if(restrict==TRUE){
    metier_totals <- rowSums(cast_mv)
    total_trips <- sum(metier_totals)
    cumulatives <- cumsum(sort(metier_totals/total_trips,decreasing=T))
    # check to make a single metier not it
    if(cumulatives[1]>.95){
      metiers_to_keep <- names(cumulatives[1])
    }else{
      metiers_to_keep <- names(which(cumulatives<=.95))
    }
    cast_mv <- cast_mv[metiers_to_keep,]
    cast_mv <- cast_mv[,-which(colSums(cast_mv)==0)]
    m_by_v <- subset(m_by_v, metier %in% metiers_to_keep)
  }
  
  if(measure=="bray_curtis"){
    bc_drvid <- as.matrix(1-vegdist(cast_mv))
  }
  if(measure=="hellinger"){
    bc_drvid <- as.matrix(1-dist(decostand(cast_mv,"hellinger")))
  }
  if(measure=="total"){
    edge.width <- matrix(ncol=length(metiers_to_keep), nrow=length(metiers_to_keep),0)
    row.names(edge.width) <- metiers_to_keep
    colnames(edge.width) <- metiers_to_keep
    sizes <- rep(NA, ncol(edge.width))
    for(i in 1:nrow(edge.width)){
      for(j in 1:ncol(edge.width)){
        row_met <- row.names(edge.width)[i]
        col_met <- colnames(edge.width)[j]
        both <- intersect(unique(m_by_v$drvid[which(m_by_v$metier==row_met)]),
                          unique(m_by_v$drvid[which(m_by_v$metier==col_met)]))
        all <- union(unique(m_by_v$drvid[which(m_by_v$metier==row_met)]),
                         unique(m_by_v$drvid[which(m_by_v$metier==col_met)]))
        edge.width[i,j] <- length(both)/length(all)
        if(i==j){sizes[i] <- length(all)}
      }
    }
    bc_drvid <- edge.width
    diag(bc_drvid) <- sizes
    
  }
  
  g <- graph.adjacency(bc_drvid,weighted = TRUE, mode="undirected", diag = FALSE)
  par(mai=rep(0,4))
  V(g)$size <- diag(bc_drvid)
  
  im_g <- infomap.community(g)
  
  paint <- colorRampPalette(brewer.pal( 8, "Dark2"))(length(im_g))
  V(g)$color <- paint[im_g$membership]
  V(g)$membership <- im_g$membership
  
  if(graph==TRUE){
    l <- layout.fruchterman.reingold(g, niter=500, narea=vcount(g)^2.5, 
                                   repulserad = vcount(g)^5)
    
    plot(g,vertex.size=V(g)$size/10, edge.width=E(g)$weight*100, 
       vertex.label.family="sans",layout=l,
       vertex.frame.color=V(g)$color, vertex.label.color="black")
  }
  return(g)
  
}
#----
# calculate network stats
#----
library(NetIndices)

all_ports$CZ<- NA
all_ports$NZ <- NA
all_ports$N <- NA
all_ports$C <- NA

for(i in 1:length(all_ports$Pcid)){
  g <- define_participationPlot(port=all_ports$Pcid[i])
  if(class(g)=="character"){next} # if only one vessel, will generate a character warning
  all_ports$CZ[i] <- EffInd(get.adjacency(g, sparse=F))$CZ
  all_ports$NZ[i] <- EffInd(get.adjacency(g, sparse = F))$NZ
  all_ports$N[i] <- GenInd(get.adjacency(g, sparse=F))$N
  all_ports$C[i] <- GenInd(get.adjacency(g, sparse=F))$C
}
#----
# add total volume to ports
#----
library(plyr)
port_vol <- ddply(tickets, .(pcid), summarize, landed = sum(landed_wt))
all_ports <- merge(all_ports, port_vol, by.x = "Pcid",by.y="pcid", all.x = TRUE)
# save data.frame
saveRDS(all_ports,"/Users/efuller/1/CNH/Analysis/Metiers/results/2015-01-09/code/3_exploreBuildwebs/all_ports.RDS")
#----
# plotting
#----
library(scales)
with(all_ports, plot(lat, CZ,lwd=2,col="slategrey"))
with(all_ports, plot(NZ, CZ, lwd=2, col="slategrey")) # but really just number of nodes
with(all_ports, plot(lat, C), lwd=2, col="slategrey")
with(all_ports, plot(N,C))
with(all_ports, plot(N, NZ))
with(all_ports, plot(C, CZ))
with(all_ports, plot(N, CZ))
with(all_ports, plot(lat, N))

with(all_ports, plot(lat, CZ, cex = (landed/100000000)+.05,col="slategrey",lwd=2,col.axis="black",col.lab="black",col.sub="black",bty="n"))
with(all_ports, points(lat, CZ, cex = (landed/100000000)+.05,col=alpha("slategrey",.75),pch=19))
text(all_ports$lat, all_ports$CZ, all_ports$Name,cex=(all_ports$landed/1000000000)+.5,col="black")

# with a map
library(maps); library(ggplot2)
states <- map_data("state")
states <- subset(states, region %in% c("washington","oregon","california","nevada","idaho"))

 p <- ggplot(all_ports, aes(x=lon, y=lat))  + 
  theme(panel.background=element_rect(colour="steelblue",fill="steelblue"), 
        panel.grid.major=element_line(colour="steelblue"), 
        legend.key=element_rect(fill="white")) + 
  geom_polygon(data=states,aes(x = long, y = lat, group=group),
               fill="grey",colour="white") + 
  coord_map(xlim=range(all_ports$lon,na.rm=T)+c(-.5,.5), 
            ylim=range(all_ports$lat, na.rm=T)+c(-.5,.5)) + 
  scale_colour_gradient2(low="#fc8d59",high="#99d594", 
                         midpoint=6,mid="#ffffbf") +  
  geom_point(aes(colour=CZ, size = log(landed))) 

p

saveRDS(p, "/Users/efuller/1/CNH/Analysis/Metiers/results/2015-01-09/code/3_exploreBuildwebs/p_spatial_map.RDS")
  

  
ggplot(all_ports, aes(x=lon, y=lat,color=CZ)) + 
  geom_point() + theme_minimal() + geom_polygon(data=states,aes(x = long, y = lat, group=group)) 



# specific port networks
sb <- define_participationPlot(port="SB")
morro <- define_participationPlot(port="MRO")
coos <- define_participationPlot(port="COS")
newport <- define_participationPlot(port="NEW")
brookings <- define_participationPlot(port="BRK")
eureka <- define_participationPlot(port="ERK")

compare_ports <- list(Santa.Barbara = sb, Morro.Bay=morro, Coos.Bay=coos, Newport=newport,Brookings=brookings,Eureka=eureka)
saveRDS(compare_ports, "/Users/efuller/1/CNH/Analysis/Metiers/results/2015-01-09/code/3_exploreBuildwebs/compare_ports.RDS")
