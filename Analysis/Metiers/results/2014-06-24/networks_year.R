# looking at how fisheries participation changes over year
rm(list=ls())
require(plyr); require(dplyr); require(igraph); require(RColorBrewer); require(qgraph)
load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-06-21/tickets.Rdata")

# look at network over time for all vessels

t9 <- subset(tickets, year==2009)
t10 <- subset(tickets, year==2010)
t11 <- subset(tickets, year==2011)
t12 <- subset(tickets, year==2012)
t13 <- subset(tickets, year==2013)


nodes = 8
plot_network <- function(num_clust_part=NA,data=NA, tickets_len){
  if(!is.na(num_clust_part)){ # if I want to subset by number of fisheries participated in
    c1 <- subset(data, num_clust==num_clust_part)
    c1_trips <- subset(tickets_len, veid %in% c1$veid,select=c(ftid,veid,c8))
    }else{ # if don't care
      c1_trips <- select(tickets_len, ftid, veid, c8)
      c1_trips <- c1_trips[!duplicated(c1_trips),]
    }
  
  network <- matrix(nrow=nodes,ncol=nodes)
  
  for(i in 1:length(unique(c1_trips$c8))){
    # for catch profile i
    set1 <- unique(c1_trips$veid[c1_trips$c8==i])
    # where are the unique vessel ids
    for(j in 1:length(unique(c1_trips$c8))){
      # for catch profile j
      set2 <- unique(c1_trips$veid[c1_trips$c8==j])
      # what are the unique vessel ids
      network[i,j] = length(Reduce(intersect, list(set1, set2)))
      # element is the number of vessel IDs in common
    }
  }
  
  # remove any NAs
  network[!is.finite(network)] <- 0
  
  sizes <- diag(network)
  diag(network) <- rep(0,nodes)
  g <- graph.adjacency(network, weighted=TRUE, mode="undirected")
  # plot(g, vertex.size = log(diag(network)), layout=layout.fruchterman.reingold, width=g)
  
  paint <- colorRampPalette(brewer.pal(8,"Accent"))(nodes)
  
  qgraph(network, vsize=log(sizes), 
         layout="spring", posCol="black", 
         curveAll = FALSE, esize=30,
         colFactor=.25,bg="white",
         color=paint,borders=FALSE, 
         title=paste(length(unique(c1_trips$veid)), "vessels",sep=" "),
         labels = c("Shellfish","Shrimp","Crab","Other","Salmon","Groundfish","Albacore","Pelagics"),
         edge.labels=TRUE)
  
}

plot_network(tickets_len=tickets)
plot_network(tickets_len=t9)
plot_network(tickets_len=t10)
plot_network(tickets_len=t11)
plot_network(tickets_len=t12)
plot_network(tickets_len=t13)

# looking at how 2009-2013 2 fishery vessels look
par(mfrow=c(2,3))
numcl9 <- ddply(t9, .(veid),summarize, num_clust = length(unique(c8)))
plot_network(num_clust_part=2,data=numcl9, tickets_len=tickets)

numcl10 <- ddply(t10, .(veid),summarize, num_clust = length(unique(c8)))
plot_network(num_clust_part=2,data=numcl10, tickets_len=tickets)

numcl11 <- ddply(t11, .(veid),summarize, num_clust = length(unique(c8)))
plot_network(num_clust_part=2,data=numcl11, tickets_len=tickets)

numcl12 <- ddply(t12, .(veid),summarize, num_clust = length(unique(c8)))
plot_network(num_clust_part=2,data=numcl12, tickets_len=tickets)

numcl13 <- ddply(t13, .(veid),summarize, num_clust = length(unique(c8)))
plot_network(num_clust_part=2,data=numcl13, tickets_len=tickets)

# 2009-2013 3 fisheries
plot_network(num_clust_part=3,data=numcl9, tickets_len=tickets)
plot_network(num_clust_part=3,data=numcl10, tickets_len=tickets)
plot_network(num_clust_part=3,data=numcl11, tickets_len=tickets)
plot_network(num_clust_part=3,data=numcl12, tickets_len=tickets)
plot_network(num_clust_part=3,data=numcl13, tickets_len=tickets)

# 2009-2013 4 fisheries
plot_network(num_clust_part=4,data=numcl9, tickets_len=tickets)
plot_network(num_clust_part=4,data=numcl10, tickets_len=tickets)
plot_network(num_clust_part=4,data=numcl11, tickets_len=tickets)
plot_network(num_clust_part=4,data=numcl12, tickets_len=tickets)
plot_network(num_clust_part=4,data=numcl13, tickets_len=tickets)

# 2009-2013 5 fisheries
plot_network(num_clust_part=5,data=numcl9, tickets_len=tickets)
plot_network(num_clust_part=5,data=numcl10, tickets_len=tickets)
plot_network(num_clust_part=5,data=numcl11, tickets_len=tickets)
plot_network(num_clust_part=5,data=numcl12, tickets_len=tickets)
plot_network(num_clust_part=5,data=numcl13, tickets_len=tickets)

# 2009-2013 6 fisheries
plot_network(num_clust_part=6,data=numcl9, tickets_len=tickets)
plot_network(num_clust_part=6,data=numcl10, tickets_len=tickets)
plot_network(num_clust_part=6,data=numcl11, tickets_len=tickets)
plot_network(num_clust_part=6,data=numcl12, tickets_len=tickets)
plot_network(num_clust_part=6,data=numcl13, tickets_len=tickets)

# 2009-2013 7 fisheries
plot_network(num_clust_part=7,data=numcl9, tickets_len=tickets)
plot_network(num_clust_part=7,data=numcl10, tickets_len=tickets)
plot_network(num_clust_part=7,data=numcl11, tickets_len=tickets)
plot_network(num_clust_part=7,data=numcl12, tickets_len=tickets)
plot_network(num_clust_part=7,data=numcl13, tickets_len=tickets)

# 2009-2013 8 fisheries
plot_network(num_clust_part=8,data=numcl9, tickets_len=tickets)
plot_network(num_clust_part=8,data=numcl10, tickets_len=tickets)
plot_network(num_clust_part=8,data=numcl11, tickets_len=tickets)
plot_network(num_clust_part=8,data=numcl12, tickets_len=tickets)
plot_network(num_clust_part=8,data=numcl13, tickets_len=tickets)

# what about looking at the number of vessels that did groundfish plus something before and after catch shares. 