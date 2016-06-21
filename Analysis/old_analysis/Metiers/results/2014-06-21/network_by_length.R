rm(list=ls())
require(qgraph); require(igraph); require(RColorBrewer)
load("tickets_len.Rdata")

above60 <- subset(tickets_len, len>=60)
below60 <- subset(tickets_len, len<60)

num_cl_a60 <- ddply(above60, .(veid),summarize, num_clust = length(unique(c8)))
num_cl_b60 <- ddply(below60, .(veid),summarize, num_clust = length(unique(c8)))

nodes = 8

par(mfrow=c(1,2))
barplot(table(num_cl_a60$num_clust),ylim=c(0,650), main="number of fisheries vessels > 60 ft participate in")
barplot(table(num_cl_b60$num_clust),ylim=c(0,650), main="number of fisheries vessels < 60 ft participate in")

plot_network <- function(num_clust_part,data){
  c1 <- subset(data, num_clust==num_clust_part)
  c1_trips <- subset(tickets_len, veid %in% c1$veid,select=c(ftid,veid,c8))
  c1_trips <- c1_trips[!duplicated(c1_trips),]
  
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
  
  qgraph(network, vsize=log(sizes), layout="spring", posCol="black", curveAll = FALSE, esize=30,colFactor=.25,bg="white",color=paint,borders=FALSE, title=paste(nrow(c1), "vessels",sep=" "))
  
}


for(i in 1:8){
pdf(file=paste("plots/Fign_svCG",i,".pdf",sep=""),width=10, height=8)
par(mfrow=c(2,1))
plot_network(i, num_cl_b60)
plot_network(i, num_cl_a60)

dev.off()
}

# compare who I lost between using all tickets, and only tickets for which i had vessel length

load("tickets.Rdata")
num_cl <- ddply(tickets, .(veid),summarize, num_clust = length(unique(c8)))

vchar <- select(tickets_len, veid, len, c8)
vchar <- vchar[!duplicated(vchar),]

length(which(!num_cl$veid %in% vchar$veid))

# what's being dropped?

dropped = num_cl[which(!num_cl$veid %in% vchar$veid),]

v_drop <- select(dropped, veid)

drop_tick <- merge(tickets, v_drop, by="veid")

vchar_drop <- select(drop_tick, veid, c8)
vchar_drop <- vchar_drop[!duplicated(vchar_drop),]
barplot(rbind(table(vchar_drop$c8),table(vchar$c8)),beside=TRUE,legend.text=c("dropped","kept"),col=c("dark blue", "sky blue"),bor=F,args.legend=c(x = "topleft",bty="n",border="white",title="trips that were"), names.arg = c("shellfish", "shrimp", "crab","other","salmon","groundfish","tuna","pelagics"),las=1)

# this looks like a lot of shellfish, which is expected. As for the shrimp, which is most concerning. Looking through by hand, it looks like a lot of bait/ghost shrimp. And vessels that only operate one year in that fishery. Which is why maybe they don't have information in state/CG for length.

subset(tickets, veid==subset(vchar_drop,c8==2)$veid[15])

# Although there are a few vessels that are more concerning that do pink shrimp and operate for awhile. so doesn't completely explain it. 

subset(tickets, veid==subset(vchar_drop,c8==2)$veid[17])