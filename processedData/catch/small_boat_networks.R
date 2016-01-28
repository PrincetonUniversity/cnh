# given length cutoff, make new networks
library(plyr); library(dplyr)
ftl_len <- readRDS("/Users/efuller/1/CNH/processedData/catch/5_add_length/ftl_len.RDS")

length_split <- 40
ftl_small <- ftl_len[which(ftl_len$len <= length_split),]

tickets.df <- ftl_small
# building participation networks ----

library(igraph); library(reshape2); library(vegan); library(RColorBrewer)

# calculate the bray curtis dissimilarity to look at vessel composition across fisheries. A fishery is very similar if the same vessels participate in the same way. 
# need a metier by drvid matrix to run vegdist on

define_participationPlot <- function(year_choose, port=NA, restrict=TRUE, tickets = tickets.df){
  if(is.na(port)){
    yr_tickets <- tickets[which(tickets$year %in% year_choose),]}
  if(!is.na(port)){
    yr_tickets <- tickets[which(tickets$year %in% year_choose & tickets$pcid==port),]
  }
  m_by_v <- melt(yr_tickets, id.vars = c("metier","drvid"), measure.vars = "trip_id")
  m_by_v <- unique(m_by_v)
  cast_mv <- dcast(m_by_v, metier~drvid, length) # calculate number of trips
  rownames(cast_mv) <- cast_mv$metier
  cast_mv <- cast_mv[,-1]
  # remove any metiers that aren't used at all
  
  # restrict to metiers responsible for at least 95% of trips 
  if(restrict==TRUE){
    cumulatives <- cumsum(sort(rowSums(cast_mv)/sum(rowSums(cast_mv)),decreasing=T))
    r.f <- names(which(cumulatives<=.95))
    cast_mv <- cast_mv[r.f,]
    if(any(colSums(cast_mv)==0)) cast_mv <- cast_mv[,-which(colSums(cast_mv)==0)]
  }
  
  bc_drvid <- as.matrix(1-vegdist(cast_mv))
  
  g <- graph.adjacency(bc_drvid,weighted = TRUE, mode="undirected", diag = FALSE)
  par(mai=rep(0,4))
  V(g)$size <- rowSums(cast_mv)/sum(rowSums(cast_mv))*100
  
#   strong_edges <- E(g)$weight
#   strong_edges[strong_edges<=0.02] <- 0
  g_s <- g
#   E(g_s)$weight <- strong_edges  
  
  im_gs <- infomap.community(g_s)
  
  paint <- colorRampPalette(brewer.pal( 8, "Dark2"))(length(im_gs))
  V(g_s)$color <- paint[im_gs$membership]
  
  l <- layout.fruchterman.reingold(g_s,niter=500)
  
  plot(g_s, edge.width = E(g_s)$weight*30, layout = l, vertex.size = (V(g_s)$size), vertex.label.color = "black", vertex.label.cex = .5, vertex.label.family="sans", vertex.label.degree=-pi/2, vertex.label.dist=.2, main = port)
  return(g_s)
}

# do participation networks by port
port_popularity <- ddply(tickets.df, .(pcid), summarize, num_Ves = length(unique(drvid)), num_trips = length(unique(trip_id)))
port_popularity <- port_popularity[order(port_popularity$num_trips, decreasing = T),]

# first will look at top ten
par(mai = c(0,0,4,0), omi = c(0,0,1,0))

for(i in 1:20){
  define_participationPlot(2009:2013, port_popularity$pcid[i])
}

# time series of catch for crab and sablefish ----
port = "ORF" # port orford
orf_ftl <- ftl_len[which(ftl_len$pcid == port),]



orf.ts <- ftl_len %>%
  group_by(pcid, metier,tdate) %>%
  summarize(n.trips = length(unique(trip_id))) %>%
  mutate(tdate = as.Date(tdate, format = "%d-%b-%y")) %>%

orf.ts <- orf.ts[order(orf.ts$pcid,orf.ts$tdate),]

with(subset(orf.ts, pcid == "ORF" & metier == "POT_1"), plot(tdate, n.trips, type = 'l', col = "indianred"))
with(subset(orf.ts, pcid == "ORF" & metier == "HKL_1"), points(tdate, n.trips, type = 'l', col = "steelblue"))

# find week, this is too noisy
ftl_len$weeknum <-as.numeric( format(as.Date(ftl_len$tdate, format = "%d-%b-%y")+3, "%U"))

week.ts <- ftl_len %>%
  group_by(pcid, metier, year, weeknum) %>%
  summarize(n.trips = length(unique(trip_id))) 

week.ts <- week.ts[order(week.ts$pcid, week.ts$year, week.ts$weeknum),]
week.ts$weekID = paste(week.ts$weeknum, week.ts$year, sep = "-")
with(subset(week.ts, pcid == "ORF" & metier == "POT_1"), barplot(n.trips, type = 'h', col = "indianred", bor = F))
with(subset(week.ts, pcid == "ORF" & metier == "HKL_1"), barplot(n.trips, type = 'h', col = "steelblue", bor = F,add=T))

# by year?
year.ts <- tickets.df %>%
  group_by(pcid, metier, year) %>%
  summarize(n.trips = length(unique(trip_id))) 
par(mfrow=c(2,1), mai = c(.75,1,0.1,0.1))
with(subset(year.ts, pcid == "ORF" & metier == "POT_1"), plot(year, n.trips, type = 'o', col = "indianred", ylim = c(50, 1200), pch = 19), xaxt='n')
with(subset(year.ts, pcid == "ORF" & metier == "HKL_1"), lines(year, n.trips, type = 'o', col = "steelblue", pch = 19))

with(subset(year.ts, pcid == "BRG" & metier == "POT_1"), plot(year, n.trips, type = 'o', col = "indianred", ylim = c(50, 1200), pch = 19))
with(subset(year.ts, pcid == "BRG" & metier == "HKL_1"), lines(year, n.trips, type = 'o', col = "steelblue", pch = 19))

# looking at specialists

# looking at port orford
crab_sab <- cast_mv[which(rownames(cast_mv) %in% c("POT_1","HKL_1","POT_4")),]
crab_sab <- crab_sab[,-which(colSums(crab_sab)==0)]
sab_only <- crab_sab[,which(crab_sab[2,]==0)]
crab_only <- crab_sab[,which(colSums(crab_sab[c(1,3),])==0)]

both <- crab_sab[,which(crab_sab[2,]>0 & crab_sab[c(1,3),] > 0)]

# plot n.trips for boats that do sablefish only

# plot n.trips for boats that do crab only


