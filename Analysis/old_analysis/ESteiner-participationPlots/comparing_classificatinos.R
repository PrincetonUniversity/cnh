ftid <- read.csv("/Users/efuller/1/CNH/Analysis/ESteiner-participationPlots/ftiddesig.csv", stringsAsFactors = FALSE)

tickets <- readRDS("/Users/efuller/1/CNH/processedData/catch/1_cleaningData/tickets.RDS")

tickets_id <- unique(tickets$ftid)

any(ftid$FTID %in% tickets_id)

inds <- which(ftid$FTID %in% tickets_id)

# have about 80% of the fish tickets in my data. Ones I don't have are marked
length(inds)/nrow(ftid)

ftid$emma.has <- 0
ftid$emma.has[inds] <- 1

# of those, merging to put in metier
ftid$X <- NULL

library(dplyr)
colnames(ftid) <- tolower(colnames(ftid))
data <- left_join(tickets, ftid) # should have been full join probably.. 
data.lim <- subset(data, emma.has == 1)
all(data.lim$emma.has==1) # should be true
data.lim <- unique(data.lim[,c("drvid","ftid","metier","fisherycode")])

# look at bipartite networks for correspondance. Nodes are each fishery code
library(bipartite)
plotweb(table(data.lim$metier, data.lim$fisherycode))

ftid_name <- read.csv("/Users/efuller/1/CNH/Analysis/ESteiner-participationPlots/fisherycodes.csv",stringsAsFactors = FALSE)

# actually group based on names
ftid_name$X <- NULL
colnames(ftid_name) <- tolower(colnames(ftid_name))
data.lim <- left_join(data.lim, ftid_name)

# give proper names to my metiers
met_names <- data.frame(metier = unique(data.lim$metier), stringsAsFactors = FALSE)
met_names$metier.name <- c("albacore troll", "chinook troll", "squid troll (small)", "nearshore trawl", "DTS trawl", "sablefish pot","pink shrimp","sablefish longline", "whiting", "yellowtail trawl (whiting bycatch?)", "other shrimp (small)","california halibut shrimp trawl","diverse rockfish pot", "diverse skates, rockcrab (small)", "hagfish", "albacore hook-line","dungeness crab pot", "dungeness crab other gear", "octopus pot (small)","california halibut trawl", "vermilion hook-line (small)", "bonito, bigeye, bluefin yellowtail hook-line", "rock crab pot","diverse rockfish hook-line", "lingcod hook-line", "baitshrimp trawl", "sea cucumbers", "white seabass (small)", "DTS hook and line", "sea cucumbers")

data.lim <- left_join(data.lim, met_names)
# look at correspondance ----

pdf("/Users/efuller/1/CNH/Analysis/ESteiner-participationPlots/comparison_fisheries.pdf", width = 20, height = 10)
plotweb(table(data.lim$metier.name, data.lim$fishery))
dev.off()

# now use Erin's ftids to make participation network ----
library(igraph); library(reshape2); library(vegan); library(RColorBrewer)

  yr_tickets <- data.lim
  m_by_v <- melt(yr_tickets, id.vars = c("fishery_wrap","drvid"), measure.vars = "ftid")
  m_by_v <- unique(m_by_v)
  cast_mv <- dcast(m_by_v, fishery_wrap~drvid, length) # calculate number of trips
  rownames(cast_mv) <- cast_mv$fishery_wrap
  cast_mv <- cast_mv[,-1]
  # remove any metiers that aren't used at all

  
  bc_drvid <- as.matrix(1-vegdist(cast_mv))
  
  g <- graph.adjacency(bc_drvid,weighted = TRUE, mode="undirected", diag = FALSE)
  V(g)$size <- rowSums(cast_mv)/sum(rowSums(cast_mv))*100
  
  g_s <- g
  
  l <- layout.fruchterman.reingold(g_s,niter=500)
  
  V(g_s)$name  <- gsub("\\n"," \n", V(g_s)$name, fixed = TRUE)
  
  pdf("/Users/efuller/1/CNH/Analysis/ESteiner-participationPlots/particp_plot.pdf",width = 20, height = 20)
  plot(g_s, edge.width = E(g_s)$weight*30, layout = l, vertex.size = (V(g_s)$size), vertex.label.color = "black", vertex.label.cex = c(1,1,1,1,1,1,1,1), vertex.label.family="sans", vertex.label.degree=c(0,-pi,pi/2,-pi/2,-pi/12,pi/5,-pi/1.1,0), vertex.label.dist=c(0,.75,.5,.35,.9,.5,.6,.5), vertex.frame.color = NA)
  dev.off()
 
  # use mine for participation plot
  library(igraph); library(reshape2); library(vegan); library(RColorBrewer)
  
  yr_tickets <- data.lim
  m_by_v <- melt(yr_tickets, id.vars = c("metier.name","drvid"), measure.vars = "ftid")
  m_by_v <- unique(m_by_v)
  cast_mv <- dcast(m_by_v, metier.name~drvid, length) # calculate number of trips
  rownames(cast_mv) <- cast_mv$metier.name
  cast_mv <- cast_mv[,-1]
  # remove any metiers that aren't used at all
  
  
  bc_drvid <- as.matrix(1-vegdist(cast_mv))
  
  g <- graph.adjacency(bc_drvid,weighted = TRUE, mode="undirected", diag = FALSE)
  V(g)$size <- rowSums(cast_mv)/sum(rowSums(cast_mv))*100
  
  g_s <- g
  
  l <- layout.fruchterman.reingold(g_s,niter=500)
  
  pdf("/Users/efuller/1/CNH/Analysis/ESteiner-participationPlots/particp_plot_mets.pdf",width = 20, height = 20)
  plot(g_s, edge.width = E(g_s)$weight*30, layout = l, vertex.size = (V(g_s)$size), vertex.label.color = "black", vertex.label.cex = c(1,1,1,1,1,1,1,1), vertex.label.family="sans", vertex.label.degree=c(0,-pi,pi/2,-pi/2,-pi/12,pi/5,-pi/1.1,0), vertex.label.dist=c(0,.75,.5,.35,.9,.5,.6,.5), vertex.frame.color = NA)
  dev.off()
  
  # look at ones that don't match
  
  # shrimp
    subset(data.lim, fishery == "Shrimp" & metier != "TWS_1")

