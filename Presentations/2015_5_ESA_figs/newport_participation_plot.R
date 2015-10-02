# pub quality figure for James - Newport participation network

filtered_ftl <- readRDS("/Users/efuller/1/CNH/processedData/catch/5_add_length/ftl_len.RDS")

library(igraph); library(reshape2); library(vegan); library(RColorBrewer)

# calculate the bray curtis dissimilarity to look at vessel composition across fisheries. A fishery is very similar if the same vessels participate in the same way. 
# need a metier by drvid matrix to run vegdist on

# common names and colors ----
descrp <- read.csv("/Users/efuller/1/CNH/processedData/catch/3_exploreBuildwebs/ref_tables/metier_descrp_gn.csv", stringsAsFactors = FALSE)

library(RColorBrewer)
# each major gear is a color, and then color ramp within gear between light and dark color
# pot = reds
# tws = pinks
# tls = yellows
# msc = purples
# hkl = greens
# twl = oranges
# net = blues

descrp$paint <- NA

# find number of pot gears
n.gear = length(grep("POT",descrp$Metier))
paint = rev(colorRampPalette(brewer.pal(9, "Reds"))(n.gear))
descrp$paint[grep("POT",descrp$Metier)] <- paint

n.gear = length(grep("TWS",descrp$Metier))
paint = colorRampPalette(c("#FA9FB5","#E7298A"))(n.gear)
descrp$paint[grep("TWS",descrp$Metier)] <- paint

n.gear = length(grep("TLS",descrp$Metier))
paint = colorRampPalette(c("#FFEDA0","#FED976"))(n.gear)
descrp$paint[grep("TLS",descrp$Metier)] <- paint

n.gear = length(grep("MSC",descrp$Metier))
paint = colorRampPalette(brewer.pal(9, "Purples"))(n.gear)
descrp$paint[grep("MSC",descrp$Metier)] <- paint

n.gear = length(grep("HKL",descrp$Metier))
paint = rev(colorRampPalette(brewer.pal(9, "Greens"))(n.gear))
descrp$paint[grep("HKL",descrp$Metier)] <- paint

n.gear = length(grep("TWL",descrp$Metier))
paint = colorRampPalette(brewer.pal(9, "Oranges"))(n.gear)
descrp$paint[grep("TWL",descrp$Metier)] <- paint

n.gear = length(grep("NET",descrp$Metier))
paint = colorRampPalette(brewer.pal(9, "Blues"))(n.gear)
descrp$paint[grep("NET",descrp$Metier)] <- paint

# hm, the color ramps dont' look great. maybe should just do a single color for each gear group

# define participation plots ----
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
  V(g)$size <- rowSums(cast_mv)/sum(rowSums(cast_mv))*100
  
  contain_metier <- which(descrp$Metier %in% V(g)$name)
  
  cn <- data.frame(cn = paste(descrp$Major_species[contain_metier], descrp$Major_gear[contain_metier],sep="\n"), metier = descrp$Metier[contain_metier], paint = descrp$paint[contain_metier], stringsAsFactors = FALSE)
  cn <- cn[match(V(g)$name, cn$metier),] # reorder to match
  
  #   strong_edges <- E(g)$weight
  #   strong_edges[strong_edges<=0.02] <- 0
  g_s <- g
  #   E(g_s)$weight <- strong_edges  
  V(g_s)$name <- cn$cn
  
  V(g_s)$color <- cn$paint
  
  l <- layout.fruchterman.reingold(g_s,niter=500)
  
  pdf("/Users/efuller/1/CNH/Presentations/2015_5_ESA_figs/newport_participation_plot.pdf",width = 10, height = 10,bg = "transparent")
  plot(g_s, edge.width = E(g_s)$weight*30, layout = l, vertex.size = (V(g_s)$size), vertex.label.color = "black", vertex.label.cex = c(1,1,1,1,1,1,1,1), vertex.label.family="sans", vertex.label.degree=c(0,-pi,pi/2,-pi/2,-pi/12,pi/5,-pi/1.1,0), vertex.label.dist=c(0,.75,.5,.35,.9,.5,.6,.5), vertex.frame.color = NA)
  dev.off()
  
  return(g_s)
}


newport <- define_participationPlot(2009:2013, "NEW", tickets = filtered_ftl)

paint_df <- data.frame(as.character(V(newport)$name),stringsAsFactors = FALSE)
