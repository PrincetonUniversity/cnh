define_participationPlot <- function(year_choose, port=NA, restrict=FALSE, tickets = tickets.df){
  library(dplyr); library(reshape2); library(igraph); library(tidyr)
  descrp <- read.csv("/Users/efuller/Desktop/CNH/processedData/catch/3_exploreBuildwebs/ref_tables/metier_descrp.csv", stringsAsFactors = FALSE)
  
  descrp$paint <- NA
  library(RColorBrewer)
  descrp$Metier <- tolower(descrp$Metier)
  
  # find number of pot gears
  n.gear = length(grep("pot",tolower(descrp$Metier)))
  paint = rev(colorRampPalette(brewer.pal(9, "Reds"))(n.gear))
  descrp$paint[grep("pot",tolower(descrp$Metier))] <- paint
  
  n.gear = length(grep("tws",descrp$Metier))
  paint = colorRampPalette(c("#FA9FB5","#E7298A"))(n.gear)
  descrp$paint[grep("tws",descrp$Metier)] <- paint
  
  n.gear = length(grep("tls",descrp$Metier))
  paint = colorRampPalette(c("#FFEDA0","#FED976"))(n.gear)
  descrp$paint[grep("tls",descrp$Metier)] <- paint
  
  n.gear = length(grep("msc",descrp$Metier))
  paint = colorRampPalette(brewer.pal(9, "Purples"))(n.gear)
  descrp$paint[grep("msc",descrp$Metier)] <- paint
  
  n.gear = length(grep("hkl",descrp$Metier))
  paint = rev(colorRampPalette(brewer.pal(9, "Greens"))(n.gear))
  descrp$paint[grep("hkl",descrp$Metier)] <- paint
  
  n.gear = length(grep("twl",descrp$Metier))
  paint = colorRampPalette(brewer.pal(9, "Oranges"))(n.gear)
  descrp$paint[grep("twl",descrp$Metier)] <- paint
  
  n.gear = length(grep("net",descrp$Metier))
  paint = colorRampPalette(brewer.pal(9, "Blues"))(n.gear)
  descrp$paint[grep("net",descrp$Metier)] <- paint
  if(any(is.na(port))){
    yr_tickets <- tickets[which(tickets$year %in% year_choose),]}
  if(any(!is.na(port))){
    yr_tickets <- tickets[which(tickets$year %in% year_choose & tickets$pcid %in% port),]
  }
  m_by_v <- melt(yr_tickets, id.vars = c("metier.2010","drvid"), measure.vars = "trip_id")
  m_by_v <- unique(m_by_v)
  if(nrow(m_by_v)<2) return(NA)
  cast_mv <- dcast(m_by_v, metier.2010~drvid, length) # calculate number of trips
  rownames(cast_mv) <- cast_mv$metier.2010
  cast_mv <- cast_mv[,-1, drop=FALSE]
  
  # want to calculate total number of vessels that do each fishery, and then 
  # make connections be what proportion does both
  # total number is row sums. And percentage that does both, relative to base 
  # nodes 
  
  # so symmetric matrix, of metiers, entries number of boats do both. 
  # Divide rows by row/sums to get directon. then each entry is the proprotion 
  # of boats that do both, relative to row-metier.
  
  met_mat <- matrix(data = 0, nrow = nrow(cast_mv), ncol = nrow(cast_mv))
  colnames(met_mat) <- rownames(cast_mv)
  rownames(met_mat) <- rownames(cast_mv)
  for(i in 1:nrow(met_mat)){
    for(j in 1:ncol(met_mat)){
      met_mat[i,j] <- sum(apply(cast_mv, MARGIN = 2, function(x) ifelse(x[i] & x[j]>0, 1, 0)))
    }
  }
  
  # make directed network, divide by diagonals
  met_mat <- met_mat/diag(met_mat)
  
  # put diagonals back in - number of vessels
  diag(met_mat) <- apply(X = cast_mv, MARGIN = 1, function(x) length(which(x>0)))
  
  # make network
  g <- graph.adjacency(met_mat,weighted = TRUE, mode="directed", diag = FALSE)
  V(g)$size <- diag(met_mat)
  
  contain_metier <- which(descrp$Metier %in% tolower(V(g)$name))
  
  cn <- data.frame(cn = paste(descrp$Major_species[contain_metier], descrp$Major_gear[contain_metier],sep="\n"), metier = descrp$Metier[contain_metier], paint = descrp$paint[contain_metier], stringsAsFactors = FALSE)
  cn <- cn[match(tolower(V(g)$name), cn$metier),] # reorder to match
  
  g_s <- g
  V(g_s)$name <- cn$cn
  
  V(g_s)$color <- cn$paint
  
  l <- layout.fruchterman.reingold(g_s,niter=500)
  
  #plot(g_s, edge.width = E(g_s)$weight*3, layout = l, vertex.size = log(V(g_s)$size)*4, vertex.label.color = "grey30", vertex.label.cex = .75, vertex.label.family="sans", vertex.frame.color = NA, edge.curved=TRUE, edge.arrow.size = .25, main = port, edge.color = "black")
  
  return(g_s)
}