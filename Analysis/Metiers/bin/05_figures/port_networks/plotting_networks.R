# plot all ports networks (if possible)
source("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/02_define_participationPlot.R")

# aux ploting - drop nodes with < 3 vessels
gp <- function(g){
  plot(g, edge.width = E(g)$weight*3, layout = layout.davidson.harel, vertex.size = log(V(g)$size)*4, vertex.label.color = "black", vertex.label.cex = .75, vertex.label.family="sans", vertex.frame.color = NA, edge.curved=TRUE, edge.arrow.size = .25, edge.color = "grey70")
}

tickets <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/vessel_landings_data.RDS")
ports <- unique(tickets$pcid)
yrs = 2009:2010

for(i in 1:length(ports)){
  pdf(paste0("Analysis/Metiers/bin/05_figures/port_networks/",ports[i],".pdf"),width = 5, height = 5)
  par(bg="transparent")
  g <- define_participationPlot(year_choose = yrs, port = ports[i], tickets = tickets)
  # if no nodes, don't plot
  if(!is.igraph(g)) next
  # drop nodes with fewer than 3 boats
  g=delete.vertices(g,which(V(g)$size<3))
  # if no nodes, don't plot
  if(length(V(g))==0) next
  g <- gp(g)
  dev.off()
}