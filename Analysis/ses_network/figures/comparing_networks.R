library(igraph)
trip_mat <- m1.trips.ignore
trip_mat[is.na(trip_mat)] <- 0
trip_mat <- abs(trip_mat)
paint <- matrix("indianred",nrow = nrow(trip_mat), ncol = ncol(trip_mat))
paint[m1.trips.ignore<0] <- "grey"
paint[is.na(m1.trips.ignore)] <- NA

paint_vec <- as.vector(paint)
paint_vec <- paint_vec[-which(is.na(paint_vec))]
g <- graph_from_adjacency_matrix(trip_mat,mode = "undirected",weighted = TRUE, add.rownames=TRUE)
E(g)$paint <- paint_vec
pdf(file = "Analysis/ses_network/figures/correlation_trips_coastwide.pdf",width = 8, height = 8)
par(bg="transparent",oma = rep(0,4),mai=rep(0,4))
plot(g, edge.width = E(g)$weight*10, edge.color = E(g)$paint, 
     vertex.frame.color = "grey",vertex.color='white', 
     vertex.label.family = "sans", vertex.label = tolower(V(g)$name), 
     vertex.label.color='black', vertex.label.cex = 1, edge.curved = TRUE, 
     layout = layout.grid)
dev.off()


# now my old stuff  
# get common names and color
colors_common <- function(g){
  # get colors ----
  descrp <- read.csv("processedData/catch/3_exploreBuildwebs/ref_tables/metier_descrp.csv", stringsAsFactors = FALSE)
  
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
  
  # match to graph ----
  contain_metier <- which(descrp$Metier %in% tolower(V(g)$name))
  
  cn <- data.frame(cn = paste(descrp$Major_species[contain_metier], descrp$Major_gear[contain_metier],sep="\n"), metier = descrp$Metier[contain_metier], paint = descrp$paint[contain_metier], stringsAsFactors = FALSE)
  cn <- cn[match(tolower(V(g)$name), cn$metier),] # reorder to match
  
  V(g)$name <- cn$cn
  
  V(g)$color <- cn$paint
  
  return(g)
}

# resurect some old code - participation plots
participation_plots <- function(type = "jaccard",port_choose, years, cool_plot = FALSE, save = FALSE, tickets){
  library(dplyr); library(tidyr)
  if(type=="jaccard"){
    if(port_choose == "coastwide"){
      ports <- unique(tickets$pcid)
    }else{
      ports <- port_choose
    }
    trips <- tickets %>%
      dplyr::filter(pcid %in% ports, year %in% years) %>%
      dplyr::select(drvid, metier.2010) %>%
      distinct()
    
    # reshape
    library(reshape2)
    cast_trips <- dcast(trips, metier.2010 ~ drvid, fun.aggregate = length)
    rownames(cast_trips) <- cast_trips$metier.2010
    cast_trips$metier.2010 <- NULL
    
    # calculate similarity
    library(vegan)
    
    dist_mat <- vegdist(cast_trips, method = "jaccard")
    
    # make similarity matrix
    sim_mat <- as.matrix(1-dist_mat)
    
    # make network
    library(igraph)
    g <- graph_from_adjacency_matrix(sim_mat, mode = "undirected", weighted = TRUE, diag=NULL)
    
    # drop nodes that have fewer than 3 vessels
    # first assign fleet size to each node
    fleet_size <- rowSums(cast_trips)
    V(g)$size <- fleet_size
    g=delete.vertices(g,which(V(g)$size<3))
    # find common names
    
    g <- colors_common(g)
    
    if(cool_plot){
      network <- make_plotly(g, save, port_id = port_choose, n.fisheries = length(E(g)))
    }else{
      plot(g, edge.width = E(g)$weight*10, layout=layout_nicely, vertex.size = sqrt(V(g)$size))
    }
  }
  return(g)
}

tt <- subset(tickets, metier.2010 %in% V(g)$name)
tickets <- tt
g2 <- participation_plots(port_choose = "coastwide", years = 2009:2013, tickets = tt)

pdf(file = "Analysis/ses_network/figures/jacquard_coastwide.pdf",width = 8, height = 8)
par(bg="transparent",oma = rep(0,4),mai=rep(0,4))
plot(g2, edge.width = E(g)$weight*10, 
     vertex.label.family = "sans", vertex.label = tolower(V(g)$name), 
     vertex.label.color='black', vertex.label.cex = 1, edge.curved = TRUE, 
     layout = layout.grid, vertex.size = 20)
dev.off()

# but have to drop the connections in g2 that are not in g, since we only care about 
# interactions among fisheries with at least 10 in common

g_mat <- as.matrix(as_adj(g))
g2_mat <- as.matrix(as_adj(g2, attr = "weight"))
colnames(g2_mat) <- colnames(g_mat)
rownames(g2_mat) <- rownames(g_mat)
g2_mat[which(g_mat==0)] <- 0

g3 <- graph_from_adjacency_matrix(g2_mat, mode = "undirected", weighted = TRUE)
V(g3)$color <- V(g2)$color
pdf(file = "Analysis/ses_network/figures/coastwide_jacquard__subset.pdf",
    width = 8, height = 8)
par(bg="transparent",oma = rep(0,4),mai=rep(0,4))
plot(g3, edge.width = E(g)$weight*10, 
     vertex.label.family = "sans", vertex.label = tolower(V(g)$name), 
     vertex.label.color='black', vertex.label.cex = 1, edge.curved = TRUE, 
     layout = layout.grid, vertex.size = 20)
dev.off()
