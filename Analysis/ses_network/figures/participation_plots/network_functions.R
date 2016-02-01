# create undirected networks for coastwide and all ports that have > 3 vessels
# and land POT_1 
# undirected network will use using Jaccard distance 
# (interaction strengths between nodes in the network will be defined based on 
# the similarity in vessel composition among pairs of métiers)

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

# use sweet interactive plots
make_plotly <- function(g, save = FALSE, port_id = ports, n.fisheries){
  library(plotly)
  G <- g
  L <- layout_nicely(G)
  vs <- V(G)
  es <- as.data.frame(get.edgelist(G),stringsAsFactors = FALSE)
  Nv <- length(vs)
  Ne <- length(es[1]$V1)
  Xn <- L[,1]
  Yn <- L[,2]
  network <- plot_ly(type = "scatter", x = Xn, y = Yn, mode = "markers", 
                     text = paste(vs$name, vs$size,sep=", n = "), hoverinfo = "text", 
                     marker = list(size = sqrt(V(G)$size)*5, 
                                   color = V(G)$color, opacity = 1))
  if(n.fisheries>1){
    edge_shapes <- list()
    for(i in 1:Ne) {
      v0 <- es[i,]$V1
      v1 <- es[i,]$V2
      
      edge_shape = list(
        type = "line",
        line = list(width = sqrt(E(g)$weight[i])*10),
        x0 = Xn[which(vs$name==v0)],
        y0 = Yn[which(vs$name==v0)],
        x1 = Xn[which(vs$name==v1)],
        y1 = Yn[which(vs$name==v1)],
        opacity = E(G)$weight[i]*.5
      )
      
      edge_shapes[[i]] <- edge_shape
    }
  
    network <- layout(
      network,
      title = port_id,
      shapes = edge_shapes,
      xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
      yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
    )
  }else{
    network <- layout(
      network,
      title = port_id,
      xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
      yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
    )
  }
  if(save){
    save_dir <-  paste0("/Users/efuller/Desktop/CNH/Analysis/ses_network/figures/participation_plots/plots/")
    htmlwidgets::saveWidget(as.widget(network), file= paste0(save_dir,port_id,".html"))
  }
  return(network)
  
}

# resurect some old code - participation plots
participation_plots <- function(type = "jaccard",port_choose, years, cool_plot = FALSE, save = FALSE){
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
  return(list(g=g, network=network))
}

