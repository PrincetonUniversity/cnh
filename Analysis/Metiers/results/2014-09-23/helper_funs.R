# find ecological targets

library(reshape2); library(vegan); library(plyr); library(dplyr); library(igraph)


# load data
#ftl <- read.csv("/Users/efuller/1/CNH/Data/Catch/FTL_2009-2013_w-o-c_samhouri.csv", stringsAsFactors = F)

find_target <- function(gear_groups, years, message = "NO"){
  library(igraph); library(vegan); library(reshape2)
  
  if(message == "YES") cat("subset and set up trip table\n")
  # subset and set up trip table
  port_trips <- ftl[ ftl[["grgroup"]] == gear_groups & ftl[["year"]] == years , ]
  melt_trips <- melt(port_trips, id.vars = c("veid","ftid","spid","tdate","grid"), measure.vars = "landed_wt")
  cast_trips <- dcast(melt_trips, ftid ~ spid, fun.aggregate = sum)
  rownames(cast_trips) <- cast_trips$ftid
  cast_trips$ftid <- NULL
  
  if(message == "YES") cat("calculate bray-curtis\n")
  # calculating bray-curtis
  bc <- vegdist(cast_trips, method = "bray")
  bc_sim <- abs(bc-1)
  bc_mat <- as.matrix(bc_sim)
  
  if(message == "YES") cat("build network, find communities\n")
  # build network - find communities
  bc_net <- graph.adjacency(bc_mat, mode="undirected", weighted = TRUE)
  ic <- infomap.community(bc_net, e.weights = E(bc_net)$weight)

  cg <- contract.vertices(bc_net, membership(ic))
  cg2 <- simplify(cg, remove.loops=TRUE, edge.attr.comb = median) # take median similarity
  V(cg2)$size <- table(membership(ic))
  
  return(list(cg2=cg2, port_trips=port_trips, bc_mat=bc_mat, ic=ic, cast_trips=cast_trips))
}

vertex.type <- function(i, cast_trips, cg2, nout = 3){
  catch <- cast_trips[sample(V(cg2)$name[[i]], nout), ]
  if(is.null(dim(catch[,which(colSums(catch)>0)]))){
    return(which.max(colSums(catch)))
  }else{
    return(catch[,which(colSums(catch)>0)])
  }
}

plot_targets <- function(cg2){
  library(scales)
  
  l <- layout.fruchterman.reingold(cg2, niter=1000, area=vcount(cg2)^3, repulserad=vcount(cg2)^3)
  par(mai = rep(0,4), bg="white")
  plot(cg2, vertex.size = log(V(cg2)$size)*2.5, 
       layout = l, 
       edge.width = E(cg2)$weight*20, 
       edge.label = "", 
       vertex.label.font=1, 
       vertex.label.color="black", 
       vertex.frame.color="steelblue", 
       vertex.color = "steelblue", 
       edge.color=alpha("grey",1-E(cg2)$weight), 
       vertex.label.family = "sans", 
       vertex.label.cex = .75)
}

bipartite_network <- function(port_trips, bc_mat, ic, cg2, scale_edge=1000){
  # assign target cluster to each trip
  trips <- port_trips
  trips$target <- rep(NA, nrow(trips))
  for(i in 1:length(communities(ic))){
    trips$target[which(trips$ftid %in% rownames(bc_mat[communities(ic)[[i]],]))] <- V(cg2)$name[[i]]
  }

  # now make bipartite network for gear
  trip_types <- table(trips$target, trips$grid)
  trip_gear <- melt(trip_types)
  trip_gear <- subset(trip_gear, value > 10)
  colnames(trip_gear) <- c("from","to","weight")

  fisheries <- graph.data.frame(trip_gear, directed = FALSE)
  E(fisheries)$weight <- trip_gear$weight
  num_targets <- length(unique(trip_gear$from))
  num_gears <- length(unique(trip_gear$to))
  V(fisheries)$type <- c(rep(TRUE,num_targets), rep(FALSE, num_gears))
  V(fisheries)$color <- c(rep("steelblue", num_targets), rep("brown", num_gears))
  par(mai=rep(0,4), bg="white")
  plot(fisheries, 
       edge.width = E(fisheries)$weight/scale_edge, 
       layout=layout.bipartite, 
       ylim=c(-1,1),
       vertex.label.cex=.5, 
       vertex.label.color="white", 
       vertex.label.family = "sans", 
       vertex.frame.color=V(fisheries)$color)
  
  return(list(trips = trips, trip_gear = trip_gear))
}

participation_network <- function(trips, trip_gear){
  # now make participation network. 
  trips$fishery <- paste(trips$target, trips$grid, sep="_")
  # restrict to fisheries used
  used_fisherys <- unique(paste(trip_gear$from, trip_gear$to, sep="_"))
  used_trips <- subset(trips, fishery %in% used_fisherys)
  by_fishery <- table(used_trips$veid, used_trips$fishery)

  fishery_mat <- matrix(ncol=length(used_fisherys), nrow=length(used_fisherys))
  colnames(fishery_mat) <- colnames(by_fishery)
  rownames(fishery_mat) <- colnames(by_fishery)

    for(i in 1:ncol(by_fishery)){ # for each fishery except the last one
      fish_i <- colnames(by_fishery)[i]
      for(j in i:ncol(by_fishery)){
        fish_j <- colnames(by_fishery)[j]
        num_ves = length(which(which(by_fishery[,i]>0) %in% which(by_fishery[,j] > 0)))
        
        mat_row <- which(rownames(fishery_mat)==fish_j)
        mat_col <- which(colnames(fishery_mat)==fish_i)
        
        fishery_mat[mat_row, mat_col] <- num_ves
      }
    }

  g<-graph.adjacency(fishery_mat,weighted=T, diag=F, mode = "lower")
  l <- layout.fruchterman.reingold(g, niter=500, area=vcount(g)^2, repulserad=vcount(g)^2)

  # looking for communities of vessels 
  
  ic_gear <- infomap.community(g)
  library(RColorBrewer)
  paint <- brewer.pal(6,"Dark2")
  V(g)$membership <- ic_gear$membership
  V(g)[membership==1]$color <- paint[1]
  V(g)[membership==1]$frame.color <- paint[1]
  V(g)[membership==2]$color <-paint[2]
  V(g)[membership==2]$frame.color <-paint[2]
  V(g)[membership==3]$color <- paint[3]
  V(g)[membership==3]$frame.color <-paint[3]
  V(g)[membership==4]$color <-paint[4]
  V(g)[membership==4]$frame.color <-paint[4]
  V(g)[membership==5]$color <- paint[5]
  V(g)[membership==5]$frame.color <- paint[5]
  V(g)[membership==6]$color <- paint[6]
  V(g)[membership==6]$frame.color <- paint[6]
  par(mai=rep(0,4))
  
  V(g)$size <- colSums(by_fishery)
  
  plot(g, vertex.color=V(g)$color, edge.width = E(g)$weight, vertex.size=log(V(g)$size)*3, vertex.frame.color=V(g)$frame.color, vertex.label.color="black",layout=l, vertex.label.family = "sans", vertex.label.cex = .75)
}

# portofolio plots by year
# ----

find_strategies <- function(year, message = "NO"){
  
  if(message == "YES") cat("format trips correctly\n")
  port_trips <- ftl[ftl[["year"]] == year , ]
  port_trips <- subset(port_trips, !(veid %in% c("0","UNKNOWN","********")))
  melt_trips <- melt(port_trips, id.vars = c("veid","ftid","spid","tdate","grid"), measure.vars = "landed_wt")
  cast_trips <- dcast(melt_trips, veid ~ spid, fun.aggregate = sum)
  rownames(cast_trips) <- cast_trips$veid
  cast_trips$veid <- NULL
  
  if(message == "YES") cat("calculating bray-curtis\n")
  bc <- vegdist(cast_trips, method = "bray")
  bc_sim <- abs(bc-1)
  bc_mat <- as.matrix(bc_sim)
  
  if(message == "YES") cat("build network - find communities\n")
  bc_net <- graph.adjacency(bc_mat, mode="undirected", weighted = TRUE)
  ic <- infomap.community(bc_net, e.weights = E(bc_net)$weight)
  
  cg <- contract.vertices(bc_net, membership(ic))
  cg2 <- simplify(cg, remove.loops=TRUE, edge.attr.comb = median) # take median similarity
  V(cg2)$size <- table(membership(ic))
  
  cg2 <- delete.vertices(cg2, which(V(cg2)$size < 10))
  
  #V(cg2)$name <- c("acds", "SAL_HAL","acds_ling_ytr","as_sword_rock","lobs_etc","sable_top","pelagics","urchin_etc","rockfish","as_rockfish","generalist_shrimp","cockles","razor_clam","whiting_plus","ghost_shrimp_plus","herring", "hagfish_plus", "cukes_plus", "sword_plus", "bait_shrimp_plus", "surfperch_plus","shad","spotted_prawn_plus", "mackeral_plus", "pelagics_udab_plus","sharks","misc","omsks")
  
  l <- layout.fruchterman.reingold(cg2, niter=1000, area=vcount(cg2)^3, repulserad=vcount(cg2)^3)
  
  return(list(ic=ic, port_trips=port_trips, cast_trips = cast_trips))
}

calculate_strategy_stats <- function(ic, port_trips, cast_trips){
  
  # connect strategy to trips
  members <- data.frame(veid = names(membership(ic)), strategy = membership(ic), row.names=NULL)
  trips <- merge(port_trips, members, by = "veid", all.x = T, all.y = F)
  
  # remove strategies for which there are fewer than 5 vessels
  too_small <- which(table(members$strategy)<5)
  trips <- subset(trips, !(strategy %in% too_small) )
  # calculate yearly revenue
  yr_trips <- ddply(trips, .(veid), summarize, revenue = sum(ppp*landed_wt), strategy = unique(strategy))
  
  # calculate shannon weiner for coloring plot 
  sw <- data.frame(veid = names(diversity(cast_trips)), diversity = diversity(cast_trips), row.names=NULL)
  yr_trips <- merge(yr_trips, sw, by = "veid")
  
  # calculate states on stratagies
  strategy <- ddply(yr_trips, .(strategy), summarize, 
                    mean_rev=mean(revenue), sd_rev = sd(revenue), 
                    mean_div = mean(diversity))
  
  return(list(strategy = strategy, yr_trips = yr_trips))
}

plot_strategies <- function(strategy, title, yr_trips){
  
  # calculate paint colors
  div_max <- round(max(strategy$mean_div+.1)*10)  # need whole numbers for colors
  paint <- colorRampPalette(brewer.pal(11, "Spectral"))(div_max)
  
  plot( log(strategy$sd_rev), 
        log(strategy$mean_rev),
        col=alpha(rev(paint)[round(strategy$mean_div*10)],.9),
        pch=19, cex = log(table(yr_trips$strategy)), bty = "n", 
        xlab = "log(sd yrly revenue)", ylab = "log(mean yrly revenue)",
        main = title, xlim = c(4,15), ylim = c(4,15), asp = 1)
  
#   text( log(strategy$sd_rev), 
#         log(strategy$mean_rev),
#         unlist(V(cg2)$name)[strategy$strategy], 
#         col="black", 
#         cex = log(table(yr_trips$strategy))/6)
  
  text( log(strategy$sd_rev), 
        log(strategy$mean_rev),
        strategy$strategy, 
        col="black", 
        cex = log(table(yr_trips$strategy))/9)
}

get_slope <- function(strategy){
  lm_2012 <- lm(log(mean_rev) ~ log(sd_rev), strategy)
  return(lm_2012$coefficients[2])
}

ordered_boxplot <- function(yr_trips, log="yes"){
  bymedian <- with(yr_trips, reorder(strategy, -revenue, median))
  
  if(log=="yes"){
    boxplot(revenue ~ bymedian, 
            data = yr_trips, 
            log = "y", ylab = "log(revenue)", 
            col="steelblue", xlab = "strategy")
  } else{
    boxplot(revenue ~ bymedian, data = yr_trips)
  }
}

# metier portfolios by year
#----

find_metiers <- function(tickets, year, gear_group, message = "NO"){
  if(message == "YES") cat("format trips correctly\n")
  port_trips <- tickets[tickets[["year"]] == year & tickets[["grgroup"]] == gear_group, ]
  port_trips <- subset(port_trips, !(veid %in% c("0","UNKNOWN","********")))
  melt_trips <- melt(port_trips, id.vars = c("veid","ftid","spid","tdate","grid"), measure.vars = "landed_wt")
  cast_trips <- dcast(melt_trips, ftid ~ spid, fun.aggregate = sum)
  rownames(cast_trips) <- cast_trips$ftid
  cast_trips$ftid <- NULL
  
  if(message == "YES") cat("calculating bray-curtis\n")
  bc <- vegdist(cast_trips, method = "bray")
  bc_sim <- abs(bc-1)
  bc_mat <- as.matrix(bc_sim)
  
  if(message == "YES") cat("build network - find communities\n")
  bc_net <- graph.adjacency(bc_mat, mode="undirected", weighted = TRUE)
  ic <- infomap.community(bc_net, e.weights = E(bc_net)$weight)
    
  members <- data.frame(ftid = names(membership(ic)), 
                        metier = membership(ic), row.names=NULL)
  
  # remove strategies for which there are fewer than 5 vessels
  too_small <- which(table(members$metier)<5)
  members <- subset(members, !(metier %in% too_small) )
  members$metier <- paste(gear_group, members$metier, sep = "_")
  return(members)
}

find_metier_strategy <- function(trips, message = "NO"){
  library(reshape2); library(vegan); library(igraph)
  if(message == "YES") cat("format trips correctly\n")
  melt_trips <- melt(trips, id.vars = c("veid","ftid","metier","tdate","grid"), 
                     measure.vars = "landed_wt")
  cast_trips <- dcast(melt_trips, veid ~ metier, fun.aggregate = length)
  rownames(cast_trips) <- cast_trips$veid
  cast_trips$veid <- NULL
  
  if(message == "YES") cat("calculating bray-curtis\n")
  bc <- vegdist(cast_trips, method = "bray")
  bc_sim <- abs(bc-1) 
  bc_mat <- as.matrix(bc_sim)
  
  if(message == "YES") cat("build network - find communities\n")
  bc_net <- graph.adjacency(bc_mat, mode="undirected", weighted = TRUE)
  ic <- infomap.community(bc_net, e.weights = E(bc_net)$weight)
  
  cg <- contract.vertices(bc_net, membership(ic))
  cg2 <- simplify(cg, remove.loops=TRUE, edge.attr.comb = median) # take median similarity
  
  members <- data.frame(ftid = names(membership(ic)), strategy = membership(ic), row.names=NULL)
  trips <- merge(trips, members, by = "veid", all.x = T, all.y = F)
  
  # remove strategies for which there are fewer than 5 vessels
  too_small <- which(table(members$strategy)<5)
  trips <- subset(trips, !(strategy %in% too_small) )
  
  return(trips)
}

calc_metier_strategy_stats <- function(){
  
}