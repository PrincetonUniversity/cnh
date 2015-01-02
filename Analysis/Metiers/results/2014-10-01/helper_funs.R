# functions for profit stats
library(reshape2); library(vegan); library(igraph); library(plyr); library(RColorBrewer); library(scales)
# 

find_strategies <- function(year, df, message = "NO"){
  
  if(message == "YES") cat("format trips correctly\n")
  port_trips <- df[df[["year"]] == year , ]
  port_trips <- subset(port_trips, !(veid %in% c("0","UNKNOWN","********")))
  melt_trips <- melt(port_trips, id.vars = c("veid","ftid","spid","tdate","grid","metier"), measure.vars = "landed_wt")
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