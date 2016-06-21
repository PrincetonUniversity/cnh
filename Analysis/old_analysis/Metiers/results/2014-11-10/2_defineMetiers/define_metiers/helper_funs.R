#
# portfolios by year
#----

find_metiers <- function(tickets, year, gear_group, message = "NO"){
  if(message == "YES") cat("format trips correctly\n")
  port_trips <- tickets[tickets[["year"]] == year & tickets[["grgroup"]] == gear_group, ]
  port_trips <- subset(port_trips, !(veid %in% c("0","UNKNOWN","********")))
  melt_trips <- melt(port_trips, id.vars = c("veid","ftid","modified","tdate","grid"), measure.vars = "landed_wt")
  cast_trips <- dcast(melt_trips, ftid ~ modified, fun.aggregate = sum)
  rownames(cast_trips) <- cast_trips$ftid
  cast_trips$ftid <- NULL
  
  if(message == "YES") cat("calculating Jaccard\n")
  bc <- vegdist(cast_trips, method = "jaccard", binary=TRUE)
  bc_sim <- abs(bc-1)
  bc_mat <- as.matrix(bc_sim)
  
  if(message == "YES") cat("build network - find communities\n")
  bc_net <- graph.adjacency(bc_mat, mode="undirected", weighted = TRUE)
  ic <- infomap.community(bc_net, e.weights = E(bc_net)$weight)
    
  members <- data.frame(ftid = names(membership(ic)), 
                        metier = membership(ic), row.names=NULL)
  
  # remove strategies for which there are fewer than 5 vessels
#  too_small <- which(table(members$metier)<5)
#  members <- subset(members, !(metier %in% too_small) )
  members$metier <- paste(gear_group, members$metier, sep = "_")
  return(members)
}

find_metier_strategy <- function(trips, message = "NO"){
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

