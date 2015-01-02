# generate link lists for input gear and year
args <- commandArgs(trailingOnly = TRUE)

library(dplyr,warn.conflicts = FALSE,logical.return = FALSE)
library(igraph,warn.conflicts = FALSE,logical.return = FALSE)
library(vegan,warn.conflicts = FALSE, logical.return = FALSE)
library(reshape2,warn.conflicts = FALSE, logical.return = FALSE)

ftl <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-19/define_metiers/filtered_ftl.RDS")

just_gear <- select(ftl, ftid, grid)
just_gear <- unique(just_gear)
gear_types <- table(just_gear$ftid, just_gear$grid)
total_gear <- rowSums(gear_types)
extra_gear_trips <- names(total_gear)[which(total_gear > 1)]

tickets <- subset(ftl, !(ftid %in% extra_gear_trips))

make_link_list <- function(tickets, year, gear_group, message = "NO"){
  
  if(message == "YES") cat("format trips correctly\n")
  port_trips <- tickets[tickets[["year"]] == year & 
                          tickets[["grgroup"]] == gear_group, ]
  melt_trips <- melt(port_trips, 
                     id.vars = c("veid","ftid","modified","tdate","grid"), 
                     measure.vars = "landed_wt")
  cast_trips <- dcast(melt_trips, 
                      ftid ~ modified, fun.aggregate = sum)
  rownames(cast_trips) <- cast_trips$ftid
  cast_trips$ftid <- NULL
  
  if(message == "YES") cat("calculating hellinger distance\n")
  foo <- decostand(cast_trips, "hellinger")
  hellinger_dist <- as.matrix(vegdist(decostand(cast_trips, "hellinger"), "euclidean"))
  hellinger_sim <- max(hellinger_dist) - hellinger_dist
  
  if(message == "YES") cat("build network\n")
  hellinger_net <- graph.adjacency(hellinger_sim, mode="undirected", weighted = TRUE)
  
  link_list <- as.data.frame(get.edgelist(hellinger_net))
  levels(link_list[,1]) <- levels(link_list[,2])
  link_list[,1] <- as.integer(link_list[,1])
  link_list[,2] <- as.integer(link_list[,2])
  
  link_list$weight <- E(hellinger_net)$weight
  
  if(message == "YES") cat("writing link list output file\n")
  write.table(link_list, file=paste0("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-20/",gear_group,year,".txt"),row.names = FALSE, col.names = FALSE,sep=" ")
}

now = Sys.time()
make_link_list(tickets=tickets, year = args[2], gear_group = args[1], message = "YES")
Sys.time() - now
