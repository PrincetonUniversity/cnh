#link lists for input gear and year
args <- commandArgs(trailingOnly = TRUE)

library = "/tigress/efuller/R_packages"
library(permute, lib.loc = library)
library(dplyr, lib.loc = library)
library(igraph, lib.loc = library)
library(vegan, lib.loc = library)
library(reshape2, lib.loc = library)

ftl <- readRDS("/tigress/efuller/raw_infoMap/filtered_ftl.RDS")

tickets = ftl

make_link_list <- function(tickets, year, gear_group, message = "NO"){
  
  if(message == "YES") cat("format trips correctly\n")
  port_trips <- tickets[tickets[["pacfin_year"]] == year & 
                          tickets[["pacfin_group_gear_code"]] == gear_group, ]
  melt_trips <- melt(port_trips, 
                     id.vars = c("drvid","trip_id","modified","landing_date","pacfin_group_gear_code"), 
                     measure.vars = "adj_revenue")
  cast_trips <- dcast(melt_trips, 
                      trip_id ~ modified, fun.aggregate = sum)
  rownames(cast_trips) <- cast_trips$trip_id
  cast_trips$trip_id <- NULL
  
  if(message == "YES") cat("calculating hellinger distance\n")
  hellinger_dist <- as.matrix(vegdist(decostand(cast_trips, "hellinger"), "euclidean"))
  hellinger_sim <- sqrt(2) - hellinger_dist #sqrt(2) is max of hellinger
  # are some rounding errors, so make anything < 0, 0 
  # http://stackoverflow.com/questions/19444674/approximation-rounding-errors-in-r-in-simple-situations
  hellinger_sim[which(hellinger_sim < 0)] <- 0
  
  if(message == "YES") cat("build network\n")
  hellinger_net <- graph.adjacency(hellinger_sim, mode="undirected", weighted = TRUE)
  
  link_list <- as.data.frame(get.edgelist(hellinger_net))
  levels(link_list[,1]) <- levels(link_list[,2])
  key = data.frame(trip_id=levels(link_list[,1]))
  key$node = as.integer(key$trip_id)
  link_list[,1] <- as.integer(link_list[,1])
  link_list[,2] <- as.integer(link_list[,2])
  
  link_list$weight <- E(hellinger_net)$weight
  
  if(message == "YES") cat("writing link list output file\n")
  write.table(link_list, file=paste0("/tigress/efuller/raw_infoMap/",gear_group,year,".txt"),row.names = FALSE, col.names = FALSE,sep=" ")
  # write also the levels and integers to be able to figure out their order
  write.csv(key, file=paste0("/tigress/efuller/raw_infoMap/",gear_group,year,"key.txt"), row.names=FALSE)
}

make_link_list(tickets=tickets, year = args[2], gear_group = args[1], message = "YES")

